;+
; NAME:
;   reconload
; PURPOSE:   (one line only)
; DESCRIPTION:
;  The RECON data repository is scanned.  Files already in the database are
;    not examined.  New entries into the database contain as much information
;    as can be deduced at the start.  If the file names are properly
;    constructed the UTdate will be set, otherwise left blank.  Many fields
;    will require subsequent processing.
; CATEGORY:
;  Database
; CALLING SEQUENCE:
;  reconload,DIR=dir,MAXTODO=maxtodo,OLD=old
; INPUTS:
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;  DIR - base directory of data repository, default is
;           /net/mandor/raid/buie/
;  MAXTODO - mostly for testing, if set will restrict this program to process
;              no more than MAXXTODO files.
;  OLD     - Flag, if set, indicates that the old video data repository should
;              be processed.  This is found in DIR+'recon/'.  Otherwise
;              the new repository is scanned in DIR+'RECON/'.
; OUTPUTS:
;  All output goes to the database.  Files that are already in the database
;   are skipped.
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  2016/03/08, Written by Marc W. Buie, Southwest Research Institute
;  2016/03/10, MWB, added check sum fields to database insert command
;  2016/03/11, MWB, added email notification
;  2016/03/16, MWB, added senseup field
;-
pro reconload,DIR=dir,MAXTODO=maxtodo,OLD=old

   self='reconload: '
   if badpar(dir,[0,7],0,caller=self+'(DIR) ', $
             default='/net/mandor/raid/buie/') then return
   if badpar(maxtodo,[0,2,3],0,caller=self+'(MAXTODO) ', $
             default=0) then return
   if badpar(old,[0,1,2,3],0,caller=self+'(OLD) ', $
             default=0) then return

   if not exists(dir) then begin
      print,dir
      print,self,'ERROR!  Directory does not exist.'
      return
   endif

   jdstr,systime(/julian,/ut),0,str
   msg=['RECONLOAD: video data database load', $
        'Execution started at '+str+' UT','']
   print,self,' starting execution at ',str

   dir=addslash(dir)
   if old then fdir=dir+'recon/' else fdir=dir+'RECON/'

   fnlist=file_search(fdir,'*.avi',count=nfiles)
   if nfiles eq 0 then fnlist=[]
   fnlist2=file_search(fdir,'*.AVI',count=nfiles2)
   if nfiles2 gt 0 then begin
      fnlist=[fnlist,fnlist2]
      nfiles += nfiles2
   endif

   if nfiles eq 0 then begin
      print,fdir
      print,self,'No avi files found to load.'
      return
   endif

   openmysql,dblun,'recon'
   c=','

   nbroken=0
   nupdate=0
   nadded=0
   for i=0,nfiles-1 do begin
      pos=strpos(fnlist[i],'/',/reverse_search)
      vdir=strmid(fnlist[i],0,pos+1)
      vfile=strmid(fnlist[i],pos+1)
      vdir=strmid(vdir,strlen(dir))

      ; check to see if already in the database, if so, skip
      cmd='select idx,broken from data' + $
          ' where old='+strn(old) + $
          ' and dir='+quote(vdir) + $
          ' and filename='+quote(vfile)+';'
      mysqlquery,dblun,cmd,fileidx,broken,ngood=ncheck,format='l,i'
      update = 0
      if ncheck eq 1 then begin
         if broken then begin
            nbroken++
            finfo=file_info(fnlist[i])
            cmd='select ctime,mtime,filesize from data where idx='+ $
                   strn(fileidx)+';'
            mysqlquery,dblun,cmd,ctime,mtime,filesize,format='l,l,ll'
;if fileidx eq 709 then begin
;print,ctime,mtime,filesize
;help,finfo
;endif
            if finfo.mtime ne mtime or finfo.size ne filesize then begin
               update=trimrank(fileidx)
            endif else begin
               continue
            endelse
         endif
         if update eq 0 then continue
      endif

print,fnlist[i]
      vidinfo,fnlist[i],vinfo,error
      if error then begin
         msg=[msg,fnlist[i], $
                  'ERROR encountered getting video information']
         goto,bailout
      endif
      if vinfo.mode eq '600' then begin
         msg=[msg,fnlist[i], $
              'Skipping, bad file mode (600).']
         continue
      endif
      if systime(/seconds)-vinfo.mtime lt 3600.0 then begin
         msg=[msg,fnlist[i], $
              'Skipping, less than an hour old.']
         continue
      endif

      subdir=strmid(vinfo.dir,strlen(dir))
      sitename=strmid(vinfo.dir,strlen(fdir))
      sitename=strmid(sitename,0,strlen(sitename)-1)
      pos=strpos(sitename,'/')
      if pos gt 0 then sitename=strmid(sitename,0,pos)

      if update eq 0 then begin
         cmd='select code from sites where dirname='+quote(sitename)+';'
         mysqlquery,dblun,cmd,sitecode,ngood=ncheck
         if ncheck ne 1 then begin
            msg=[msg,cmd, $
                    'Bad query for site code']
            goto,bailout
         endif
         ; figure out the utdate.
         utdate='NULL'  ; default if I can't figure it out
         if old then begin
            words=strsplit(vinfo.filename,'.',/extract)
            fnroot=strmid(words[-2],0,8)
            utdate=fnroot
            if string(long('0'+utdate),format='(i8.8)') ne utdate then $
                  utdate='NULL'
         endif else begin
            words=strsplit(vinfo.filename,'.',/extract)
            utdate=strmid(vinfo.filename,0,8)
            if string(long('0'+utdate),format='(i8.8)') ne utdate then $
                  utdate='NULL'
         endelse

      endif

      if vinfo.nframes eq vinfo.cframes then begin
         broken=0
      endif else begin
         if abs(vinfo.nframes - vinfo.cframes) lt 30 then begin
            broken=0
         endif else begin
            broken=1
         endelse
      endelse
      if vinfo.nframes eq 0 and vinfo.cframes eq 0 then begin
         broken=1
      endif

      if update gt 0 then begin

         cmd=['update data set', $
              'filetime='+quote(vinfo.datetime)+c, $
              'ctime='+strn(vinfo.ctime)+c, $
              'mtime='+strn(vinfo.mtime)+c, $
              'filesize='+strn(vinfo.size)+c, $
              'cksum=NULL,', $
              'chkdate=NULL,', $
              'mode='+strn(vinfo.mode)+c, $
              'duration='+strn(vinfo.length)+c, $
              'nframes='+strn(vinfo.nframes)+c, $
              'cframes='+strn(vinfo.cframes)+c, $
              'xsize='+strn(vinfo.width)+c, $
              'ysize='+strn(vinfo.height)+c, $
              'codec='+quote(vinfo.codec)+c, $
              'pixfmt='+quote(vinfo.pixfmt)+c, $
              'broken='+strn(broken), $
              'where idx='+strn(update), $
              ';']

      endif else begin

         cmd=['insert into data values(', $
             'NULL,', $ ; idx (auto incremented and set automatically)
             quote(sitecode)+c, $
             strn(old)+c, $
             quote(subdir)+c, $
             quote(vinfo.filename)+c, $ 
             quote(vinfo.datetime)+c, $
             strn(vinfo.ctime)+c, $
             strn(vinfo.mtime)+c, $
             strn(vinfo.size)+c, $
             'NULL,', $ ; checksum (to be filled in later)
             'NULL,', $ ; checksum generation time (to be filled in later)
             quote(vinfo.mode)+c, $
             'NULL,', $ ; catid (type of data in the video file)
             utdate+c, $
             'NULL,', $ ; utstart
             'NULL,', $ ; utend
             'NULL,', $ ; evidx
             'NULL,', $ ; jdstart
             strn(vinfo.length)+c, $
             strn(vinfo.nframes)+c, $
             strn(vinfo.cframes)+c, $
             '0,', $ ; fieldchk initial value
             strn(vinfo.width)+c, $
             strn(vinfo.height)+c, $
             quote(vinfo.codec)+c, $
             quote(vinfo.pixfmt)+c, $
             'NULL,', $ ; SENSEUP
             'NULL,', $ ; xtarg initial value
             'NULL,', $ ; ytarg initial value
             quote('U')+c, $ ; focus initial value
             '-1,', $ ; event initial value
             '0,', $ ; reversed, default value
             strn(broken)+c, $
             quote(' '), $
             ');']

      endelse
;print,cmd
      mysqlcmd,dblun,cmd

      if update eq 0 then utag='N' else utag='U'
      str=string(sitename,utag,vinfo.filename,vinfo.length,vinfo.width, $
                 vinfo.height,vinfo.codec, $
                 format='(a-12,1x,a,1x,a-40,1x,f6.1,2(1x,i3),1x,a)')
      msg=[msg,str]

      if update eq 0 then nadded++ else nupdate++
      if maxtodo gt 0 and (nadded+nupdate) ge maxtodo then break
   endfor

bailout:
   free_lun,dblun

   msg=[msg,'',strn(nadded)+' records added.']
   msg=[msg,strn(nupdate)+' records updated.']
   if nbroken gt 0 then $
      msg=[msg,strn(nbroken)+' broken records seen.']
   jdstr,systime(/julian,/ut),0,str
   msg=[msg,'','Execution completed at '+str+' UT']
   for i=0,n_elements(msg)-1 do print,msg[i]

   if nadded gt 0 or nupdate gt 0 then begin
      sendto='buie@boulder.swri.edu'
      ccto=['john.m.keller@colorado.edu']
      subject='[RECONLOAD] '
      if nadded ne 0 then subject=subject+strn(nadded)+' records added '
      if nupdate ne 0 then subject=subject+strn(nupdate)+' records updated '
      mailmsg,sendto,subject,msg,ccaddr=ccto
   endif

end
