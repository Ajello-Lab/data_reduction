;+
; NAME:
;  recontupd
; PURPOSE:   (one line only)
;  Update start and ending times for RECON event video files in the database
; DESCRIPTION:
;  The recon.data database is queried for all event videos (catid='E')
;    whose utdate is known and utstart or utend is still set to NULL.
;    Broken video files are skipped.
;  For those videos scanned, the beginning and end of the video is extracted
;    to determine the start and stop time of the video.  The location of the
;    video end is expected to be consistent with the video duration posted
;    in the database.  If the file ends early that file will be marked
;    broken in the database and utend will be left as NULL.
; CATEGORY:
;  Database
; CALLING SEQUENCE:
;  recontupd
; INPUTS:
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;  DT - integer number of seconds to skip into the file to read the starting
;         time code.  The default is 0 which will lead to decoding the second
;         video frame.  This should be used sparingly and usually when there
;         is a single stubborn frame remaining to be processed.  If used,
;         the starting time is estimated from the frame read, pushed backward
;         by the offset, DT, used.
;  IDX - If provided, turns on a special mode and just a single database
;         entry is scanned.  In this mode, the usually email report is
;         disabled.
;  MAXTODO - mostly for testing, if set will restrict this program to process
;              no more than MAXXTODO records.
; OUTPUTS:
;  all output is to the recon.data database table
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  2016/03/06, Written by Marc W. Buie, Southwest Research Institute
;  2016/03/11, MWB, added mail notification and MAXTODO keyword
;  2016/03/25, MWB, added IDX keyword
;-
pro recontupd,DT=dt,MAXTODO=maxtodo,IDX=oneidx

   self='recontupd: '
   if badpar(dt,[0,2,3],0,caller=self+'(DT) ',default=0) then return
   if badpar(maxtodo,[0,2,3],0,caller=self+'(MAXTODO) ',default=0) then return
   if badpar(oneidx,[0,2,3],0,caller=self+'(IDX) ',default=0) then return

   droot='/net/mandor/raid/buie/'

   jdstr,systime(/julian,/ut),0,str

   print,self,'execution starting at ',str

   openmysql,dblun,'recon'
   c=','

   if oneidx gt 0 then begin
      cmd='select idx,dir,filename,utdate,utstart,utend,duration from data'+ $
          ' where idx='+strn(oneidx)+';'
   endif else begin
      cmd='select idx,dir,filename,utdate,utstart,utend,duration from data'+ $
          ' where catid='+quote('E')+ $
          ' and broken=0'+ $
          ' and utdate is not null'+ $
          ' and (utstart is null or utend is null);'
   endelse
   if oneidx gt 0 then print,cmd
   mysqlquery,dblun,cmd,idx,dir,filename,utdate,utstart,utend,duration, $
      format='l,a,a,a,a,a,f',ngood=nupdates

   if nupdates eq 0 then goto,bailout

   ngood=0
   nbad=0
   msg=['RECONTUPD: video time update program', $
        'Execution started at '+str+' UT', $
        strn(nupdates)+' entries found that need updating.']
   if dt gt 0 then $
      msg=[msg,'Using DT='+strn(dt)+' for start timing information.']

   for i=0,nupdates-1 do begin
      fn=droot+dir[i]+filename[i]
      if not exists(fn) then begin
         msg=[msg, $
              fn+', file not found.']
         nbad++
         continue
      endif

;      cmd1='unsetenv LD_LIBRARY_PATH ;'+ $
;           ' ffmpeg -loglevel quiet '+ $
;           '-ss '+strn(dt)+' -i "'+fn+'" -vframes 2 rtmp0.%04d.png'
      cmd1='ffmpeg -loglevel quiet '+ $
           '-ss '+strn(dt)+' -i "'+fn+'" -vframes 2 rtmp0.%04d.png'
      if oneidx gt 0 then print,cmd1
      spawn,cmd1,results
      if oneidx gt 0 then for j=0,n_elements(results)-1 do print,results[j]
      fntmp=file_search('rtmp0.*.png',count=ntmp)
      fnpng='rtmp0.0002.png'
      if not exists(fnpng) then begin
         msg=[msg, $
              'ffmpeg extraction failed for '+fnpng]
         nbad++
         if ntmp gt 0 then for j=0,ntmp-1 do file_delete,fntmp[j]
         continue
      endif
      timeinfo,fnpng,otime,onum,etime,enum
      if oneidx gt 0 then print,'Start time: otime=',otime,', etime=',etime
      if onum gt enum then reversed=1 else reversed=0
      for j=0,ntmp-1 do file_delete,fntmp[j]
      if otime eq '' then begin
         msg=[msg,'Failed to decode start time from '+fn+' '+strn(idx[i])]
         nbad++
         continue
      endif
      jdstart=jdparse(utdate[i]+' '+otime)-double(dt)/86400.0d0
      if oneidx gt 0 then print,jdstart,format='(f13.5)'
      jdstr,jdstart,300,jds
      if oneidx gt 0 then print,'Start time ',jds
      utstart[i]=strmid(jds,8)

;      utstart[i]=strmid(otime,0,2)+strmid(otime,3,2)+strmid(otime,6,2)


      quiet='-loglevel quiet '
;      quiet=''
;      cmd2='unsetenv LD_LIBRARY_PATH ;'+ $
;           ' ffmpeg '+quiet+'-ss '+strn(fix(duration[i]-1))+ $
;           ' -i "'+fn+'" -vframes 100 rtmp1.%04d.png'
      cmd2='ffmpeg '+quiet+'-ss '+strn(fix(duration[i]-1))+ $
           ' -i "'+fn+'" -vframes 100 rtmp1.%04d.png'
      if oneidx gt 0 then print,cmd2
      spawn,cmd2,results
      if oneidx gt 0 then for j=0,n_elements(results)-1 do print,results[j]
      fntmp=file_search('rtmp1.*.png',count=ntmp)
      if oneidx gt 0 then print,strn(ntmp),' files decoded at the end'
      if ntmp eq 0 then begin
         msg=[msg,'Failed to get images at the end of file '+fn]
         nbad++
         broken=1
      endif else begin
         broken=0
         if oneidx gt 0 then print,'End file read ',fntmp[-1]
         timeinfo,fntmp[-1],otime,onum,etime,enum
         if oneidx gt 0 then print,'End   time: otime=',otime,', etime=',etime
         for j=0,ntmp-1 do file_delete,fntmp[j]
         if otime eq '' then begin
            msg=[msg,'Failed to decode end time from file '+fn]
            nbad++
            continue
         endif
         utend[i]=strmid(otime,0,2)+strmid(otime,3,2)+strmid(otime,6,2)
         ngood++
      endelse

;      if reversed then print,'Fields are reversed in video'

      cmd=['update data set', $
           'jdstart='+string(jdstart,format='(f13.5)')+c, $
           'utstart='+utstart[i]+c, $
           'reversed='+strn(reversed)+c]
      if broken then cmd=[cmd,'broken=1'] $
      else cmd=[cmd,'utend='+utend[i]]
      cmd=[cmd,'where idx='+strn(idx[i])+';']
      if oneidx gt 0 then print,cmd
      mysqlcmd,dblun,cmd

      msg=[msg, $
         string(idx[i],dir[i],filename[i],utdate[i],utstart[i],utend[i], $
         format='(i,5(1x,a))')]

      if maxtodo gt 0 and (ngood+nbad) ge maxtodo then break

   endfor

bailout:
   free_lun,dblun

   if n_elements(msg) gt 0 then begin
      msg=[msg,strn(ngood)+' records successfully updated.']
      if nbad gt 0 then $
         msg=[msg,strn(nbad)+' records that could not be updated.']
      jdstr,systime(/julian,/ut),0,str
      msg=[msg,'Execution completed at '+str+' UT']
      for i=0,n_elements(msg)-1 do print,msg[i]
      sendto='buie@boulder.swri.edu'
      ccto=['john.m.keller@colorado.edu']
      subject='[RECONTUPD] '+strn(ngood)+' records updated'
      if oneidx eq 0 then $
         mailmsg,sendto,subject,msg,ccaddr=ccto
   endif

end
