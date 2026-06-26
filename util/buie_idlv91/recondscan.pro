;+
; NAME:
;  recondscan
; PURPOSE:   (one line only)
;  Scan for RECON data for a given event
; DESCRIPTION:
; CATEGORY:
;  Occultations
; CALLING SEQUENCE:
;  recondscan
; INPUTS:
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;  SHOWNEW - Flag, if set only reports on new datasets that are not already
;              posted in the current config.ini file.  Note that this logic
;              is not particularly smart.  If there is a section for a team
;              then anything for that team is considered to be old.  This is
;              really looking to see if there is data from new teams.
; VERBOSE - Flag, if set will generate more output to the screen as it works.
; OUTPUTS:
;  Two file are written, newconfig.ini and newsites.dat that contain the
;    formatted information that may be useful for incorporation into the
;    related files.
; KEYWORD OUTPUT PARAMETERS:
; CONFIGURATION:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  Written by Marc W. Buie, Southwest Research Institute, 2021/01/31
;  2023/02/07, MWB, changed output for new sites.dat file
;  2024/03/06, MWB, added VERBOSE keyword flag
;  2024/03/08, MWB, added check for missing config.ini file
;-
pro recondscan,SHOWNEW=shownew,VERBOSE=verbose

   self='recondscan: '
   if badpar(shownew,[0,1,2,3],0,caller=self+'(SHOWNEW) ',default=0) then return
   if badpar(verbose,[0,1,2,3],0,caller=self+'(VERBOSE) ',default=0) then return

   if nofile('config.ini','Configuration File') then return

   loadini,info

   getvalue,info,'global','event',event
   getvalue,info,'global','date',date
   getvalue,info,'global','dir',subdir,default=''
   getvalue,info,'ddir','',tdir
   print,'Scanning for data for event ',event,' on ',date
   print,'Scanning the following directories'

   openw,lun1,'newconfig.ini',/get_lun
   openw,lun2,'newsites.dat',/get_lun
   openw,lun3,'new'+event+'.toc',/get_lun,width=132

   for i=0,n_elements(tdir)-1 do begin
      print,'*** ',tdir[i]
      if verbose then print,'Scan ',addslash(tdir[i])+subdir,' for data on ',date
      dirlist=file_search(addslash(tdir[i])+subdir,date,/test_directory,count=ndir)
      if verbose then print,strn(ndir),' userful directories found.'
      for j=0,ndir-1 do begin
         if verbose then print,dirlist[j]
         ddir=strmid(dirlist[j],0,strlen(dirlist[j])-strlen(date))
         scindex,date,dinfo,ddir=ddir,silent=verbose eq 0
         stemdir=strmid(ddir,strlen(addslash(tdir[i])))

         words=strsplit(stemdir,'/',/extract)
         team=words[-1]
         if shownew then begin
            getvalue,info,team,'dirtime',dirtime
            if dirtime ne '' then continue
         endif

         if dinfo.nobs gt 0 then begin
            print,stemdir
            print,dinfo.str
            printf,lun3,stemdir
            printf,lun3,dinfo.str

            if dinfo.nobs eq 1 then begin
               pick=0
            endif else begin
               sel=picker(dinfo.str,index=pick)
               if sel eq '[[[CANCEL]]]' then continue
            endelse

            printf,lun1,'[',team,']'
            printf,lun1,'dirtime = ',dinfo.folder[pick]
            printf,lun1,'stemdir = ',addslash(stemdir)+date
            printf,lun1,''

            tag='       x        x            x        0.    0.'

            printf,lun2,team,dinfo.lat[pick],dinfo.lon[pick], $
               dinfo.alt[pick], $
               format='(a-10,1x,"x",1x,f10.6,1x,f11.6,1x,f6.1)'
         endif
      endfor
   endfor

   free_lun,lun1,lun2,lun3

end
