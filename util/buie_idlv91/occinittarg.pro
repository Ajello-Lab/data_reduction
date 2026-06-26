;+
; NAME:
;  occinittarg
; PURPOSE:   (one line only)
;  Initialize the target star position for an occultation dataset
; DESCRIPTION:
; CATEGORY:
;  Occultations
; CALLING SEQUENCE:
;  occinittarg
; INPUTS:
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
; OUTPUTS:
; KEYWORD OUTPUT PARAMETERS:
; CONFIGURATION:
;   File: config.ini
;   [global]
;     event   - string with short ID name of event (ex: OR20200914)
;   [ddir]   - section to list potential root directories to find data
;     XXX - <value>
;       XXX is a unique string (eg., D01)
;       <value> is a directory path
;       for additional details see getddir.pro
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  Written by Marc W. Buie, Southwest Research Institute, 2020/09/24
;  2021/12/03, MWB, fixed a nasty bug with the cross reference between the
;                     info and target tables.  Values output were correct
;                     but the output was incomplete.
;-
pro occinittarg

   pname='occinittarg'
   self=pname+': '
   openmysql,dblun,'occlc'
   c=','
   dirty=0

;   getddir,cinfo,root
   loadini,cinfo
   getvalue,cinfo,'global','event',event

   cmd=['select idx,filename,team,stemdir,binfac from info', $
        'where event='+quote(event), $
        'and wcs=1', $
        'and (qual !='+quote('bad')+' or qual is NULL);']
   print,cmd
   mysqlquery,dblun,cmd,idx,fn,team,stemdir,binfac, $
      format='l,a,a,a,i',ngood=nfiles

   cmd='select geomid,ora,odec from campaign where event='+quote(event)+';'
   mysqlquery,dblun,cmd,geomid,ra,dec,format='a,d,d',ngood=ncheck
   if ncheck ne 1 then begin
      print,cmd
      print,'campaign db entry not found.'
      goto,bailout
   endif

   if nfiles eq 0 then begin
      print,'No files found in database for event ',event
      goto,bailout
   endif

   print,strn(nfiles),' source files found to be loaded.'

   ndone=0L
   lastteam=''
   for i=0,nfiles-1 do begin

      cmd='select count(*) from target where imidx='+strn(idx[i])+';'
      mysqlquery,dblun,cmd,ncheck
      if ncheck ne 0 then continue

      if lastteam ne team[i] then begin
         getddir,cinfo,root,team=team[i]
         lastteam=team[i]
      endif
      ddir=root+stemdir[i]
      if binfac[i] eq 1 then begin
         fnim=ddir+fn[i]
      endif else begin
         fnim=ddir+'binned_'+strn(binfac[i])+'/'+fn[i]
      endelse
      print,fnim
      if not exists(fnim) then begin
         print,'Image file not found'
         goto,bailout
      endif
      hdr=headfits(fnim)
      astinfo,hdr,info,error
      if error then begin
         print,'Image does not have astrometry information'
         goto,bailout
      endif

      astcvt,'rd',ra,dec,info,'xy',x,y

      cmd=['insert into target set', $
           'imidx='+strn(idx[i])+c, $
           'event='+quote(event)+c, $
           'x='+strn(x,format='(f10.3)')+c, $
           'y='+strn(y,format='(f10.3)')+c, $
           'status='+quote('refit')+';']
;print,cmd
      mysqlcmd,dblun,cmd
      dirty=1
      ndone++
;if i ge 10 then break
   endfor

bailout:
   if dirty then begin
      action='Initialized target position for '+strn(ndone)+ $
             ' files out of '+strn(nfiles)+'.'
      jdcur=systime(/julian,/ut)
      jdstr,jdcur,300,jds
      cmd=['insert into history set', $
           'posted='+jds+c, $
           'event='+quote(event)+c, $
           'tool='+quote(pname)+c, $
           'action='+quote(action)+';']
      print,cmd
      mysqlcmd,dblun,cmd
   endif
   free_lun,dblun
   

end
