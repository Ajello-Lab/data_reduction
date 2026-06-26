;+
; NAME:
;  occpopspos
; PURPOSE:   (one line only)
;  Update star positions after WCS generated for occultation datasets
; DESCRIPTION:
; CATEGORY:
;  Occultations
; CALLING SEQUENCE:
;  occpopspos
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
;-
pro occpopspos

   pname='occpopspos'

   openmysql,dblun,'occlc'
   c=','
   dirty=0

;   getddir,cinfo,root
   loadini,cinfo
   getvalue,cinfo,'global','event',event

   cmd=['select info.idx,count(*)', $
              'from info,stars', $
              'where info.idx=stars.imidx', $
              'and stars.event='+quote(event), $
              'and stars.ra is NULL', $
              'and (qual != '+quote('bad')+' or qual is NULL)', $
              'and wcs=1', $
              'group by idx', $
              ';']
   print,cmd
   mysqlquery,dblun,cmd,idx,nstars,format='l,l',ngood=nframes
   if nframes eq 0 then begin
      print,'Nothing to process'
      goto,bailout
   endif else begin
      print,strn(nframes),' to process.'
   endelse

   lastteam=''
   for i=0,nframes-1 do begin

      cmd=['select team,stemdir,filename,event,binfac', $
           'from info where idx='+strn(idx[i])+';']
      mysqlquery,dblun,cmd,team,stemdir,filename,event,binfac,format='a,a,a,a,i'

      if lastteam ne team then begin
         getddir,cinfo,root,team=team
         lastteam=team
      endif
      ddir=root+stemdir

      if binfac eq 1 then begin
         fnim=ddir+filename
      endif else begin
         fnim=ddir+'binned_'+strn(binfac)+'/'+filename
      endelse

      hdr=headfits(fnim)
      astinfo,hdr,tinfo,error,/silent
      if error then begin
         print,fnim
         print,'no astrometry in image header'
         goto,bailout
      endif

      cmd=['select idx,xref,yref,x,y from stars', $
           'where imidx='+strn(idx[i]), $
           ';']
      mysqlquery,dblun,cmd,stidx,xref,yref,xsrc,ysrc,format='l,i,i,f,f',ngood=nsrcs

      astcvt,'xy',xsrc,ysrc,tinfo,'rd',ra,dec

      for j=0,nsrcs-1 do begin
         cmd=['update stars set', $
              'ra='+strn(ra[j],format='(f20.14)')+c, $
              'decl='+strn(dec[j],format='(f20.14)'), $
              'where idx='+strn(stidx[j]), $
              ';']
;         print,cmd
         mysqlcmd,dblun,cmd
         dirty=1
      endfor
;goto,bailout

   endfor

bailout:
   if dirty then begin
      action='star positions updated'
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
