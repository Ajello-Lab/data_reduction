;+
; NAME:
;  occbuildpsf
; PURPOSE:   (one line only)
;  Build a stacked PSF for images in an occultation dataset
; DESCRIPTION:
; CATEGORY:
;  Occultations
; CALLING SEQUENCE:
;  occbuildpsf
; INPUTS:
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;  NODBUPDATE - Flag, if set suppressess saving information to the database
;  HIDDEN  - Flag, if set let's the program use the Z buffer instead of
;               and active X display.  Most effective for working over a
;               slow network and you don't really need to see what it's doing.
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
;  2022/01/22, MWB, add HIDDEN keyword
;-
pro occbuildpsf,NODBUPDATE=nodbupdate,MAXTODO=maxtodo,HIDDEN=hidden

   pname='occbuildpsf'
   self=pname+': '
   if badpar(hidden,[0,1,2,3],0,caller=self+'(HIDDEN) ', $
                                     default=0) then return
   if badpar(nodbupdate,[0,1,2,3],0,caller=self+'(NODBUPDATE) ', $
                                    default=0) then return
   if badpar(maxtodo,[0,2,3],0,caller=self+'(MAXTODO) ', $
                               default=0) then return

   d_name=!d.name
   ndone=0
   c=','
   openmysql,dblun,'occlc'

   if nofile('config.ini','Configuration file') then return

;   getddir,cinfo,root
   loadini,cinfo
   getvalue,cinfo,'global','event',event

   cmd=['select info.idx,team,stemdir,filename,fwhm,binfac', $
        'from info,stars', $
        'where info.idx=stars.imidx', $
        'and stars.event='+quote(event)]
   if not nodbupdate then $
      cmd=[cmd,'and status='+quote('needpsf')]
   cmd=[cmd, $
        'and (qual!='+quote('bad')+'or qual is NULL)', $
        'group by info.idx']
   if maxtodo gt 0 then $
      cmd=[cmd,'limit '+strn(maxtodo)]
   cmd=[cmd,';']
   mysqlquery,dblun,cmd,idx,team,stemdir,filename,fwhm,binfac, $
      format='l,a,a,a,f,i',ngood=nfiles
   if nfiles eq 0 then begin
      print,cmd
      print,'Nothing found that needs processing.'
      goto,bailout
   endif

   lastteam=''
   for i=0,nfiles-1 do begin

      if lastteam ne team[i] then begin
         getddir,cinfo,root,team=team[i]
         lastteam=team[i]
      endif

      ddir=root+stemdir[i]

      psfdir='PSF/'+team[i]+'/'
      if binfac[i] eq 1 then begin
         fnpsf=psfdir+filename[i]+'.psf'
         fnpng=psfdir+filename[i]+'.png'
         fnim=ddir+filename[i]
      endif else begin
         fnpsf=psfdir+filename[i]+'.'+strn(binfac[i])+'.psf'
         fnpng=psfdir+filename[i]+'.'+strn(binfac[i])+'.png'
         fnim=ddir+'binned_'+strn(binfac[i])+'/'+filename[i]
      endelse

      if not exists(psfdir) then file_mkdir,psfdir
      if exists(fnpsf) then continue

      cmd=['select xref,yref', $
           'from stars', $
           'where imidx='+strn(idx[i])+';']
print,cmd

      mysqlquery,dblun,cmd,xref,yref, $
         format='i,i',ngood=nstars
      print,'Image index ',strn(idx[i]),'  Team:',team[i],', event ',event
      print,strn(nstars),' stars for PSF, fwhm=',fwhm[i]
      print,ddir+filename[i]

      im=float(readfits(fnim))
      backsub,im,/row
      psfstack,im,xref,yref,psf,dw=2*round(fwhm[i]) ; ,/silent

      sz=size(psf,/dimen)
      nx=sz[0]
      ny=sz[1]
      zf=4
      if hidden then begin
         set_plot,'Z'
         device,decomposed=1,z_buffering=0, $
            set_resolution=[nx*4,ny*4],set_pixel_depth=24
      endif else begin
         setwin,0,xsize=nx*4,ysize=ny*4
      endelse
      tvscl,rebin(psf,nx*4,ny*4,/sample)

      writefits,fnpsf,psf
      if hidden then begin
         tvgrab,fnpng,-1,/png
      endif else begin
         tvgrab,fnpng,0,/png
      endelse
      print,'Saving ',fnpsf

      if not keyword_set(nodbupdate) then begin
         cmd='update stars set status='+quote('refit')+ $
             ' where imidx='+strn(idx[i])+';'
         mysqlcmd,dblun,cmd
      endif

      ndone++

   endfor

bailout:
   if ndone gt 0 then begin
      action='Built PSFs for '+strn(ndone)+' images.'
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
   set_plot,d_name

end
