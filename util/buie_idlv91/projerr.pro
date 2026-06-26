;+
; NAME:
;  projerr
; PURPOSE:   (one line only)
;  Re-project an error ellipse onto a new coordinate system
; DESCRIPTION:
;  The new coordinate system is rotated with respect to the one natural
;     to the error ellipse.
; CATEGORY:
;  Astrometry
; CALLING SEQUENCE:
;  projerr,smajor,sminor,pang,rotang,rsmajor,rsminor
; INPUTS:
;  smajor - Length of the semi-major axis of the error ellipse (1 sigma)
;  sminor - Length of the semi-minor axis of the error ellipse (1 sigma)
;  pang  - Position angle of the major axis, measured eastward from
;            north [radians].
;  rotang - Position angle of the axis to decompose the error along.
;              measured eastward from north [radians].
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
; OUTPUTS:
;  intrack - 1-sigma in-track error component
;  xtrack  - 1-sigma cross-track error component
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  2019/07/12, Written by Marc W. Buie, Southwest Research Institute
;-
pro projerr,smajor,sminor,in_pang,in_rotang,intrack,xtrack,DEGREES=degrees

   self='roterr: '
   if badpar(smajor,[2,3,4,5],0,caller=self+'(smajor) ') then return
   if badpar(sminor,[2,3,4,5],0,caller=self+'(sminor) ') then return
   if badpar(in_pang,[2,3,4,5],0,caller=self+'(pang) ') then return
   if badpar(in_rotang,[2,3,4,5],0,caller=self+'(rotang) ') then return
   if badpar(degrees,[0,1,2,3],0,caller=self+'(DEGREES) ',default=0) then return

   if degrees then begin
      pang = in_pang*!dpi/180.0d0
      rotang = in_rotang*!dpi/180.0d0
   endif else begin
      pang = in_pang
      rotang = in_rotang
   endelse

   nsamp=10000

   dy=randomn(seed,nsamp)*smajor
   dx=randomn(seed,nsamp)*sminor
   dz=replicate(0.0d0,nsamp)

   rotpoint,dx,dy,dz,'z',-pang,dxp,dyp,dzp

   mx=max([max(abs(dxp)),minmax(abs(dyp))])
   xr=[mx,-mx]
   yr=[-mx,mx]

   rotpoint,[0.,0.],[0.,mx],[0.,0.],'z',-pang,px1,py1,pz1
   rotpoint,[0.,0.],[0.,mx],[0.,0.],'z',-rotang,px2,py2,pz2

   setwin,0
   plot,dxp,dyp,psym=3,/iso,xr=xr,yr=yr
   oplot,px1,py1,color=cpalette(1)
   oplot,px2,py2,color=cpalette(2)
   xyouts,0.15,0.13,'Motion',/normal,color=cpalette(2)
   xyouts,0.15,0.1,'Error',/normal,color=cpalette(1)

   rotpoint,dxp,dyp,dzp,'z',rotang,dxpp,dypp,dzpp

   setwin,1
   plot,dxpp,dypp,psym=3,/iso,xr=xr,yr=yr

   intrack = stddev(dypp)
   xtrack  = stddev(dxpp)

   print,'in-track',intrack
   print,' x-track',xtrack

end
