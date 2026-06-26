;+
; NAME:
;  col2teff
; PURPOSE:   (one line only)
;  Find a black-body temperature for a star based on a photometric color
; DESCRIPTION:
; CATEGORY:
;  Astronomy
; CALLING SEQUENCE:
;  col2teff,wv,zp,color,teff
; INPUTS:
;  wv - two element vector with the pivot wavelength for each filter in the
;         color.  Example, color could be (V-R) so you provide the wavelength
;         for V and R.  Units are in microns.
;  zp - System photometric Vegamag constant for each filter in the color.
;  color - color magnitude that matches wv and zp
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
; OUTPUTS:
;  teff - Black-body temperature that will reproduce the color.
; KEYWORD OUTPUT PARAMETERS:
; CONFIGURATION:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  Written by Marc W. Buie, Southwest Research Institute, 2021/09/01
;-
pro col2teff,wv,zp,color,teff,DEBUG=debug

   self='col2teff: '
   if badpar(wv,[4,5],1,caller=self+'(wv) ') then return
   if badpar(zp,[4,5],1,caller=self+'(zp) ') then return
   if badpar(color,[4,5],0,caller=self+'(color) ') then return
   if badpar(debug,[0,1,2,3],0,caller=self+'(DEBUG) ',default=0) then return

   if debug then print,'fluxes ',zp[0],zp[1],' and color ',color

   fr = zp[0]/zp[1]*10^(color/(-2.5))

   if debug then print,'flux ratio = ',fr

   wave=wv*10000.0
   temp0=5800.0
   if debug then print,'search down'
   repeat begin
      temp0=temp0-1000.0
      bbflux=planck(wave,temp0)
      fr0=bbflux[0]/bbflux[1]
      if debug then print,temp0,fr0
   endrep until fr0 lt fr or temp0 lt 1000.0

   temp1=5800.0
   if debug then print,'search up'
   repeat begin
      temp1=temp1+1000.0
      bbflux=planck(wave,temp1)
      fr1=bbflux[0]/bbflux[1]
      if debug then print,temp1,fr1
   endrep until fr1 gt fr or temp1 gt 15000.0

   i=0
   if debug then print,'binary search'
   if temp0 gt 1000.0 and temp1 lt 15000.0 then begin
      newtemp=0.
      repeat begin
         lasttemp=newtemp
         newtemp = (temp0+temp1)/2.0
         bbflux=planck(wave,newtemp)
         newfr=bbflux[0]/bbflux[1]
         if debug then print,i,temp0,fr0,temp1,fr1,newtemp,newfr
         if newfr ge fr then begin
            temp1=newtemp
            fr1=newfr
         endif else begin
            temp0=newtemp
            fr0=newfr
         endelse
         i++
      endrep until abs(fr-newfr)/fr lt 0.001 or abs(newtemp-lasttemp) lt 1.0
      teff = (temp0+temp1)/2.0
   endif else begin
      teff=6000.0 ; arbitrary
      newfr=0.
   endelse

   if debug then print,teff,newfr

end
