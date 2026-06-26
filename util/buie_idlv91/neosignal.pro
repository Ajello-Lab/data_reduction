;+
; NAME:
;  neosignal
; PURPOSE:   (one line only)
; DESCRIPTION:
; CATEGORY:
;  Asteroids
; CALLING SEQUENCE:
;  signal=neosignal(diam,albedo,sun,delta,phang)
; INPUTS:
;  diam - Diameter of object in km
;  albedo - geometric albedo
;  sun  - Heliocentric distance (AU)
;  delta - distance between observer and object (AU)
;  phang - observer-object-Sun angle (phase angle) in radians
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;  exptime - exposure time in seconds for integration (default=180)
; OUTPUTS:
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
;  This is not very generally written yet.  There are many hardcoded values
;    such as the band pass of integration, telescope aperture, and more.
; PROCEDURE:
; MODIFICATION HISTORY:
;  2016/01/21, Written by Marc W. Buie, Southwest Research Institute
;-
function neosignal,diam,albedo,sun,delta,phang,VERBOSE=verbose,EXPTIME=exptime

   self='neosignal: '
   if badpar(exptime,[0,2,3,4,5],0,caller=self+'(EXPTIME) ', $
                                   default=180.0) then return,0
   if badpar(verbose,[0,1,2,3],0,caller=self+'(VERBOSE) ',default=0) then return

   npts=11
   wave0=5.0
   wave1=10.2
   wave=findgen(npts)/(npts-1)*(wave1-wave0)+wave0 ; microns
   wave_cm=wave*1.0d-4 ; cm
   planck=6.6262d-27 ; erg s
   c=2.997925e10 ; cm s-1
   deltat=30
   emiss=0.8

   freq=c/wave_cm ; Hz

   trans=replicate(0.85*0.885,npts)

   aperture = !pi*25.0^2 ; cm^2   (use radius here)

   ; assume a canonical phase coefficient, G=0.2
   ; convert from geometric to bolometric albedo
   g=0.2
   q=0.290 + 0.684 * g
   bondalb=albedo*q

   tneo=equtemp(sun,bondalb,emiss,/iso)
   tbb=tneo+deltat*cos(phang)

   flux=neoflux(diam,delta,wave,tbb)
   energy=emiss*flux*aperture*exptime ; erg

   ; convert to photons
   onephoton = planck*c/wave_cm
   photons = energy/onephoton*trans

   signal=int_tabulated(wave_cm,photons)

; crude approximation of integral
;print,(photons[-1]+photons[0])/2.0 * (freq[0]-freq[-1])*trans[0]

   if verbose then begin
      print,'Object diameter          ',diam,' km'
      print,'Geometric albedo         ',albedo
      print,'Bond albedo              ',bondalb
      print,'Object-Sun distance      ',sun,' AU'
      print,'Object-Observer distance ',delta,' AU'
      print,'Solar phase angle        ',phang,' radians'
      print,'                         ',phang*!radeg,' degrees'
      print,'Wavelength range',wave0,wave1,' microns'
      print,'Wavelength range',min(wave_cm),max(wave_cm),' cm'
      print,'T_NEO ',tneo,' K'
      print,'T_BB  ',tbb,' K'
      print,'Center wavelength             ',wave[npts/2],' microns'
      print,'                              ',wave_cm[npts/2],' cm'
      print,'Center frequency              ',freq[npts/2],' Hz-1'
      print,'Photon energy at center       ',onephoton[npts/2],' erg'
      print,'NEO flux at center wavelength ',flux[npts/2],' erg cm-2 s-1'
      print,'Intercepted energy from NEO   ',energy[npts/2],' erg'
      print,'Photons from NEO at center    ',photons[npts/2],' photons'
      print,'Total of trans*photons vector ',total(trans*photons)
      print,strn(npts),' points in integral'
   endif

   return,signal

end
