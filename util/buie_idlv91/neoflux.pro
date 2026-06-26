;+
; NAME:
;  neoflux
; PURPOSE:   (one line only)
;  Compute the estimated thermal flux emitted from the surface of an asteroid
; DESCRIPTION:
; CATEGORY:
;  Asteroids
; CALLING SEQUENCE:
;  flux = neoflux(diam,delta,wave,temp)
; INPUTS:
;   diam - object diameter in km
;   delta - distance to object in AU
;   wave  - wavelength of emission in microns
;   temp  - effective black-body temperature (K)
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
; OUTPUTS:
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  2015/03/09, Written by Marc W. Buie, Southwest Research Institute
;-
function neoflux,diam,delta,wave,temp

   delta_km = delta*1.495979e8 ; convert to km from AU
   wave_cm = wave*1.0e-4 ; convert from microns to cm
   planck=6.6262d-27 ; erg s
   c=2.997925e10 ; cm s-1
   boltz=1.38062e-16 ; erg deg-1

   flux = (diam/delta_km)^2 / 4.0 * $
          2.*!pi*planck*c^2/wave_cm^5 / $
          (exp(planck*c/wave_cm/boltz/temp)-1.0)

   return,flux ; erg cm-2 s-1

end
