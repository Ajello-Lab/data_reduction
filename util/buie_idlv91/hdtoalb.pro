;+
; NAME:
;  hdtoalb
; PURPOSE:   (one line only)
;  Compute V geometric albedo of an asteroid given H$_V$ and diameter
; DESCRIPTION:
; CATEGORY:
;  Astronomy
; CALLING SEQUENCE:
;  pv = hdtoalb(hv,d)
; INPUTS:
;  hv - V-band absolute magnitude
;  d  - Diameter of the asteroid in kilometers
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
; OUTPUTS:
;  return value is the V-band geometric albedo
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  Written by Marc W. Buie, Southwest Research Institute, 2014/05/19
;-
function hdtoalb,h,diam

   logp = 6.259 - 2.0*alog10(diam)-0.4*h
   return,10.0^logp

end
