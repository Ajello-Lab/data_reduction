;+
; NAME:
;  hptodiam
; PURPOSE:   (one line only)
;  Compute diameter of an asteroid given H$_V$ and V-band geometric albedo
; DESCRIPTION:
; CATEGORY:
;  Astronomy
; CALLING SEQUENCE:
;  d = hptodiam(hv,pv)
; INPUTS:
;  hv - V-band absolute magnitude
;  pv - V-band geometric albedo
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
; OUTPUTS:
;  return value is the diameter of the asteroid in kilometers
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  Written by Marc W. Buie, Southwest Research Institute, 2014/05/19
;-
function hptodiam,h,p

   logd = (6.259 - alog10(p)-0.4*h)/2.0
   return,10.0^logd

end
