;+
; NAME:
;  diamptoh
; PURPOSE:   (one line only)
;  Compute asteroid absolute magnitude given diameter and V geometric albedo
; DESCRIPTION:
; CATEGORY:
;  Astronomy
; CALLING SEQUENCE:
;  hv = diamptoh(d,pv)
; INPUTS:
;  d  - diameter of asteroid in kilometers
;  pv - V-band geometric albedo
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
; OUTPUTS:
;  return value is the Hv (absolute magnitude)
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  Written by Marc W. Buie, Southwest Research Institute, 2014/05/19
;-
function diamptoh,d,p

   h = 2.5*(6.259 - alog10(p) - 2.0*alog10(d))
   return,h

end
