;+
; NAME:
;  occmag
; PURPOSE:   (one line only)
;  Compute a scaled magnitude for occultation planning
; DESCRIPTION:
; CATEGORY:
;  Occultations
; CALLING SEQUENCE:
;  val=occmag(mag,diam,vel)
; INPUTS:
;  mag  - Actual star magnitude
;  diam - Diameter of object [km]
;  vel  - Shadown velocity [km/sec]
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
; OUTPUTS:
; KEYWORD OUTPUT PARAMETERS:
; CONFIGURATION:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  Written by Marc W. Buie, Southwest Research Institute, 2022-01-11
;-
function occmag, mag, diam, vel

  dclip = ((diam/vel)<2.0) * vel
;print,dclip

  magstar = mag - 2.5*alog10((diam/40.0)/(vel/20.0))
;print,magstar

  magstar = mag - 2.5*alog10((dclip/40.0)/(vel/20.0))
;  magstar = mag - 2.5*alog10((dclip/dclip)/(vel/20.0))

  return, magstar

end
