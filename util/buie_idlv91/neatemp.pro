;+
; NAME:
;  neatemp
; PURPOSE:   (one line only)
;  Compute a simple effective black-body temperature for a near-Earth asteroid
; DESCRIPTION:
; CATEGORY:
;  Asteroids
; CALLING SEQUENCE:
;  neatemp,geoalb,sun,phang
; INPUTS:
;  geoalb - Geometric albedo in the visible (traditionally in V)
;  sun    - Heliocentric distance in AU
;  phang  - NEA-observer-Sun angle (phase angle) in radians
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
; OUTPUTS:
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  Written by Marc W. Buie, Southwest Research Institute based on some
;    simplified formulas provided by Roger Linfield at Ball Aerospace with
;    essential input from Jeff VanCleve.
;-
function neatemp,geoalb,sun,phang

   self='neatemp: '
   if badpar(geoalb,[4,5],[0,1],caller=self+'(geoalb) ') then return,-1
   if badpar(sun,[4,5],[0,1],caller=self+'(sun) ') then return,-1
   if badpar(phang,[4,5],[0,1],caller=self+'(phang) ') then return,-1

   ; temperature difference between illuminated an unilluminated
   ; portions of the surface
   deltat=30

   ; solar luminosity
   lumin=1.374e6 ; solar constant, Hanel et al., erg cm-2 s-1

   ; Stefan-Boltzman constant
   boltz=5.67373e-5 ; erg cm-2 s-1 K-4

   ; infrared emissivity
   emis=0.9

   ; dayside temperature
   tnea=(lumin*(1.0-geoalb)/(!pi*boltz*emis*sun^2))^0.25

   ; apply simplistic phase angle correction
   tbb=tnea+deltat*cos(phang)

   return,trimrank(tbb)

end
