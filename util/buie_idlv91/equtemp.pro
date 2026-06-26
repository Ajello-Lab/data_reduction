;+
; NAME:
;  equtemp
; PURPOSE:   (one line only)
;  Compute a simple thermal equilibrium temperature for an asteroidal surface
; DESCRIPTION:
;  This is a very simplistic calculation that is based on a simple radiative
;    balance and does not include any physics for surface and sub-surface
;    effects.
; CATEGORY:
;  Asteroids
; CALLING SEQUENCE:
;  temp=equtemp(r,alb,emvty)
; INPUTS:
;  r     - helicentric distance, AU
;  alb   - Bond albedo (should be bolometric)
;  emvty - emissivity (default 1.0)
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;  fast  - if set, do fast-rotator model, otherwise do slow-rotator
;  iso   - if set, do isothermal model (Trafton-like frost or conducting sphere)
;  inc   - if set, the incidence angle (for fast=0) or latitude for
;            zero subsolar latitude (for fast=1), in radians, otherwise 
;            assume normal incidence
; OUTPUTS:
;   return value is the estimated temperature
; KEYWORD OUTPUT PARAMETERS:
; CONFIGURATION:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  2016/01/21, Written by Marc W. Buie, Southwest Research Institute
;-
function equtemp,r,alb,emvty,fast=fast,inc=inc,iso=iso

   if n_elements(emvty) eq 0 then emvty=1.0
   if not keyword_set(inc) then inc=0.0

   solk=1.374e6 ; solar constant, Hanel et al., erg cm-2 s-1
   sigma=5.670e-5  ; Stefan/Boltzmann, erg cm-2 s-1 K-4

   if keyword_set(fast) then fastfactor=!pi else $
   if keyword_set(iso) then fastfactor=4 else fastfactor=1.0

   teq=((1-alb)*(cos(inc)>0)*solk/r^2/emvty/sigma/fastfactor)^0.25

   return,teq

end
