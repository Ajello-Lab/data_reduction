;+
; NAME:
;  bondalb
; PURPOSE:   (one line only)
;  Compute the Bond albedo given the single-scattering albedo
; DESCRIPTION:
;  This compute the spectral Bond albedo from the single-scattering
;    albedo.
; CATEGORY:
;  Miscellaneous
; CALLING SEQUENCE:
;  bondalb,salb,bond
; INPUTS:
;  salb - Single scattering albedo
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
; OUTPUTS:
;  bond - Bond albedo
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  2015/12/15, Written by Marc W. Buie, Southwest Research Institute
;-
pro bondalb,salb,bond

compile_opt strictarrsubs

   gamma = sqrt(1-salb)

   r0 = (1-gamma)/(a+gamma)

   bond = r0 * ( 1 - gamma/(3*(1+gamma)) )

end
