;+
; NAME:
;  kmphi2
; PURPOSE:   (one line only)
;  Basis set function 2 for the HG1G2 photometric system
; DESCRIPTION:
;  Based on Muinonen et al, Icarus, 209, 542 (2010).
; CATEGORY:
;  Photometry
; CALLING SEQUENCE:
;  value=kmphi2(alpha)
; INPUTS:
;  alpha - phase angle (degrees)
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
; OUTPUTS:
;  return value are the values of the function evaluated for the input
; KEYWORD OUTPUT PARAMETERS:
; CONFIGURATION:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
;  This uses a lookup table provided in the reference and the built in
;    IDL spline procedure.  The derivative constraints are not used.
;    An extra grid point is added at high phase to give a more "sensible"
;    result.
; MODIFICATION HISTORY:
;  Written by Marc W. Buie, Southwest Research Institute, 2024/10/29
;-
function kmphi2,alpha

   self='kmphi2: '
   if badpar(alpha,[4,5],[0,1],caller=self+'(alpha)', $
                               npts=npts) then return,!null

   akm = [0., 7.5, 30., 60, 90, 120, 138, 150]
   pkm = [1.0, 9.25e-1, 6.2884169e-1, 3.1755495e-1, $
          1.2716367e-1, 2.2373903e-2, 4.8e-3, 1.6505689e-4]

   ; input to spline function must be sorted
   idx=lindgen(npts)
   sidx=sort(alpha)
   salpha = alpha[sidx]

   phi2=spline(akm,pkm,salpha)

   ; move data back to original order
   phi2=phi2[idx[sidx]]

   return,phi2

end
