;+
; NAME:
;  kmphi1
; PURPOSE:   (one line only)
;  Basis set function 1 for the HG1G2 photometric system
; DESCRIPTION:
;  Based on Muinonen et al, Icarus, 209, 542 (2010).
; CATEGORY:
;  Photometry
; CALLING SEQUENCE:
;  value=kmphi1(alpha)
; INPUTS:
;  alpha - phase angle (degrees)
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
; OUTPUTS:
;  return values are the function evaluated for the input
; KEYWORD OUTPUT PARAMETERS:
; CONFIGURATION:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
;  This uses a lookup table provided in the reference and the built in
;    IDL spline procedure.  The derivative constraints are not used.
;    an extra grid point is added to create a "sensible" result at high
;    phase.
; MODIFICATION HISTORY:
;  Written by Marc W. Buie, Southwest Research Institute, 2024/10/29
;-
function kmphi1,alpha

   self='kmphi1: '
   if badpar(alpha,[4,5],[0,1],caller=self+'(alpha)', $
                               npts=npts) then return,!null

   akm = [0., 7.5, 30., 60, 90, 120, 140, 150]
   pkm = [1.0, 7.5e-1, 3.3486016e-1, 1.3410560e-1, $
               5.1104756e-2, 2.1465687e-2, 8.0e-3, 3.6396989e-3]

   ; input to spline function must be sorted
   idx=lindgen(npts)
   sidx=sort(alpha)
   salpha = alpha[sidx]

   phi1=spline(akm,pkm,salpha)

   ; move data back to original order
   phi1=phi1[idx[sidx]]

   return,phi1

end
