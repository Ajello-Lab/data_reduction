;+
; NAME:
;  kmphi3
; PURPOSE:   (one line only)
;  Basis set function 3 for the HG1G2 photometric system
; DESCRIPTION:
;  Based on Muinonen et al, Icarus, 209, 542 (2010).
; CATEGORY:
;  Photometry
; CALLING SEQUENCE:
;  value=kmphi3(alpha)
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
;    An extra point is added to the grid to create a more "sensible"
;    result.
; MODIFICATION HISTORY:
;  Written by Marc W. Buie, Southwest Research Institute, 2024/10/29
;-
function kmphi3,alpha

   self='kmphi3: '
   if badpar(alpha,[4,5],[0,1],caller=self+'(alpha)', $
                               npts=npts) then return,!null

   akm = [0.0, 0.3, 1., 2., 4., 5.8, 8., 12., 20., 27., 30.]
   pkm = [1., 8.3381185e-1, 5.7735424e-1, 4.2144772e-1, 2.3174230e-1, $
               0.15, $
               1.0348178e-1, 6.1733473e-2, 1.6107006e-2, 3.6107006e-3, 0.]

   ; input to spline function must be sorted
   idx=lindgen(npts)
   sidx=sort(alpha)
   salpha = alpha[sidx]

   ; initialize the output to zeros keeping same type as input
   phi3 = alpha*0.0

   z=where(alpha le 30,count)

   if count ne 0 then begin
      phi3[z]=spline(akm,pkm,salpha[z],0.04)
   endif

   ; move data back to original order
   phi3=phi3[idx[sidx]]

   return,phi3

end
