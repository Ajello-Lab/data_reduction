;+
; NAME: 
;  num_size
; PURPOSE:   (one line only)
;  Gives the relative number vs. diameter of craters
; DESCRIPTION:
;  This is an approximation of the number of craters as a function of
;  size.  The function is a power law and is intended to
;  represent a simple distribution of asteroids.
;  This function is meant to be probabalistically sampled with smplprb.pro.
; CATEGORY:
;  Miscellaneous
; CALLING SEQUENCE:
;  array=num_size(m)
; INPUTS:
;  m      :Vector or scalar of crater diameters (units do not matter)
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
; OUTPUTS:
;  array  :Returns an array of the same size as m.
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  2004/07/13, Written by Maureen Teyssier, REU Lowell Observatory
;  2004/07/15, MWB, incorporated into library.
;  2022/03/07, MWB, cloned from num_mag.pro
;-
function num_size,m

   self='num_size: '
   if badpar(m,[1,2,3,4,5,6,7],[0,1],CALLER=self+'(m) ') then return,-1

   ; slope of the power-law density function, eg., Bernstein et al., AJ, 2004
   alpha= -3.0

;   p=alpha*alog(10)*10^(alpha*m)

   p= 100.0*m^alpha

   return,p

end
