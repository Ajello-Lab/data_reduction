;+
; NAME: 
;  gauss1d
; PURPOSE: 
;  Compute a two dimensional gaussian within an array.
; DESCRIPTION:
; CATEGORY:
;  Mathematical
; CALLING SEQUENCE:
;  gauss1d,nx,x,fwhm,array
; INPUTS:
;  nx   - size of output array
;           X values of the array are the array indicies, running from 0 to
;           nx-1.  This is the behavior if nx is a scalar.
;           In the case of nx as a vector, it is taken to be an array of the
;           x values to be used.
;  x    - location of gaussian in array
;  fwhm - Full width at half-maximum of gaussian.
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;  SIGMA - Flag, if set means that the input fwhm is the 1/e half-width.
; OUTPUTS:
;  vector - Result vector with gaussian.
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  2016/12/27, Written by Marc W. Buie, Southwest Research Institute
;-
pro gauss1d,nx,x,fwhm,vector,SIGMA=sigma

   self='gauss1d: '
   if badpar(nx,  [2,3,4,5],[0,1],CALLER=self+'(nx)',rank=nxrank) then return
   if badpar(x,   [2,3,4,5],0,CALLER=self+'(x)') then return
   if badpar(fwhm,[2,3,4,5],0,CALLER=self+'(fwhm)') then return
   if badpar(sigma,[0,1,2,3],0,CALLER=self+'(SIGMA)',default=0) then return

   if sigma then begin
      ehwd = fwhm
   endif else begin
      ehwd = fwhm/2.0/sqrt(alog(2.0))
   endelse

   if nxrank eq 0 then begin
      ix = findgen(nx)
   endif else begin
      ix = nx
   endelse

   xsq = ((ix-x)/ehwd)^2
   vector = fltarr(n_elements(ix))

; Protection against underflow in exp call.
   big = where(xsq le 87.3, count)
   if count ne 0 then vector[big] = exp(-xsq[big])

end
