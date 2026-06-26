;+
; NAME:
;  cumsum
; PURPOSE:   (one line only)
;  Cumulative sum of a vector
; DESCRIPTION:
;  Element i in the output vector equals TOTAL(invec[0:i])
; CATEGORY:
;  Mathematical
; CALLING SEQUENCE:
;  csum = cumsum(vec)
; INPUTS:
;  vec - input vector to sum
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
; OUTPUTS:
;  return value is the cumulative sum of the input vector
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  Written by Marc W. Buie, Southwest Research Institute, 2020/05/19
;-
function cumsum,vec

   nelem=n_elements(vec)

   outvec=vec
   for i=0,nelem-1 do begin
      outvec[i]=total(vec[0:i])
   endfor

   return,outvec

end
