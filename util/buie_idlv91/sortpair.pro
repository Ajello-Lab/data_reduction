;+
; NAME:
;  sortpair
; PURPOSE:   (one line only)
;  Sort a pair of vectors by the first and return a sorted copy
; DESCRIPTION:
; CATEGORY:
;  Miscellaneous
; CALLING SEQUENCE:
;  sortpair,vec1,vec2,sorted1,sorted2
; INPUTS:
;  vec1 - First input vector (numbers, not strings)
;  vec2 - Second input vector (numbers, not strings)
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
; OUTPUTS:
;  sorted1 - sorted copy of vec1, sorted by vec1
;  sorted2 - sorted copy of vec2, sorted by vec1
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  Written by Marc W. Buie, Southwest Research Institute, 2019/06/04
;-
compile_opt strictarrsubs
pro sortpair,vec1,vec2,sorted1,sorted2

   self='sortpair: '
   if badpar(vec1,[2,3,4,5],1,caller=self+'(vec1) ') then return
   if badpar(vec2,[2,3,4,5],1,caller=self+'(vec2) ') then return

   idx=sort(vec1)

   sorted1 = vec1[idx]
   sorted2 = vec2[idx]

end
