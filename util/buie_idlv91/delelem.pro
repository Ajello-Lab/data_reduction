;+
; NAME:
;  delelem
; PURPOSE:   (one line only)
;  Delete one or more array elements from an array
; DESCRIPTION:
; CATEGORY:
;  Utility
; CALLING SEQUENCE:
;  delelem,array,idx1,idx2
; INPUTS:
;  array - Input array (modified)
;  idx1  - Index into array for first element to delete
;  idx2  - This can have three values:
;            negative - delete abs(idx2) elements
;            0 - (default), delete just one element
;            positive - second index for end of delete
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
; OUTPUTS:
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  Written by Marc W. Buie, Southwest Research Institute, 2014/03/06
;-
pro delelem,array,in_idx1,in_idx2

   self='delelem: '
   if badpar(in_idx1,[2,3],0,caller=self+'(idx1) ') then return
   if badpar(in_idx2,[0,2,3],0,caller=self+'(idx2) ',default=0) then return

   nel=n_elements(array)

   ; if start is out of range, ignore
   if in_idx1 ge nel then return

   if in_idx1 lt 0 then idx1=0 else idx1=in_idx1

   if in_idx2 lt 0 then begin
      idx2 = idx1 + (abs(in_idx2)-1)
   endif else if in_idx2 eq 0 then begin
      idx2 = idx1
   endif else begin
      idx2 = in_idx2
   endelse

   if idx2 ge nel then idx2=nel-1

   ; ignore request to delete the entire array
   if idx2-idx1+1 eq nel then return

   if idx1 eq 0 then begin
      array = array[idx2+1:*]
   endif else if idx2 eq nel-1 then begin
      array = array[0:idx1-1]
   endif else begin
      array = [array[0:idx1-1],array[idx2+1:*]]
   endelse

end
