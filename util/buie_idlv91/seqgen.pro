;+
; NAME:
;  seqgen
; PURPOSE:   (one line only)
;  Simplified sequence generation utility
; DESCRIPTION:
;  Miscellaneous
; CATEGORY:
;  Miscellaneous
; CALLING SEQUENCE:
;  result=seqgen(p1,p1,dp)
; INPUTS:
;  p1 - Numeric value for the start of the sequence.  This will be the value
;        of the first element returned.
;  p2 - Numeric value for the end of the sequence.  This will be the value of
;        the last element returned as close as the precision of the data type
;        will allow.
;  dp - Step size
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
; OUTPUTS:
; KEYWORD OUTPUT PARAMETERS:
; CONFIGURATION:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  Written by Marc W. Buie, Southwest Research Institute, 2024/11/14
;-
function seqgen,p1,p2,dp

   npts = (p2-p1)/dp+1
   res = indgen(npts)*dp+p1
   return,res

end
