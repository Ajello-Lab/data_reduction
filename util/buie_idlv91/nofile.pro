;+
; NAME:
;  nofile
; PURPOSE:   (one line only)
;  Validate the existence of a file
; DESCRIPTION:
; CATEGORY:
;  Utility
; CALLING SEQUENCE:
;  val = nofile(file,desc)
; INPUTS:
;  file - name of a file to check.  This can a relative or absolute
;           reference.
;  desc - String to print out with the message if the file does not exist.
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
; OUTPUTS:
;  Return value is true (1) if the file is missing.  False (0) if it exists.
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  Written by Marc W. Buie, Southwest Research Institute, 2020/03/26
;-
function nofile,file,desc

   if exists(file) then return,0

   print,desc,' file ',file,' not found.'
   return,1

end
