;+
; NAME:
;  idstring
; PURPOSE:   (one line only)
;  Generate a (hopefully) unique string
; DESCRIPTION:
;  This is used to provide a string that can be used to identify something
;    unique to the current calculation.  The provided string can be used to
;    tag things, like a unique process that is running on a given computer
;    that can be used to tag and refer to different things running on the same
;    (or different) machine.
; CATEGORY:
;  Miscellaneous
; CALLING SEQUENCE:
;  str=idstring(seed)
; INPUTS:
; OPTIONAL INPUT PARAMETERS:
;  seed - optional input, seed for the random number generator.  the default
;            is an undefined variable which is then passed to randomu.
; KEYWORD INPUT PARAMETERS:
;  SILENT - flag, if set will suppress informative output
; OUTPUTS:
;  Returns a single string with the host name, followed by '_' followed by
;    4 digits to make it (nearly) unique
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  2016/12/31 - Written by Marc W. Buie, Southwest Research Institute
;-
function idstring,seed,SILENT=silent

   uniqval=strb36(long(randomu(seed)*36L^4),pad=4)
   spawn,'hostname',result
   words=strsplit(result,'.',/extract)
   hosttag=trimrank(words[0])+'_'+uniqval
   if not keyword_set(silent) then $
      print,'Host/process tag for this run: ',hosttag
   return,hosttag

end
