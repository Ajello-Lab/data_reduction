;+
; NAME:
;  wrstrarr
; PURPOSE:   (one line only)
;  Write a string array to a file
; DESCRIPTION:
; CATEGORY:
;  File I/O
; CALLING SEQUENCE:
;  wrstrarr,str,file
; INPUTS:
;  str - String array to be written, one element per line of the file.
;  file - Name of file to write.
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;  APPEND - Flag, if set causes the write to be appended to an existing file,
;             otherwise a pre-existing file of the same name will be overwritten
; OUTPUTS:
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  Written by Marc W. Buie, Southwest Research Institute, 2020/12/17
;  2020/12/28, MWB, added APPEND keyword
;-
pro wrstrarr,str,file,APPEND=append

   self='wrstrarr: '
   if badpar(str,7,[0,1],caller=self+'(file) ') then return
   if badpar(file,7,0,caller=self+'(file) ') then return
   if badpar(append,[0,1,2,3],0,caller=self+'(APPEND) ') then return

   openw,lun,file,/get_lun,append=append
   for i=0,n_elements(str)-1 do begin
      printf,lun,str[i],format='(a)'
   endfor
   free_lun,lun

end

