;+
; NAME:
;  rdstrarr
; PURPOSE:   (one line only)
;  Read a file into a string array
; DESCRIPTION:
; CATEGORY:
;  File I/O
; CALLING SEQUENCE:
;  rdstrarr,file,str
; INPUTS:
;  file - Name of file to read.
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
; OUTPUTS:
;  str - String array with contents of file.  Each line in the file is
;             an entry in the array.  If the input file does not exist this
;             will be returned with a value of !null.
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  Written by Marc W. Buie, Southwest Research Institute, 2020/12/17
;-
pro rdstrarr,file,str

   self='rdstrarr: '
   if badpar(file,7,0,caller=self+'(file) ') then return

   if not exists(file) then begin
      str=!null
      print,'File ',file,' not found.'
      return
   endif

   line=''
   str=[]
   openr,lun,file,/get_lun
   while not eof(lun) do begin
      readf,lun,line,format='(a)'
      str=[str,line]
   endwhile
   free_lun,lun

end

