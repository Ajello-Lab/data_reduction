;+
; NAME:
;  orb2obj
; PURPOSE:   (one line only)
;  Extract object names from a slop orbit output file
; DESCRIPTION:
; CATEGORY:
;  Astronomy
; CALLING SEQUENCE:
;  orb2obj,fn
; INPUTS:
;  fn - name of orbit file to read
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
; OUTPUTS:
;  The input file name is expected to be something like <name>.orb.  A file
;    <name>.obj is written based on what is found in the .orb file.
; KEYWORD OUTPUT PARAMETERS:
; CONFIGURATION:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  Written by Marc W. Buie, Southwest Research Institute, 2023/03/27
;-
pro orb2obj,fn

   self='orb2obj: '
   if badpar(fn,7,0,caller=self+'(fn) ') then return

   if nofile(fn,'Input .orb file') then return

   fnout=repchar(fn,'.orb','.obj')

   if fnout eq fn then begin
      print,self,'Error! input and output files are the same, quitting.'
      return
   endif

   tab=string("11b) ;"
   newname=''
;   name=[]
   dummy=''
   fullname=''
   fmt1='(a24)'
   openr,lun,fn,/get_lun
   openw,lunobj,fnout,/get_lun
   while not eof(lun) do begin
      readf,lun,fullname,format=fmt1
      fullname=strtrim(fullname,2)
      newname=strcompress(fullname)
      newname=strsplit(newname,' ',/extract)
      newname=newname[0]
      for i=0,7 do begin
         readf,lun,dummy,format=fmt1
      endfor
      printf,lunobj,'A',newname,tab,"'",fullname
   endwhile
   free_lun,lun,lunobj

end
