;+
; NAME:
;  htmltcell
; PURPOSE:   (one line only)
;  Format a single cell of information for an html table
; DESCRIPTION:
; CATEGORY:
;  Miscellaneous
; CALLING SEQUENCE:
;   str = htmltcell(tstr)
; INPUTS:
;  tstr - String that is the visible content of a cell
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;  CENTER - Flag, if set makes field centered
;  URL    - String, if provided will be setup as a hyperlink for the
;             contents of the cell
; OUTPUTS:
;  Return value is the string that encodes all the formatting requests
; KEYWORD OUTPUT PARAMETERS:
; CONFIGURATION:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  Written by Marc W. Buie, Southwest Research Institute, 2024/11/09
;-
function htmltcell,tstr,CENTER=center,URL=url

   self='htmltcell: '
   if badpar(tstr,7,0,caller=self+'(tstr) ') then return,''
   if badpar(center,[0,1,2,3],0,caller=self+'(CENTER) ', $
                                default=0) then return,''
   if badpar(url,[0,7],0,caller=self+'(URL) ',default='') then return,''

   str='<td'
   if center then str=str+' align=center'
   str=str+'>'
   if url ne '' then str=str+'<a href='+url+'>'
   str=str+tstr
   str=str+'</td>'

   return,str

end
