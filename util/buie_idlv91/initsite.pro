;+
; NAME:
;  initsite
; PURPOSE:   (one line only)
;  Initialize a regular grid of occulation station into a file
; DESCRIPTION:
; CATEGORY:
;  Occultations
; CALLING SEQUENCE:
;  initsite,fn,nsites,spacing
; INPUTS:
;  fn      - String with name of file to write to.
;  nsites  - Number of observing stations
;  spacing - Cross-track spacing between each station, in km.
;              If this number is negative, the value is taken to be the
;              cross-track spread in km and the spacing is then computed
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;  OFFSET  - Offset to the pattern, in km. default is 0 which would be centered
;             on the middle of all the sites.
;  PREFIX  - Prefix character for site name, default='Z'
; OUTPUTS:
;  File is written with the desired information.
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
;  Limited to 99 sites due to the site code output string.
; PROCEDURE:
; MODIFICATION HISTORY:
;  Written by Marc W. Buie, Southwest Research Institute, 2018/10/24
;  2019/09/14, MWB, added PREFIX keyword option
;-
compile_opt strictarrsubs
pro initsite,fn,nsites,in_spacing,OFFSET=offset,PREFIX=prefix

   self='initsite: '
   if badpar(fn,7,0,caller=self+'(fn) ') then return
   if badpar(nsites,[2,3],0,caller=self+'(nsites) ') then return
   if badpar(in_spacing,[2,3,4,5],0,caller=self+'(spacing) ') then return
   if badpar(offset,[0,2,3,4,5],0,caller=self+'(OFFSET) ',default=0) then return
   if badpar(prefix,[0,7],0,caller=self+'(PREFIX) ',default='Z') then return

   if in_spacing gt 0 then begin
      spacing = in_spacing
   endif else if in_spacing lt 0 then begin
      spacing = abs(in_spacing)/float(nsites-1)
   endif else begin
      print,'spacing=0 is invalid.'
      return
   endelse

   x=findgen(nsites)*spacing
   x=x-mean(x)+offset

   openw,lun,fn,/get_lun
   for i=1,nsites do begin
      sname=prefix+string(i,format='(i2.2)')
      printf,lun,sname,x[i-1],format='(a,1x,f6.1)'
   endfor
   free_lun,lun

end
