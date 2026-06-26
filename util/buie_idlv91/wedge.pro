;+
; NAME: 
;  wedge
; PURPOSE: 
;  Compute and return a gray scale step wedge.
; DESCRIPTION:
; CATEGORY:
;  Image display
; CALLING SEQUENCE:
; INPUTS:
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;  NCOLORS - Number of colors in the wedge default is !d.n_colors, but not
;              bigger than 255.  If you set it manually, there is no limit.
; OUTPUTS:
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
; Oct 1991, Written by Marc W. Buie, Lowell Observatory
; 2016/12/15, MWB, added NCOLORS keyword, this changes the default behavior
;                   very slightly.
;-
function wedge,NCOLORS=ncolors

   self='wedge: '
   if badpar(ncolors,[0,2,3],caller=self+'(NCOLORS) ', $
                             default=!d.n_colors<255) then return,0

   w=20
   ramp=bytarr(ncolors,5*w)
   for i=0,w-1 do begin
      for j=0,ncolors-1 do begin
         ramp[j,i]=j/16*16
         ramp[j,i+w]=j/8*8
         ramp[j,i+2*w]=j/4*4
         ramp[j,i+3*w]=j/2*2
         ramp[j,i+4*w]=j
      endfor
   endfor
   ramp=[ramp,ramp]
   return,ramp
end
