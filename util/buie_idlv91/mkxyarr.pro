;+
; NAME:
;  mkxyarr
; PURPOSE:   (one line only)
;  Create two arrays that contain the x,y positions of the elements in an array
; DESCRIPTION:
; CATEGORY:
;  Miscellaneous
; CALLING SEQUENCE:
;  mkxyarr,xsize,ysize,xarr,yarr
; INPUTS:
;  xsize - x-dimension (width) of the array
;  ysize - y-dimension (height) of the array
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;  X0 - location of the X origin in the image, default=0
;  Y0 - location of the Y origin in th eimage, default=0
;  SCALE - scale factor of image pixel to your desired x,y scale,
;             default=1.0
;  Note: if all three are defaulted, the return arrays will be integers,
;    otherwise you get back two floating point arrays.
; OUTPUTS:
;  xarr  - Array of dimension [xsize,ysize] where each value is the x coordinate
;             of the element.  The values range from 0 to xsize-1.
;  yarr  - Array of dimension [xsize,ysize] where each value is the y coordinate
;             of the element.  The values range from 0 to ysize-1.
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  2016/09/23, Written by Marc W. Buie, Southwest Research Institute, originally
;                 as posarr.pro
;  2019/12/20, renamed to mkxyarr with some style modifications
;-
pro mkxyarr,xsize,ysize,xarr,yarr,X0=x0,Y0=y0,SCALE=scale

   self='mkxyarr: '
   if badpar(xsize,[2,3],0,caller=self+'(xsize) ') then return
   if badpar(ysize,[2,3],0,caller=self+'(ysize) ') then return

   if badpar(x0,[0,2,3,4,5],0,caller=self+'(X0) ',default=0) then return
   if badpar(y0,[0,2,3,4,5],0,caller=self+'(Y0) ',default=0) then return
   if badpar(scale,[0,2,3,4,5],0,caller=self+'(SCALE) ',default=1) then return

   xidx = indgen(xsize)
   yone = replicate(1,ysize)
   xarr = xidx#yone

   yidx = indgen(ysize)
   xone = replicate(1,xsize)
   yarr = xone#yidx

   ; avoid returning float in the full default case
   if x0 eq 0 and y0 eq 0 and scale eq 1 then return

   xarr = (xarr-x0)*scale
   yarr = (yarr-y0)*scale

end

