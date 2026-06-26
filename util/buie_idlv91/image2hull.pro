;+
; NAME:
;  image2hull
; PURPOSE:   (one line only)
;  Convert a bit-mapped image of an object to a hull
; DESCRIPTION:
; CATEGORY:
;  Occultations
; CALLING SEQUENCE:
;   image2hull,image,hx,hy
; INPUTS:
;   image - 2-d array that is expected to be an image of an object (real or
;             syntethic)
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
; OUTPUTS:
;   hx - X position of a point on the exterior hull of the object
;   hy - Y position of a point on the exterior hull of the object
; KEYWORD OUTPUT PARAMETERS:
; CONFIGURATION:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
;   Interior openings are ignored.  There is an internal scaling to a 1-bit
;   image and this scaling is not yet generalized.
; PROCEDURE:
; MODIFICATION HISTORY:
;  Written by Marc W. Buie, Southwest Research Institute, 2023/08/21
;-
pro image2hull,image,hx,hy

   self='image2hull: '
   if badpar(image,[1,2,3,4,5,12,13,14,15],2,caller=self+'(image) ') then return

   sz=size(image,/dimen)
   nx=sz[0]
   ny=sz[1]
   setwin,0,xsize=nx,ysize=ny
   tvscl,image

   scl=100.0

   image1 = fix(((image*scl)>0)<1)
   setwin,1,xsize=nx,ysize=ny
   tvscl,image1

   limbimg=intarr(nx,ny)

   for i=0,7 do begin
      tmp=shift_diff(image1,/center,direction=i,/edge_zero)
      z=where(tmp ne 0,count)
      if count ne 0 then limbimg[z] = limbimg[z]+1
   endfor
   limbimg = limbimg<1

   setwin,2,xsize=nx,ysize=ny
   tvscl,limbimg

   z=where(limbimg ne 0,/null)

   xraw = float(z mod nx)
   yraw = float(z / ny)

   plot,xraw,yraw,psym=3,/iso

   mkhull2,xraw,yraw,hx,hy,ares=5,npts=360

   oplot,hx,hy,color=cpalette(1)

;tool,limbimg,/block

end
