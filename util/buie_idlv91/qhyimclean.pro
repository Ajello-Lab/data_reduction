;+
; NAME:
;  qhyimclean
; PURPOSE:   (one line only)
;  Apply standard image cleaning to a raw QHY174 camera image
; DESCRIPTION:
; CATEGORY:
;  Image Processing
; CALLING SEQUENCE:
;  outim=qhyimclean(image)
; INPUTS:
;  image - raw image read from the saved image file
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
; OUTPUTS:
;  return value is a floating point image that has been cleaned
; KEYWORD OUTPUT PARAMETERS:
;  MEANSKY - Average sky (background) level in image
;  SKYSIG  - standard deviation of the sky
; CONFIGURATION:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  Written by Marc W. Buie, Southwest Research Institute, 2022/02/25
;-
function qhyimclean,image,MEANSKY=sky,SKYSIG=skysig,ORDER=order

   self='qhyimclean: '
   if badpar(order,[0,2,3],0,caller=self+'(ORDER) ',default=2) then return,!null

   im=float(image)
   sz=size(im,/dimen)

   ; measure the sky level in the original image
   skysclim,im,lowval,hival,sky,skysig,npts=20000
;   robomean,im,3.0,0.5,sky,dummy,skysig

   ; blot out the GPS data at the start of the image, paste in fake sky
   idx=lindgen(10648)
   im[idx]=randomn(seed,n_elements(idx))*skysig+sky

;   print,'sky signal, pass 1 ',sky,' +/-',skysig

;   itool,im,/block

   backsub,im,/row,order=order,max_value=sky+20*skysig
   im += sky

   ; remeasure sky after backsub call, won't change the mean but will
   ;   change the noise level
   skysclim,im,lowval,hival,sky,skysig,npts=20000

;   print,'sky signal, pass 2 ',sky,' +/-',skysig

;   showsrc,im,window=0

;   writefits,fnout,im,hdr
   return,im

end
