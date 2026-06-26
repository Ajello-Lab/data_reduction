;+
; NAME:
;  dispmask
; PURPOSE:   (one line only)
;  Combine an image and a mask so that you can see both using color
; DESCRIPTION:
; CATEGORY:
;  Image display
; CALLING SEQUENCE:
;  bim = dispmask(im,mask,zf)
; INPUTS:
;  im   - byte image pre-scaled for display, 2-D
;  mask - byte mask image, same size as im
;  zf   - zoom factor for output color cube
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
; OUTPUTS:
;  return is a cube for tv,true=3
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  Written by Marc W. Buie,  Southwest Research Institute, 2020/06/30
;-
function dispmask,im,mask,zf

      sz=size(im,/dimen)
      r = im
      b = im ; g=b
      z=where(mask ne 0,count)
      if count ne 0 then begin
         b[z]=0B
         r[z]=80B
      endif
      bsub=[[[r]],[[b]],[[b]]]
      bsub=rebin(bsub,sz[0]*zf,sz[1]*zf,3,/sample)
      return,bsub

end
