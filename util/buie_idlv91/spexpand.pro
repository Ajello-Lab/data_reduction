;+
; NAME:
;  spexpand
; PURPOSE:   (one line only)
;  Sparse expansion of an array
; DESCRIPTION:
;  This routine expands an image by in integer factor in both dimensions.
;  The original pixels from the input image are carried over to the LLHC
;  of the nxn expanded area that maps back to the pixel.  The other pixels
;  in the expanded image are set to 0.  This effectively converts the image
;  into a 2-D shah (III) function.
;
;  This routine does a 2x2 expansion (could be generalized if needed).
;
;  The purpose of this routine is to support the reconstruction of
;  undersampled dithered images.  (see XXX.pro)
;
; CATEGORY:
;  Image Processing
; CALLING SEQUENCE:
;  result=spexpand(image)
; INPUTS:
;  image - Input image to be expanded.
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;  ROOT2 - Flag, if set requests an expansion of the image by sqrt(2).
;            The geometry of this necessitates a 45 degree rotation of the
;            image.  The default is a 2x2 expansion.
; OUTPUTS:
;  return value is the expanded image
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  2016/09/06, Written by Marc W. Buie, Southwest Research Institute
;  2017/01/12, MWB, added ROOT2 keyword option
;  2019/02/16, MWB, fixed bad list of input types, dropped byte, added double
;  2019/12/20, MWB, incorporated new routine, mkxyarr
;-
function spexpand,image,ROOT2=root2

   self='spexpand: '
   if badpar(image,[2,3,4,5],2,caller=self+'(image) ') then return,!null
   if badpar(root2,[0,1,2],0,caller=self+'(ROOT2) ',default=0) then return,!null

   info=size(image,/structure)
   nx=info.dimensions[0]
   ny=info.dimensions[1]

   if root2 then begin

      idx=indgen(nx,ny)
      x = idx mod nx
      y = idx  /  nx
      xc = float(nx-1)/2.0
      yc = float(ny-1)/2.0
      dx = x-xc
      dy = y-yc
      cang=cos(45./!radeg)
      sang=sin(45./!radeg)
      dxp = (dx*cang-dy*sang)*sqrt(2.)
      dyp = (dx*sang+dy*cang)*sqrt(2.)
      nxy=max([nx,ny])*2
      xcp = float(nxy-1)/2.0
      ycp = float(nxy-1)/2.0
      xp = round(dxp+xcp)
      yp = round(dyp+ycp)
      output = make_array(dimension=[nxy,nxy],type=info.type)
      output[xp,yp]=image[idx]

   endif else begin

      output = make_array(dimension=info.dimensions[0:1]*2,type=info.type)

      mkxyarr,nx,ny,xarr_in,yarr_in

      xarr_out = xarr_in*2
      yarr_out = yarr_in*2

      output[xarr_out,yarr_out] = image[xarr_in,yarr_in]

   endelse

   return,output

end
