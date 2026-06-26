;+
; NAME:
;    getannul
; PURPOSE: (one line)
;    Extract an annulus from a 2-D array.
; DESCRIPTION:
;
; CATEGORY:
;    CCD data processing
; CALLING SEQUENCE:
;    Getannul, image, xcen, ycen, inradius, outradius, data
;
; INPUTS:
;    image       : CCD image array.
;    xcen,ycen   : Center of annulus.
;    inradius    : Radius of inner circle.
;    outradius   : Radius of outer circle.
;
; OPTIONAL INPUT PARAMETERS:
;
; KEYWORD PARAMETERS:
; OUTPUTS:
;    data        : Array of data from the image array.
;    idx         : Optional output of 1D indices (in image) of data.
;
; COMMON BLOCKS:
;
; SIDE EFFECTS:
;
; RESTRICTIONS:
;
; PROCEDURE:
;
; MODIFICATION HISTORY:
;    Written by Doug Loucks, Lowell Observatory, April, 1993.
;    Reference: getannul.c by Marc Buie.
;    This version is completely different than the previous version and
;    somewhat faster.
;    5/12/93, DWL, Fixed a bug involving fringe pixels.  In certain cases,
;                  the determination of the row indices led to a bad array
;                  subscript range.
;    1/5/94, DWL, Added optional output parameter idx.
;    95/06/12, MWB, Fixed bug, base needed to be LONG for large sky apertures
;    2017/11/30, MWB, promote radii to long internally
;    2018/01/27, MWB, fixed a promote to long that was previously missed
;-
pro getannul,image,xcen,ycen,inradius,outradius,data,idx

   data = 0

   ; Get input image array size.
   image_size = size(image)
   xsize = image_size[1]
   ysize = image_size[2]

   ; Compute the exact area of the annulus and reserve enough space for
   ;   the pixel index arrays.
   r1 = long(inradius)
   r2 = long(outradius) + 1
   npts = long( !PI * ( r2^2 - r1^2 ) )
   x = intarr(npts)
   y = intarr(npts)
   base = 0L

   if ( 0 le inradius ) and ( inradius lt outradius ) then begin
      ; Compute the y-range limits.
      r2 = long(outradius)
      r1 = long(inradius)
      r2sq = r2 * r2
      r1sq  = r1 * r1
      yp = ycen - r2
      outy0 = fix( yp )
      if outy0 lt yp then outy0=outy0+1
      yp = ycen + r2
      outy3 = fix( yp + 1 )
      if outy3 gt yp then outy3=outy3 - 1
      for yp = outy0, outy3 DO begin
         ; Step through each row and compute the x-range (or ranges).
         dy = yp - ycen
         dysq = dy * dy
         dx2 = sqrt( r2sq - dysq )
         xp = xcen - dx2
         outx0 = fix( xp )
         if outx0 lt xp then outx0=outx0+1
         xp = xcen + dx2
         outx3 = fix( xp + 1 )
         if outx3 gt xp then outx3=outx3-1
         if dysq gt r1sq then begin
            ; No intersection with inner radius.
            n = outx3 - outx0 + 1
            if n eq 0 then n = 1
            x[ base : base+n-1 ] = outx0 + indgen( n )
            y[ base : base+n-1 ] = replicate( yp, n )
            base = base + n
         endif else begin
            ; Passing through the inner circle.  Select two parts.
            dx1 = sqrt( r1sq - dysq )
            xp = xcen - dx1
            inx0 = fix( xp + 1 )
            if inx0 gt xp then inx0 = inx0 - 1
            xp = xcen + dx1
            inx3 = fix( xp )
            if inx3 lt xp then inx3=inx3+1
            n1 = inx0 - outx0 + 1
            n2 = outx3 - inx3 + 1
            if n1 eq 0 then n1 = 1
            if n2 eq 0 then n2 = 1
            x[ base : base+n1-1 ] = outx0 + indgen( n1 )
            y[ base : base+n1-1 ] = replicate( yp, n1 )
            base = base + n1
            x[ base : base+n2-1 ] = inx3 + indgen( n2 )
            y[ base : base+n2-1 ] = replicate( yp, n2 )
            base = base + n2
         endelse
      endfor

      ; Final verification and selection.
      t = where( x[0:base-1] lt 0 or x[0:base-1] ge xsize or $
                 y[0:base-1] lt 0 or y[0:base-1] ge ysize, count )
      if count gt 0 then begin
         t = where( x[0:base-1] ge 0 and x[0:base-1] lt xsize and $
                    y[0:base-1] ge 0 and y[0:base-1] lt ysize, count )
         data = image[ x[t], y[t] ]
         if n_params() eq 7 then idx = x[t] + xsize * y[t]
      endif else begin
         data = image[ x[0:base-1], y[0:base-1] ]
         if n_params() eq 7 then idx = x[0:base-1] + xsize * y[0:base-1]
      endelse
   endif
end
