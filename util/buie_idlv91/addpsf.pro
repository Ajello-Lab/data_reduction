;+
; NAME:
;  addpsf
; PURPOSE:   (one line only)
;  Insert (add) one or more PSFs into an image
; DESCRIPTION:
; CATEGORY:
;  CCD data processing
; CALLING SEQUENCE:
;  addpsf,image,x,y,f,psf
; INPUTS:
;  image - 2-d array to insert the PSF(s) into (MODIFIED).
;  x     - PSF x position (expected to be a float)
;  y     - PSF y position (expected to be a float)
;  f     - Scaling factor for source (multiplied against psf)
;  psf   - Normalized PSF to use
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;  PSFMAX - 2-element vector that contains the x,y position of the
;              peak of the input PSF.  These coordinates are in the
;              native image position coordinates of the PSF.  If not
;              provided it will be computed.  You can save on this
;              calculation by taking care of this outside this routine.
; OUTPUTS:
;  image - modified version of the image with the PSFs added
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  Written by Marc W. Buie, Southwest Research Institute, 2012/09/28
;  2016/03/27, MWB, rewrite logic to eliminate possibility of bad array
;                indexing.  If the requested location is not on the image
;                nothing is added.
;-
pro addpsf,image,x,y,f,psf,VERBOSE=verbose,PSFMAX=psfmax

   self='addpsf: '
   if badpar(image,[2,3,4,5],2,caller=self+'(image) ') then return
   if badpar(x,[2,3,4,5],[0,1],caller=self+'(x) ',npts=npts) then return
   if badpar(y,[2,3,4,5],[0,1],caller=self+'(y) ') then return
   if badpar(f,[2,3,4,5],[0,1],caller=self+'(f) ') then return
   if badpar(psf,[2,3,4,5],2,caller=self+'(psf) ') then return
   if badpar(psfmax,[0,2,3,4,5],1,caller=self+'(PSFMAX) ',default=[-1,-1]) then return
   if badpar(verbose,[0,1,2,3],0,caller=self+'(VERBOSE) ',default=0) then return

   szi=size(image,/dimen)
   nx=szi[0]
   ny=szi[1]

   szp=size(psf,/dimen)
   pnx=szp[0]
   pny=szp[1]

   xdw=pnx/2
   ydw=pny/2
   xmidp=szp[0]/2.0-0.5
   ymidp=szp[1]/2.0-0.5

   if min(psfmax) lt 0 then begin
      boxm,psf,xdw,ydw,xdw-1,ydw-1,xm,ym
      findmax,xm,ym,psf,xmax,ymax,fm
   endif else begin
      xmax=psfmax[0]
      ymax=psfmax[1]
   endelse

   if verbose then begin
      print,'psf pos ',xmax,ymax
      print,'psf size',szp[0],szp[1],' and middle',xmidp,ymidp
   endif

   for i=0,npts-1 do begin
      ; fractional pixel shift
      dx = x[i]-(xmax-xmidp)
      dy = y[i]-(ymax-ymidp)
      if verbose then begin
         print,'raw dx,dy',dx,dy
      endif

      ix=fix(x[i])
      iy=fix(y[i])

      dx = dx - ix
      dy = dy - iy
      spsf = sshift2d(psf,[dx,dy],/edge_zero)

      if verbose then begin
         print,'desired position ',x[i],y[i]
         print,'shift is ',dx,dy
         boxm,spsf,xdw,ydw,xdw-1,ydw-1,xmc,ymc
         findmax,xmc,ymc,spsf,xmaxc,ymaxc,fmc
         print,'computed psf pos',xmax+dx,ymax+dy
         print,'psf pos ',xmaxc,ymaxc
      endif

      ; image coordinates for inserted region without any clipping
      i0=ix-xdw
      i1=ix+xdw
      j0=iy-ydw
      j1=iy+ydw

      ; easy testing for completely out of bounds region
      if i0 ge nx then continue
      if i1 lt  0 then continue
      if j0 ge ny then continue
      if j1 lt  0 then continue

      ; boundaries of the sub-array in PSF array coordinates
      pi0=0
      pi1=pnx-1
      pj0=0
      pj1=pny-1

      ; clipped sub-array boundaries in image coordinates
      ci0 = i0>0
      ci1 = i1<(nx-1)
      cj0 = j0>0
      cj1 = j1<(ny-1)

      ; map the clipping back onto the sub-array coordinates
      cpi0=pi0 + (ci0-i0)
      cpi1=pi1 + (ci1-i1)
      cpj0=pj0 + (cj0-j0)
      cpj1=pj1 + (cj1-j1)

      ;insert psf in desired location in image
      image[(ci0):ci1,(cj0):cj1] += spsf[(cpi0):cpi1,(cpj0):cpj1]*f[i]

   endfor

end
