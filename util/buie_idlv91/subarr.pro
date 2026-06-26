;+
; NAME:
;  subarr
; PURPOSE:   (one line only)
;  Extract a sub-array from an image with bound checking
; DESCRIPTION:
;  Return a portion of an image as a sub-array.  The requested location for
;    the sub-array is checked against the real boundary of the original image
;    and portions of the sub-array that fall off the image are padded out
;    with either zeros or a noisy background filler.
;  An error is generated only when there is no overlap between the requested
;    sub-array and the image.
; CATEGORY:
;  Image display
; CALLING SEQUENCE:
;  subarr,image,i0,i1,j0,j1,sub,error
; INPUTS:
;  image - The input image
;  i0    - Bottom of the sub-array location
;  i1    - Top of the sub-array location
;  j0    - Left boundary of the sub-array location
;  j1    - Right boundary of the sub-array location
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;  FILL  - value to use to fill non-overlap regions between sub and image
;             (default=0)
;  NOISE - noise value for FILL (default=0), this will not always be useful
;             for non-floating point data types.
;  SEED  - seed value for random number generator.  This is used only when
;             NOISE is non-zero.  It is only needed if you are trying to
;             create the same set of random values for testing.  Under normal
;             circumstances there is no need to ever specify this value.
; OUTPUTS:
;  sub   - sub-array from image, if an error occurs this will be returned as
;            a scalar zero.
;  error - Flag, set if an error occurred.  The only time this happens is if
;            the requested area has no overlap with the image.
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  Written by Marc W. Buie, Southwest Research Insitute, 2015/06/30
;  2016/12/28, MWB, added byte input option for FILL
;-
pro subarr,image,i0,i1,j0,j1,sub,error,FILL=fill,NOISE=noise,SEED=seed

   self='subarr: '
   if badpar(image,[1,2,3,4,5],2,caller=self+'(image) ',type=type) then return
   if badpar(i0,[2,3],0,caller=self+'(i0) ') then return
   if badpar(i1,[2,3],0,caller=self+'(i1) ') then return
   if badpar(j0,[2,3],0,caller=self+'(j0) ') then return
   if badpar(j1,[2,3],0,caller=self+'(j1) ') then return
   if badpar(fill,[0,1,2,3,4,5],0,caller=self+'(FILL) ',default=0) then return
   if badpar(noise,[0,2,3,4,5],0,caller=self+'(NOISE) ',default=0) then return
   if badpar(seed,[0,2,3,4,5],0,caller=self+'(SEED) ') then return

   sz=size(image,/dimen)
   nx=sz[0]
   ny=sz[1]

   sub=0
   error=0

   ; Check for errors
   ; --> left >= right
   if i0 ge i1 then error=1
   ; --> bot >= top
   if j0 ge j1 then error=1
   ; --> non-overlapping regions
   if i0 ge nx then error=1
   if i1 lt 0  then error=1
   if j0 ge ny then error=1
   if j1 lt 0  then error=1

   if error then return

   ; setup output array
   nxs=i1-i0+1L
   nys=j1-j0+1L
   sub=make_array(nxs,nys,type=type)

   ; boundaries of the sub-array in sub-array coordinates
   si0=0
   si1=nxs-1
   sj0=0
   sj1=nys-1

   ; clipped sub-array boundaries in image coordinates
   ci0 = i0>0
   ci1 = i1<(nx-1)
   cj0 = j0>0
   cj1 = j1<(ny-1)

   ; map the clipping back onto the sub-array coordinates
   csi0=si0 + (ci0-i0)
   csi1=si1 + (ci1-i1)
   csj0=sj0 + (cj0-j0)
   csj1=sj1 + (cj1-j1)

   ; is fill needed?  If so, pre-fill the array
   if si0 ne csi0 or si1 ne csi1 or sj0 ne csj0 or sj1 ne csj1 then begin
      if noise gt 0 then begin
         sub[*]=randomn(seed,nxs*nys)*noise+fill
      endif else begin
         sub[*]=fill
      endelse
   endif

   ; copy data to sub-array
   sub[csi0:csi1,csj0:csj1]=image[ci0:ci1,cj0:cj1]

end
