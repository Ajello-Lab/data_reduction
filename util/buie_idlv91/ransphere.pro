;+
; NAME:
;  ransphere
; PURPOSE:   (one line only)
;  compute random points on a unit sphere
; DESCRIPTION:
; CATEGORY:
;  Mathematical
; CALLING SEQUENCE:
;  ransphere,a0,a1,b0,b1,npts,VEC=vec,ANG=ang
; INPUTS:
;  a0 - angle range start, this is RA, longitude, azimuth, [0,2pi)
;  a1 - angle range end, if a1>a0 means range passes through 0
;  b0 - angle range start, this is Decl, latitude, elevation, [-pi/2,pi/2]
;  b1 - angle range stop, b0 <= b1
;  npts - number of random values to generate
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
; OUTPUTS:
; KEYWORD OUTPUT PARAMETERS:
;  VEC - cartesian unit vectors for the points, Nx3 array,
;             these are unit vectors
;  ANG - angles, Nx2 array, [a,b]
;  SEED - random number seed, default is undefined and begins sequence
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
;  The latitude sampling is discretized due to the limitations of smplprb.
;
;  Only supports single-precision floating point output regardless of input.
; PROCEDURE:
;  Sample uniformly in longitude, sample with cosine probability for latitude
; MODIFICATION HISTORY:
;  2018/09/29, Written by Marc W. Buie
;-
pro ransphere,a0,a1,b0,b1,npts,VEC=vec,ANG=ang,SEED=seed

   self='ransphere: '
   if badpar(a0,[4,5],0,caller=self+'(a0) ') then return
   if badpar(a1,[4,5],0,caller=self+'(a1) ') then return
   if badpar(b0,[4,5],0,caller=self+'(b0) ') then return
   if badpar(b1,[4,5],0,caller=self+'(b1) ') then return
   if badpar(npts,[2,3],0,caller=self+'(npts) ') then return
   if badpar(seed,[0,13],0,caller=self+'(SEED) ') then return

   ; maps into longitude, starts out from 0 to 1
   ran1=randomu(seed,npts)

   ; convert to the range requested, this is a direct calculation
   if a0 le a1 then begin
      rana=ran1*(a1-a0) + a0
   endif else begin
      tmp_a0 = a0-2.*!pi
      rana=ran1*(a1-tmp_a0) + tmp_a0
      z=where(rana lt 0.,count)
      if count ne 0 then rana[z]=rana[z]+2.0*!pi
   endelse

   ranb=smplprb('cos',b0,b1,npts*100L,/randomize)

   ang=[[rana],[ranb[0:npts-1]]]

   sphrec,1.0,ranb[0:npts-1],rana,x,y,z

   vec=[[x],[y],[z]]

end
