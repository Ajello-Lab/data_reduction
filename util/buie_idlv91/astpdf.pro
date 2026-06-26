;+
; NAME:
;  astpdf
; PURPOSE:   (one line only)
;  Compute a discrete numerical probability sample for astrometry
; DESCRIPTION:
; CATEGORY:
;  Astrometry
; CALLING SEQUENCE:
;  astpdf,ra,dec,sig1,sig2,sys1,sys2,pang,pdf
; INPUTS:
;  ra - right ascension of the position [radians]
;  dec - declination of the position [radians]
;  sig1 - major axis of error, gaussian [arcsec]
;  sig2 - minor axis of error, gaussian [arcsec]
;           by convention, sig1 >= sig2
;  sys1 - systematic component along major axis [arcsec]
;           If provided as a scalar value, the range is +/- of that range
;           about zero.  If provided as a two-element vector, this gives
;           the positive and negative limit (as signed numbers).  For example,
;             [-1,2] means the range is from -1 to +2.  The probability is
;             computed as a uniform distribution over the range.
;  sys2 - systematic component along minor axis [arcsec]
;           This input works just like sys1.  The relative magnitudes of sys1
;           and sys2 are arbitrary.
;  pang - position angle of the major axis, measured eastward from north
;           [radians].  This value should be treated over the range of [0,2pi)
;           because the positive component of the systematic distribution of
;           the major axis points in this direction.
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;  NSAMPLES - number of samples in the distribution (default=10000)
; OUTPUTS:
;  pdf - 2D array of ra,dec values dimension [N,2]
; KEYWORD OUTPUT PARAMETERS:
; CONFIGURATION:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  Written by Marc W. Buie, Southwest Research Institute, 2024/01/02
;-
pro astpdf,ra,dec,sig1,sig2,in_sys1,in_sys2,pang,pdf,NSAMPLES=nsamp,SEED=seed

   self='astpdf: '
   if badpar(ra,[4,5],0,caller=self+'(ra) ') then return
   if badpar(dec,[4,5],0,caller=self+'(ra) ') then return
   if badpar(sig1,[4,5],0,caller=self+'(sig1) ') then return
   if badpar(sig2,[4,5],0,caller=self+'(sig2) ') then return
   if badpar(in_sys1,[4,5],[0,1],caller=self+'(sys1) ') then return
   if badpar(in_sys2,[4,5],[0,1],caller=self+'(sys2) ') then return
   if badpar(pang,[4,5],0,caller=self+'(pang) ') then return
   if badpar(nsamp,[0,2,3],0,caller=self+'(NSAMPLES) ',default=10000) then return
   if badpar(seed,[0,1,2,3,4,5],0,caller=self+'(SEED) ') then return

   compute_sys1=0
   if n_elements(in_sys1) eq 1 then begin
      if in_sys1 ne 0 then compute_sys1=1
      sys1 = [-in_sys1,in_sys1]
   endif else begin
      sys1 = in_sys1[0:1]
      compute_sys1=1
   endelse

   compute_sys2=0
   if n_elements(in_sys2) eq 1 then begin
      if in_sys2 ne 0 then compute_sys2=1
      sys2 = [-in_sys2,in_sys2]
   endif else begin
      sys2 = in_sys2[0:1]
      compute_sys2=1
   endelse

   dxi=randomn(seed,nsamp)*sig1
   deta=randomn(seed,nsamp)*sig2

   if compute_sys1 then $
      dxi += randomu(seed,nsamp)*(sys1[1]-sys1[0]) + sys1[0]

   if compute_sys2 then $
      deta += randomu(seed,nsamp)*(sys2[1]-sys2[0]) + sys2[0]

   ; convert from astronomical angle to trig angle
   pangp = pang - !dpi/2.0d0

   dxip  =  dxi*cos(pangp) + deta*sin(pangp)
   detap = -dxi*sin(pangp) + deta*cos(pangp)

   astsn2rd,dxip,detap,ra,dec,rapdf,decpdf,/arcsec

   pdf = [[rapdf],[decpdf]]

;setwin,0
;plot,dxip,detap,psym=3,xr=maxmin(dxip),yr=minmax(detap),/iso

end
