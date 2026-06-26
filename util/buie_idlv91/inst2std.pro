;+
; NAME:
;	inst2std
; PURPOSE: (one line)
;	Apply photometric transformation from instrumental to standard mags.
; DESCRIPTION:
;  The formula for applying transformation to a photometric measurement
;  follows the basic formalism (including signs) from Hardie.  A time
;  dependent term has been added.  The formula looks like this:
;
;    m0 = m - kX - n(t-t0)X - k"CX + eC + Z
;
;     where
;        m  = instrumental magnitude
;        k  = extinction coefficient, mag/airmass
;        X  = airmass
;        n  = coefficient of the 1st order expansion of extinction as a
;               function of time
;        t  = Time of observation (in hours)
;        t0 = Reference time for n, time dependent correction is zero at
;               this time, usually is the middle of the observation set.
;        k" = second order extinction coefficient
;        C  = Standard system color of the object
;        e  = color term
;        Z  = zero point
;        m0 = Standard magnitude
;
;    This routine also supports two extra terms that fill out the second
;       order expansion.  These terms are:
;
;        + fX^2 + gC^2
;
; CATEGORY:
;	Photometry
; CALLING SEQUENCE:
;	inst2std,jd,am,inst,instsig,color,colorsig, $
;     tran,transig,jdref,std,stdsig
; INPUTS:
;  jd       - Julian date of observation for each entry.
;  am       - Floating point array of the airmass of observations.
;	inst     - Instrumental magnitude
;	instsig  - Uncertainty of the instrumental magnitude
;	color    - Standard system color for object.
;	colorsig - Uncertainty on the standard color
;	tran     - Transformation coefficients (vector)
;                tran[0] = (k)  principal extinction coefficient
;                tran[1] = (k") second order extinction coefficient
;                tran[2] = (e)  color term
;                tran[3] = (Z)  zero-point
;                tran[4] = (n)  time-dependent extinction term
;     if provided:
;                tran[5] = (f)  airmass squared term
;                tran[6] = (g)  color squared term
;  transig  - Uncertainty on the transformation coefficients (vector).
;                (no uncertainty on reference time)
;     tran and transig can either be 5-element vectors or 5xN element arrays.
;                       or either be 7-element vectors or 7xN element arrays.
;         These two arrays must match in rank and size
;  jdref    - Time reference point for extinction
;                must match the number of transformation sets provided
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;  REFAM - Optional air mass reference for the transformation.  The default
;             is an airmass=0.  The rules for this input are the same as
;             REFCOLOR.  Either scalar or length N from TIDX.
;  REFCOLOR - Optional color reference for the color term transformation.
;                 This term is normally C * (B-V) or some other standard
;                 color.  As written, the reference color is zero and this
;                 is the default.  By providing this value you are using
;                     C * ( (B-V) - REFCOLOR ) for the term instead.
;                 This can either be a scalar or a vector.  If a vector it
;                 must match the length N in TIDX.  Vector input is not
;                 allowed if you are not using an array of coefficients.
;  TIDX     - Indexing array, same length as the input observations.  This
;                keyword is ignored if the input transformation vectors is
;                a simple 5-element vector.  If the transformation is provided
;                as a 5xN array then this keyword is REQUIRED.
;             Each element gives the index into the set of transformation
;             values.  This allows combining data that have different
;             photometric transformation coefficients.
; KEYWORD OUTPUT PARAMETERS:
; OUTPUTS:
;	std      - Standard magnitude.
;	stdsig   - Uncertainty of the standard magnitude.
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;	Written: Marc W. Buie, Lowell Observatory, 1992/03/31.
;  1997/02/10, MWB, total rewrite
;  2013/06/02, MWB, allow option for data to be covered by different
;                     transformation coefficients.
;  2013/06/25, MWB, added REFCOLOR keyword
;  2014/02/26, MWB, added vector option for REFCOLOR, added REFAM keyword,
;                     added option for extra transformation terms
;-
pro inst2std,jd,am,inst,instsig,color,colorsig, $
   tran,transig,jdref,std,stdsig,TIDX=tidx,REFCOLOR=in_refcolor,REFAM=in_refam

   if n_params() eq 0 then begin
      print,'Usage: inst2std,jd,am,inst,instsig,color,colorsig,', $
                    'tran,transig,jdref,std,stdsig'
      return
   endif

   self='INST2STD: '
   if badpar(jd,5,[0,1],caller=self+'(jd) ',npts=n1) then return
   if badpar(am,[4,5],[0,1],caller=self+'(am) ',npts=n2) then return
   if badpar(inst,[4,5],[0,1],caller=self+'(inst) ',npts=n3) then return
   if badpar(instsig,[4,5],[0,1],caller=self+'(instsig) ',npts=n4) then return
   if badpar(color,[4,5],[0,1],caller=self+'(color) ',npts=n5) then return
   if badpar(colorsig,[4,5],[0,1],caller=self+'(colorsig) ',npts=n6) then return

   if badpar(tran,[4,5],[1,2],caller=self+'(tran) ', $
                              npts=n7,rank=trank1) then return
   if badpar(transig,[4,5],[1,2],caller=self+'(transig) ', $
                              npts=n8,rank=trank2) then return
   if badpar(jdref,5,[0,1],caller=self+'(jdref) ',npts=n9) then return

   if badpar(tidx,[0,2,3],[0,1],caller=self+'(TIDX) ', $
                              default=0,npts=n10) then return
   if badpar(in_refcolor,[0,2,3,4,5],[0,1],caller=self+'(REFCOLOR) ', $
                                   default=0.0,npts=n11) then return
   if badpar(in_refam,[0,2,3,4,5],[0,1],caller=self+'(REFAM) ', $
                                   default=0.0,npts=n12) then return

   alln=[n1,n2,n3,n4,n5,n6]
   if min(alln) ne max(alln) then begin
      print,self+'Error!  jd,am,mag,err,color,colorsig must be the same length.'
      return
   endif

   if trank1 ne trank2 then begin
      print,self+'Error!  tran and transig must have the same rank.'
      return
   endif

   if trank1 eq 1 then begin

      if n7 ne 5 and n7 ne 7 then begin
         print,self+'Error!  tran must a 5 or 7 element vector.'
         return
      endif
      if n8 ne 5 and n8 ne 7 then begin
         print,self+'Error!  transig must be a 5 or 7 element vector.'
         return
      endif
      if n7 ne n8 then begin
         print,self+'Error!  tran and transig must have the same length.'
      endif
      if n9 ne 1 then begin
         print,self+'Error!  jdref must be a scalar with this tran array.'
         return
      endif
      if n11 ne 1 then begin
         print,self+'Error!  refcolor must be a scalar with this tran array.'
         return
      endif
      if n12 ne 1 then begin
         print,self+'Error!  refam must be a scalar with this tran array.'
         return
      endif

      tidx=0
      refcolor=in_refcolor
      refam   =in_refam
      ncof = n7

   endif else begin
      if n7 ne n8 then begin
         print,self+'Error!  tran and transig must be the same length'
         return
      endif
      sz=size(tran,/dimen)
      if sz[0] ne 5 and sz[0] ne 7 then begin
         print,self+'Error!  tran array must be 5xN or 7xN'
         return
      endif
      ncof=sz[0]
      ntran=sz[1]
      sz=size(transig,/dimen)
      if sz[0] ne 5 and sz[0] ne 7 then begin
         print,self+'Error!  transig array must be 5xN or 7XN'
         return
      endif
      if n9 ne ntran then begin
         help,tran,jdref
         print,self+'Error!  jdref length does not match size of tran array'
         return
      endif
      if n10 ne n1 then begin
         help,inst,tidx
         print,self+'Error!  TIDX length does not match size of input data'
         return
      endif
      if n11 eq 1 then begin
         refcolor=replicate(in_refcolor,ntran)
      endif else if n11 eq ntran then begin
         refcolor=in_refcolor
      endif else begin
         print,self+'Error!  Length of refcolor must be either 1 or match tran'
         return
      endelse
      if n12 eq 1 then begin
         refam=replicate(in_refam,ntran)
      endif else if n12 eq ntran then begin
         refam=in_refam
      endif else begin
         print,self+'Error!  Length of refam must be either 1 or match tran'
         return
      endelse

   endelse

   ; Compute standard magnitudes for all observations.
   dt = float((jd-jdref[tidx])*24.0)
   dam = am-refam[tidx]
   dcolor = (color-refcolor[tidx])
   std = inst - tran[0,tidx]*dam $
              - tran[1,tidx]*dam*dcolor $
              + tran[2,tidx]*dcolor $
              + tran[3,tidx] $
              - tran[4,tidx]*dam*dt
   if ncof eq 7 then begin
      std = std $
              + tran[5,tidx]*dam^2 $
              + tran[6,tidx]*dcolor^2
   endif

   ; Propagate uncertainties.
   stdvarall = instsig^2 $
                + (dam*transig[0,tidx])^2 $
                + (dam*dcolor*transig[1,tidx])^2 $
                + (dam*tran[1,tidx]*colorsig)^2 $
                + (dcolor*transig[2,tidx])^2 $
                + transig[3,tidx]^2 $
                + (dam*transig[4,tidx]*dt)^2
   if ncof eq 7 then begin
      stdvarall = stdvarall $
                + (transig[5,tidx]*dam^2)^2 $
                + (transig[6,tidx]*dcolor^2)^2
   endif

   stdsig=trimrank(sqrt(stdvarall))
   std=trimrank(std)

end
