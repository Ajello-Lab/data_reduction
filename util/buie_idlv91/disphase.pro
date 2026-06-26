;+
; NAME: 
;  disphase
; PURPOSE: 
;  Apply distance and phase angle correction to observed magnitudes.
; DESCRIPTION:
;  Apply standard asteroidal-law corrections to observed magnitudes
;     given the distance, phase angle, and the G coefficient.  Magnitudes
;     are corrected to 1 AU from Sun and Earth and to 0 degrees phase angle.
;  The default of this routine is to use equation A4 from Bowell etal. in
;     Asteroids II (see page 550) and called the Lumme and Bowell model.
;     An option is provided to compute the IAU standard correction from
;     equation A5 found on page 551.
;
;  According to the Bowell chapter, these expressions are valid only
;     for 0<=G<=1 and phase angle less than 120 degrees.  However, the
;     chapter also claims the expressions are useful outside this range.
;
;  Note that the computation blows up in single precision if G < -0.85.  If
;     the input value for G is out of range then the returned magnitude is
;     set to -99.99.  This prevents getting back a value of NaN for hmag.
; CATEGORY:
;  Photometry
; CALLING SEQUENCE:
;  disphase,mag,r,d,phang,g,hmag
; INPUTS:
;     These first four inputs are scalars or vectors.  If vector, it is
;       generally expected (but not enforced) that they be the same length.
;       Mixing some of these as scalars with vectors that have the same
;       length should work.
;
;     mag   - Observed magnitude.
;     r     - Sun-object distance in AU.
;     d     - Earth-object distance in AU.
;     phang - Phase angle of observation in degrees.
;
;     g     - IAU standard G value (phase angle coefficient), if this is
;                a scalar value (either for Lumme and Bowell or IAU).  If
;                this is a vector, it is assumed to be a two-element vector
;                for the Muinonen HG1G2 system and this formalism will be
;                used regardless of the value of the IAU keyword.
;                   G1 = g[0]
;                   G2 = g[1]
;
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;     H2MAG - Flag, if set reverses the sense of the calculations and takes
;                H as the input magnitude and then computes the apparent
;                magnitude for the output.
;     IAU   - Flag, if set will force the use of the lower accuracy IAU
;                standard calculation.  The default is to use the
;                Lumme and Bowell model.
; OUTPUTS:
;     hmag  - Magnitude corrected for distance and phase angle.
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;   2008/04/12, MWB, slight change to deal with bogus values for G
;   2016/01/06, MWB, added H2MAG keyword option
;   2016/01/13, MWB, added IAU keyword option
;   2024/10/29, MWB, added G1G2 system option, some clarifications in
;                      expected input variable structure added to info.
;-
pro disphase,mag,r,d,phang,g,hmag,H2MAG=h2mag,IAU=iau,DEBUG=debug

   self='disphase: '
   if badpar(mag,[4,5],[0,1],caller=self+'(mag) ') then return
   if badpar(r,[4,5],[0,1],caller=self+'(r) ') then return
   if badpar(d,[4,5],[0,1],caller=self+'(d) ') then return
   if badpar(phang,[4,5],[0,1],caller=self+'(phang) ') then return
   if badpar(g,[4,5],[0,1],caller=self+'(g) ',npts=gnpts) then return
   if badpar(h2mag,[0,1,2,3],0,caller=self+'(H2MAG) ',default=0) then return
   if badpar(iau,[0,1,2,3],0,caller=self+'(IAU) ',default=0) then return
   if badpar(debug,[0,1,2,3],0,caller=self+'(DEBUG) ',default=0) then return

   ; for safety. phase angle should be positive definite but some methods
   ;   of calculation need to distinguish between pre-opposition phase
   ;   and post-opposition phase.  This routine treats both the same.

   if gnpts eq 2 then begin
      if debug then print,'G1G2 enabled'
      phanga = abs(phang)
      if h2mag then begin
         hmag = mag + 5.0*alog10(r*d) $
                    - 2.5*alog10(g[0]*kmphi1(phanga) + $
                                 g[1]*kmphi2(phanga) + $
                                 (1-g[0]-g[1])*kmphi3(phanga))
      endif else begin
         hmag = mag - 5.0*alog10(r*d) $
                    + 2.5*alog10(g[0]*kmphi1(phanga) + $
                                 g[1]*kmphi2(phanga) + $
                                 (1-g[0]-g[1])*kmphi3(phanga))
      endelse
      hmag=trimrank(hmag)
   endif else begin
      phangr = abs(phang)/!radeg

      sphang = sin(phangr)
      tphang2= tan( phangr * 0.5 )

      if iau then begin
         if debug then print,'Old IAU G enabled'
         phi1 = exp( -3.33 * (tan(phangr/2.0)^0.63 ) )
         phi2 = exp( -1.87 * (tan(phangr/2.0)^1.22 ) )
         if keyword_set(debug) then begin
            print,'Phi1'
            print,phi1
            print,'Phi2'
            print,phi2
         endif
      endif else begin
         if debug then print,'Lumme and Bowell G enabled'
;         if debug then print,-90.56 * tphang2 * tphang2
         w = exp( (-90.56 * tphang2 * tphang2) > (-80.0) ) ; avoids underflow
         t = sphang / (0.119 + 1.341*sphang - 0.754*sphang*sphang )
         phi1s = 1.0 - 0.986 * t
         phi2s = 1.0 - 0.238 * t
         phi1l = exp( -3.332*tphang2^0.631 )
         phi2l = exp( -1.862*tphang2^1.218 )

         phi1 = w*phi1s + (1.0-w)*phi1l
         phi2 = w*phi2s + (1.0-w)*phi2l
      endelse

      safe_g = g > (-0.85)

      if keyword_set(h2mag) then begin
         hmag = mag + 5.0*alog10(r*d) - 2.5*alog10( (1-safe_g)*phi1 + safe_g*phi2 )
      endif else begin
         hmag = mag - 5.0*alog10(r*d) + 2.5*alog10( (1-safe_g)*phi1 + safe_g*phi2 )
      endelse
   endelse

end

