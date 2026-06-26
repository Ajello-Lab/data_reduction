;+
; NAME:
;  bidr4
; PURPOSE: (one line)
;  Compute the bi-directional reflectance (Hapke 2012+enhancements).
; DESCRIPTION:
;  This function is based on the rough reflectance treatment
;    from equation 12.55 on page 323 in Hapke's book,
;    "Theory of Reflectance and Emittance Spectroscopy", second edition.
;    All aspects of the Hapke formalism that is built for rough surfaces
;    is used.  Also included is an additional modification and the
;    relationship between K and h_s taken from Helfenstein & Shepard,
;    Icarus, v215, p83, (2011).
;  This function is intended to be as close as possible to bidr1 and bidr2
;     but the change in the theory does require additional inputs.
; CATEGORY:
;  Miscellaneous
; CALLING SEQUENCE:
;  ans = bidr4(ssalb,emu,imu,phang,hs,hc,ppp,b0s,b0c,theta)
; INPUTS:
;  all input arguments are protected against modification by this program
;  ssalb - Single scattering albedo.
;  emu   - Cosine of the emission angle.
;  imu   - Cosine of the incidence angle.
;  phang - Phase angle, in radians.
;  hs    - Shadow hiding (SHOE) compaction parameter value (2012 formalism).
;  hc    - Coherent backscatter (CBOE) compaction parameter (2012 formalism).
;  ppp   - Parameters of the single particle phase function
;          (default "function" is a constant of value ppp
;          There are four legal input forms for ppp
;            1. a scalar
;            2. an array of dimensionality same as ssalb, emu, imu, etc.
;            3. an array of dimensionality Pparms
;            4. an array of dimensionality (n_elements(ssalb),Pparms)
;  b0s   - SHOE backscatter amplitude.
;  b0c   - CBOE backscatter amplitude.
;  theta - Surface roughness value.  (radians)
; OPTIONAL INPUT PARAMETERS:
;  None.
; KEYWORD PARAMETERS:
;  Pfn   - Specify procedure to use for P(phang) 
;            The routine must be a procedure taking arguments phang,a,F,/radians
;              phang - phase angle in radians (with keyword /radians set)
;              a     - an array of Pparms parameters
;              F     - phase function evaluated at phase angles phang
;            Use "fn_hg3.pro" as a model.
;  Pparms- Specify number of parameters to be passed to P(phang).
;             (default=1)
;  pedantic - Flag, if set, returns NaN for ssalb > 1.
; OUTPUTS:
;  Return value is the bi-directional reflectance.
; COMMON BLOCKS:
;  None.
; SIDE EFFECTS:
;  None.
; RESTRICTIONS:
;  Any input may be a vector.  If more than one is a vector then the
;     lengths must match.  The return will have the same dimensions as
;     the input.
; PROCEDURE:
; MODIFICATION HISTORY:
;  Written by Marc W. Buie, Lowell Observatory, 2023/08/01
;    and was cloned from bidr2
;-
function bidr4,in_ssalb,in_emu,in_imu,in_phang,in_hs,in_hc,in_ppp,in_b0s,in_b0c,in_theta, $
               Pfn=Pfn,Pparms=Pparms,pedantic=pedantic

   if not keyword_set(Pparms) then Pparms = 1
   check=[n_elements(in_ssalb),n_elements(in_emu),n_elements(in_imu), $
          n_elements(in_phang),n_elements(in_hs),n_elements(in_hc), $
          n_elements(in_ppp)/Pparms, $
          n_elements(in_b0s),n_elements(in_b0c),n_elements(in_theta)]
   tlen = max(check)
   z=where(check ne 1 and check ne tlen,count)
   if count ne 0 then begin
      help,check
      print,check
      print,'BIDR4: Error, lengths of inputs must match or be 1.'
      return,0.0
   endif

   ; Promote all inputs to same length
   if n_elements(in_ssalb) eq 1 then ssalb = replicate(in_ssalb,tlen) $
                                else ssalb = in_ssalb
   if n_elements(in_emu)   eq 1 then emu   = replicate(in_emu,tlen) $
                                else emu   = in_emu
   if n_elements(in_imu)   eq 1 then imu   = replicate(in_imu,tlen) $
                                else imu   = in_imu
   if n_elements(in_phang) eq 1 then phang = replicate(in_phang,tlen) $
                                else phang = in_phang
   if n_elements(in_hs)    eq 1 then hs    = replicate(in_hs,tlen) $
                                else hs    = in_hs
   if n_elements(in_hc)    eq 1 then hc    = replicate(in_hc,tlen) $
                                else hc = in_hc
   if n_elements(in_b0s)   eq 1 then b0s   = replicate(in_b0s,tlen) $
                                else b0s    = in_b0s
   if n_elements(in_b0c)   eq 1 then b0c    = replicate(in_b0c,tlen) $
                                else b0c    = in_b0c
   if n_elements(in_theta) eq 1 then theta  = replicate(in_theta,tlen) $
                                else theta = in_theta

   ; Four P input options: ppp(scalar), ppp[Pparms], ppp[tlen], ppp[tlen,Pparms]
   unhappy = 1

   ; ppp[tlen,Pparms]
   if n_elements(in_ppp) eq tlen*Pparms and Pparms gt 1 then begin
      sz = size(in_ppp)
      if sz[1] eq tlen and sz[2] eq Pparms then begin
         ppp = in_ppp
         unhappy = 0
      endif
   endif

   ; ppp[tlen]
   if n_elements(in_ppp) eq tlen and Pparms eq 1 then begin
      ppp = fltarr(tlen,Pparms)
      ; this works for both ppp[tlen,1] and ppp[tlen]
      for i=0,tlen-1 do ppp[i,0] = in_ppp[i]
      unhappy = 0
   endif

   ; ppp[Pparms]
   if n_elements(in_ppp) eq Pparms and Pparms gt 1 then begin
      ppp = fltarr(tlen,Pparms)
      ; this works for both ppp[1,Pparms] and ppp[Pparms]
      for i=0,tlen-1 do ppp[i,*] = in_ppp
      unhappy = 0
   endif

   ; ppp(scalar)
   if n_elements(in_ppp) eq 1 and Pparms eq 1 then begin
      ppp = fltarr(tlen,Pparms)
      for i=0L,tlen-1 do ppp[i,0] = in_ppp
      unhappy = 0
   endif

   if unhappy then begin
      print,'BIDR4: Error, dimensionality of input ppp is bad'
      return,0.0
   endif

   gamma = sqrt(1.-ssalb)
   if not keyword_set(pedantic) then begin
      ; Unless we're in pedantic mode, this makes bidr4 return a non-NaN
      ; value where ssalb > 1 (a meaningless situation, but it often arises
      ; when using optical constants which have some associated noise,
      ; or when iteratively fitting observed reflectances).
      ;  USE THIS OPTION WITH CARE!  It's far better to avoid giving this 
      ;  routine an out of range ssalb in the first place.  This option
      ;  acts like a mirror giving you a value reflected about ssalb=1.
      ;  If your fitting routine needs mirrors or walls you really should
      ;  handle that externally.  This is meant for non-critical rendering
      ;  cases and not critical fitting work.
      z = where(ssalb gt 1.,count)
      if count gt 0 then gamma[z] = sqrt(ssalb[z]-1.)
   endif
   r0 = 2.0/(1.0+gamma)-1.0

   bidr = dblarr(tlen)

   emue  = dblarr(tlen)
   imue  = dblarr(tlen)
   emue0 = dblarr(tlen)
   imue0 = dblarr(tlen)
   sfun  = dblarr(tlen)

   ; correcting according to eq. 12.62, p. 331
   tanthe  = tan((1-r0)*theta)
   costhe  = cos((1-r0)*theta)
   cotthe  = 1.0/tanthe
   cotthe2 = cotthe^2

   i       = acos(imu)
   sini    = sin(i)
   e       = acos(emu)
   sine    = sin(e)

   cosphang    = cos(phang)
   cosphi  = replicate(1.0,tlen)
   z = where(i*e ne 0.0,count)
   if count ne 0 then $
      cosphi[z]=(cosphang - imu[z]*emu[z])/(sini[z]*sine[z])
   z = where(cosphi gt 1.0,count)
   if count ne 0 then cosphi[z] = 1.0
   z = where(cosphi lt -1.0,count)
   if count ne 0 then cosphi[z] = -1.0
   ; If cosphi hits -1, then fphi = exp(-2*tan(phi/2.0)) becomes infinite
   cosphi = (cosphi > (-0.999999999D0))
   phi     = acos(cosphi)
   sinphi2_2=sin(phi/2.0)^2

   gold=1.0e-7
   z=where(abs(sini) lt gold,count)
   if count ne 0 then sini[z]=gold
   z=where(abs(sine) lt gold,count)
   if count ne 0 then sine[z]=gold

   coti    = imu/sini
   coti2   = coti^2
   cote    = emu/sine
   cote2   = cote^2

   e1i = exp( -2.0/!dpi*cotthe*coti   )    ; eqn. 12.45b, p. 321
   e1e = exp( -2.0/!dpi*cotthe*cote   )

   e2i = exp( -1.0/!dpi*cotthe2*coti2 )    ; eqn. 12.45c, p. 321
   e2e = exp( -1.0/!dpi*cotthe2*cote2 )

   chi = 1.0/sqrt(1.0+!pi*tanthe^2)        ; eqn. 12.45a, p. 321
   fphi = exp( -2.0 * tan(phi/2.0) )       ; eqn. 12.51, p. 322

   ; equations 12.48 and 12.49 but notation is different, not using eta.
   emue0 = chi * ( emu + sine * tanthe * e2e / ( 2.0 - e1e ) )
   imue0 = chi * ( imu + sini * tanthe * e2i / ( 2.0 - e1i ) )

   denom  = dblarr(tlen)

   ; e >= i    This has the problem
   z = where(e ge i,count)
   if count ne 0 then begin

      denom[z] = 2.0 - e1e[z] - (phi[z]/!dpi)*e1i[z]

      ; these are eqn. 12.46 and 12.47
     imue[z] = chi[z] * ( imu[z] + sini[z] * tanthe[z] * $
                      ( cosphi[z]*e2e[z] + sinphi2_2[z]*e2i[z] ) / denom[z] )
     emue[z] = chi[z] * ( emu[z] + sine[z] * tanthe[z] * $
                      ( e2e[z] - sinphi2_2[z]*e2i[z] ) / denom[z] )

      sfun[z] = emue[z]/emue0[z] * imu[z]/imue0[z] * chi[z] / $
                 ( 1.0 - fphi[z] + fphi[z]*chi[z]*imu[z]/imue0[z] )

   endif

   ; e < i
   z = where(e lt i,count)
   if count ne 0 then begin

      denom[z] = 2.0 - e1i[z] - (phi[z]/!dpi)*e1e[z]

      imue[z] = chi[z] * ( imu[z] + sini[z] * tanthe[z] * $
                 ( e2i[z] - sinphi2_2[z]*e2e[z] ) / denom[z] )
      emue[z] = chi[z] * ( emu[z] + sine[z] * tanthe[z] * $
                 ( cosphi[z]*e2i[z] + sinphi2_2[z]*e2e[z] ) / denom[z] )

      sfun[z] = emue[z]/emue0[z] * imu[z]/imue0[z] * chi[z] / $
                 ( 1.0 - fphi[z] + fphi[z]*chi[z]*emu[z]/emue0[z] )
   endif

   ; Helfenstein and Shepard treatment
   k = 1.069 + 2.109*hs + 0.577*hs^2 - 0.062*hs^3

   supp={r0:r0}
   hobs=chfun(emue/k,ssalb,supp=supp)
   hsun=chfun(imue/k,ssalb,supp=supp)

   if not keyword_set(Pfn) then begin
      Pp = reform(ppp[*,0])
   endif else begin
      call_procedure,Pfn,phang,ppp,Pp,/radians
   endelse

   tanp2 = tan(phang/2)

   ; shadow hiding (9.22, p.232)
   bs = 1.0/(1+tanp2/hs)

   ; coherent backscatter (9.43, p.244)
   bc = 1.0/(1+(1.3+k)*(tanp2/hc+(tanp2/hc)^2))

;print,'g ',phang[0],phang[0]*!radeg
;print,'Pp',pp

   ; combined model, eqn. 12.55, p. 323
   bidr = k * 0.25*ssalb/!dpi * imue/(imue+emue) * $
          ( Pp*(1+b0s*bs)+(hsun*hobs-1) ) * (1+b0c*bc)*sfun

   err=check_math()
   if (err and '1B'x) ne 0 then $ 
      print,'BIDR4: Math error detected, code ',err

   return,bidr

end
