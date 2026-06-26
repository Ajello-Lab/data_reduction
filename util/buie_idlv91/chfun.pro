;+
; NAME:
;  chfun
; PURPOSE:   (one line only)
;  Compute various approximations of the Chandrasekhar H scattering function
; DESCRIPTION:
;  This function provide support for computing the Chandrasekhar H-function
;    for isotropic scatterers.  The approximations here are taken from
;    Hapke's book, Theory of Reflectance and Emittance Spectroscopy,
;      second edition.  In the keyword list, the equation and page number is
;      listed.
; CATEGORY:
; CALLING SEQUENCE:
;  result=chfun(x)
; INPUTS:
;  x = Input value, usually cos(i) or cos(e), valid range from [0,1]
;  w = single scattering albedo, valid range from [0,1]
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;  Select which treatment of H(x) you want by setting one of these flags
;  SUPP - You can eliminate extraneous computations by providing an
;           anonymous structure.  The required tags depend on the
;           approximation being used.  You can provide more tags than
;           needed.  Only those needed are used.
;  TWOS - Two stream approximation (eq 8.53, p203)
;              supp.gamma 
;  H93  - approximation from Hapke 1993
;              supp.gamma, supp.r0
;  H12  - (default) approximation from Hapke 2012 (ea 8.56, p204)
;              supp.r0
;           this keyword can be specified if you wish but leaving it
;           off
; OUTPUTS:
;  return value is the function evaluation
; KEYWORD OUTPUT PARAMETERS:
; CONFIGURATION:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
;  Very little parameter validation is done inside this routine to keep
;    it as fast as possible.  The inputs should be either float or double.
;    The type and rank should be preserved but it will be according to
;    default language behaviors.
;  Don't ever specify two approximation types.  No checking is done for
;    this and you'll get something that is unpredictable in the long run.
; PROCEDURE:
; MODIFICATION HISTORY:
;  Written by Marc W. Buie, Southwest Research Insitute, 2023/08/02
;-
function chfun,x,w,TWOS=twos,H93=h93,H12=h12,supp=supp

   if keyword_set(twos) then begin
      twox = 2.0*x
      if isa(supp,'Anonymous') then begin
         hfun = (1+twox)/(1+supp.gamma*twox)
      endif else begin
         gamma = sqrt(1.0-w)
         hfun = (1+twox)/(1+gamma*twox)
      endelse
      return,hfun
   endif

   if keyword_set(h93) then begin
      lnt=1+x
      z=where(x ne 0,count)
      if count ne 0 then lnt[z]=alog(lnt[z]/x[z]) else lnt[z]=0.0
      if isa(supp,'Anonymous') then begin
         hfun =  1.0/( 1.0 - (1.0-supp.gamma) * $
                       x*(supp.r0+(1.0-0.5*supp.r0-supp.r0*x)*lnt) )
      endif else begin
         gamma = sqrt(1.0-w)
         r0 = (1-gamma)/(1+gamma)
         hfun =  1.0/(1.0 - (1.0-gamma)*x*(r0+(1.0-0.5*r0-r0*x)*lnt))
      endelse
      return,hfun
   endif

   ; sleight of hand for lnt to avoid dividing by 0 and inheriting the type of x
   lnt=1+x
   z=where(x ne 0,count)
   if count ne 0 then lnt[z]=alog(lnt[z]/x[z]) else lnt[z]=0.0
   if isa(supp,'Anonymous') then begin
      hfun = 1.0/(1.0-w*x*(supp.r0+0.5*(1-2*supp.r0*x)*lnt))
   endif else begin
      gamma = sqrt(1.0-w)
      r0 = (1-gamma)/(1+gamma)
      hfun = 1.0/(1.0-w*x*(r0+0.5*(1-2*r0*x)*lnt))
   endelse
   return,hfun

end
