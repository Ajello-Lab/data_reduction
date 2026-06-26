;+
; NAME:
;	fourpdm
; PURPOSE: (one line)
;	Period search by Fourier fit dispersion minimization ($\chi^2$ based).
; DESCRIPTION:
;	This routine is based on computing the Theta statistic for period searching
;	in time-series data using the technique described by Stellingwerf,
;	ApJ, 224, pp. 953-960 (1978).
;
;  Instead of looking to minimize the per-bin scatter, this version
;    attempts a fourier series fit to the data phased with each trial
;    period.  The chisq of the fit is used instead of theta.
;    Be careful to avoid using a series order that is too high.
;
; CATEGORY:
;  Photometry
; CALLING SEQUENCE:
;	fourpdm,t,x,sigx0,freq1,freq2,dfreq,freq,period,chisq
; INPUTS:
;	t     - independent variable (usually time)
;	x0    - dependent variable (usually magnitude or intensity)
;	sigx0 - Uncertainty on x.
;	freq1 - Lower limit to frequency to compute statistic  (units=[1/t])
;	freq2 - Upper limit to frequency to compute statistic  (units=[1/t])
;             if freq2<freq1 the input values are swapped and this can
;             potentially propagate back out to the caller
;	dfreq - Frequency interval.  (units=[1/t])
;             The scan is uniformly spaced in frequency
;          If this value is negative, it is then interpreted to be the
;             number of points to break the scan up into.
; OPTIONAL INPUT PARAMETERS:
; KEYWORD PARAMETERS:
;  ORDER - order of the fourier fit (see fourfit.pro). Default=2
; OUTPUTS:
;	freq  - Frequency vector.
;	period - inverse frequency.
;	chisq - goodness of fit statistic (this is a reduced chisq so you are
;             are looking for a value of 1 for a good fit)
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;	Written 1992/02/11, by Marc W. Buie, Lowell Observatory
;  2018/06/01, Cloned from pdm.pro by Amanda Zangari to use fourier fitting
;                 for the statistic
;  2018/12/07, MWB, added ORDER keyword
;-
pro fourpdm,t,x0,sigx0,freq1,freq2,in_dfreq,freq,period,chisq,ORDER=order

   self='fourpdm: '
   if badpar(t,[2,3,4,5],1,caller=self+'(t) ',npts=npts_t) then return
   if badpar(x0,[2,3,4,5],1,caller=self+'(x0) ',npts=npts_x0) then return
   if badpar(sigx0,[2,3,4,5],1,caller=self+'(sigx0) ', $
                   npts=npts_sigx0) then return
   if badpar(freq1,[2,3,4,5],0,caller=self+'(freq1) ') then return
   if badpar(freq2,[2,3,4,5],0,caller=self+'(freq2) ') then return
   if badpar(in_dfreq,[2,3,4,5],0,caller=self+'(dfreq) ') then return
   if badpar(order,[0,2,3],0,caller=self+'(ORDER) ',default=2) then return

   if npts_t le 1 then begin
      print,self,'ERROR! You must provide two or more input points'
      return
   endif

   if npts_x0 ne npts_t then begin
      help,npts_x0,npts_t
      print,self,'ERROR! Length of x0 vector must match the length of t'
      return
   endif

   if npts_x0 ne npts_sigx0 then begin
      help,npts_x0,npts_sigx0
      print,self,'ERROR! Length of sigx0 vector must match the length of t'
      return
   endif

   if (freq1 le 0 or freq2 le 0 or freq1 eq freq2) then begin
      help,freq1,freq2
      print,self,'Illegal frequency values.'
      return
   endif

   if in_dfreq eq 0 then begin
      print,self,'dfreq must not be zero.'
      return
   endif

   if (freq1 gt freq2) then begin
      tmp=freq1
      freq1=freq2
      freq2=tmp
   endif

   if in_dfreq lt 0 then begin
      dfreq=(freq2-freq1)/double(abs(in_dfreq))
   endif else begin
      dfreq = in_dfreq
   endelse
   freq = dindgen((freq2-freq1)/dfreq)
   freq = freq/(n_elements(freq)-1)
   freq = freq * (freq2-freq1) + freq1
   period = 1.0/freq
   chisq = dblarr(n_elements(freq))

   nbins = 60
   width = 1.0/20.0
   pbinl = dindgen(nbins)/float(nbins)
   pbinr = pbinl + width

;   meanerr,x0,sigx0,mx,dummy,sigsamp
;   varsamp = sigsamp^2
;   varsamp = x0 - mean(x0)
;   varsamp = varsamp^2
;   varsamp = total(varsamp)/(n_elements(x0)-1)
;   print,varsamp

;  Double the input data to eliminate phase wrapping
   x=double([x0,x0])
   sigx=double([sigx0,sigx0])

   for i=0,n_elements(freq)-1 do begin
      lphase = t * freq[i]
      lphase = lphase - fix(lphase)
      lphase = [lphase,lphase+1.0]

      samp = intarr(nbins)
      sig  = fltarr(nbins)
;      vars  = fltarr(nbins)

      fourfit,lphase,x,sigx,order,c,csig,chisq=chisqout

      chisq[i] = chisqout
   endfor

end
