;+
; NAME:
;  mcmcsamp
; PURPOSE:   (one line only)
;  Markov-chain Monte-Carlo sampling tool
; DESCRIPTION:
; CATEGORY:
;  Statistics
; CALLING SEQUENCE:
;  mcmcsamp,vals,scale,region,pdf
; INPUTS:
;  vals - n-dimensional vector with initial guess for solution
;  scale - inital estimate of size of variation to sample, roughly 2x of
;            the expected standard deviation.  One value per input variable.
;            If burn-in is running, this input information is modified and will
;            contain the final dynamically determined values.
;  region - size of sampling region, roughly 5x bigger than scale.  One value
;            per input variable.
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;  FUNCTION_NAME - [REQUIRED!!!]  Name of function that evaluates the merit
;            function being minimized and sampled.   This function must take
;            a single vector of input (ie., vals) and will return a scalar
;            value (typically a chi-squared metric).
;  NSTEPS - Number of samples to collect in the chain.  Default=1000
;  NOBURNIN - Flag, if set, will suppress any burn-in calculations and
;                vals/scale are assumed to already be fully tuned and ready
;                to go.  With judicious use of the input arguments you can,
;                if desired, fully control the burn-in process if the built-in
;                algorithm is not effective.
;  VERBOSE - Flag, if set, will generate some verbose comments to be printed
;             during operation.  Default is no printed output.
;  DISPLAY - Flag, if set, will provide output graphics to watch the sampling
;             process.
; OUTPUTS:
;  pdf    - [NxM] array that is the Markov chain.  N is the dimenison of vals
;             and M is the number of steps requested.
; KEYWORD OUTPUT PARAMETERS:
;  ACCEPTANCE - Scalar value that is the total acceptance rate for the returned
;                   chain (does not include burn-in).
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  2016/12/22, Written by Marc W. Buie, Southwest Research Institute with
;                input from Alex Parker.
;  2017/01/06, MWB, modified to allow imposing restricted domains for sampling
;  2017/09/21, MWB, fixed a bug when the number of free parameters was not
;                equal to three
;  2020/03/30, MWB, minor modifications to plot output for only two fitted
;                parameters
;-
pro mcmcsamp_onechain,vals,scale,region,nsteps,function_name,chain,keeprate, $
       verbose,display,domain

   nvals=n_elements(vals)
   chisq=call_function(function_name,vals)

;fmt='(i4,2(1x,f7.2),1x,f5.3,1x,f7.2,1x,f6.3)'
;print,-1,vals[*],chisq,format=fmt

   chain=fltarr(nvals,nsteps)
   accept=bytarr(nsteps)

   stride=100

   jdlast=systime(/julian)

   for i=0,nsteps-1 do begin

      newval=vals+(randomu(seed,nvals)*2-1)*region
      z=where(newval gt domain[*,0] and newval lt domain[*,1],count)
      if count eq nvals then begin
         newchisq=call_function(function_name,newval)
         a = exp((chisq-newchisq)/2.0)  ; actual a1
      endif else begin
         newchisq=!values.f_infinity   ; never used
         a = 0
      endelse
;print,i,newval[*],newchisq,a,format=fmt

      ; The proposal density function is symmetric making a2=1, this is
      ;   where you put the function if not symmetric
;;      arg = ((newval-vals)/scale)^2
;;      pdft=fltarr(nvals)
;;      z=where(arg le 87.3,count)
;;      if count ne 0 then pdft[z]=exp(-arg[z])
;;      pdft=product(pdft)

;;      arg = ((vals-newval)/scale)^2
;;      pdfp=fltarr(nvals)
;;      z=where(arg le 87.3,count)
;;      if count ne 0 then pdfp[z]=exp(-arg[z])
;;      pdfp=product(pdfp)

;;      a2 = pdfp/pdft
;;      a = a1*a2

      if a ge 1 then begin
         chain[*,i]=newval
         accept[i]=1B
         vals=newval
         chisq=newchisq
      endif else if a eq 0 then begin
         chain[*,i]=vals
      endif else begin
         coin=randomu(seed)
         if coin lt a then begin
            chain[*,i]=newval
            accept[i]=1B
            vals=newval
            chisq=newchisq
         endif else begin
            chain[*,i]=vals
         endelse
      endelse

      if i ne 0 and i mod stride eq 0 then begin
         if verbose then begin
            jd=systime(/julian)
            secs = (jd-jdlast)*86400.0d0
            jdfin=jd+secs/float(stride)*(nsteps-i)/86400.0d0
            jdstr,jdfin,-2,jds
            print,i,' recent rate is ',total(accept[i-stride:i])/stride, $
               secs,' ',jds
            jdlast=jd
         endif
         if display then begin
            setwin,0,xsize=512,ysize=512
            plot,chain[0,0:i],chain[1,0:i],psym=3,xtitle='X',ytitle='Y'
            oplot,chain[0,i-stride:i],chain[1,i-stride:i], $
               psym=3,color='0000ff'xl
            if nvals gt 2 then begin
               setwin,1,xsize=512,ysize=512
               plot,chain[0,0:i],chain[2,0:i],psym=3,xtitle='X',ytitle='Mag'
               oplot,chain[0,i-stride:i],chain[2,i-stride:i], $
                  psym=3,color='0000ff'xl
               setwin,2,xsize=512,ysize=512
               plot,chain[1,0:i],chain[2,0:i],psym=3,xtitle='Y',ytitle='Mag'
               oplot,chain[1,i-stride:i],chain[2,i-stride:i], $
                  psym=3,color='0000ff'xl
            endif
            stats,chain[0,0:i],window=3,/silent,nbins=100,xtitle='X'
            stats,chain[1,0:i],window=4,/silent,nbins=100,xtitle='Y'
            if nvals gt 2 then $
               stats,chain[2,0:i],window=5,/silent,nbins=100,xtitle='Mag'
         endif
      endif

   endfor
   keeprate=total(accept)/nsteps

end

pro mcmcsamp,in_vals,scale,region,chain, $
        FUNCTION_NAME=function_name, $
        DOMAIN=domain, $
        NSTEPS=nsteps, $
        NOBURNIN=noburnin, $
        VERBOSE=verbose, $
        DISPLAY=display, $
        ACCEPTANCE=keeprate

   compile_opt strictarrsubs

   self='mcmcsamp: '
   if badpar(in_vals,[2,3,4,5],1,caller=self+'(vals) ', $
                                 npts=nvals,type=valtype) then return
   if badpar(scale,[2,3,4,5],1,caller=self+'(scale) ') then return
   if badpar(region,[2,3,4,5],1,caller=self+'(region) ') then return
   if badpar(function_name,7,0,caller=self+'(FUNCTION_NAME) ') then return
   if badpar(nsteps,[0,2,3],0,caller=self+'(NSTEPS) ',default=1000) then return
   if badpar(noburnin,[0,1,2,3],0,caller=self+'(NOBURNIN) ',default=0) then return
   if badpar(verbose,[0,1,2,3],0,caller=self+'(VERBOSE) ',default=0) then return
   if badpar(display,[0,1,2,3],0,caller=self+'(DISPLAY) ',default=0) then return

   if valtype eq 5 then begin
      a=replicate(!values.d_infinity,nvals)
   endif else begin
      a=replicate(!values.f_infinity,nvals)
   endelse
   b=-a
   def_domain=[[b],[a]]
   if badpar(domain,[0,2,3,4,5],2,caller=self+'(DOMAIN) ', $
                                  default=def_domain) then return

   ; The input scale vector is modified.

   vals=in_vals
   chisq=call_function(function_name,vals)

   ; State variable
   ;  0 = initial burn-in state, solution and magnitude adjustment
   ;  1 = tweak up, magnitude and relative scale adjustments
   ;  2 = confirmation of settings
   ;  3 = sampling
   ;  4 = done

   if noburnin then state=3 else state=0

   npass=0
   npass0=0
   npass1=0
   npass2=0
   ngood=0
   if verbose then print,'Initial values',vals
   if verbose then print,'Initial scale ',scale
   while state ne 4 do begin
;if npass eq 2 then return
      case state of
         0: begin
               mcmcsamp_onechain,vals,scale,region,101, $
                  function_name,chain,keeprate,verbose,display,domain
;               vals=trimrank(chain[*,-1])
               sigs=fltarr(nvals)
               for i=0,nvals-1 do begin
                  sigs[i]=stdev(chain[i,*])
                  vals[i]=mean(chain[i,*])
               endfor
;print,'Sigmas',sigs
               ngood++
               if keeprate gt 0.35 then begin
                  ; if keeprate=1, factor is 3
                  ; if keeprate=0.35 factor is 1.1
                  b = (3.0-1.1)/(0.7-0.05)
                  c = 1.1 - 0.05*b
                  fac = b*(keeprate-0.30)+c
                  scale=scale*fac
                  region=scale*5
                  if verbose then begin
                     print,state,npass0,keeprate,'  Scale by',fac
                     print,'New scale ',scale
                  endif
                  ngood=0
               endif else if keeprate lt 0.20 then begin
                  ; if keeprate=0, factor is 0.1
                  ; if keeprate=0.2, factor is 0.9
                  b = (0.5-0.9)/(0.2-0)
                  c = 0.9
                  fac = b*(0.2-keeprate)+c
                  scale=scale*fac
                  region=scale*5
                  if verbose then begin
                     print,state,npass0,keeprate,'  Scale by',fac
                     print,'New scale ',scale
                  endif
                  ngood=0
               endif else begin
                  if verbose then print,state,npass0,keeprate,ngood
               endelse
               if verbose then print,vals
               npass0++
               npass++
               if npass0 gt 10 or ngood eq 3 then begin
                  state=1
                  ngood=0
               endif
            end
         1: begin

               if npass1 gt 10 or ngood eq 3 then begin
                  state=2
                  ngood=0
               endif

               scalevol=product(scale)
               newscale=sigs/((product(sigs)/scalevol)^(1./nvals))

               if verbose then begin
                  print,'burnin-stage2 pass ',strn(npass1)
                  print,'sig/scale',sigs/scale
                  print,'old scale',scale
                  print,'new scale',newscale
                  print,'sig/newscale',sigs/newscale
               endif

               scale=newscale
               region=scale*5

               ngood++
               if keeprate gt 0.35 then begin
                  ; if keeprate=1, factor is 3
                  ; if keeprate=0.35 factor is 1.1
                  b = (3.0-1.1)/(0.7-0.05)
                  c = 1.1 - 0.05*b
                  fac = b*(keeprate-0.30)+c
                  scale=scale*fac
                  region=scale*5
                  if verbose then begin
                     print,state,npass0,keeprate,'  Scale by',fac
                     print,'New scale ',scale
                  endif
                  ngood=0
               endif else if keeprate lt 0.20 then begin
                  ; if keeprate=0, factor is 0.1
                  ; if keeprate=0.2, factor is 0.9
                  b = (0.5-0.9)/(0.2-0)
                  c = 0.9
                  fac = b*(0.2-keeprate)+c
                  scale=scale*fac
                  region=scale*5
                  if verbose then begin
                     print,state,npass0,keeprate,'  Scale by',fac
                     print,'New scale ',scale
                  endif
                  ngood=0
               endif else begin
                  if verbose then print,state,npass1,keeprate,ngood
               endelse

               mcmcsamp_onechain,vals,scale,region,101, $
                  function_name,chain,keeprate,verbose,display,domain
               sigs=fltarr(nvals)
               for i=0,nvals-1 do begin
                  sigs[i]=stdev(chain[i,*])
                  vals[i]=mean(chain[i,*])
               endfor
;print,'Sigmas',sigs

               npass1++

            end
         2: begin
               if verbose and npass2 eq 0 then print,'burn-in stage 3'
               for j=0,2 do begin
                  mcmcsamp_onechain,vals,scale,region,101, $
                     function_name,chain,keeprate,verbose,display,domain
                  sigs=fltarr(nvals)
                  for i=0,nvals-1 do begin
                     sigs[i]=stdev(chain[i,*])
                     vals[i]=mean(chain[i,*])
                  endfor
                  npass2++
                  if verbose then print,state,npass2,keeprate
               endfor
               state=3
            end
         3: begin
               mcmcsamp_onechain,vals,scale,region,nsteps, $
                  function_name,chain,keeprate,verbose,display,domain
               state=4
               npass++
               if verbose then print,'Final acceptance rate ',keeprate, $
                                     ', total runs ',strn(npass)
            end
         else: begin
               print,self,'This should not happen!'
            end
      endcase
   endwhile

   if verbose then print,'Total number of passes ',strn(npass)

   if display then begin
      setwin,0,xsize=512,ysize=512
      plot,chain[0,*],chain[1,*],psym=3,xtitle='X',ytitle='Y'
      if nvals gt 2 then begin
         setwin,1,xsize=512,ysize=512
         plot,chain[0,*],chain[2,*],psym=3,xtitle='X',ytitle='Mag'
         setwin,2,xsize=512,ysize=512
         plot,chain[1,*],chain[2,*],psym=3,xtitle='Y',ytitle='Mag'
         stats,chain[2,*],window=5,/silent,nbins=100,xtitle='Mag'
      endif
      stats,chain[0,*],window=3,/silent,nbins=100,xtitle='X'
      stats,chain[1,*],window=4,/silent,nbins=100,xtitle='Y'
   endif

end
