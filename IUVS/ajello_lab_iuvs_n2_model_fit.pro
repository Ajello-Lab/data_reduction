;+
; PURPOSE
;  This routine will fit LBH model vprime component spectra to 
;   a measured IUVS N2 spectrum.
;
; INPUTS
;  wave_spec: Dimension [NW], corrected wavelength scale, nm
;  spec: Dimension [NW], observed and calibrated N2 spectrum 
;  
; OUTPUTS
;  param_fit : Dimension [NP], Fit parameters 
;  param_id : Dimension [NP], identification of each parameter 
;  fcf_fit : Dimension [NP], Derived Franck-Condon factors
;  model_fit_arr : Dimension [NW,NP], array of model spectra fits 
;  spec_fit : Dimension [NW], final spectral fit to the data
;  
; KEYWORDS
;  wl1_fit: (optional) lower bound of wavelength range to consider in fit, nm
;    Default: 126.5 nm
;  wl2_fit: (optional) upper bound of wavelength range to consider in fit, nm
;    Default: 155.0 nm
;-

;
; model fit to the data
;
pro mlr_no_intercept, X, A, F
  sz = size(x,/dim)
  f = fltarr(sz[1])
  for i = 0, sz[0] - 1 do $
    f += abs(a[i]) * X[i,*]
END

pro ajello_lab_iuvs_n2_model_fit, wave_spec, spec, $
  param_fit, $
  param_id, $
  fcf_fit, $
  model_fit_arr, $
  spec_fit, $
  wl1_fit=wl1_fit, $
  wl2_fit=wl2_fit, $
  show_plots=show_plots

  ;
  ; define default values of the fit boundary wavelengths if not provided
  ;
  if keyword_set(wl1_fit) eq 0 then wl1_fit = 126.5
  if keyword_set(wl2_fit) eq 0 then wl2_fit = 155.0

  ;
  ; If no parameters provided, use test data 
  ;
  if n_params() eq 0 then begin
    show_plots = 1
    
    ajello_lab_set_paths, path_base, path_repo
  
    case (get_login_info()).user_name of
      'holsclaw': begin
        ;file_data_helper = '/Volumes/projects/Phase_Development/MAVEN/IUVS_Data/IUVS_Breadboard/Round_12/data_reduction/'
        path_save = '/users/holsclaw/Documents/'
        file_data = '/Volumes/projects/Phase_Development/MAVEN/IUVS_Data/IUVS_Breadboard/Ajello_Round14/Data_Reduction/N2_30EV_FUV_TEST19_IMAGE1.idl'
      end
      'benjamincondit': begin
        ;file_data_helper = '/Users/benjamincondit/Desktop/IUVS_Breadboard/Round 12/data_reduction/'
        path_save = '/Users/benjamincondit/idl/data_reduction/IUVS/Sensitivities/'
        file_data = ''
      end
    endcase
  
    if file_test(file_data) eq 0 then begin
      print, 'data file not found or not defined'
      stop
    endif
    
    ; LOAD IN DATA ***********************
    ;restore, file_data, /verbose  
    sObj = obj_new('IDL_Savefile', file_data)
    sObj.restore, 'arr'
    obj_destroy, sObj
  
    ;
    ; spatial profile
    ;
    spat = total( arr         , 1, /nan )
    
    ;
    ; create a single spectrum
    ;
    y1_key = 130 ;outside key hole
    y2_key = 940
    
    ndx_spat_peak = findndx( spat, max(spat) )
    yw = 100
    y1 = ( ndx_spat_peak - yw ) > y1_key
    y2 = ( ndx_spat_peak + yw ) < y2_key
    
;    p = plot( spat )
;    markerp,p,x=y1,linestyle=2
;    markerp,p,x=y2,linestyle=2
  
    spec = total( arr[*,y1:y2], 2, /nan )  ; for rotated image  
    
;    p1 = plot( spec, xtitle='pixel' )
  
    ndx_spec_peak = findndx( spec, max(spec) )
    
    ;
    ; retrieve a notional wavelength scale
    ;
    ajello_lab_pixel_scale_rot, wlfuv, wlmuv, yfuv, ymuv
  
    ;
    ; correct wavelength scale
    ;
    wave_spec = wlfuv - wlfuv[ndx_spec_peak] + 120.0
    
;    p1 = plot( wave_spec, spec, xtitle='wavelength scale corrected (nm)' )
;    markerp,p1,x=120.0,linestyle=2
    
    ;
    ; retrieve sensitivity
    ;
    ajello_lab_sensitivity_fuv_2026_07, wave_spec, sens

    ;
    ; calibrate the data
    ;
    spec_cal = spec / sens

    stop
    
  endif
  
  ;
  ; normalize the calibrated data as a plotting convenience 
  ;
  ;ndx_spec_norm = where( wave_spec gt wl1_fit and wave_spec lt wl2_fit )
  ;ndx_lbh_norm = where( wave_lbh gt wl1_fit and wave_lbh lt wl2_fit )
  ;spec_cal_norm = spec_cal / total(spec_cal[ndx_spec_norm]) ;/ mean(deriv(wlfuv))
  spec_cal_norm = spec_cal
  
;  
;  ;
;  ; force the observed data to be positive
;  ;
;  ;spec_cal_norm >= 0.
;  ;
;  ; force observed data out of band to be zero
;  ;
;  ndx_out = where( wave_spec lt 115., count )
;  spec_cal_norm[ndx_out] = 0.
;  
;  yr = [0, max( spec_cal_norm[ndx_spec_norm] )*1.1 ]
  
  ;
  ; retrieve LBH model
  ;
  wave1_lbh = 100.
  wave2_lbh = 450.
  waved_lbh = 0.04
  wave_lbh = findgen( (wave2_lbh - wave1_lbh) / waved_lbh ) * waved_lbh + wave1_lbh
  lbh_temp = 300.  ; Kelvin
  lbh = slbh2( lbh_temp, wave_lbh*10. )
  wave_lbh_step = mean( deriv( wave_lbh ) )
  num_wave_lbh = n_elements(wave_lbh)
  
  sz = size(lbh,/dim)
  num_wave_lbh = sz[0]
  num_band_lbh = sz[1]
  
  ;
  ; create a PSF for LBH model
  ;
  scaled_iuvs_psf_model, wave_lbh[0:301], psf
  
  if keyword_set(show_plots) then begin
    ;
    ; Show each vprime component
    ;
    win = window(dim=[1200,800])
    wave_first = [ 145.1, 141.7, 138.4, 135.5, 132.6, 130.0, 127.4 ]
    norm1 = fltarr(num_band_lbh)
    normw = 2.
    xr = [min(wave_lbh),max(wave_lbh)]
    for i = 0, num_band_lbh - 1 do begin
      v1 = lbh[*,i] / total(lbh[*,i]) ;/wave_lbh_step
      p1i = plot( wave_lbh, v1, layout=[1,num_band_lbh,i+1], current=win, /ylog, xr=xr )
    
      w1 = wave_first[i] - normw
      w2 = wave_first[i] + normw
      ndx1 = where( $
        ( wave_lbh gt w1 ) and $
        ( wave_lbh lt w2 ), count1 )
      norm1[i] = total( v1[ndx1] ) / total( v1 )
      ;ps = plot_shade( p1i, w1, w2, fill_transparency=70, fill_color='blue'  )
    endfor
    ;win.save, path_save + 'ajello_lab_n2_models_plot_each_vprime.png'

    ;    win = window(dim=[800,600])
    ;    p1 = plot( total( lbh, 1 ) / total( lbh ), current=win, font_size=14, $
    ;      yr=[0,0.25], symbol='o', /sym_filled, $
    ;      title="ratio sum across v'' to sum", xtitle="v'" )
  endif
  
  ;
  ; retrieve atomic nitrogen emissions and filter
  ;
  ajello_lab_nitrogen_emission_nist, arr_nist
  n = where( (arr_nist.ion eq 1) and $
    (arr_nist.rel_int gt 0.) and $
    (arr_nist.wave_obs ge wl1_fit) and $
    (arr_nist.wave_obs le wl2_fit), num_atomic )
  arr_nist = arr_nist[n]
  
  ;
  ; Create a larger 'model' array that will hold both LBH
  ;  and atomic emissions
  ;
  model = fltarr( num_wave_lbh, num_band_lbh + num_atomic )
  model[*, 0:num_band_lbh-1] = lbh
  wave_model = wave_lbh

  ;
  ; introduce a delta function at the atomic emission wavelength
  ;  scale the magnitude to place it on a similar scale as the LBH emissions,
  ;  but preserve the relative intensity
  ;
  for i = 0, num_atomic - 1 do begin
    n = findndx( wave_lbh, arr_nist[i].wave_obs )
    model[ n, num_band_lbh + i ] = arr_nist[i].rel_int / max(arr_nist.rel_int) * max(lbh)
  endfor
  
  ;
  ; total number of feature components is the sum of the number of LBH
  ;  vibration bands and atomic emissions
  ;
  num_feat = num_band_lbh + num_atomic
  
  ;
  ; convolve each spectral component by the PSF
  ;
  model_sm = model
  for i = 0, num_feat - 1 do $
    model_sm[*, i] = convol( model[*, i], psf ) / total(psf) / wave_lbh_step
  
;  modelsm = model_sm[*,0:num_band_lbh-1]
;  save,filename=path_save+'slbh2_convolved_with_IUVS_PSF.sav', modelsm, wave_model
;  stop
  
  ;
  ; interpolate the model to the wavelength scale of the data
  ;
  num_wave_spec = n_elements( wave_spec ) 
  model_smi = fltarr( num_wave_spec, num_feat )
  for i = 0, num_feat - 1 do $
    model_smi[*,i] = interpol( model_sm[*,i], wave_model, wave_spec )
    
  ;
  ; limit the regression to the given wavelength range
  ;
  ndx_wave_fit = where( wave_spec gt wl1_fit and wave_spec lt wl2_fit )
  wave_spec_fit = wave_spec[ndx_wave_fit]
  x = transpose( model_smi[ndx_wave_fit,*] )
  y = spec_cal_norm[ndx_wave_fit]
  ;y = spec_cal[ndx_wave_fit]

  ;-----------------------------------------------------------------------
  ; MPCURVEFIT
  ;-----------------------------------------------------------------------

;   YFIT = MPCURVEFIT(X, Y, WEIGHTS, P, [SIGMA,] FUNCTION_NAME=FUNC,
;                     ITER=iter, ITMAX=itmax,
;                     CHISQ=chisq, NFREE=nfree, DOF=dof,
;                     NFEV=nfev, COVAR=covar, [/NOCOVAR, ] [/NODERIVATIVE, ]
;                     FUNCTARGS=functargs, PARINFO=parinfo,
;                     FTOL=ftol, XTOL=xtol, GTOL=gtol, TOL=tol,
;                     ITERPROC=iterproc, ITERARGS=iterargs,
;                     NPRINT=nprint, QUIET=quiet,
;                     ERRMSG=errmsg, STATUS=status)
 

  ; MPCURVEFIT docs: WEIGHTS: "1D/Y     - Poisson weighting (counting statistics)"
  weights = 1./y
  ;r3 = reform(r) > 0.
  param = fltarr(num_feat)
  param[*] = 1.
  ;xtol = 1D-10 ; default
  ;xtol = 4D-10
  parinfo = replicate({value:0.D, fixed:0, limited:[0,0], limits:[0.D,0]}, $
    num_feat)
  parinfo.limited[0] = 1  ; require all parameters to require specified lower bound
  parinfo.limits[0] = 0.  ; set lower bound for all parameters to zero (i.e. positive)
  yfit = MPCURVEFIT( X, Y, WEIGHTS, param, /NODERIVATIVE, parinfo=parinfo, $
    FUNCTION_NAME='mlr_no_intercept', status=status, $  ; , xtol=xtol
    ERRMSG=errmsg )

  param_fit = param
  param_id = [ 'vp0', 'vp1', 'vp2', 'vp3', 'vp4', 'vp5', 'vp6', $
    string( arr_nist.wave_obs, format='(F6.2)' ) ]
  
  ;
  ; create an array of fit model spectral components 
  ;
  model_fit_arr = fltarr( num_wave_spec, num_feat )
  for i = 0, num_feat - 1 do $
    model_fit_arr[*,i] = model_smi[*,i] * param_fit[i]
  
  ;
  ; create a final model fit spectrum 
  ;
  spec_fit = total( model_fit_arr, 2 )

  ;
  ; calculate the Franck-Condon factors from the model
  ; note that this needs to come from the model over the wider wavelength 
  ;  range
  ;
  fcf_model = total( model[*, 0:num_band_lbh-1], 1 )
  fcf_model /= total(fcf_model)
  
  ;
  ; calculate the Franck-Condon factors from the fit
  ; note that this should NOT be calculated from model_fit_arr because
  ;  it subtends a narrower wavelength range than the full model
  ;
  fcf_fit = fcf_model * param_fit[0:num_band_lbh-1]
  fcf_fit /= total(fcf_fit)
  
  
  if keyword_set(show_plots) then begin
    win = window(dim=[1200,800])
    thick = 2
    p1 = plot( wave_spec, spec_cal_norm, current=win, thick=thick, xr=[110,185], layout=[1,2,1], title='mpcurvefit' )
    ;p2 = plot( wave_spec_fit, yfit3, color='red', /over )
    p3 = plot( wave_spec, spec_fit, color='red', /over )
    markerp,p1,x=wl1_fit,linestyle=2
    markerp,p1,x=wl2_fit,linestyle=2
    ;
    ;win = window(dim=[1200,600])
    thick = 2
    p1 = plot( wave_spec, spec_cal_norm, current=win, thick=thick, xr=[110,185], layout=[1,2,2] )
    p2 = plot( wave_spec, model_fit_arr[*,0], /over, color='red', thick=thick )
    p3 = plot( wave_spec, model_fit_arr[*,1], /over, color='orange', thick=thick )
    p4 = plot( wave_spec, model_fit_arr[*,2], /over, color='yellow', thick=thick )
    p5 = plot( wave_spec, model_fit_arr[*,3], /over, color='green', thick=thick )
    p6 = plot( wave_spec, model_fit_arr[*,4], /over, color='blue', thick=thick )
    p7 = plot( wave_spec, model_fit_arr[*,5], /over, color='indigo', thick=thick )
    p8 = plot( wave_spec, model_fit_arr[*,6], /over, color='violet', thick=thick )
    for i = 0, num_atomic - 1 do $
      pi = plot( wave_spec, model_fit_arr[*,7+i], /over, color='red', thick=thick )
    markerp,p1,x=wl1_fit,linestyle=2
    markerp,p1,x=wl2_fit,linestyle=2
    ;win.save, path_save + 'ajello_lab_mpcurvefit.png'
    
    win = window(dim=[1200,800])
    ;p1 = plot( wave_spec, spec_cal_norm, current=win, thick=thick, xr=[110,185] )
    xr=[120,185]
    p2 = plot( wave_spec, model_fit_arr[*,0], current=win, color='red', thick=thick, xr=xr, layout=[1,7,1] ) ; , title='0'
    p3 = plot( wave_spec, model_fit_arr[*,1], current=win, color='orange', thick=thick, xr=xr, layout=[1,7,2] )
    p4 = plot( wave_spec, model_fit_arr[*,2], current=win, color='yellow', thick=thick, xr=xr, layout=[1,7,3] )
    p5 = plot( wave_spec, model_fit_arr[*,3], current=win, color='green', thick=thick, xr=xr, layout=[1,7,4] )
    p6 = plot( wave_spec, model_fit_arr[*,4], current=win, color='blue', thick=thick, xr=xr, layout=[1,7,5] )
    p7 = plot( wave_spec, model_fit_arr[*,5], current=win, color='indigo', thick=thick, xr=xr, layout=[1,7,6] )
    p8 = plot( wave_spec, model_fit_arr[*,6], current=win, color='violet', thick=thick, xr=xr, layout=[1,7,7] )

    win = window(dim=[800,600])
    yr = [ 0, 0.25 ]
    p1 = plot( fcf_model, current=win, symbol='o', /sym_filled, name='model', title='FCF', font_size=14, yr=yr )  ; yr=yr,
    p2 = plot( fcf_fit, /over, color='red', symbol='o', /sym_filled, sym_color='red', name='derived' )
    leg = legend(target=[p1,p2]) ; ,position=[0.9,0.4]

  endif

  if n_params() eq 0 then stop
  
end