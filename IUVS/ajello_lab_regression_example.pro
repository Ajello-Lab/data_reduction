;+
; PURPOSE
;  
;
; INPUTS
;
; OUTPUTS
;
; KEYWORDS
;-

pro mlr_no_intercept, X, A, F
  sz = size(x,/dim)
  f = fltarr(sz[1])
  for i = 0, sz[0] - 1 do $
    f += abs(a[i]) * X[i,*]
END

pro ajello_lab_regression_example

  ajello_lab_set_paths, path_base, path_repo

  case (get_login_info()).user_name of
    'holsclaw': begin
      ;file_data_helper = '/Volumes/projects/Phase_Development/MAVEN/IUVS_Data/IUVS_Breadboard/Round_12/data_reduction/'
      path_save = '/users/holsclaw/Documents/'
      file_data = '/Volumes/projects/Phase_Development/MAVEN/IUVS_Data/IUVS_Breadboard/Ajello_Round14/Data_Reduction/N2_30EV_FUV_TEST19_IMAGE1.idl'
      file_model = path_repo + 'data/N2/n2_lbh_rot_293K.sav'
    end
    'benjamincondit': begin
      ;file_data_helper = '/Users/benjamincondit/Desktop/IUVS_Breadboard/Round 12/data_reduction/'
      path_save = '/Users/benjamincondit/idl/data_reduction/IUVS/Sensitivities/'
      file_data = ''
      file_model = ''
    end
  endcase

  if file_test(file_data) eq 0 then begin
    print, 'data file not found or not defined'
    stop
  endif

  if file_test(file_data) eq 0 then begin
    print, 'model file not found or not defined'
    stop
  endif
  


  ;
  ; generate an estimate of the instrument point spread function
  ;  (actually, this is the line-spread function)
  ;
  restore, file_model, /relax
  ;  % RESTORE: Recovering incompatible definition of structure LIST using relaxed structure assignment rules.
  ;  % RESTORE: Recovering incompatible definition of structure HASH using relaxed structure assignment rules.
  ;  % RESTORE: Restored variable: TROT.
  ;  % RESTORE: Restored variable: WAVE.
  ;  % RESTORE: Restored variable: LBH.
  ;  % RESTORE: Restored variable: LIST.
  scaled_iuvs_psf_model, wave[0:301], psf
  wave_lbh = wave / 10.

  lbh_orig = lbh
  lbh_orig_tot = total( lbh_orig, 2 )

  ajello_lab_nitrogen_emission_nist, arr_nist
  
  wl1_fit = 126.5
  ;wl1_fit = 125.0
  ;wl2_fit = 186.2
  wl2_fit = 155.0

  n = where( (arr_nist.ion eq 1) and $
             (arr_nist.rel_int gt 0.) and $
             (arr_nist.wave_obs ge wl1_fit) and $
             (arr_nist.wave_obs le wl2_fit), num_atomic )

  arr_nist = arr_nist[n]
  
  ;
  ; Include atomic emissions
  ;
  sz = size(lbh,/dim)
  num_wave_lbh = sz[0]
  num_band_lbh = sz[1]

  model1 = fltarr( num_wave_lbh, num_band_lbh + num_atomic )
  model1[*, 0:num_band_lbh-1] = lbh_orig  
  for i = 0, num_atomic - 1 do begin
    n = findndx( wave_lbh, arr_nist[i].wave_obs )
    model1[ n, num_band_lbh + i ] = arr_nist[i].rel_int / max(arr_nist.rel_int) * max(lbh_orig_tot)
  endfor
  
  num_feat = num_band_lbh + num_atomic
  
  ;
  ; convolve each rotational band by the PSF
  ;
  model = model1
  for i = 0, num_feat - 1 do $ 
    model[*, i] = convol( model1[*, i], psf )

;  p1 = plot( wave, lbh_orig[*,0] )
;  p2 = plot( wave, lbh[*,0]/total(psf), /over, color='red' )

  lbh_tot = total( model[*,0:num_band_lbh-1], 2 ) / total(psf)

  p1 = plot( wave, lbh_orig_tot )
  p2 = plot( wave, lbh_tot, /over, color='red' )
  ; peak at 1354 angstroms

  k = 0
  p1 = plot( wave_lbh, model1[*,k] / total(model1[*,k]) )
  p2 = plot( wave_lbh, model[*,k] / total(model[*,k]), /over, color='red' )
  
  stop


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
  
  p = plot( spat )
  markerp,p,x=y1,linestyle=2
  markerp,p,x=y2,linestyle=2

  spec = total( arr[*,y1:y2], 2, /nan )  ; for rotated image  
  
  p1 = plot( spec, xtitle='pixel' )

  ndx_spec_peak = findndx( spec, max(spec) )
  
  ;
  ; retrieve a notional wavelength scale
  ;
  ajello_lab_pixel_scale_rot, wlfuv, wlmuv, yfuv, ymuv

;  p1 = plot( wlfuv, spec, xtitle='wavelength guess (nm)' )
;  markerp,p1,x=135.4,linestyle=2
  
  wave_spec = wlfuv - wlfuv[ndx_spec_peak] + 120.0
  
  p1 = plot( wave_spec, spec, xtitle='wavelength scale corrected (nm)' )
  markerp,p1,x=120.0
   
  ; 
  ; retrieve sensitivity
  ;
  ajello_lab_sensitivity_fuv_2026_07, wave_spec, sens

  ;
  ; normalization wavelength range
  ;
  w1_norm = 128.5
  w2_norm = 148.4
  
  spec_cal = spec / sens
  ndx_spec_norm = where( wave_spec gt w1_norm and wave_spec lt w2_norm )
  ndx_lbh_norm = where( wave_lbh gt w1_norm and wave_lbh lt w2_norm )
  
  spec_cal_norm = spec_cal / total(spec_cal[ndx_spec_norm]) / mean(deriv(wlfuv))
  
  ;
  ; force the observed data to be positive
  ;
  ;spec_cal_norm >= 0.
  ;
  ; force observed data out of band to be zero
  ;
  ndx_out = where( wave_spec lt 115., count )
  spec_cal_norm[ndx_out] = 0.
  
  yr = [0, max( spec_cal_norm[ndx_spec_norm] )*1.1 ]
  
  lbh_tot_norm = lbh_tot / total(lbh_tot[ndx_lbh_norm]) / mean(deriv(wave_lbh))
  
  p1 = plot( wave_spec, spec_cal_norm, yr=[0,0.3] )
  p2 = plot( wave_lbh, lbh_tot_norm, /over, color='red' )
  
  ;
  ; interpolate the model to the wavelength scale of the data
  ;
  num_wave_spec = n_elements( wave_spec ) 
  modeli = fltarr( num_wave_spec, num_feat )
  for i = 0, num_feat - 1 do $
    modeli[*,i] = interpol( model[*,i], wave_lbh, wave_spec )

  ;
  ; limit the regression to only the range available in the model 
  ;
  ndx_wave_fit = where( wave_spec gt wl1_fit and wave_spec lt wl2_fit )

  ;-----------------------------------------------------------------------
  ; REGRESS
  ;-----------------------------------------------------------------------

  ;Result = REGRESS( X, Y, [, CHISQ=variable] [, CONST=variable] [, CORRELATION=variable]
  ;[, /DOUBLE] [, FTEST=variable] [, MCORRELATION=variable] [, MEASURE_ERRORS=vector]
  ;[, SIGMA=variable] [, STATUS=variable] [, YFIT=variable] )
  ;
  wave_spec_fit = wave_spec[ndx_wave_fit]
  x = transpose( modeli[ndx_wave_fit,*] )
  y = spec_cal_norm[ndx_wave_fit]
  ; regress help: "For Poisson or statistical weighting, MEASURE_ERRORS should be set to SQRT(Y)."
  measure_errors = sqrt(y)
  ;
  param = regress( x, y, const=const, yfit=yfit, measure_errors=measure_errors )
  
  model_fit_arr = fltarr( num_wave_spec, num_feat )
  for i = 0, num_feat - 1 do $
    model_fit_arr[*,i] = modeli[*,i] * param[i]

  spec_fit = total( model_fit_arr, 2 ) + const

  win = window(dim=[1200,800])
  thick = 2
  p1 = plot( wave_spec, spec_cal_norm, current=win, thick=thick, xr=[110,185], layout=[1,2,1], title='regress' )
  ;p2 = plot( wave_spec_fit, yfit2, color='red', /over )
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
  ;win.save, path_save + 'ajello_lab_curvefit.png'
  ;win.save, path_save + 'ajello_lab_regress.png'

  
  ;-----------------------------------------------------------------------
  ; CURVEFIT
  ;-----------------------------------------------------------------------
  
  ;Result = CURVEFIT( X, Y, Weights, A [, Sigma] [, CHISQ=variable] 
  ;[, /DOUBLE] [, FITA=vector] [, FUNCTION_NAME=string] [, ITER=variable] 
  ;[, ITMAX=value] [, /NODERIVATIVE] [, STATUS={0 | 1 | 2}] 
  ;[, TOL=value] [, YERROR=variable] )
  
  param2 = fltarr(num_feat)
  param2[*] = 1.0 
  ; Curvefit help: "For statistical (Poisson) weighting, Weightsi = 1.0/Yi."
  weights = 1./y
  yfit2 = curvefit( x, y, weights, param2, FUNCTION_NAME='mlr_no_intercept', /NODERIVATIVE) ;

  model_fit_arr2 = fltarr( num_wave_spec, num_feat )
  for i = 0, num_feat - 1 do $
    model_fit_arr2[*,i] = modeli[*,i] * param2[i]

  spec_fit2 = total( model_fit_arr2, 2 )

  win = window(dim=[1200,800])
  thick = 2
  p1 = plot( wave_spec, spec_cal_norm, current=win, thick=thick, xr=[110,185], layout=[1,2,1], title='curvefit' )
  ;p2 = plot( wave_spec_fit, yfit2, color='red', /over )
  p3 = plot( wave_spec, spec_fit2, color='red', /over )
  markerp,p1,x=wl1_fit,linestyle=2
  markerp,p1,x=wl2_fit,linestyle=2
  ;
  ;win = window(dim=[1200,600])
  thick = 2
  p1 = plot( wave_spec, spec_cal_norm, current=win, thick=thick, xr=[110,185], layout=[1,2,2] )
  p2 = plot( wave_spec, model_fit_arr2[*,0], /over, color='red', thick=thick )
  p3 = plot( wave_spec, model_fit_arr2[*,1], /over, color='orange', thick=thick )
  p4 = plot( wave_spec, model_fit_arr2[*,2], /over, color='yellow', thick=thick )
  p5 = plot( wave_spec, model_fit_arr2[*,3], /over, color='green', thick=thick )
  p6 = plot( wave_spec, model_fit_arr2[*,4], /over, color='blue', thick=thick )
  p7 = plot( wave_spec, model_fit_arr2[*,5], /over, color='indigo', thick=thick )
  p8 = plot( wave_spec, model_fit_arr2[*,6], /over, color='violet', thick=thick )
  for i = 0, num_atomic - 1 do $
    pi = plot( wave_spec, model_fit_arr2[*,7+i], /over, color='red', thick=thick )
  ;win.save, path_save + 'ajello_lab_curvefit.png'


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
  param3 = fltarr(num_feat)
  param3[*] = 1.
  ;xtol = 1D-10 ; default
  ;xtol = 4D-10
  parinfo = replicate({value:0.D, fixed:0, limited:[0,0], limits:[0.D,0]}, $
    num_feat)
  parinfo.limited[0] = 1  ; require all parameters to require specified lower bound
  parinfo.limits[0] = 0.  ; set lower bound for all parameters to zero (i.e. positive)
  yfit3 = MPCURVEFIT( X, Y, WEIGHTS, param3, /NODERIVATIVE, parinfo=parinfo, $
    FUNCTION_NAME='mlr_no_intercept', status=status, $  ; , xtol=xtol
    ERRMSG=errmsg )
      
  model_fit_arr3 = fltarr( num_wave_spec, num_feat )
  for i = 0, num_feat - 1 do $
    model_fit_arr3[*,i] = modeli[*,i] * param3[i]
    
  spec_fit3 = total( model_fit_arr3, 2 )

  win = window(dim=[1200,800])
  thick = 2
  p1 = plot( wave_spec, spec_cal_norm, current=win, thick=thick, xr=[110,185], layout=[1,2,1], title='mpcurvefit' )
  ;p2 = plot( wave_spec_fit, yfit3, color='red', /over )
  p3 = plot( wave_spec, spec_fit3, color='red', /over )
  markerp,p1,x=wl1_fit,linestyle=2
  markerp,p1,x=wl2_fit,linestyle=2
  ;
  ;win = window(dim=[1200,600])
  thick = 2
  p1 = plot( wave_spec, spec_cal_norm, current=win, thick=thick, xr=[110,185], layout=[1,2,2] )
  p2 = plot( wave_spec, model_fit_arr3[*,0], /over, color='red', thick=thick )
  p3 = plot( wave_spec, model_fit_arr3[*,1], /over, color='orange', thick=thick )
  p4 = plot( wave_spec, model_fit_arr3[*,2], /over, color='yellow', thick=thick )
  p5 = plot( wave_spec, model_fit_arr3[*,3], /over, color='green', thick=thick )
  p6 = plot( wave_spec, model_fit_arr3[*,4], /over, color='blue', thick=thick )
  p7 = plot( wave_spec, model_fit_arr3[*,5], /over, color='indigo', thick=thick )
  p8 = plot( wave_spec, model_fit_arr3[*,6], /over, color='violet', thick=thick )
  for i = 0, num_atomic - 1 do $
    pi = plot( wave_spec, model_fit_arr3[*,7+i], /over, color='red', thick=thick )
  ;win.save, path_save + 'ajello_lab_mpcurvefit.png'
  
  
  win = window(dim=[800,600])
  yr = [ 0, max([param[0:num_band_lbh-1],param2[0:num_band_lbh-1],param3[0:num_band_lbh-1]])*1.1 ]
  p1 = plot( param[0:num_band_lbh-1], yr=yr, current=win, symbol='o', /sym_filled, title='LBH band weights', name='regress', font_size=14 )
  p2 = plot( param2[0:num_band_lbh-1], /over, color='red', symbol='o', /sym_filled, sym_color='red', name='curvefit' )
  p3 = plot( param3[0:num_band_lbh-1], /over, color='blue', symbol='o', /sym_filled, sym_color='blue', name='mpcurvefit' )
  leg = legend(target=[p1,p2,p3],position=[0.9,0.4])  

  win = window(dim=[800,600])
  yr = [ 0, max([param[num_band_lbh:num_feat-1],param2[num_band_lbh:num_feat-1],param3[num_band_lbh:num_feat-1]])*1.1 ]
  x1 = arr_nist.wave_obs
  p1 = plot( x1, param[num_band_lbh:num_feat-1], current=win, symbol='o', /sym_filled, name='regress', title='atomic emission weights', font_size=14 )  ; yr=yr, 
  p2 = plot( x1, param2[num_band_lbh:num_feat-1], /over, color='red', symbol='o', /sym_filled, sym_color='red', name='curvefit' )
  p3 = plot( x1, param3[num_band_lbh:num_feat-1], /over, color='blue', symbol='o', /sym_filled, sym_color='blue', name='mpcurvefit' )
  markerp,p1,y=0,linestyle=2
  p4 = plot( x1, arr_nist.rel_int / total(arr_nist.rel_int)*0.05, /over, color='green', symbol='o', /sym_filled, sym_color='green', name='NIST' )
  leg = legend(target=[p1,p2,p3,p4],position=[0.9,0.4])

  win = window(dim=[800,600])
  yr = [ 0, max([param[num_band_lbh:num_feat-1],param2[num_band_lbh:num_feat-1],param3[num_band_lbh:num_feat-1]])*1.1 ]
  p1 = plot( param[num_band_lbh:num_feat-1], current=win, symbol='o', /sym_filled, name='regress', title='atomic emission weights', font_size=14 )  ; yr=yr,
  p2 = plot( param2[num_band_lbh:num_feat-1], /over, color='red', symbol='o', /sym_filled, sym_color='red', name='curvefit' )
  p3 = plot( param3[num_band_lbh:num_feat-1], /over, color='blue', symbol='o', /sym_filled, sym_color='blue', name='mpcurvefit' )
  markerp,p1,y=0,linestyle=2
  p4 = plot( arr_nist.rel_int / total(arr_nist.rel_int)*0.05, /over, color='green', symbol='o', /sym_filled, sym_color='green', name='NIST' )
  leg = legend(target=[p1,p2,p3,p4],position=[0.9,0.4])
  
  stop
  
  
stop

end