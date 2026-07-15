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

pro ajello_lab_n2_model_fit, wave_spec, spec, $
  wl1_fit=wl1_fit, wl2_fit=wl2_fit

  if keyword_set(wl1_fit) eq 0 then wl1_fit = 126.5
  if keyword_set(wl2_fit) eq 0 then wl2_fit = 155.0

  if n_params() eq 0 then begin
    
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
    ; restore the LBH model 
    ;
    restore, file_model, /relax, /ver
    ;  % RESTORE: Recovering incompatible definition of structure LIST using relaxed structure assignment rules.
    ;  % RESTORE: Recovering incompatible definition of structure HASH using relaxed structure assignment rules.
    ;  % RESTORE: Restored variable: TROT.
    ;  % RESTORE: Restored variable: WAVE.
    ;  % RESTORE: Restored variable: LBH.
    ;  % RESTORE: Restored variable: LIST.    
    sz = size(lbh,/dim)
    num_wave_lbh = sz[0]
    num_band_lbh = sz[1]

    ;
    ; create a PSF for LBH model 1
    ;
    scaled_iuvs_psf_model, wave[0:301], psf
    wave_lbh = wave / 10.
    wave_lbh_step = mean( deriv( wave_lbh ) )
  
    ;
    ; retrieve LBH model 2
    ;
    wave1_lbh = 100.
    wave2_lbh = 450.
    waved_lbh = 0.04
    wave_lbh2 = findgen( (wave2_lbh - wave1_lbh) / waved_lbh ) * waved_lbh + wave1_lbh
    lbh_temp = 300.  ; Kelvin
    lbh2 = slbh2( lbh_temp, wave_lbh2*10. ) 
    ;lbh2_sum = total( lbh2, 1 )
    ;fcf1 = lbh1_sum / total(lbh1_sum)    
    ;vp2 = total( lbh2, 1 )
    ;print, vp2
    wave_lbh2_step = mean( deriv( wave_lbh2 ) )
    num_wave_lbh2 = n_elements(wave_lbh2)
    
    ;
    ; create a PSF for LBH model 2
    ;
    scaled_iuvs_psf_model, wave_lbh2[0:301], psf2
    
;    wave_first = [ 145.1, 141.7, 138.4, 135.5, 132.6, 130.0, 127.4 ] 
;    num_vp = 7
;    norm1 = fltarr(num_vp)
;    norm2 = fltarr(num_vp)
;    norm3 = fltarr(num_vp)
;    norm4 = fltarr(num_vp)
;    normw = 2.
    
    
    ;
    ; compare the two LBH models for one vprime system
    ;
    k = 0
    win = window(dim=[800,600])
    p1 = plot( wave_lbh, lbh[*,k] / total(lbh[*,k]), xr=[140,170], current=win )
    p2 = plot( wave_lbh2, lbh2[*,k] / total(lbh2[*,k]), /over, color='red' )

    ; 
    ; Overplot the two LBH models for all vprime systems
    ;
    win = window(dim=[1200,800])
    xr = [min(wave_lbh2),max(wave_lbh2)]
    for i = 0, num_band_lbh - 1 do begin
      v1 = lbh[*,i] / total(lbh[*,i]) ;/wave_lbh_step
      v2 = lbh2[*,i] / total(lbh2[*,i]) ;/wave_lbh2_step
      p1i = plot( wave_lbh, v1, layout=[1,num_band_lbh,i+1], current=win, /ylog, xr=xr )
      p2i = plot( wave_lbh2, v2, /over, color='red' )
    
;      w1 = wave_first[i] - normw
;      w2 = wave_first[i] + normw
;      
;      ndx1 = where( $
;        ( wave_lbh gt w1 ) and $ 
;        ( wave_lbh lt w2 ), count1 )
;        
;      ndx2 = where( $
;        ( wave_lbh2 gt w1 ) and $
;        ( wave_lbh2 lt w2 ), count2 )
;           
;      norm1[i] = total( v1[ndx1] ) / total( v1 )
;      norm2[i] = total( v2[ndx2] ) / total( v2 )

;      norm3[i] = total( v1[ndx1] ) 
;      norm4[i] = total( v2[ndx2] ) 
      
      ;ps = plot_shade( p1i, w1, w2, fill_transparency=70, fill_color='blue'  )

    endfor
    ;win.save, path_save + 'ajello_lab_n2_models_plot_each_vprime.png'
  
    desc1 = file_basename(file_model)
    desc2 = 'slbh2'
    
;    win = window(dim=[800,600])
;    p1 = plot( norm1, name=desc1, current=win, font_size=14, yr=[0,0.5], symbol='o', /sym_filled, title="ratio v''0 to sum v''", xtitle="v'" )
;    p2 = plot( norm2, /over, color='red', name=desc2, symbol='s', /sym_filled, sym_color='red' )
;    leg = legend(target=[p1,p2], position=[0.8,0.4])
;    ;win.save, path_save + 'ajello_lab_n2_models_ratio_vdoubleprime0_to_sum_vdoubleprime.png'
    
    win = window(dim=[800,600])  
    p1 = plot( total( lbh, 1 ) / total( lbh ), name=desc1, current=win, font_size=14, yr=[0,0.25], symbol='o', /sym_filled, title="ratio sum across v'' to sum", xtitle="v'" )
    p2 = plot( total( lbh2, 1 ) / total( lbh2 ), /over, color='red', name=desc2, symbol='s', /sym_filled, sym_color='red' )
    leg = legend(target=[p1,p2], position=[0.8,0.4])
    ;win.save, path_save + 'ajello_lab_n2_models_ratio_sum_vdoubleprime_to_sum.png'

    ;    p1 = plot( norm3 )
    ;    p2 = plot( norm4, /over, color='red' )
    
  ;stop
  
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
    ; Include atomic emissions
    ;
    
    model1 = fltarr( num_wave_lbh, num_band_lbh + num_atomic )
    model1[*, 0:num_band_lbh-1] = lbh

    model2 = fltarr( num_wave_lbh2, num_band_lbh + num_atomic )    
    model2[*, 0:num_band_lbh-1] = lbh2

    ;
    ; introduce a delta function at the atomic emission wavelength
    ;  scale the magnitude to place it on a similar scale as the LBH emissions,
    ;  but preserve the relative intensity 
    ;
    for i = 0, num_atomic - 1 do begin
      n = findndx( wave_lbh, arr_nist[i].wave_obs )
      model1[ n, num_band_lbh + i ] = arr_nist[i].rel_int / max(arr_nist.rel_int) * max(lbh)
      
      n = findndx( wave_lbh2, arr_nist[i].wave_obs )
      model2[ n, num_band_lbh + i ] = arr_nist[i].rel_int / max(arr_nist.rel_int) * max(lbh2)
    endfor
    
    ;
    ; total number of feature components is the sum of the number of LBH 
    ;  vibration bands and atomic emissions
    ;
    num_feat = num_band_lbh + num_atomic
    
    ;
    ; convolve each component by the PSF
    ;
    model1_sm = model1
    for i = 0, num_feat - 1 do $ 
      model1_sm[*, i] = convol( model1[*, i], psf ) / total(psf) / wave_lbh_step
  
    model2_sm = model2
    for i = 0, num_feat - 1 do $
      model2_sm[*, i] = convol( model2[*, i], psf2 ) / total(psf2) / wave_lbh2_step

;    lbh_tot = total( model[*,0:num_band_lbh-1], 2 ) / total(psf)
;  
;    p1 = plot( wave, lbh_orig_tot )
;    p2 = plot( wave, lbh_tot, /over, color='red' )
;    ; peak at 1354 angstroms
  
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
  
    ;
    ; correct wavelength scale
    ;
    wave_spec = wlfuv - wlfuv[ndx_spec_peak] + 120.0
    
    p1 = plot( wave_spec, spec, xtitle='wavelength scale corrected (nm)' )
    markerp,p1,x=120.0,linestyle=2
    
    model = model2_sm
    wave_model = wave_lbh2
    
    stop
    
  endif
  
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
  
;  lbh_tot_norm = lbh_tot / total(lbh_tot[ndx_lbh_norm]) / mean(deriv(wave_lbh))
  
;  p1 = plot( wave_spec, spec_cal_norm, yr=[0,0.3] )
;  p2 = plot( wave_lbh, lbh_tot_norm, /over, color='red' )
  
  ;
  ; interpolate the model to the wavelength scale of the data
  ;
  num_wave_spec = n_elements( wave_spec ) 
  modeli = fltarr( num_wave_spec, num_feat )
  for i = 0, num_feat - 1 do $
    modeli[*,i] = interpol( model[*,i], wave_model, wave_spec )
    
  
  ;
  ; limit the regression to only the range available in the model 
  ;
  ndx_wave_fit = where( wave_spec gt wl1_fit and wave_spec lt wl2_fit )
  wave_spec_fit = wave_spec[ndx_wave_fit]
  x = transpose( modeli[ndx_wave_fit,*] )
  y = spec_cal_norm[ndx_wave_fit]

;
;  ;-----------------------------------------------------------------------
;  ; REGRESS
;  ;-----------------------------------------------------------------------
;
;  ;Result = REGRESS( X, Y, [, CHISQ=variable] [, CONST=variable] [, CORRELATION=variable]
;  ;[, /DOUBLE] [, FTEST=variable] [, MCORRELATION=variable] [, MEASURE_ERRORS=vector]
;  ;[, SIGMA=variable] [, STATUS=variable] [, YFIT=variable] )
;  ;
;  wave_spec_fit = wave_spec[ndx_wave_fit]
;  x = transpose( modeli[ndx_wave_fit,*] )
;  y = spec_cal_norm[ndx_wave_fit]
;  ; regress help: "For Poisson or statistical weighting, MEASURE_ERRORS should be set to SQRT(Y)."
;  measure_errors = sqrt(y)
;  ;
;  param = regress( x, y, const=const, yfit=yfit, measure_errors=measure_errors )
;  
;  model_fit_arr = fltarr( num_wave_spec, num_feat )
;  for i = 0, num_feat - 1 do $
;    model_fit_arr[*,i] = modeli[*,i] * param[i]
;
;  spec_fit = total( model_fit_arr, 2 ) + const
;
;  win = window(dim=[1200,800])
;  thick = 2
;  p1 = plot( wave_spec, spec_cal_norm, current=win, thick=thick, xr=[110,185], layout=[1,2,1], title='regress' )
;  ;p2 = plot( wave_spec_fit, yfit2, color='red', /over )
;  p3 = plot( wave_spec, spec_fit, color='red', /over )
;  markerp,p1,x=wl1_fit,linestyle=2
;  markerp,p1,x=wl2_fit,linestyle=2
;  ;
;  ;win = window(dim=[1200,600])
;  thick = 2
;  p1 = plot( wave_spec, spec_cal_norm, current=win, thick=thick, xr=[110,185], layout=[1,2,2] )
;  p2 = plot( wave_spec, model_fit_arr[*,0], /over, color='red', thick=thick )
;  p3 = plot( wave_spec, model_fit_arr[*,1], /over, color='orange', thick=thick )
;  p4 = plot( wave_spec, model_fit_arr[*,2], /over, color='yellow', thick=thick )
;  p5 = plot( wave_spec, model_fit_arr[*,3], /over, color='green', thick=thick )
;  p6 = plot( wave_spec, model_fit_arr[*,4], /over, color='blue', thick=thick )
;  p7 = plot( wave_spec, model_fit_arr[*,5], /over, color='indigo', thick=thick )
;  p8 = plot( wave_spec, model_fit_arr[*,6], /over, color='violet', thick=thick )
;  for i = 0, num_atomic - 1 do $
;    pi = plot( wave_spec, model_fit_arr[*,7+i], /over, color='red', thick=thick )
;  ;win.save, path_save + 'ajello_lab_curvefit.png'
;  ;win.save, path_save + 'ajello_lab_regress.png'
;
;  
;  ;-----------------------------------------------------------------------
;  ; CURVEFIT
;  ;-----------------------------------------------------------------------
;  
;  ;Result = CURVEFIT( X, Y, Weights, A [, Sigma] [, CHISQ=variable] 
;  ;[, /DOUBLE] [, FITA=vector] [, FUNCTION_NAME=string] [, ITER=variable] 
;  ;[, ITMAX=value] [, /NODERIVATIVE] [, STATUS={0 | 1 | 2}] 
;  ;[, TOL=value] [, YERROR=variable] )
;  
;  param2 = fltarr(num_feat)
;  param2[*] = 1.0 
;  ; Curvefit help: "For statistical (Poisson) weighting, Weightsi = 1.0/Yi."
;  weights = 1./y
;  yfit2 = curvefit( x, y, weights, param2, FUNCTION_NAME='mlr_no_intercept', /NODERIVATIVE) ;
;
;  model_fit_arr2 = fltarr( num_wave_spec, num_feat )
;  for i = 0, num_feat - 1 do $
;    model_fit_arr2[*,i] = modeli[*,i] * param2[i]
;
;  spec_fit2 = total( model_fit_arr2, 2 )
;
;  win = window(dim=[1200,800])
;  thick = 2
;  p1 = plot( wave_spec, spec_cal_norm, current=win, thick=thick, xr=[110,185], layout=[1,2,1], title='curvefit' )
;  ;p2 = plot( wave_spec_fit, yfit2, color='red', /over )
;  p3 = plot( wave_spec, spec_fit2, color='red', /over )
;  markerp,p1,x=wl1_fit,linestyle=2
;  markerp,p1,x=wl2_fit,linestyle=2
;  ;
;  ;win = window(dim=[1200,600])
;  thick = 2
;  p1 = plot( wave_spec, spec_cal_norm, current=win, thick=thick, xr=[110,185], layout=[1,2,2] )
;  p2 = plot( wave_spec, model_fit_arr2[*,0], /over, color='red', thick=thick )
;  p3 = plot( wave_spec, model_fit_arr2[*,1], /over, color='orange', thick=thick )
;  p4 = plot( wave_spec, model_fit_arr2[*,2], /over, color='yellow', thick=thick )
;  p5 = plot( wave_spec, model_fit_arr2[*,3], /over, color='green', thick=thick )
;  p6 = plot( wave_spec, model_fit_arr2[*,4], /over, color='blue', thick=thick )
;  p7 = plot( wave_spec, model_fit_arr2[*,5], /over, color='indigo', thick=thick )
;  p8 = plot( wave_spec, model_fit_arr2[*,6], /over, color='violet', thick=thick )
;  for i = 0, num_atomic - 1 do $
;    pi = plot( wave_spec, model_fit_arr2[*,7+i], /over, color='red', thick=thick )
;  ;win.save, path_save + 'ajello_lab_curvefit.png'


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
  
  
  win = window(dim=[1200,800])
  ;p1 = plot( wave_spec, spec_cal_norm, current=win, thick=thick, xr=[110,185] )
  xr=[110,185]
  p2 = plot( wave_spec, model_fit_arr3[*,0], current=win, color='red', thick=thick, xr=xr, layout=[1,7,1], title='0' )
  p3 = plot( wave_spec, model_fit_arr3[*,1], current=win, color='orange', thick=thick, xr=xr, layout=[1,7,2], title='1' )
  p4 = plot( wave_spec, model_fit_arr3[*,2], current=win, color='yellow', thick=thick, xr=xr, layout=[1,7,3], title='2' )
  p5 = plot( wave_spec, model_fit_arr3[*,3], current=win, color='green', thick=thick, xr=xr, layout=[1,7,4], title='3' )
  p6 = plot( wave_spec, model_fit_arr3[*,4], current=win, color='blue', thick=thick, xr=xr, layout=[1,7,5], title='4' )
  p7 = plot( wave_spec, model_fit_arr3[*,5], current=win, color='indigo', thick=thick, xr=xr, layout=[1,7,6], title='5' )
  p8 = plot( wave_spec, model_fit_arr3[*,6], current=win, color='violet', thick=thick, xr=xr, layout=[1,7,7], title='6' )

  
  
  fcf_band = [ 0, 1, 2, 3, 4, 5, 6 ]
  fcf_val =  [ 0.043, 0.115, 0.170, 0.182, 0.159, 0.121, 0.082 ]
;  ; Constraining the Upper Level Vibrational Populations of the N2 Lyman-Birge-Hopfield Band System Using GOLD Mission's Dayglow Observations
;  ; Saurav Aryal, J. Scott Evans, J. M. Ajello, S. C. Solomon, A. W. Burns, R. W. Eastes, W. E. McClintock
;  ; First published: 13 September 2022
;  ; https://agupubs-onlinelibrary-wiley-com.colorado.idm.oclc.org/doi/full/10.1029/2021JA029869
;
;;  lbh_orig = lbh
;;  lbh_orig_tot = total( lbh_orig, 2 )
;
;  slbh_fc,at,ag_vj,xt,xg_vj,nu_vv,q_vv,ad_vv,aq_vv
;
;  common sblhfc_data,fc
;
;  ;
;  ; Calculate the energy levels: electronic (t), vibrational (g),
;  ; and rotational (f) components.  G and F components stored separately
;  ; allows calculation of band origins.  G and F stored as 2-D arrays
;  ; allows addressing according to v and j values.
;  ;
;  nmax = 40
;  nv = 7                ;a-state vibration levels
;  nvv = 20              ;X-state vibration levels
;  slbh_enlev,nmax,nv,nvv,at,ag_vj,af_vj,xt,xg_vj,xf_vj,bv,bvv
;  ;
;  ; Read in the Franck-Condon factors, compute the band orgins,
;  ; and generate the band transition probabilities for mag dipole
;  ; and elec quadr transitions
;  ;
;  slbh_fc,at,ag_vj,xt,xg_vj,nu_vv,q_vv,ad_vv,aq_vv
;
;  fcarr = fc[0:nv-1,0:19]
;  print, fcarr
;  print, total(fcarr,2)
;
;  p1 = plot( fcf_band, fcf_val )
;  p2 = plot( total(lbh_orig,1), /over, color='green' )
;  

  
;  win = window(dim=[800,600])
;  yr = [ 0, max([param[0:num_band_lbh-1],param2[0:num_band_lbh-1],param3[0:num_band_lbh-1]])*1.1 ]
;  p1 = plot( param[0:num_band_lbh-1], yr=yr, current=win, symbol='o', /sym_filled, title='LBH band weights', name='regress', font_size=14 )
;  p2 = plot( param2[0:num_band_lbh-1], /over, color='red', symbol='o', /sym_filled, sym_color='red', name='curvefit' )
;  p3 = plot( param3[0:num_band_lbh-1], /over, color='blue', symbol='o', /sym_filled, sym_color='blue', name='mpcurvefit' )
;  p4 = plot( fcf_band, fcf_val/max(fcf_val)*0.005, /over, color='green', symbol='o', /sym_filled, sym_color='green', name='FCF theory' ) 
;  leg = legend(target=[p1,p2,p3,p4],position=[0.9,0.4])  
;  
;  win = window(dim=[800,600])
;  yr = [ 0, max([param[num_band_lbh:num_feat-1],param2[num_band_lbh:num_feat-1],param3[num_band_lbh:num_feat-1]])*1.1 ]
;  x1 = arr_nist.wave_obs
;  p1 = plot( x1, param[num_band_lbh:num_feat-1], current=win, symbol='o', /sym_filled, name='regress', title='atomic emission weights', font_size=14 )  ; yr=yr, 
;  p2 = plot( x1, param2[num_band_lbh:num_feat-1], /over, color='red', symbol='o', /sym_filled, sym_color='red', name='curvefit' )
;  p3 = plot( x1, param3[num_band_lbh:num_feat-1], /over, color='blue', symbol='o', /sym_filled, sym_color='blue', name='mpcurvefit' )
;  markerp,p1,y=0,linestyle=2
;  p4 = plot( x1, arr_nist.rel_int / total(arr_nist.rel_int)*0.05, /over, color='green', symbol='o', /sym_filled, sym_color='green', name='NIST' )
;  leg = legend(target=[p1,p2,p3,p4],position=[0.9,0.4])

;  win = window(dim=[800,600])
;  yr = [ 0, max([param[num_band_lbh:num_feat-1],param2[num_band_lbh:num_feat-1],param3[num_band_lbh:num_feat-1]])*1.1 ]
;  p1 = plot( param[num_band_lbh:num_feat-1], current=win, symbol='o', /sym_filled, name='regress', title='atomic emission weights', font_size=14 )  ; yr=yr,
;  p2 = plot( param2[num_band_lbh:num_feat-1], /over, color='red', symbol='o', /sym_filled, sym_color='red', name='curvefit' )
;  p3 = plot( param3[num_band_lbh:num_feat-1], /over, color='blue', symbol='o', /sym_filled, sym_color='blue', name='mpcurvefit' )
;  markerp,p1,y=0,linestyle=2
;  p4 = plot( arr_nist.rel_int / total(arr_nist.rel_int)*0.05, /over, color='green', symbol='o', /sym_filled, sym_color='green', name='NIST' )
;  leg = legend(target=[p1,p2,p3,p4],position=[0.9,0.4])
  
  
  fcf_model = total( model[*,0:num_band_lbh-1], 1 )
  fcf_model /= total(fcf_model)
  fcf_deriv = fcf_model * param3[0:num_band_lbh-1]
  fcf_deriv /= total(fcf_deriv)
  
  win = window(dim=[800,600])
  yr = [ 0, 0.25 ]
  p1 = plot( fcf_model, current=win, symbol='o', /sym_filled, name='model', title='FCF', font_size=14, yr=yr )  ; yr=yr,
  p2 = plot( fcf_deriv, /over, color='red', symbol='o', /sym_filled, sym_color='red', name='derived' )
  leg = legend(target=[p1,p2]) ; ,position=[0.9,0.4]

  
;  function slbh2,temperature,wavelength,print=print,verbose=verbose,$
;  population_in=uspw_in,population_out=uspw,hash_out=hash_out,$
;  list_out=list_out
;
;  w = findgen( (600.-100)/0.01 ) *0.01 + 100.
;  w *= 10.
;  a = slbh2( 300., w )
;  
;  win = window(dim=[1600,600])
;  p1 = plot( w, a[*,0]/max(a[*,0]), current=win, font_size=16, thick=2, /ylog )
;  p2 = plot( wave_lbh*10., lbh_orig[*,0]/max(lbh_orig[*,0]), /over, color='red', thick=2, linestyle=2 )
;  
  stop
  
  
stop

end