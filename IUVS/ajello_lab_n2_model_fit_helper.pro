;

pro ajello_lab_n2_model_fit_helper, ndx_spec_peak, spec, model, wave_model, wl1_fit, wl2_fit, num_feat, arr_nist, num_band_lbh, $
  best
  compile_opt idl2

  tic
  best_so_far = 10000000
  int_of_best = -1
  length = 1000
  iter_list = dindgen(length) / length / 5 - 0.05
  redo:
  for l = 0, n_elements(iter_list) - 1 do begin
    ; retrieve a notional wavelength scale
    ;
    ajello_lab_pixel_scale_rot, wlfuv, wlmuv, yfuv, ymuv
    ; wlfuv *= iter_list[l]
    wlfuv *= 1.001570
    ; wlfuv -= 0.01
    ;
    ; correct wavelength scale
    ;
    wave_spec = wlfuv - wlfuv[ndx_spec_peak] + 135.44
    ; wave_spec *= 1.0000634672028672
    wave_spec += iter_list[l]
    ; wave_spec += 0.009354

    ; Subtract residual background
    ;
    ; left_pt = mean(spec[0 : long(0.1 * n_elements(spec))])
    ; right_pt = mean(spec[long(0.85 * n_elements(spec)) : n_elements(spec) - 1])
    left_end = 0.05
    right_begin = 0.95
    spec_left = spec[0 : long(left_end * n_elements(spec))]
    spec_right = spec[long(right_begin * n_elements(spec)) : n_elements(spec) - 1]
    left_ndx = spec_left[sort(spec_left)]
    right_ndx = spec_right[sort(spec_right)]
    left_pt = left_ndx[long(0.25 * n_elements(left_ndx))]
    right_pt = right_ndx[long(0.25 * n_elements(right_ndx))]

    background_slope = (left_pt - right_pt) / (wave_spec[0] - wave_spec[-1])
    spec_significant = spec - (background_slope * (wave_spec - wave_spec[0]) + left_pt)
    ; slope_start = [wave_spec[1], left_pt]
    ; p1 = plot( wave_spec, spec, xtitle='wavelength scale corrected (nm)' )
    ; markerp,p1,x=120.0,linestyle=2

    ;
    ; retrieve sensitivity
    ;
    ajello_lab_sensitivity_fuv_2026_07, wave_spec, sens

    ;
    ; calibrate the data
    ;
    spec_cal = spec_significant / sens
    spec_cal_norm = spec_cal
    ; stop

    ;
    ; interpolate the model to the wavelength scale of the data
    ;
    num_wave_spec = n_elements(wave_spec)
    modeli = fltarr(num_wave_spec, num_feat)
    for i = 0, num_feat - 1 do $
      modeli[*, i] = interpol(model[*, i], wave_model, wave_spec)

    ;
    ; limit the regression to the given wavelength range
    ;
    ndx_wave_fit = where(wave_spec gt wl1_fit and wave_spec lt wl2_fit)
    wave_spec_fit = wave_spec[ndx_wave_fit]
    x = transpose(modeli[ndx_wave_fit, *])
    y = spec_cal_norm[ndx_wave_fit]
    ; y = spec_cal[ndx_wave_fit]

    ; -----------------------------------------------------------------------
    ; MPCURVEFIT
    ; -----------------------------------------------------------------------

    ; YFIT = MPCURVEFIT(X, Y, WEIGHTS, P, [SIGMA,] FUNCTION_NAME=FUNC,
    ; ITER=iter, ITMAX=itmax,
    ; CHISQ=chisq, NFREE=nfree, DOF=dof,
    ; NFEV=nfev, COVAR=covar, [/NOCOVAR, ] [/NODERIVATIVE, ]
    ; FUNCTARGS=functargs, PARINFO=parinfo,
    ; FTOL=ftol, XTOL=xtol, GTOL=gtol, TOL=tol,
    ; ITERPROC=iterproc, ITERARGS=iterargs,
    ; NPRINT=nprint, QUIET=quiet,
    ; ERRMSG=errmsg, STATUS=status)

    ; MPCURVEFIT docs: WEIGHTS: "1D/Y     - Poisson weighting (counting statistics)"
    weights = 1. / y
    ; r3 = reform(r) > 0.
    param = fltarr(num_feat)
    param[*] = 1.
    ; xtol = 1D-10 ; default
    ; xtol = 4D-10
    parinfo = replicate({value: 0.d, fixed: 0, limited: [0, 0], limits: [0.d, 0]}, $
      num_feat)
    parinfo.limited[0] = 1 ; require all parameters to require specified lower bound
    parinfo.limits[0] = 0. ; set lower bound for all parameters to zero (i.e. positive)
    yfit = mpcurvefit(x, y, weights, param, sigma, /noderivative, parinfo = parinfo, $
      function_name = 'mlr_no_intercept', status = status, $ ; , xtol=xtol
      errmsg = errmsg, /quiet)

    param_fit = param
    param_id = ['vp0', 'vp1', 'vp2', 'vp3', 'vp4', 'vp5', 'vp6', $
      string(arr_nist.wave_obs, format = '(F6.2)')]

    model_fit_arr = fltarr(num_wave_spec, num_feat)
    for i = 0, num_feat - 1 do $
      model_fit_arr[*, i] = modeli[*, i] * param_fit[i]

    spec_fit = total(model_fit_arr, 2)

    fcf_model = total(model[*, 0 : num_band_lbh - 1], 1)
    fcf_model /= total(fcf_model)

    fcf_fit = fcf_model * param_fit[0 : num_band_lbh - 1]
    fcf_fit_err = sigma[0 : num_band_lbh - 1] / total(fcf_fit)
    fcf_fit /= total(fcf_fit)

    spec_residual = transpose(spec_cal_norm) - spec_fit
    spec_res_sdv = stddev(spec_residual[ndx_wave_fit])
    print, string(l) + '|||' + string(iter_list[l]) + '|||' + string(spec_res_sdv)

    if spec_res_sdv lt best_so_far then begin
      best_so_far = spec_res_sdv
      best = iter_list[l]
      best_int = l
    endif
  endfor
  print, string(best_so_far) + ' @ ' + string(best)
  if (best_int eq 0 or best_int eq n_elements(iter_list) - 1) then begin
    print, 'Boundary is best fit. Scaling larger'
    iter_list *= 10
    goto, redo
  endif
  toc
end
