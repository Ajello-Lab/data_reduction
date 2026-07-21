;+
; PURPOSE
;  This routine will fit an LBH model vprime component spectra to
;   a measured IUVS N2 spectrum.
;
; INPUTS
;  wave_spec: [NW] corrected wavelength scale, nm
;  spec: [NW] observed and calibrated N2 spectrum
;
; OUTPUTS
;  param_fit : [NP] Fit parameters
;  param_id : [NP] identification of each parameter
;  fcf_fit : [NP] Derived Franck-Condon factors
;  model_fit_arr : [NW,NP] array of model spectra fits
;  spec_fit : [NW] final spectral fit to the data
;
; KEYWORDS
;  wl1_fit: (optional) lower bound of wavelength range to consider in fit, nm
;    Default: 126.5 nm
;  wl2_fit: (optional) upper bound of wavelength range to consider in fit, nm
;    Default: 155.0 nm
;
;  NOTES
; [NW] = Number of Wavelengths
; [NP] = Number of Parameters
; -
pro mlr_no_intercept, X, A, F
  compile_opt idl2
  sz = size(X, /dim)
  F = fltarr(sz[1])
  for i = 0, sz[0] - 1 do $
    F += abs(A[i]) * X[i, *]
end

pro ajello_lab_n2_model_fit, wave_spec, spec, $
  param_fit, $
  param_id, $
  fcf_fit, $
  model_fit_arr, $
  spec_fit, $
  wl1_fit = wl1_fit, $
  wl2_fit = wl2_fit, $
  show_plots = show_plots, $
  save_data = save_data, $
  align_data = align_data
  compile_opt idl2

  if keyword_set(wl1_fit) eq 0 then wl1_fit = 126.5
  if keyword_set(wl2_fit) eq 0 then wl2_fit = 162.0

  if n_params() eq 0 then begin
    show_plots = 1
    save_data = 0
    align_data = 1

    ajello_lab_set_paths, path_base, path_repo

    case (get_login_info()).user_name of
      'holsclaw': begin
        ; file_data_helper = '/Volumes/projects/Phase_Development/MAVEN/IUVS_Data/IUVS_Breadboard/Round_12/data_reduction/'
        path_save = '/users/holsclaw/Documents/'
        file_data = '/Volumes/projects/Phase_Development/MAVEN/IUVS_Data/IUVS_Breadboard/Ajello_Round14/Data_Reduction/N2_30EV_FUV_TEST19_IMAGE1.idl'
      end
      'benjamincondit': begin
        ; file_data_helper = '/Users/benjamincondit/Desktop/IUVS_Breadboard/Round 12/data_reduction/'
        save_data = '/Users/benjamincondit/Desktop/Data_Reduction copy/'
        file_data = '/Users/benjamincondit/Desktop/Data_Reduction copy/N2_30EV_FUV_TEST19_IMAGE1.idl'
      end
    endcase

    if file_test(file_data) eq 0 then begin
      print, 'data file not found or not defined'
      stop
    endif
    ;
    ; retrieve LBH model
    ;
    wave1_lbh = 100.
    wave2_lbh = 450.
    waved_lbh = 0.04
    wave_lbh = findgen((wave2_lbh - wave1_lbh) / waved_lbh) * waved_lbh + wave1_lbh
    lbh_temp = 300. ; Kelvin
    lbh = slbh2(lbh_temp, wave_lbh * 10.)
    wave_lbh_step = mean(deriv(wave_lbh))
    num_wave_lbh = n_elements(wave_lbh)

    sz = size(lbh, /dim)
    num_wave_lbh = sz[0]
    num_band_lbh = sz[1]

    ;
    ; create a PSF for LBH model
    ;
    scaled_iuvs_psf_model, wave_lbh[0 : 301], psf

    ; if keyword_set(show_plots) then begin
    ; ;
    ; ; Show each vprime component
    ; ;
    ; win = window(dim = [1200, 800])
    ; wave_first = [145.1, 141.7, 138.4, 135.5, 132.6, 130.0, 127.4]
    ; norm1 = fltarr(num_band_lbh)
    ; normw = 2.
    ; xr = [min(wave_lbh), max(wave_lbh)]
    ; for i = 0, num_band_lbh - 1 do begin
    ; v1 = lbh[*, i] / total(lbh[*, i]) ; /wave_lbh_step
    ; p1i = plot(wave_lbh, v1, layout = [1, num_band_lbh, i + 1], current = win, /ylog, xr = xr)

    ; w1 = wave_first[i] - normw
    ; w2 = wave_first[i] + normw
    ; ndx1 = where( $
    ; (wave_lbh gt w1) and $
    ; (wave_lbh lt w2), count1)
    ; norm1[i] = total(v1[ndx1]) / total(v1)
    ; ; ps = plot_shade( p1i, w1, w2, fill_transparency=70, fill_color='blue'  )
    ; endfor
    ; ; win.save, path_save + 'ajello_lab_n2_models_plot_each_vprime.png'
    ; endif

    ; win = window(dim=[800,600])
    ; p1 = plot( total( lbh, 1 ) / total( lbh ), current=win, font_size=14, $
    ; yr=[0,0.25], symbol='o', /sym_filled, $
    ; title="ratio sum across v'' to sum", xtitle="v'" )

    ;
    ; retrieve atomic nitrogen emissions and filter
    ;
    ajello_lab_nitrogen_emission_nist, arr_nist
    n = where((arr_nist.ion eq 1) and $
      (arr_nist.rel_int gt 0.) and $
      (arr_nist.wave_obs ge wl1_fit) and $
      (arr_nist.wave_obs le wl2_fit), num_atomic)
    arr_nist = arr_nist[n]

    ;
    ; Include atomic emissions
    ;
    model = fltarr(num_wave_lbh, num_band_lbh + num_atomic)
    model[*, 0 : num_band_lbh - 1] = lbh

    ;
    ; introduce a delta function at the atomic emission wavelength
    ; scale the magnitude to place it on a similar scale as the LBH emissions,
    ; but preserve the relative intensity
    ;
    for i = 0, num_atomic - 1 do begin
      n = findndx(wave_lbh, arr_nist[i].wave_obs)
      model[n, num_band_lbh + i] = arr_nist[i].rel_int / max(arr_nist.rel_int) * max(lbh)
    endfor

    ;
    ; total number of feature components is the sum of the number of LBH
    ; vibration bands and atomic emissions
    ;
    num_feat = num_band_lbh + num_atomic

    ;
    ; convolve each component by the PSF
    ;
    model_sm = model
    for i = 0, num_feat - 1 do $
      model_sm[*, i] = convol(model[*, i], psf) / total(psf) / wave_lbh_step

    model = model_sm
    wave_model = wave_lbh

    ; LOAD IN DATA ***********************
    ; restore, file_data, /verbose
    sObj = obj_new('IDL_Savefile', file_data)
    sObj.restore, 'arr'
    obj_destroy, sObj

    ;
    ; spatial profile
    ;
    spat = total(arr, 1, /nan)

    ;
    ; create a single spectrum
    ;
    y1_key = 130 ; outside key hole
    y2_key = 940

    ndx_spat_peak = findndx(spat, max(spat))
    yw = 100
    y1 = (ndx_spat_peak - yw) > y1_key
    y2 = (ndx_spat_peak + yw) < y2_key

    ; p = plot( spat )
    ; markerp,p,x=y1,linestyle=2
    ; markerp,p,x=y2,linestyle=2

    spec = total(arr[*, y1 : y2], 2, /nan) ; for rotated image

    ; p1 = plot( spec, xtitle='pixel' )

    ; ndx_spec_peak = findndx(spec, max(spec))
    spec_modded = spec
    spec_modded[0 : 335] = 0
    spec_modded[359 : n_elements(spec_modded) - 1] = 0
    modded_wl = findgen(n_elements(spec_modded))
    gfit = gaussfit(modded_wl, spec_modded, A, nterms = 4)
    ndx_spec_peak = (where(gfit eq max(gfit)))[0]
    ; ndx_spec_peak = (where(spec_modded eq max(spec_modded)))[0]
    ;

    ; Runs through iterations to find the best offset of the data using the mpfit
    if keyword_set(align_data) then begin
      ajello_lab_n2_model_fit_helper, ndx_spec_peak, spec, model, wave_model, wl1_fit, wl2_fit, num_feat, arr_nist, num_band_lbh, $
        best
    endif

    ; retrieve a notional wavelength scale
    ;
    ajello_lab_pixel_scale_rot, wlfuv, wlmuv, yfuv, ymuv
    wlfuv *= 1.001570
    ;
    ; correct wavelength scale
    ;
    wave_spec = wlfuv - wlfuv[ndx_spec_peak] + 135.44

    if keyword_set(align_data) then $
      wave_spec += best

    ; wave_model_offset = dindgen(10000) / 10000 - 2
    ; ajello_lab_spectrum_wavelength_offset, wave_spec, spec, wave_model, model, wl1_fit, wl2_fit, $
    ; wave_model_offset, wave_model_offset_opt, show_plots = 1
    ; ; wave_spec += 0.009354
    ; wave_spec -= wave_model_offset_opt

    ; p1 = plot( wave_spec, spec, xtitle='wavelength scale corrected (nm)' )
    ; markerp,p1,x=120.0,linestyle=2

    ;
    ; retrieve sensitivity
    ;
    ajello_lab_sensitivity_fuv_2026_07, wave_spec, sens

    ;
    ; calibrate the data
    ;
    spec_cal = spec / sens
    spec_cal_norm = spec_cal
    ; stop
  endif

  ;
  ; normalize the calibrated data as a plotting convenience
  ;
  ; ndx_spec_norm = where( wave_spec gt wl1_fit and wave_spec lt wl2_fit )
  ; ndx_lbh_norm = where( wave_lbh gt wl1_fit and wave_lbh lt wl2_fit )
  ; spec_cal_norm = spec_cal / total(spec_cal[ndx_spec_norm]) ;/ mean(deriv(wlfuv))

  if n_params() ne 0 then $
    spec_cal_norm = spec
  ;
  ; ;
  ; ; force the observed data to be positive
  ; ;
  ; ;spec_cal_norm >= 0.
  ; ;
  ; ; force observed data out of band to be zero
  ; ;
  ; ndx_out = where( wave_spec lt 115., count )
  ; spec_cal_norm[ndx_out] = 0.
  ;
  ; yr = [0, max( spec_cal_norm[ndx_spec_norm] )*1.1 ]

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
  print, spec_res_sdv

  if keyword_set(show_plots) then begin
    if keyword_set(save_data) then $
      file_name = file_basename(save_data, '.idl') + '_LBH_fitted' else file_name = ''
    yr_ndx = where(wave_spec gt wl1_fit and wave_spec lt wl2_fit)
    lbh_tot = total(model_fit_arr[*, 0 : 6], 2)

    win = window(dim = [1900, 800])
    win.refresh, /disable
    thick = 2
    p1 = plot(wave_spec, spec_cal_norm, current = win, thick = thick, xr = [110, 185], yr = [1.1 * min(spec_residual[yr_ndx]), max(spec_cal_norm[yr_ndx]) * 1.1], position = [0.04, 0.53, 0.42, 0.95], title = file_name, $
      xtitle = 'Wavelength (nm)', ytitle = 'Intensity (arb. units)', font_name = 'times', font_size = 12)
    ; p2 = plot( wave_spec_fit, yfit3, color='red', /over )
    p3 = plot(wave_spec, spec_fit, color = 'red', /over)
    markerp, p1, x = wl1_fit, linestyle = 2
    markerp, p1, x = wl2_fit, linestyle = 2
    p4 = plot_shade(p1, wave_spec[335], wave_spec[359], fill_transparency = 70, fill_color = 'blue')
    ps = plot([1, 1], [1, 1], /overplot, color = 'medium slate blue', thick = 10, name = 'Reference Peak')
    p5 = plot(wave_spec, spec_residual, color = 'light blue', /over)
    p6 = plot(wave_spec, 0 * wave_spec, color = 'black', /over, linestyle = 5)

    p1 = plot(wave_spec, spec_cal_norm, current = win, thick = thick, xr = [126.5, 155], yr = [1.1 * min(spec_residual[yr_ndx]), max(lbh_tot[yr_ndx]) * 1.2], position = [0.46, 0.53, 0.68, 0.95], $
      xtitle = 'Wavelength (nm)', ytitle = 'Intensity (arb. units)', font_name = 'times', font_size = 12)
    ; p2 = plot( wave_spec_fit, yfit3, color='red', /over )
    pf = plot(wave_spec, spec_fit, color = 'red', /over, name = 'Fit Total')
    pr = plot(wave_spec, spec_residual, color = 'light blue', /over, name = 'Fit Residual')
    p2 = plot(wave_spec, 0 * wave_spec, color = 'black', /over, linestyle = 5)

    thick = 2
    p1 = plot(wave_spec, spec_cal_norm, current = win, thick = thick, xr = [110, 185], yr = [0, max(spec_cal_norm[yr_ndx]) * 1.1], position = [0.04, 0.06, 0.42, 0.47], $
      xtitle = 'Wavelength (nm)', ytitle = 'Intensity (arb. units)', name = 'Data', font_name = 'times', font_size = 12)
    p2 = plot(wave_spec, model_fit_arr[*, 0], /over, color = 'firebrick', thick = thick, name = 'LBH0')
    p3 = plot(wave_spec, model_fit_arr[*, 1], /over, color = 'orange', thick = thick, name = 'LBH1')
    p4 = plot(wave_spec, model_fit_arr[*, 2], /over, color = 'yellow', thick = thick, name = 'LBH2')
    p5 = plot(wave_spec, model_fit_arr[*, 3], /over, color = 'green', thick = thick, name = 'LBH3')
    p6 = plot(wave_spec, model_fit_arr[*, 4], /over, color = 'blue', thick = thick, name = 'LBH4')
    p7 = plot(wave_spec, model_fit_arr[*, 5], /over, color = 'indigo', thick = thick, name = 'LBH5')
    p8 = plot(wave_spec, model_fit_arr[*, 6], /over, color = 'violet', thick = thick, name = 'LBH6')
    for i = 0, num_atomic - 1 do $
      pi = plot(wave_spec, model_fit_arr[*, 7 + i], /over, color = 'gray', thick = thick, name = 'N1 Spectra')
    leg = legend(target = [p1, pf, pr, p2, p3, p4, p5, p6, p7, p8, pi, ps], sample_width = 0.08, position = [0.8, 0.95], $
      font_size = 8, font_name = 'times', linestyle = 6, font_style = 'bold')
    markerp, p1, x = wl1_fit, linestyle = 2
    markerp, p1, x = wl2_fit, linestyle = 2

    p1 = plot(wave_spec, spec_cal_norm, current = win, thick = thick, xr = [126.5, 155], yr = [0, max(lbh_tot[yr_ndx]) * 1.2], position = [0.46, 0.06, 0.68, 0.47], $
      xtitle = 'Wavelength (nm)', ytitle = 'Intensity (arb. units)', font_name = 'times', font_size = 12)
    p2 = plot(wave_spec, model_fit_arr[*, 0], /over, color = 'firebrick', thick = thick)
    p3 = plot(wave_spec, model_fit_arr[*, 1], /over, color = 'orange', thick = thick)
    p4 = plot(wave_spec, model_fit_arr[*, 2], /over, color = 'yellow', thick = thick)
    p5 = plot(wave_spec, model_fit_arr[*, 3], /over, color = 'green', thick = thick)
    p6 = plot(wave_spec, model_fit_arr[*, 4], /over, color = 'blue', thick = thick)
    p7 = plot(wave_spec, model_fit_arr[*, 5], /over, color = 'indigo', thick = thick)
    p8 = plot(wave_spec, model_fit_arr[*, 6], /over, color = 'violet', thick = thick)
    for i = 0, num_atomic - 1 do $
      pi = plot(wave_spec, model_fit_arr[*, 7 + i], /over, color = 'gray', thick = thick)

    p1 = plot(fcf_model, current = win, symbol = 'o', /sym_filled, name = 'model', $
      title = 'FCF', font_size = 12, font_name = 'times', yr = [0, 0.25], position = [0.71, 0.06, 0.97, 0.48])
    p2 = errorplot(fcf_fit, fcf_fit_err, /over, color = 'red', symbol = 'o', /sym_filled, sym_color = 'red', name = 'derived')
    leg = legend(target = [p1, p2], position = [0.8, 0.65], font_name = 'times', font_size = 8, sample_width = 0.1, font_style = 'bold', linestyle = 6)
    win.refresh
    if keyword_set(save_data) then $
      win.save, file_dirname(save_data) + path_sep() + file_name + '.png'

    ; win = window(dim = [1200, 800])
    ; thick = 2
    ; p1 = plot(wave_spec, spec_cal_norm, current = win, thick = thick, xr = [110, 185], layout = [1, 2, 1], title = 'mpcurvefit')
    ; ; p2 = plot( wave_spec_fit, yfit3, color='red', /over )
    ; p3 = plot(wave_spec, spec_fit, color = 'red', /over)
    ; markerp, p1, x = wl1_fit, linestyle = 2
    ; markerp, p1, x = wl2_fit, linestyle = 2
    ; ;
    ; ; win = window(dim=[1200,600])
    ; thick = 2
    ; p1 = plot(wave_spec, spec_cal_norm, current = win, thick = thick, xr = [110, 185], layout = [1, 2, 2])
    ; p2 = plot(wave_spec, model_fit_arr[*, 0], /over, color = 'red', thick = thick)
    ; p3 = plot(wave_spec, model_fit_arr[*, 1], /over, color = 'orange', thick = thick)
    ; p4 = plot(wave_spec, model_fit_arr[*, 2], /over, color = 'yellow', thick = thick)
    ; p5 = plot(wave_spec, model_fit_arr[*, 3], /over, color = 'green', thick = thick)
    ; p6 = plot(wave_spec, model_fit_arr[*, 4], /over, color = 'blue', thick = thick)
    ; p7 = plot(wave_spec, model_fit_arr[*, 5], /over, color = 'indigo', thick = thick)
    ; p8 = plot(wave_spec, model_fit_arr[*, 6], /over, color = 'violet', thick = thick)
    ; for i = 0, num_atomic - 1 do $
    ; pi = plot(wave_spec, model_fit_arr[*, 7 + i], /over, color = 'red', thick = thick)
    ; markerp, p1, x = wl1_fit, linestyle = 2
    ; markerp, p1, x = wl2_fit, linestyle = 2
    ; ; win.save, path_save + 'ajello_lab_mpcurvefit.png'

    ; win = window(dim = [1200, 800])
    ; ; p1 = plot( wave_spec, spec_cal_norm, current=win, thick=thick, xr=[110,185] )
    ; xr = [110, 185]
    ; p2 = plot(wave_spec, model_fit_arr[*, 0], current = win, color = 'red', thick = thick, xr = xr, layout = [1, 7, 1], title = '0')
    ; p3 = plot(wave_spec, model_fit_arr[*, 1], current = win, color = 'orange', thick = thick, xr = xr, layout = [1, 7, 2], title = '1')
    ; p4 = plot(wave_spec, model_fit_arr[*, 2], current = win, color = 'yellow', thick = thick, xr = xr, layout = [1, 7, 3], title = '2')
    ; p5 = plot(wave_spec, model_fit_arr[*, 3], current = win, color = 'green', thick = thick, xr = xr, layout = [1, 7, 4], title = '3')
    ; p6 = plot(wave_spec, model_fit_arr[*, 4], current = win, color = 'blue', thick = thick, xr = xr, layout = [1, 7, 5], title = '4')
    ; p7 = plot(wave_spec, model_fit_arr[*, 5], current = win, color = 'indigo', thick = thick, xr = xr, layout = [1, 7, 6], title = '5')
    ; p8 = plot(wave_spec, model_fit_arr[*, 6], current = win, color = 'violet', thick = thick, xr = xr, layout = [1, 7, 7], title = '6')

    ; win = window(dim = [800, 600])
    ; yr = [0, 0.25]
    ; p1 = plot(fcf_model, current = win, symbol = 'o', /sym_filled, name = 'model', title = 'FCF', font_size = 14, yr = yr) ; yr=yr,
    ; p2 = plot(fcf_fit, /over, color = 'red', symbol = 'o', /sym_filled, sym_color = 'red', name = 'derived')
    ; leg = legend(target = [p1, p2]) ; ,position=[0.9,0.4]
  endif

  if keyword_set(save_data) then begin
    path_save = file_dirname(save_data) + path_sep()
    desc = [ $
      'arr_nist: atomic nitrogen emissions added to LBH bands and retrieved from NIST', $
      'fcf_fit: franck-condon factors calculated from lbh fit to data', $
      'fcf_fit_err: the error of the fit for the franck-condon factors', $
      'lbh_temp: temperature (K) used to retrieve LBH bands', $
      'lbh: lbh model used in fitting to the data', $
      'model_fit_arr: fitted model to data including atomic nitrogen emissions and LBH bands', $
      'num_atomic: number of included atmoic nitrogen emissions', $
      'num_band_lbh: number of included lbh bands', $
      'param_id: identification of each fit parameter', $
      'param_fit: Fit parameters for lbh bands to data', $
      'spec_fit: total spectrum of fitted data', $
      'spec: observed and calibrated N2 spectrum', $
      'wave_spec: corrected wavelength scale, nm', $
      'waved_lbh: LBH model wavelength scale step size, nm', $
      'wl1_fit: lower bound of wavelength range to consider in fit, nm', $
      'wl2_fit: upper bound of wavelength range to consider in fit, nm']
    file_name = file_basename(save_data, '.idl')
    save, filename = path_save + file_name + '_LBH_fit.idl', arr_nist, fcf_fit, fcf_fit_err, lbh_temp, lbh, waved_lbh, $
      model_fit_arr, num_atomic, num_band_lbh, param_id, param_fit, spec_fit, spec, wave_spec, wl1_fit, wl2_fit, desc
  endif

  spec_modded = [replicate(0.0, 548), spec_cal_norm[548 : 566], replicate(0.0, n_elements(spec_cal_norm) - 565)]
  spec_fit_modded = [replicate(0.0, 548), spec_fit[548 : 566], replicate(0.0, n_elements(spec_fit) - 565)]
  modded_wl = findgen(n_elements(spec_modded))
  gfit = gaussfit(modded_wl, spec_modded, A, nterms = 4)
  gfit = gaussfit(modded_wl, spec_fit_modded, B, nterms = 4)

  print, A
  print, B

  p1 = plot(wave_spec, spec_cal_norm, xr = [126.5, 155])
  p2 = plot(wave_spec, spec_fit, /over, color = 'red')
  stop
  if n_params() eq 0 then stop
end
