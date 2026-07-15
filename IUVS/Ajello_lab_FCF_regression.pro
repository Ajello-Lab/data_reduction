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
  compile_opt idl2
  sz = size(X, /dim)
  F = fltarr(sz[1])
  for i = 0, sz[0] - 1 do $
    F += abs(A[i]) * X[i, *]
end

pro ajello_lab_FCF_regression
  compile_opt idl2

  ajello_lab_set_paths, path_base, path_repo

  case (get_login_info()).user_name of
    'holsclaw': begin
      ; file_data_helper = '/Volumes/projects/Phase_Development/MAVEN/IUVS_Data/IUVS_Breadboard/Round_12/data_reduction/'
      path_save = '/users/holsclaw/Documents/'
      file_path = '/Volumes/projects/Phase_Development/MAVEN/IUVS_Data/IUVS_Breadboard/Ajello_Round14/Data_Reduction/'
      file_model = path_repo + 'data/N2/n2_lbh_rot_293K.sav'
    end
    'benjamincondit': begin
      ; file_data_helper = '/Users/benjamincondit/Desktop/IUVS_Breadboard/Round 12/data_reduction/'
      path_save = '/Users/benjamincondit/Desktop/Data_Reduction_copy/'
      file_path = '/Users/benjamincondit/Desktop/Data_Reduction_copy/'
      file_model = path_repo + 'data/N2/n2_lbh_rot_293K.sav'
    end
  endcase

  folder_searcher, file_path, '.idl', 'IMAGE1', file_names, keyword_2 = 'Calibrated'

  if file_test(file_model) eq 0 then begin
    print, 'model file not found or not defined'
    stop
  endif

  ; ------------------------
  ; Restore Model
  ; ------------------------

  restore, file_model, /relax

  scaled_iuvs_psf_model, wave[0 : 301], psf
  wave_lbh = wave / 10.

  lbh_orig = lbh
  lbh_orig_tot = total(lbh_orig, 2)

  ajello_lab_nitrogen_emission_nist, arr_nist

  wl1_fit = 126.5
  ; wl1_fit = 125.0
  ; wl2_fit = 186.2
  wl2_fit = 155.0

  n = where((arr_nist.ion eq 1) and $
    (arr_nist.rel_int gt 0.) and $
    (arr_nist.wave_obs ge wl1_fit) and $
    (arr_nist.wave_obs le wl2_fit), num_atomic)

  arr_nist = arr_nist[n]

  ;
  ; Include atomic emissions
  ;
  sz = size(lbh, /dim)
  num_wave_lbh = sz[0]
  num_band_lbh = sz[1]

  model1 = fltarr(num_wave_lbh, num_band_lbh + num_atomic)
  model1[*, 0 : num_band_lbh - 1] = lbh_orig
  for i = 0, num_atomic - 1 do begin
    n = findndx(wave_lbh, arr_nist[i].wave_obs)
    model1[n, num_band_lbh + i] = arr_nist[i].rel_int / max(arr_nist.rel_int) * max(lbh_orig_tot)
  endfor

  num_feat = num_band_lbh + num_atomic

  ;
  ; convolve each rotational band by the PSF
  ;
  model = model1
  for i = 0, num_feat - 1 do $
    model[*, i] = convol(model1[*, i], psf)

  ; p1 = plot( wave, lbh_orig[*,0] )
  ; p2 = plot( wave, lbh[*,0]/total(psf), /over, color='red' )

  lbh_tot = total(model[*, 0 : num_band_lbh - 1], 2) / total(psf)

  ; ------------------------
  ; Regression Analysis
  ; ------------------------

  ajello_lab_pixel_scale_rot, wlfuv, wlmuv, yfuv, ymuv

  for i = 0, n_elements(file_names) - 1 do begin
    file_data = file_names[i]

    if file_test(file_data) eq 0 then begin
      print, 'data file not found or not defined'
      stop
    endif

    file_name = file_basename(file_data, '.idl') + '_Calibrated'
    print, 'Processing: ' + file_data

    sObj = obj_new('IDL_Savefile', file_data)
    sObj.restore, ['spec_cal', 'wave_spec']
    obj_destroy, sObj

    w1_norm = 128.5
    w2_norm = 148.4

    ndx_spec_norm = where(wave_spec gt w1_norm and wave_spec lt w2_norm)
    ndx_lbh_norm = where(wave_lbh gt w1_norm and wave_lbh lt w2_norm)
    spec_cal_norm = spec_cal / total(spec_cal[ndx_spec_norm]) / mean(deriv(wlfuv))

    ndx_out = where(wave_spec lt 115., count)
    spec_cal_norm[ndx_out] = 0.

    lbh_tot_norm = lbh_tot / total(lbh_tot[ndx_lbh_norm]) / mean(deriv(wave_lbh))

    p1 = plot(wave_spec, spec_cal_norm, yr = [0, 0.3])
    p2 = plot(wave_lbh, lbh_tot_norm, /over, color = 'red')

    ;
    ; interpolate the model to the wavelength scale of the data
    ;
    num_wave_spec = n_elements(wave_spec)
    modeli = fltarr(num_wave_spec, num_feat)
    for i = 0, num_feat - 1 do $
      modeli[*, i] = interpol(model[*, i], wave_lbh, wave_spec)

    ;
    ; limit the regression to only the range available in the model
    ;
    ndx_wave_fit = where(wave_spec gt wl1_fit and wave_spec lt wl2_fit)

    ; -----------------------------------------------------------------------
    ; MPCURVEFIT
    ; -----------------------------------------------------------------------

    ; MPCURVEFIT docs: WEIGHTS: "1D/Y     - Poisson weighting (counting statistics)"
    x = transpose(modeli[ndx_wave_fit, *])
    y = spec_cal_norm[ndx_wave_fit]
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
    yfit = mpcurvefit(x, y, weights, param, /noderivative, parinfo = parinfo, $
      function_name = 'mlr_no_intercept', status = status, $ ; , xtol=xtol
      errmsg = errmsg, /quiet)

    model_fit_arr = fltarr(num_wave_spec, num_feat)
    for i = 0, num_feat - 1 do $
      model_fit_arr[*, i] = modeli[*, i] * param[i]

    spec_fit = total(model_fit_arr, 2)

    yr_ndx = where(wave_spec gt wl1_fit and wave_spec lt wl2_fit)
    yr = [0, max(spec_cal_norm[yr_ndx]) * 1.1]
    lbh_tot = total(model_fit_arr[*, 0 : 6], 2)
    yr1 = [0, max(lbh_tot[yr_ndx]) * 1.3]

    win = window(dim = [1900, 800])
    thick = 2
    p1 = plot(wave_spec, spec_cal_norm, current = win, thick = thick, xr = [110, 185], yr = yr, position = [0.03, 0.53, 0.42, 0.95], title = file_name, $
      xtitle = 'Wavelength (nm)', ytitle = 'Intensity (arb. units)', font_name = 'times', font_size = 12)
    ; p2 = plot( wave_spec_fit, yfit3, color='red', /over )
    p3 = plot(wave_spec, spec_fit, color = 'red', /over)
    markerp, p1, x = wl1_fit, linestyle = 2
    markerp, p1, x = wl2_fit, linestyle = 2

    p1 = plot(wave_spec, spec_cal_norm, current = win, thick = thick, xr = [126.5, 155], yr = yr1, position = [0.46, 0.53, 0.68, 0.95], $
      xtitle = 'Wavelength (nm)', ytitle = 'Intensity (arb. units)', font_name = 'times', font_size = 12)
    ; p2 = plot( wave_spec_fit, yfit3, color='red', /over )
    p3 = plot(wave_spec, spec_fit, color = 'red', /over)

    thick = 2
    p1 = plot(wave_spec, spec_cal_norm, current = win, thick = thick, xr = [110, 185], yr = yr, position = [0.02, 0.06, 0.42, 0.48], $
      xtitle = 'Wavelength (nm)', ytitle = 'Intensity (arb. units)', name = 'Data', font_name = 'times', font_size = 12)
    p2 = plot(wave_spec, model_fit_arr[*, 0], /over, color = 'red', thick = thick, name = 'LBH0')
    p3 = plot(wave_spec, model_fit_arr[*, 1], /over, color = 'orange', thick = thick, name = 'LBH1')
    p4 = plot(wave_spec, model_fit_arr[*, 2], /over, color = 'yellow', thick = thick, name = 'LBH2')
    p5 = plot(wave_spec, model_fit_arr[*, 3], /over, color = 'green', thick = thick, name = 'LBH3')
    p6 = plot(wave_spec, model_fit_arr[*, 4], /over, color = 'blue', thick = thick, name = 'LBH4')
    p7 = plot(wave_spec, model_fit_arr[*, 5], /over, color = 'indigo', thick = thick, name = 'LBH5')
    p8 = plot(wave_spec, model_fit_arr[*, 6], /over, color = 'violet', thick = thick, name = 'LBH6')
    for i = 0, num_atomic - 1 do $
      pi = plot(wave_spec, model_fit_arr[*, 7 + i], /over, color = 'gray', thick = thick, name = 'N1 Spectra')
    leg = legend(target = [p1, p2, p3, p4, p5, p6, p7, p8, pi], sample_width = 0.08, position = [0.8, 0.95], $
      vertical_spacing = 0.01, font_size = 8, font_name = 'times', linestyle = 6, horizontal_spacing = 0.01, font_style = 'bold')
    markerp, p1, x = wl1_fit, linestyle = 2
    markerp, p1, x = wl2_fit, linestyle = 2

    p1 = plot(wave_spec, spec_cal_norm, current = win, thick = thick, xr = [126.5, 155], yr = yr1, position = [0.46, 0.06, 0.68, 0.48], $
      xtitle = 'Wavelength (nm)', ytitle = 'Intensity (arb. units)', font_name = 'times', font_size = 12)
    p2 = plot(wave_spec, model_fit_arr[*, 0], /over, color = 'red', thick = thick)
    p3 = plot(wave_spec, model_fit_arr[*, 1], /over, color = 'orange', thick = thick)
    p4 = plot(wave_spec, model_fit_arr[*, 2], /over, color = 'yellow', thick = thick)
    p5 = plot(wave_spec, model_fit_arr[*, 3], /over, color = 'green', thick = thick)
    p6 = plot(wave_spec, model_fit_arr[*, 4], /over, color = 'blue', thick = thick)
    p7 = plot(wave_spec, model_fit_arr[*, 5], /over, color = 'indigo', thick = thick)
    p8 = plot(wave_spec, model_fit_arr[*, 6], /over, color = 'violet', thick = thick)
    for i = 0, num_atomic - 1 do $
      pi = plot(wave_spec, model_fit_arr[*, 7 + i], /over, color = 'gray', thick = thick)

    fcf_model = total(model[*, 0 : num_band_lbh - 1], 1)
    fcf_model /= total(fcf_model)
    fcf_deriv = fcf_model * param[0 : num_band_lbh - 1]
    fcf_deriv /= total(fcf_deriv)

    p1 = plot(fcf_model, current = win, symbol = 'o', /sym_filled, name = 'model', $
      title = 'FCF', font_size = 12, font_name = 'times', yr = [0, 0.25], position = [0.71, 0.06, 0.97, 0.48])
    p2 = plot(fcf_deriv, /over, color = 'red', symbol = 'o', /sym_filled, sym_color = 'red', name = 'derived')
    leg = legend(target = [p1, p2], position = [0.8, 0.65], font_name = 'times', font_size = 8, sample_width = 0.1, font_style = 'bold')
    ; win.save, path_save + file_name + '.png'

    stop
  endfor
  stop
end
