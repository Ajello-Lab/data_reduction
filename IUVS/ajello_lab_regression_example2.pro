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
pro ajello_lab_regression_example2
  compile_opt idl2

  ajello_lab_set_paths, path_base, path_repo

  file_data = strarr(3)
  case (get_login_info()).user_name of
    'holsclaw': begin
      ; file_data_helper = '/Volumes/projects/Phase_Development/MAVEN/IUVS_Data/IUVS_Breadboard/Round_12/data_reduction/'
      path_save = '/users/holsclaw/Documents/'
      file_data[0] = '/Volumes/projects/Phase_Development/MAVEN/IUVS_Data/IUVS_Breadboard/Ajello_Round14/Data_Reduction/N2_30EV_FUV_TEST19_IMAGE1.idl'
      file_data[1] = ''
      file_data[2] = ''
      file_model = path_repo + 'data/N2/n2_lbh_rot_293K.sav'
    end
    'benjamincondit': begin
      ; file_data_helper = '/Users/benjamincondit/Desktop/IUVS_Breadboard/Round 12/data_reduction/'
      path_save = '/Users/benjamincondit/idl/data_reduction/IUVS/Sensitivities/'
      file_data[0] = '/Users/benjamincondit/Desktop/IUVS_Breadboard/Ajello_Round14/Data_Reduction/N2_30EV_FUV_TEST19_IMAGE1.idl'
      file_data[1] = '/Users/benjamincondit/Desktop/IUVS_Breadboard/Ajello_Round14/Data_Reduction/N2_30EV_FUV_TEST20_IMAGE2.idl'
      file_data[2] = '/Users/benjamincondit/Desktop/IUVS_Breadboard/Ajello_Round14/Data_Reduction/N2_30EV_FUV_TEST21_IMAGE3.idl'
      file_model = path_repo + 'data/N2/n2_lbh_rot_293K.sav'
    end
  endcase

  num_images = 3

  if file_test(file_data[0]) eq 0 then begin
    print, 'data file 1 not found or not defined'
    stop
  endif

  if file_test(file_data[1]) eq 0 then begin
    print, 'data file 2 not found or not defined'
    stop
  endif

  if file_test(file_data[2]) eq 0 then begin
    print, 'data file 3 not found or not defined'
    stop
  endif

  if file_test(file_model) eq 0 then begin
    print, 'model file not found or not defined'
    stop
  endif

  ;
  ; generate an estimate of the instrument point spread function
  ; (actually, this is the line-spread function)
  ;
  restore, file_model, /relax
  ; % RESTORE: Recovering incompatible definition of structure LIST using relaxed structure assignment rules.
  ; % RESTORE: Recovering incompatible definition of structure HASH using relaxed structure assignment rules.
  ; % RESTORE: Restored variable: TROT.
  ; % RESTORE: Restored variable: WAVE.
  ; % RESTORE: Restored variable: LBH.
  ; % RESTORE: Restored variable: LIST.
  scaled_iuvs_psf_model, wave[0 : 301], psf
  wave_lbh = wave / 10.

  lbh_orig = lbh

  ;
  ; Add in atomic features
  sz = size(lbh, /dim)
  lbh = fltarr(sz[0], sz[1] + 4)
  lbh[*, 0 : 6] = lbh_orig

  n = findndx(wave_lbh, 149.3)
  lbh[n, 7] = 0.1

  n = findndx(wave_lbh, 131.1)
  lbh[n, 8] = 0.1

  n = findndx(wave_lbh, 131.9)
  lbh[n, 9] = 0.1

  n = findndx(wave_lbh, 141.2)
  lbh[n, 10] = 0.1

  ; convolve each rotational band by the PSF
  for i = 0, ((size(lbh))[2] - 1) do begin
    lbh[*, i] = convol(lbh[*, i], psf)
  endfor

  p1 = plot(wave, lbh_orig[*, 0], title = '1')
  p2 = plot(wave, lbh[*, 0] / total(psf), /over, color = 'red')

  lbh_orig_tot = total(lbh_orig, 2)
  lbh_tot = total(lbh, 2) / total(psf)

  p1 = plot(wave, lbh_orig_tot, title = '2')
  p2 = plot(wave, lbh_tot, /over, color = 'red')
  ; peak at 1354 angstroms

  stop

  ; *********************** LOAD IN DATA ***********************
  ; restore, file_data, /verbose

  spec = fltarr(num_images, 1024)
  spec_cal = fltarr(num_images, 1024)

  for i = 0, num_images - 1 do begin
    sObj = obj_new('IDL_Savefile', file_data[i])
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

    p = plot(spat, title = '3')
    markerp, p, x = y1, linestyle = 2
    markerp, p, x = y2, linestyle = 2

    spec[i, *] = total(arr[*, y1 : y2], 2, /nan) ; for rotated image

    p1 = plot(spec[i, *], xtitle = 'pixel ' + string(i))

    ajello_lab_pixel_scale_rot, wlfuv, wlmuv, yfuv, ymuv

    ndx_spec_peak = findndx(spec[i, *], max(spec[i, where(wlfuv gt 120 and wlfuv lt 150)]))

    p1 = plot(wlfuv, spec[i, *], xtitle = 'wavelength guess (nm)')
    markerp, p1, x = 135.4, linestyle = 2

    wave_spec = wlfuv - wlfuv[ndx_spec_peak] + 120.0

    p1 = plot(wave_spec, spec[i, *], xtitle = 'wavelength calc (nm)')
    markerp, p1, x = 120.0

    ajello_lab_sensitivity_fuv_2026_07, wave_spec, sens

    w1 = 128.5
    w2 = 148.4

    spec_cal[i, *] = spec[i, *] / sens
  endfor

  ndx_spec_norm = where(wave_spec gt w1 and wave_spec lt w2)
  ndx_lbh_norm = where(wave_lbh gt w1 and wave_lbh lt w2)

  spec_cal_norm = spec_cal[0, *] / total(spec_cal[0, ndx_spec_norm]) / mean(deriv(wlfuv))
  norm_factor = 1 / total(spec_cal[0, ndx_spec_norm]) / mean(deriv(wlfuv))

  for i = 0, num_images - 1 do $
    spec[i, *] *= norm_factor

  spec_cal_norm = total(spec, 1)

  spec_cal_norm >= 0.
  ndx_out = where(wave_spec lt 115., count)
  spec_cal_norm[ndx_out] = 0.

  yr = [0, max(spec_cal_norm[ndx_spec_norm]) * 1.1]

  lbh_tot_norm = lbh_tot / total(lbh_tot[ndx_lbh_norm]) / mean(deriv(wave_lbh))

  p1 = plot(wave_spec, spec_cal_norm, yr = [0, 0.3])
  p2 = plot(wave_lbh, lbh_tot_norm, /over, color = 'red')

  ;
  ; interpolate the model to the wavelength scale of the data
  ;
  nwave_spec = n_elements(wave_spec)
  num_vecs = n_elements(lbh[0, *])
  lbhi_norm = fltarr(nwave_spec, num_vecs)
  for i = 0, ((size(lbh))[2] - 1) do $
    lbhi_norm[*, i] = interpol(lbh[*, i], wave_lbh, wave_spec)

  ;
  ; limit the regression to only the range available in the model
  ;
  ndx_wave_fit = where(wave_spec gt 125 and wave_spec lt 155)

  ; perform regression
  ;
  ; Result = REGRESS( X, Y, [, CHISQ=variable] [, CONST=variable] [, CORRELATION=variable]
  ; [, /DOUBLE] [, FTEST=variable] [, MCORRELATION=variable] [, MEASURE_ERRORS=vector]
  ; [, SIGMA=variable] [, STATUS=variable] [, YFIT=variable] )
  ;
  wave_spec_fit = wave_spec[ndx_wave_fit]
  x = transpose(lbhi_norm[ndx_wave_fit, *])
  y = spec_cal_norm[ndx_wave_fit]
  measure_errors = sqrt(y)
  ;
  r = regress(x, y, const = const) ; , measure_errors=measure_errors

  win = window(dim = [800, 600])
  thick = 2
  p1 = plot(wave_spec, spec_cal_norm, current = win, thick = thick, yr = [-0.05, 0.5])
  p2 = plot(wave_spec, lbhi_norm[*, 0] * r[0, 0], /over, color = 'red', thick = thick)
  p3 = plot(wave_spec, lbhi_norm[*, 1] * r[0, 1], /over, color = 'orange', thick = thick)
  p4 = plot(wave_spec, lbhi_norm[*, 2] * r[0, 2], /over, color = 'yellow', thick = thick)
  p5 = plot(wave_spec, lbhi_norm[*, 3] * r[0, 3], /over, color = 'green', thick = thick)
  p6 = plot(wave_spec, lbhi_norm[*, 4] * r[0, 4], /over, color = 'blue', thick = thick)
  p7 = plot(wave_spec, lbhi_norm[*, 5] * r[0, 5], /over, color = 'indigo', thick = thick)
  p8 = plot(wave_spec, lbhi_norm[*, 6] * r[0, 6], /over, color = 'violet', thick = thick)
  p9 = plot(wave_spec, lbhi_norm[*, 7] * r[0, 7], /over, color = 'light salmon', thick = thick)
  p10 = plot(wave_spec, lbhi_norm[*, 8] * r[0, 8], /over, color = 'dark salmon', thick = thick)
  p11 = plot(wave_spec, lbhi_norm[*, 9] * r[0, 9], /over, color = 'sandy brown', thick = thick)
  p12 = plot(wave_spec, lbhi_norm[*, 10] * r[0, 10], /over, color = 'peru', thick = thick)
  markerp, p1, y = const, linestyle = 2

  ;
  ; create weighted model vectors
  ;
  lbh_fit = lbhi_norm
  for i = 0, ((size(lbh))[2] - 1) do $
    lbh_fit[*, i] = lbhi_norm[*, i] * r[0, i]

  lbh_fit_tot = total(lbh_fit, 2)

  win = window(dim = [800, 600])
  thick = 2
  p1 = plot(wave_spec, spec_cal_norm, current = win, thick = thick, linestyle = 2, title = 'final', xr = [125, 155])
  p2 = plot(wave_spec, lbh_fit_tot + const, color = 'red', /over)
  p3 = plot(wave_spec, spec[0, *], color = 'green', /over)
  p4 = plot(wave_spec, spec[1, *], color = 'blue', /over)
  p5 = plot(wave_spec, spec[2, *], color = 'purple', /over)

  print, r[0, 7 : 10]
  stop

  stop
end
