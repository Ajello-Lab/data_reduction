;+
; PURPOSE
;   Combines images 1,2, and 3 from IUVS data together into a single spectrum
;
; INPUTS
;   wave_spec1: corrected wavelength scale for image 1, nm
;   spec_image1: observed and calibrated N2 image 1 spectrum
;   wave_spec2: corrected wavelength scale for image 2, nm
;   spec_image2: observed and calibrated N2 image 2 spectrum
;   wave_spec2: corrected wavelength scale for image 3, nm
;   spec_image3: observed and calibrated N2 image 3 spectrum
;
; OUTPUTS
;   wave_spec: corrected wavelength scale, nm (taken from the scale for image1)
;   total_spec: total observed and calibrated N2 spectrum for all images
;
; KEYWORDS
;   show_plots: set to show plots
;   save_data: set to the file_path to save data in same place as main file or
;     set to path_save/file_name
;-

pro add_LBH_ticks, window, yr
  compile_opt idl2

  window.refresh, /disable
  fac = max(yr) / 1.1

  ; top black line
  t = text(145, 1.02 * fac, 'N!d2!n LBH a-X', color = 'black', font_size = 14, font_name = 'times', target = window, /data)
  p = plot([168, 168], [1 * fac, 0.975 * fac], thick = 2, color = 'black', /overplot)
  p = plot([128, 128], [1 * fac, 0.975 * fac], thick = 2, color = 'black', /overplot)
  p = plot([128, 168], [1. * fac, 1. * fac], thick = 1.5, color = 'black', /overplot)

  ; Horizonal v' lines
  p = plot([145.0, 167.2], [0.65 * fac, 0.65 * fac], color = 'dark orange', thick = 1.5, /overplot)
  p = plot([141.6, 168.8], [0.7 * fac, 0.7 * fac], color = 'dark orange', thick = 1.5, /overplot)
  p = plot([138.4, 164.2], [0.75 * fac, 0.75 * fac], color = 'dark orange', thick = 1.5, /overplot)
  p = plot([135.4, 165.8], [0.8 * fac, 0.8 * fac], color = 'dark orange', thick = 1.5, /overplot)
  p = plot([132.5, 167.4], [0.85 * fac, 0.85 * fac], color = 'dark orange', thick = 1.5, /overplot)
  p = plot([129.9, 169.0], [0.9 * fac, 0.9 * fac], color = 'dark orange', thick = 1.5, /overplot)
  p = plot([127.3, 164.8], [0.95 * fac, 0.95 * fac], color = 'dark orange', thick = 1.5, /overplot)

  wl0 = [145.0, 150.1, 155.5, 161.2, 167.2]
  wl1 = [141.6, 146.4, 151.5, 157.0, 162.7, 168.8]
  wl2 = [138.4, 143.0, 147.9, 153.0, 158.5, 164.2]
  wl3 = [135.4, 139.8, 144.4, 149.3, 154.5, 160.0, 165.8]
  wl4 = [132.5, 136.8, 141.2, 145.9, 150.8, 156.0, 161.6, 167.4]
  wl5 = [129.9, 133.9, 138.2, 142.7, 147.4, 152.3, 157.6, 163.1, 169.0]
  wl6 = [127.3, 131.2, 135.3, 139.6, 144.1, 148.9, 153.9, 159.2, 164.8]

  for i = 0, n_elements(wl0) - 1 do begin
    p = plot([wl0[i], wl0[i]], [0.625 * fac, 0.65 * fac], color = 'dark orange', thick = 2, /overplot)
  endfor

  for i = 0, n_elements(wl1) - 1 do begin
    p = plot([wl1[i], wl1[i]], [0.675 * fac, 0.7 * fac], color = 'dark orange', thick = 2, /overplot)
  endfor

  for i = 0, n_elements(wl2) - 1 do begin
    p = plot([wl2[i], wl2[i]], [0.725 * fac, 0.75 * fac], color = 'dark orange', thick = 2, /overplot)
  endfor

  for i = 0, n_elements(wl3) - 1 do begin
    p = plot([wl3[i], wl3[i]], [0.775 * fac, 0.8 * fac], color = 'dark orange', thick = 2, /overplot)
  endfor

  for i = 0, n_elements(wl4) - 1 do begin
    p = plot([wl4[i], wl4[i]], [0.825 * fac, 0.85 * fac], color = 'dark orange', thick = 2, /overplot)
  endfor

  for i = 0, n_elements(wl5) - 1 do begin
    p = plot([wl5[i], wl5[i]], [0.875 * fac, 0.9 * fac], color = 'dark orange', thick = 2, /overplot)
  endfor

  for i = 0, n_elements(wl6) - 1 do begin
    p = plot([wl6[i], wl6[i]], [0.925 * fac, 0.95 * fac], color = 'dark orange', thick = 2, /overplot)
  endfor
  ; v''
  t = text(145, 0.6 * fac, '0', color = 'orange red', font_size = 11, font_name = 'times', target = win, /data)
  t = text(150.1, 0.6 * fac, '1', color = 'orange red', font_size = 11, font_name = 'times', target = win, /data)
  t = text(155.5, 0.6 * fac, '2', color = 'orange red', font_size = 11, font_name = 'times', target = win, /data)
  t = text(161.2, 0.6 * fac, '3', color = 'orange red', font_size = 11, font_name = 'times', target = win, /data)
  t = text(167.2, 0.6 * fac, '4 = $\nu^\prime\prime$', color = 'orange red', font_size = 11, font_name = 'times', target = win, /data)
  ; v'
  t = text(142.0, 0.625 * fac, '$\nu\prime$ = 0', color = 'orange red', font_size = 11, font_name = 'times', target = win, /data)
  t = text(138.6, 0.675 * fac, '$\nu\prime$ = 1', color = 'orange red', font_size = 11, font_name = 'times', target = win, /data)
  t = text(135.4, 0.725 * fac, '$\nu\prime$ = 2', color = 'orange red', font_size = 11, font_name = 'times', target = win, /data)
  t = text(132.4, 0.775 * fac, '$\nu\prime$ = 3', color = 'orange red', font_size = 11, font_name = 'times', target = win, /data)
  t = text(129.5, 0.825 * fac, '$\nu\prime$ = 4', color = 'orange red', font_size = 11, font_name = 'times', target = win, /data)
  t = text(126.9, 0.875 * fac, '$\nu\prime$ = 5', color = 'orange red', font_size = 11, font_name = 'times', target = win, /data)
  t = text(124.3, 0.925 * fac, '$\nu\prime$ = 6', color = 'orange red', font_size = 11, font_name = 'times', target = win, /data)
  window.refresh
end

pro ajello_lab_combine_images, wave_spec1, spec_image1, wave_spec2, spec_image2, wave_spec3, spec_image3, $
  wave_spec, $
  total_spec, $
  show_plots = show_plots, $
  save_data = save_data
  compile_opt idl2

  if n_params() eq 0 then begin
    show_plots = 1
    save_data = 0

    ajello_lab_set_paths, path_base, path_repo

    case (get_login_info()).user_name of
      'holsclaw': begin
        ; file_data_helper = '/Volumes/projects/Phase_Development/MAVEN/IUVS_Data/IUVS_Breadboard/Round_12/data_reduction/'
        path_save = '/users/holsclaw/Documents/'
        file_path = '/Volumes/projects/Phase_Development/MAVEN/IUVS_Data/IUVS_Breadboard/Ajello_Round14/Data_Reduction/'
      end
      'benjamincondit': begin
        ; file_data_helper = '/Users/benjamincondit/Desktop/IUVS_Breadboard/Round 12/data_reduction/'
        path_save = '/Users/benjamincondit/Desktop/Data_Reduction_copy/'
        file_path = '/Users/benjamincondit/Desktop/Data_Reduction copy/'
      end
    endcase

    data_list = ['N2_30EV_FUV_TEST19_IMAGE1_Calibrated.idl', 'N2_30EV_FUV_TEST20_IMAGE2_Calibrated.idl', 'N2_30EV_FUV_TEST21_IMAGE3_Calibrated.idl']

    file_data = file_path + data_list[0]
    sObj = obj_new('IDL_Savefile', file_data)
    sObj.restore, ['wave_spec', 'spec']
    obj_destroy, sObj
    wave_spec1 = wave_spec
    spec_image1 = spec

    file_data = file_path + data_list[1]
    sObj = obj_new('IDL_Savefile', file_data)
    sObj.restore, ['wave_spec', 'spec']
    obj_destroy, sObj
    wave_spec2 = wave_spec
    spec_image2 = spec

    file_data = file_path + data_list[2]
    sObj = obj_new('IDL_Savefile', file_data)
    sObj.restore, ['wave_spec', 'spec']
    obj_destroy, sObj
    wave_spec3 = wave_spec
    spec_image3 = spec
  endif

  wave_spec = wave_spec1

  spec2_interp = interpol(spec_image2, wave_spec2, wave_spec)
  spec3_interp = interpol(spec_image3, wave_spec3, wave_spec)

  total_spec = spec_image1 + spec2_interp + spec3_interp

  if keyword_set(show_plots) then begin
    if keyword_set(save_data) then $
      file_name = file_basename(save_data, '.idl') + '_images_combined' else file_name = ''
    win = window(dimensions = [1000, 800])
    win.refresh, /disable
    xr = [120, 170]
    y_ndx = where(wave_spec lt 170 and wave_spec gt 125)
    yr = [0, max(total_spec[y_ndx]) * 2]
    p1 = plot(wave_spec, total_spec, current = win, font_size = 13, xr = xr, yr = yr, title = file_name, name = 'IUVS Total Spectrum', $
      xtitle = 'Wavelength (nm)', ytitle = 'Relative Calibrated Intensity [arb units]', font_name = 'times', linestyle = 2)
    p2 = plot(wave_spec, spec_image1, /over, color = 'blue', name = 'IUVS Image 1')
    p3 = plot(wave_spec, spec2_interp, /over, color = 'red', name = 'IUVS Image 2')
    p4 = plot(wave_spec, spec3_interp, /over, color = 'green', name = 'IUVS Image 3')
    leg = legend(target = [p1, p2, p3, p4], position = [0.98, 0.5], /relative, linestyle = 6, font_name = 'times')
    win.refresh
    add_LBH_ticks, win, yr
    if keyword_set(save_data) then $
      win.save, file_dirname(save_data) + path_sep() + file_name + '.png'
  endif

  if keyword_set(save_data) then begin
    path_save = file_dirname(save_data) + path_sep()
    desc = [ $
      'spec_image1: observed and calibrated N2 image 1 spectrum', $
      'spec_image2: observed and calibrated N2 image 2 spectrum', $
      'spec_image3: observed and calibrated N2 image 3 spectrum', $
      'total_spec: total observed and calibrated N2 spectrum for all images', $
      'wave_spec: corrected wavelength scale, nm (the scale for image1 too)', $
      'wave_spec2: corrected wavelength scale for image 2, nm', $
      'wave_spec3: corrected wavelength scale, for image 3 nm']
    file_name = file_basename(save_data, '.idl')
    save, filename = path_save + file_name + '_images_combined.idl', spec_image1, spec_image2, spec_image3, $
      total_spec, wave_spec, wave_spec2, wave_spec3, desc
  endif

  if n_params() eq 0 then stop
end
