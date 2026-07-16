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
;   save_data: set to the save_path to save data
;-
pro add_LBH_ticks
  compile_opt idl2

  ; top black line
  t = text(145, 1.02, 'N!d2!n LBH a-X', color = 'black', font_size = 14, font_name = 'times', target = win, /data)
  p = plot([168, 168], [1, 0.975], thick = 2, color = 'black', /overplot)
  p = plot([128, 128], [1, 0.975], thick = 2, color = 'black', /overplot)
  p = plot([128, 168], [1., 1.], thick = 1.5, color = 'black', /overplot)

  ; Horizonal v' lines
  p = plot([145.0, 167.2], [0.65, 0.65], color = 'dark orange', thick = 1.5, /overplot)
  p = plot([141.6, 168.8], [0.7, 0.7], color = 'dark orange', thick = 1.5, /overplot)
  p = plot([138.4, 164.2], [0.75, 0.75], color = 'dark orange', thick = 1.5, /overplot)
  p = plot([135.4, 165.8], [0.8, 0.8], color = 'dark orange', thick = 1.5, /overplot)
  p = plot([132.5, 167.4], [0.85, 0.85], color = 'dark orange', thick = 1.5, /overplot)
  p = plot([129.9, 169.0], [0.9, 0.9], color = 'dark orange', thick = 1.5, /overplot)
  p = plot([127.3, 164.8], [0.95, 0.95], color = 'dark orange', thick = 1.5, /overplot)

  wl0 = [145.0, 150.1, 155.5, 161.2, 167.2]
  wl1 = [141.6, 146.4, 151.5, 157.0, 162.7, 168.8]
  wl2 = [138.4, 143.0, 147.9, 153.0, 158.5, 164.2]
  wl3 = [135.4, 139.8, 144.4, 149.3, 154.5, 160.0, 165.8]
  wl4 = [132.5, 136.8, 141.2, 145.9, 150.8, 156.0, 161.6, 167.4]
  wl5 = [129.9, 133.9, 138.2, 142.7, 147.4, 152.3, 157.6, 163.1, 169.0]
  wl6 = [127.3, 131.2, 135.3, 139.6, 144.1, 148.9, 153.9, 159.2, 164.8]

  for i = 0, n_elements(wl0) - 1 do begin
    p = plot([wl0[i], wl0[i]], [0.625, 0.65], color = 'dark orange', thick = 2, /overplot)
  endfor

  for i = 0, n_elements(wl1) - 1 do begin
    p = plot([wl1[i], wl1[i]], [0.675, 0.7], color = 'dark orange', thick = 2, /overplot)
  endfor

  for i = 0, n_elements(wl2) - 1 do begin
    p = plot([wl2[i], wl2[i]], [0.725, 0.75], color = 'dark orange', thick = 2, /overplot)
  endfor

  for i = 0, n_elements(wl3) - 1 do begin
    p = plot([wl3[i], wl3[i]], [0.775, 0.8], color = 'dark orange', thick = 2, /overplot)
  endfor

  for i = 0, n_elements(wl4) - 1 do begin
    p = plot([wl4[i], wl4[i]], [0.825, 0.85], color = 'dark orange', thick = 2, /overplot)
  endfor

  for i = 0, n_elements(wl5) - 1 do begin
    p = plot([wl5[i], wl5[i]], [0.875, 0.9], color = 'dark orange', thick = 2, /overplot)
  endfor

  for i = 0, n_elements(wl6) - 1 do begin
    p = plot([wl6[i], wl6[i]], [0.925, 0.95], color = 'dark orange', thick = 2, /overplot)
  endfor
  ; v''
  t = text(145, 0.6, '0', color = 'orange red', font_size = 11, font_name = 'times', target = win, /data)
  t = text(150.1, 0.6, '1', color = 'orange red', font_size = 11, font_name = 'times', target = win, /data)
  t = text(155.5, 0.6, '2', color = 'orange red', font_size = 11, font_name = 'times', target = win, /data)
  t = text(161.2, 0.6, '3', color = 'orange red', font_size = 11, font_name = 'times', target = win, /data)
  t = text(167.2, 0.6, '4 = $\nu^{``}$', color = 'orange red', font_size = 11, font_name = 'times', target = win, /data)
  ; v'
  t = text(142.0, 0.625, '$\nu\prime$ = 0', color = 'orange red', font_size = 11, font_name = 'times', target = win, /data)
  t = text(138.6, 0.675, '$\nu\prime$ = 1', color = 'orange red', font_size = 11, font_name = 'times', target = win, /data)
  t = text(135.4, 0.725, '$\nu\prime$ = 2', color = 'orange red', font_size = 11, font_name = 'times', target = win, /data)
  t = text(132.4, 0.775, '$\nu^`$ = 3', color = 'orange red', font_size = 11, font_name = 'times', target = win, /data)
  t = text(129.5, 0.825, '$\nu^`$ = 4', color = 'orange red', font_size = 11, font_name = 'times', target = win, /data)
  t = text(126.9, 0.875, '$\nu^`$ = 5', color = 'orange red', font_size = 11, font_name = 'times', target = win, /data)
  t = text(124.3, 0.925, '$\nu^`$ = 6', color = 'orange red', font_size = 11, font_name = 'times', target = win, /data)
end

pro ajello_lab_combine_images, wave_spec1, spec_image1, wave_spec2, spec_image2, wave_spec3, spec_image3, $
  wave_spec, $
  total_spec, $
  show_plots = show_plots, $
  save_data = save_data
  compile_opt idl2

  wave_spec = wave_spec1

  spec2_interp = interpol(spec_image2, wave_spec2, wave_spec)
  spec3_interp = interpol(spec_image3, wave_spec3, wave_spec)

  total_spec = wave_spec1 + spec2_interp + spec3_interp

  if keyword_set(show_plots) then begin
    if keyword_set(save_data) then $
      file_name = file_basename(save_data, '.idl') else file_name = ''
    win = window(dimensions = [1000, 800])
    xr = []
    yr = []
    p1 = plot(wave_spec, total_spec, current = win, font_size = 13, xr = xr, yr = yr, title = file_name, name = 'IUVS Total Spectrum', $
      xtitle = 'Wavelength (nm)', ytitle = 'Relative Calibrated Intensity [arb units]', font_name = 'times', linestyle = 2)
    p2 = plot(wave_spec, spec_image1, /over, color = 'blue', name = 'IUVS Image 1')
    p3 = plot(wave_spec, spec2_interp, /over, color = 'red', name = 'IUVS Image 2')
    p4 = plot(wave_spec, spec3_interp, /over, color = 'green', name = 'IUVS Image 3')
    leg = legend(target = [p1, p2, p3, p4], position = [0.98, 0.5], /relative, linestyle = 6, font_name = 'times')
    add_LBH_ticks
    ; win.save, save_data + 'Combined_Images.png'
  endif

  if keyword_set(save_data) then begin
    path_save = save_data
    desc = [ $
      'spec_image1: observed and calibrated N2 image 1 spectrum', $
      'spec_image2: observed and calibrated N2 image 2 spectrum', $
      'spec_image3: observed and calibrated N2 image 3 spectrum', $
      'total_spec: total observed and calibrated N2 spectrum for all images', $
      'wave_spec: corrected wavelength scale, nm (the scale for image1 too)', $
      'wave_spec2: corrected wavelength scale for image 2, nm', $
      'wave_spec3: corrected wavelength scale, for image 3 nm']
    save, filename = path_save + file_name + '_LBH_fit.idl', spec_image1, spec_image2, spec_image3, $
      total_spec, wave_spec, wave_spec2, wave_spec3, desc
  endif
  stop
end
