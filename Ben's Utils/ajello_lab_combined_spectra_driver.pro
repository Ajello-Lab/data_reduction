;
; PURPOSE
; Drives a process that takes in files from a folder and then outputs a spectra plot
; where the magnitude of the signals are compounded together with the intention
; of creating a plot similar to Ajello et al. ~2026 fig 4
;
pro ajello_lab_combined_spectra_driver
  compile_opt idl2

  data_source = '/Users/benjamincondit/Desktop/IUVS_Breadboard/Ajello_Round14/Data_Reduction/' ; INPUT FILE SOURCE HERE
  data_dest = '/Users/benjamincondit/Desktop/' ; INPUT OUTPUT DESTINATION HERE
  energy = 'N2_16EV' ; INPUT ENERGY / GAS HERE

  ; search folder to retrieve .idl files for each image
  folder_searcher, data_source, '.idl', 'IMAGE1', image1_files, keyword_2 = 'FUV', keyword_3 = energy
  folder_searcher, data_source, '.idl', 'IMAGE2', image2_files, keyword_2 = 'FUV', keyword_3 = energy
  folder_searcher, data_source, '.idl', 'IMAGE3', image3_files, keyword_2 = 'FUV', keyword_3 = energy

  print, 'Energy: ' + energy
  print, 'Number of Files Found for Image 1: ' + string(n_elements(image1_files)) + $
    ' | Image 2: ' + string(n_elements(image2_files)) + ' | Image 3: ' + string(n_elements(image3_files))
  if n_elements(image1_files) eq 0 or n_elements(image2_files) eq 0 then $
    stop
  if n_elements(image3_files) eq 0 then $
    print, '* No Image 3 files found *'
  print, 'Continuing with only data from Images 1 and 2'

  ; turns files into hash tables with all data
  data_retriever, image1_files, '.idl', arrs1_out
  data_retriever, image2_files, '.idl', arrs2_out
  if n_elements(image3_files) ne 0 then $
    data_retriever, image3_files, '.idl', arrs3_out

  ; scale and aligns the data to the nm length
  scale_and_align_n2, arrs1_out, structs1_aligned
  scale_and_align_n2, arrs2_out, structs2_aligned, scale_factor = structs1_aligned[0].scale_factor
  if n_elements(image3_files) ne 0 then $
    scale_and_align_n2, arrs3_out, structs3_aligned, scale_factor = structs1_aligned[0].scale_factor

  win = window(dim = [1440, 600], buffer = 0)
  p2 = plot(structs1_aligned[0].wl_spec, structs1_aligned[0].spec, symbol = 24, sym_filled = 1, sym_size = 0.5, /over, color = 'purple', title = 'post-scaled')
  p2 = plot(structs2_aligned[0].wl_spec, structs2_aligned[0].spec, symbol = 24, sym_filled = 1, sym_size = 0.5, /over, color = 'blue')

  ; smooths and averages each dataset together
  the_smoother, structs1_aligned, struct1_smoothed, 0
  the_smoother, structs2_aligned, struct2_smoothed, 0
  if n_elements(image3_files) ne 0 then $
    the_smoother, structs3_aligned, struct3_smoothed, 0

  ; win = window(dim = [1440, 600], buffer = 0)
  ; p2 = plot(struct1_smoothed.wl_spec, struct1_smoothed.spec, symbol = 24, sym_filled = 1, sym_size = 0.5, /over, color = 'purple', title = 'post-smoothed')
  ; p2 = plot(struct2_smoothed.wl_spec, struct2_smoothed.spec, symbol = 24, sym_filled = 1, sym_size = 0.5, /over, color = 'blue')

  ; calibrates the data to account for IUVS sensitivity
  calibrator, struct1_smoothed, struct1_calibrated
  calibrator, struct2_smoothed, struct2_calibrated
  if n_elements(image3_files) ne 0 then $
    calibrator, struct3_smoothed, struct3_calibrated

  ; win = window(dim = [1440, 600], buffer = 0)
  ; p2 = plot(struct1_calibrated.wl_spec, struct1_calibrated.spec, symbol = 24, sym_filled = 1, sym_size = 0.5, /over, color = 'purple', title = 'post-calibration')
  ; p2 = plot(struct2_calibrated.wl_spec, struct2_calibrated.spec, symbol = 24, sym_filled = 1, sym_size = 0.5, /over, color = 'blue')

  ; produces the total of the spectra
  struct_calibrated = {scale1: struct1_calibrated.wl_spec, image1: struct1_calibrated.spec, $
    scale2: struct2_calibrated.wl_spec, image2: struct2_calibrated.spec}

  if n_elements(image3_files) ne 0 then $
    struct_calibrated = {scale1: struct1_calibrated.wl_spec, image1: struct1_calibrated.spec, $
      scale2: struct2_calibrated.wl_spec, image2: struct2_calibrated.spec, $
      scale3: struct3_calibrated.wl_spec, image3: struct3_calibrated.spec}
  image_total, struct_calibrated, struct_total

  win = window(dim = [1000, 800], buffer = 0)
  xr = [123, 172]
  yr = [0, 1.1]
  p1 = plot(struct_total.scale, struct_total.imagetot, current = win, color = 'Blue', font_size = 13, xrange = xr, yr = yr, $
    name = 'IUVS BB Image Total', xtitle = 'Wavelength (nm)', ytitle = 'Relative Calibrated Intensity [arb units]', font_name = 'times', title = 'IUVS Spectra - ' + energy, linestyle = 2)
  ax = p1.axes
  ax[0].ticklen = 0.02
  ax[1].ticklen = 0.02
  ax[2].ticklen = 0.0
  ax[3].ticklen = 0.0
  p2 = plot(struct1_calibrated.wl_spec, struct1_calibrated.spec, /over, color = 'purple', name = 'IUVS BB Image 1')
  p3 = plot(struct2_calibrated.wl_spec, struct2_calibrated.spec, /over, color = 'green', name = 'IUVS BB Image 2')
  if n_elements(image3_files) ne 0 then $
    p4 = plot(struct3_calibrated.wl_spec, struct3_calibrated.spec, /over, color = 'red', name = 'IUVS BB Image 3')

  ; ; fitndx1 = where(struct1_calibrated.wl_spec lt 170 and struct1_calibrated.wl_spec gt 125)
  ; fitndx2 = where(struct2_calibrated.wl_spec lt 170 and struct2_calibrated.wl_spec gt 125)
  ; ; fit1 = poly_fit(struct1_calibrated.wl_spec[fitndx1], struct1_calibrated.spec[fitndx1], 1, /double)
  ; fit2 = poly_fit(struct2_calibrated.wl_spec[fitndx2], struct2_calibrated.spec[fitndx2], 1, /double)
  ; ; print, fit1
  ; print, fit2

  ; top black line
  ; t = text(145, 1.02, 'N!d2!n LBH a-X', color = 'black', font_size = 14, font_name = 'times', target = win, /data)
  ; p = plot([168, 168], [1, 0.975], thick = 2, color = 'black', /overplot)
  ; p = plot([128, 128], [1, 0.975], thick = 2, color = 'black', /overplot)
  ; p = plot([128, 168], [1., 1.], thick = 1.5, color = 'black', /overplot)

  ; ; Horizonal v' lines
  ; p = plot([145.0, 167.2], [0.65, 0.65], color = 'dark orange', thick = 1.5, /overplot)
  ; p = plot([141.6, 168.8], [0.7, 0.7], color = 'dark orange', thick = 1.5, /overplot)
  ; p = plot([138.4, 164.2], [0.75, 0.75], color = 'dark orange', thick = 1.5, /overplot)
  ; p = plot([135.4, 165.8], [0.8, 0.8], color = 'dark orange', thick = 1.5, /overplot)
  ; p = plot([132.5, 167.4], [0.85, 0.85], color = 'dark orange', thick = 1.5, /overplot)
  ; p = plot([129.9, 169.0], [0.9, 0.9], color = 'dark orange', thick = 1.5, /overplot)
  ; p = plot([127.3, 164.8], [0.95, 0.95], color = 'dark orange', thick = 1.5, /overplot)

  ; wl0 = [145.0, 150.1, 155.5, 161.2, 167.2]
  ; wl1 = [141.6, 146.4, 151.5, 157.0, 162.7, 168.8]
  ; wl2 = [138.4, 143.0, 147.9, 153.0, 158.5, 164.2]
  ; wl3 = [135.4, 139.8, 144.4, 149.3, 154.5, 160.0, 165.8]
  ; wl4 = [132.5, 136.8, 141.2, 145.9, 150.8, 156.0, 161.6, 167.4]
  ; wl5 = [129.9, 133.9, 138.2, 142.7, 147.4, 152.3, 157.6, 163.1, 169.0]
  ; wl6 = [127.3, 131.2, 135.3, 139.6, 144.1, 148.9, 153.9, 159.2, 164.8]

  ; for i = 0, n_elements(wl0) - 1 do begin
  ; p = plot([wl0[i], wl0[i]], [0.625, 0.65], color = 'dark orange', thick = 2, /overplot)
  ; endfor

  ; for i = 0, n_elements(wl1) - 1 do begin
  ; p = plot([wl1[i], wl1[i]], [0.675, 0.7], color = 'dark orange', thick = 2, /overplot)
  ; endfor

  ; for i = 0, n_elements(wl2) - 1 do begin
  ; p = plot([wl2[i], wl2[i]], [0.725, 0.75], color = 'dark orange', thick = 2, /overplot)
  ; endfor

  ; for i = 0, n_elements(wl3) - 1 do begin
  ; p = plot([wl3[i], wl3[i]], [0.775, 0.8], color = 'dark orange', thick = 2, /overplot)
  ; endfor

  ; for i = 0, n_elements(wl4) - 1 do begin
  ; p = plot([wl4[i], wl4[i]], [0.825, 0.85], color = 'dark orange', thick = 2, /overplot)
  ; endfor

  ; for i = 0, n_elements(wl5) - 1 do begin
  ; p = plot([wl5[i], wl5[i]], [0.875, 0.9], color = 'dark orange', thick = 2, /overplot)
  ; endfor

  ; for i = 0, n_elements(wl6) - 1 do begin
  ; p = plot([wl6[i], wl6[i]], [0.925, 0.95], color = 'dark orange', thick = 2, /overplot)
  ; endfor
  ; ; v''
  ; t = text(145, 0.6, '0', color = 'orange red', font_size = 11, font_name = 'times', target = win, /data)
  ; t = text(150.1, 0.6, '1', color = 'orange red', font_size = 11, font_name = 'times', target = win, /data)
  ; t = text(155.5, 0.6, '2', color = 'orange red', font_size = 11, font_name = 'times', target = win, /data)
  ; t = text(161.2, 0.6, '3', color = 'orange red', font_size = 11, font_name = 'times', target = win, /data)
  ; t = text(167.2, 0.6, '4 = $\nu^{``}$', color = 'orange red', font_size = 11, font_name = 'times', target = win, /data)
  ; ; v'
  ; t = text(142.0, 0.625, '$\nu\prime$ = 0', color = 'orange red', font_size = 11, font_name = 'times', target = win, /data)
  ; t = text(138.6, 0.675, '$\nu\prime$ = 1', color = 'orange red', font_size = 11, font_name = 'times', target = win, /data)
  ; t = text(135.4, 0.725, '$\nu\prime$ = 2', color = 'orange red', font_size = 11, font_name = 'times', target = win, /data)
  ; t = text(132.4, 0.775, '$\nu^`$ = 3', color = 'orange red', font_size = 11, font_name = 'times', target = win, /data)
  ; t = text(129.5, 0.825, '$\nu^`$ = 4', color = 'orange red', font_size = 11, font_name = 'times', target = win, /data)
  ; t = text(126.9, 0.875, '$\nu^`$ = 5', color = 'orange red', font_size = 11, font_name = 'times', target = win, /data)
  ; t = text(124.3, 0.925, '$\nu^`$ = 6', color = 'orange red', font_size = 11, font_name = 'times', target = win, /data)
  if n_elements(image3_files) eq 0 then begin
    leg = legend(target = [p1, p2, p3], position = [0.98, 0.4], /relative, font_size = 13, font_name = 'times', linestyle = 6)
  endif else $
    leg = legend(target = [p1, p2, p3, p4], position = [0.98, 0.4], /relative, font_size = 13, font_name = 'times', linestyle = 6)
  win.save, data_dest + 'Combined_Spectra.png'
end
