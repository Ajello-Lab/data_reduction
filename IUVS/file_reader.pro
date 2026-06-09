;
; PURPOSE
; Takes in a folder of txt files and combines
; them into a single scatterplot
;
; INPUTS
; - A file that contains txt files
;
;
; CONTINUE PLOTTING THE VARIOUS OTHER FOLDERS
pro file_reader
  compile_opt idl2

  folder_path = '/Users/benjamincondit/idl/data_reduction/IUVS/Sensitivities_N2_FUV_Analyzed/Round 12/100EV/' ; insert file path here

  save_path = '/Users/benjamincondit/idl/data_reduction/IUVS/Sensitivities_N2_FUV_Analyzed/Round 12/' ; insert save path here
  temp = strsplit(folder_path, '/', /extract)
  file_data_helper = folder_path + '*.txt'
  files_to_read = file_search(file_data_helper, count = file_count)

  print, 'Number of files to read:' + string(file_count)

  all_wl_sens = list()
  all_sens = list()

  for j = 0, file_count - 1 do begin
    if j mod 5 eq 0 then print, 'processing file:' + string(j)
    file_data = files_to_read[j]

    readcol, file_data, wl_sens, sens, delimiter = ',', format = '(F10.2, F10.6)', /silent

    all_wl_sens.add, wl_sens
    all_sens.add, sens
  endfor

  all_wl_sens = all_wl_sens.toArray()
  all_sens = all_sens.toArray()

  ; Ouputs the combined data to a csv file
  ;
  file_out = save_path + strtrim(string(temp[-1]), 2) + '_combined_data.csv'
  num_sens = n_elements(all_wl_sens)
  openw, lun, file_out, /get_lun
  for i = 0, num_sens - 1 do $
    printf, lun, all_wl_sens[i], all_sens[i], format = '(F10.2,",",F10.6)'
  close, lun
  free_lun, lun

  ; scatterplot of the combined data
  win = window(dim = [1400, 600])
  xr = [110, 190]
  p1 = plot(all_wl_sens, all_sens, symbol = 'o', xr = xr, /sym_filled, current = win, $
    font_size = 16, xtitle = 'wavelength (nm)', ytitle = 'sensitivity (normalized)', $
    title = string(temp[-1]) + ' Sensitivity', linestyle = 6)

  ; Creates specific range to fit data
  ;
  lower_bound = 120.0 ; SET LOWER BOUND FOR FITTING

  line_index = where(all_wl_sens gt lower_bound)
  all_wl_sens = all_wl_sens[line_index]
  all_sens = all_sens[line_index]

  ; Linear Fit
  ;
  coeff = poly_fit(all_wl_sens, all_sens, 1)
  line_fit = findgen(500) * (max(all_wl_sens) - min(all_wl_sens)) / 499.0 + min(all_wl_sens)
  fit_y = poly(line_fit, coeff)
  name = 'linear fit'

  ; Quadratic Fit
  ;
  ; coeff = poly_fit(all_wl_sens, all_sens, 2)
  ; line_fit = findgen(500) * (max(all_wl_sens) - min(all_wl_sens)) / 499.0 + min(all_wl_sens)
  ; fit_y = poly(line_fit, coeff)
  ; name = 'quadratic fit'

  ; Exponential Fit
  ;
  ; log_sens = alog(all_sens)
  ; coeff = poly_fit(all_wl_sens, log_sens, 1)
  ; B = -coeff[1]
  ; A = exp(coeff[0])
  ; line_fit = findgen(500) * (max(all_wl_sens) - min(all_wl_sens)) / 499.0 + min(all_wl_sens)
  ; fit_y = A * exp(-B * line_fit)
  ; name = 'exponential fit'

  calc_r_squared, all_wl_sens, all_sens, r_squared
  text = 'R^2 = ' + string(r_squared, format = '(F5.3)')

  p2 = plot(line_fit, fit_y, /over, color = 'red', name = name, linestyle = 2)
  p3 = plot(all_wl_sens[0 : 1], all_sens[0 : 1], /over, color = 'white', transparency = 100, name = text)
  leg = legend(target = [p1, p2, p3], font_size = 14, position = [0.95, 0.9], /relative)
  win.save, save_path + string(temp[-1]) + '_sensitivity_plot.png'

  ; outputs a text file with the fit coefficients
  ;
  file_out = save_path + strtrim(string(temp[-1]), 2) + '_' + name + '.csv'
  openw, lun, file_out, /get_lun
  for i = 0, n_elements(line_fit) - 1 do $
    printf, lun, line_fit[i], fit_y[i], format = '(F10.3,",",F10.3)'
  close, lun
  free_lun, lun

  ; outputs a text file with the R^2 value for the fit
  ;
  file_out = save_path + strtrim(string(temp[-1]), 2) + '_r_squared.csv'
  openw, lun, file_out, /get_lun
  printf, lun, r_squared, format = '(F10.8)'
  close, lun
  free_lun, lun

  print, 'Done'
end
