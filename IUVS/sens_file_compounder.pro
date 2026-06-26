;
; PURPOSE
; Takes in a folder of txt files and combines
; them into a single scatterplot
;
; INPUTS
; folder_path: path to folder to be read
; save_path: path to folder where outputs will be saved
;
pro sens_file_compounder, folder_path, save_path
  compile_opt idl2

  ; USER INPUTS:::
  ; folder_path = '/Users/benjamincondit/Desktop/Sens_Main/Sens_set_1/7N2 High Quality' ; insert file path here
  ; save_path = '/Users/benjamincondit/Desktop/Sens_Saves/' ; insert save path here
  save_csv = 1 ; T or F

  temp = strsplit(folder_path, '/', /extract)
  file_data_helper = folder_path + '/' + '*.txt'
  files_to_read = file_search(file_data_helper, count = file_count)

  print, 'Number of files to read:' + string(file_count)

  plot_entries = list()
  name_entries = list()
  tot_entries = 0
  tot_wl_sens = []
  tot_sens = []

  for j = 0, file_count - 1 do begin
    ; if j mod 5 eq 0 then print, 'processing file:' + string(j)
    file_data = files_to_read[j]

    readcol, file_data, wl_sens, sens, delimiter = ',', format = '(F10.2, F10.6)', /silent

    if n_elements(wl_sens) ne n_elements(sens) then begin
      print, files_to_read[j] + 'contains columns of different lengths'
      stop
    endif
    arr_to_add = transpose([[wl_sens], [sens]])
    plot_entries.add, arr_to_add
    name_entries.add, (strsplit(files_to_read[j], '/', /extract))[-1]
    tot_entries++
    tot_wl_sens = [tot_wl_sens, wl_sens]
    tot_sens = [tot_sens, sens]
  endfor

  average_data_points, tot_wl_sens, tot_sens, avg_wl_sens, avg_sens

  if save_csv then begin
    csv_name = save_path + temp[-1] + '_Averaged_Values.csv'
    sens_avg_struct = {x_values: avg_wl_sens, y_values: avg_sens}
    write_csv, csv_name, sens_avg_struct, header = ['wl_sens', 'sens']
  endif
  ;
  ; Establishes colors plus the var for grid size
  colors = ['Blue', 'Firebrick', 'dark slate gray', 'Hot Pink', 'gold', 'Dark Magenta', 'Dark Green', 'Chocolate', 'Indigo', 'light sky blue']
  if tot_entries mod 2 eq 1 then tot_entries++
  if tot_entries gt n_elements(colors) then tot_entries = n_elements(colors)

  ; plots up to [n_elements(colors)] lines on a single plot
  ;
  win1 = window(dim = [1400, 600], buffer = 1)
  win2 = window(dim = [280 * tot_entries / 2, 400], buffer = 1)
  win3 = window(dim = [1400, 600], buffer = 1)
  xr = [115, 185]
  yr = [0, 2]
  for i = 0, (size(plot_entries, /dimensions))[0] - 1 do begin
    if i eq n_elements(colors) then begin
      print, 'Attempted to plot too many lines'
      break
    endif
    cur_arr = plot_entries[i]
    x = cur_arr[0, *]
    y = cur_arr[1, *]
    name = (((((name_entries[i]).replace('_', 'o')).replace('-', 'm')).replace('+', 'p')).replace(' ', 'o')).substring(9, 30)
    p = plot(x, y, xr = xr, yr = yr, color = colors[i], overplot = p, current = win1, title = temp[-2] + ' ' + temp[-1] + ' Sensitivities', $
      xtitle = 'Wavelength (nm)', font_size = 14, name = name, buffer = 1)
    leg = legend(position = [1, 1], /relative, font_size = 7, font_name = 'times')
    p1 = plot(x, y, xr = xr, yr = yr, color = colors[i], current = win2, title = name, xtitle = 'Wavelength (nm)', font_size = 9, $
      layout = [tot_entries / 2, 2, i + 1], margin = 0.15)
    p3 = plot(avg_wl_sens, avg_sens, /overplot, linestyle = 2, color = 'black') ; average line plotted on individual graphs
    p4 = plot(x, y / avg_sens, xr = xr, yr = yr, overplot = p4, color = colors[i], current = win3, title = temp[-2] + ' ' + temp[-1] + ' Sensitivities Divided by Mean', $
      xtitle = 'Wavelength (nm)', font_size = 14, name = name, font_style = 0, buffer = 1)
    leg1 = legend(position = [1, 1], /relative, font_size = 7, font_name = 'times')
  endfor
  p2 = plot(avg_wl_sens, avg_sens, current = win1, /overplot, linestyle = 2, color = 'black', buffer = 1) ; average line plotted on compound graph
  p5 = plot(avg_wl_sens, avg_sens / avg_sens, current = win3, /overplot, linestyle = 2, color = 'black', buffer = 1)
  win1.save, save_path + temp[-2] + '_' + temp[-1] + '_Sensitivities_Combined.png'
  win2.save, save_path + temp[-2] + '_' + temp[-1] + '_Sensitivities_Grided.png'
  win3.save, save_path + temp[-2] + '_' + temp[-1] + '_Divided_Sensitivities_Combined.png'
  ; win1.close
  ; win2.close
  print, 'Done'
end
