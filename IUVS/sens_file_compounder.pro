;
; PURPOSE
; Takes in a folder of txt files and combines
; them into a single plot
;
; INPUTS
; folder_path: path to the folder to be read
; save_path: path to folder where outputs will be saved
;
; KEYWORDS
; actually_files: Allows use of the program with file paths as opposed to folder paths
; title: String array to be used in constructing graph names (remnant of initial use of program)
pro sens_file_compounder, folder_path, save_path, $
  actually_files = actually_files, $
  title = title, $
  save_csv = save_csv
  compile_opt idl2

  ; USER INPUTS:::
  ; folder_path = '/Users/benjamincondit/Desktop/Sens_Main/Sens_set_1/7N2 High Quality' ; insert file path here
  ; save_path = '/Users/benjamincondit/Desktop/Sens_Saves/' ; insert save path here

  if (not keyword_set(actually_files)) or actually_files eq 0 then begin
    file_data_helper = folder_path + '/' + '*.txt'
    files_to_read = file_search(file_data_helper, count = file_count)
  endif else begin
    file_count = n_elements(folder_path)
    files_to_read = folder_path
  endelse

  if not keyword_set(title) then begin
    temp = strsplit(folder_path, '/', /extract)
    title = temp[-2] + '_' + temp[-1]
  endif

  print, 'Number of files to read:' + string(file_count)

  plot_entries = list()
  name_entries = list()
  tot_entries = 0
  tot_wl_sens = []
  tot_sens = []
  tot_err = []

  for j = 0, file_count - 1 do begin
    readcol, files_to_read[j], wl_sens, sens, err, delimiter = ',', format = '(F10.2, F10.6, F10.8)', /silent

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
    tot_err = [tot_err, err]
  endfor

  average_data_points, tot_wl_sens, tot_sens, avg_wl_sens, avg_sens, tot_err, err_out

  if keyword_set(save_csv) then begin
    csv_name = save_path + title + '_Averaged_Values.csv'
    sens_avg_struct = {x_values: avg_wl_sens, y_values: avg_sens, comb_err: err_out}
    write_csv, csv_name, sens_avg_struct
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

    iterator = 0
    y_div = fltarr(n_elements(y))
    for j = 0, n_elements(avg_sens) - 1 do begin
      if x[iterator] eq avg_wl_sens[j] then begin
        y_div[iterator] = y[iterator] / avg_sens[j]
        iterator++
      endif
      if iterator eq n_elements(y) then break
    endfor
    name = (((((name_entries[i]).replace('_', 'o')).replace('-', 'm')).replace('+', 'p')).replace(' ', 'o')).substring(8, 30)
    p = plot(x, y, xr = xr, yr = yr, color = colors[i], overplot = p, current = win1, title = title + ' Sensitivities', $
      xtitle = 'Wavelength (nm)', font_size = 14, name = name)
    leg = legend(position = [1, 1], /relative, font_size = 7, font_name = 'times')
    p1 = plot(x, y, xr = xr, yr = yr, color = colors[i], symbol = 24, sym_filled = 1, sym_size = 0.5, current = win2, title = name, xtitle = 'Wavelength (nm)', font_size = 9, $
      layout = [tot_entries / 2, 2, i + 1], margin = 0.15)
    p3 = plot(avg_wl_sens, avg_sens, /overplot, symbol = 24, sym_filled = 1, sym_size = 0.5, linestyle = 2, color = 'black') ; average line plotted on individual graphs
    p4 = plot(x, y_div, xr = xr, yr = yr, symbol = 24, sym_filled = 1, sym_size = 0.5, overplot = p4, color = colors[i], current = win3, title = title + ' Sensitivities Divided by Mean', $
      xtitle = 'Wavelength (nm)', font_size = 14, name = name, font_style = 0)
    leg1 = legend(position = [1, 1], /relative, font_size = 7, font_name = 'times')
  endfor
  p2 = plot(avg_wl_sens, avg_sens, current = win1, /overplot, symbol = 24, sym_filled = 1, sym_size = 0.5, linestyle = 2, color = 'black') ; average line plotted on compound graph
  p5 = plot(avg_wl_sens, avg_sens / avg_sens, current = win3, symbol = 24, /overplot, linestyle = 2, color = 'black')
  win1.save, save_path + title + '_Sensitivities_Combined.png'
  win2.save, save_path + title + '_Sensitivities_Grided.png'
  win3.save, save_path + title + '_Divided_Sensitivities_Combined.png'
  print, 'Done'
end
