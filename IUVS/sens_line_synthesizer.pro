; PURPOSE:
; takes a set of data and normalizes and then averages it
; INPUTS
; csv_set: array of csv's to read
; OUTPUTS
; ret: a structure containing the averaged x and y data
function avg_csv_data, csv_set
  compile_opt idl2

  tot_y = []
  tot_x = []
  strings = []
  ; read csv data
  for i = 0, n_elements(csv_set) - 1 do begin
    set = read_csv(csv_set[i])

    tot_x = [tot_x, set.field1]
    tot_y = [tot_y, set.field2]

    strings = check_name(csv_set[i], strings)
  endfor

  if tot_x ne !null then begin
    average_data_points, tot_x, tot_y, avg_x, avg_y
    ret = {avg_x: avg_x, avg_y: avg_y, names: strings}
  endif else ret = []

  return, ret
end

; PURPOSE
; takes in a file name and a list of strings and determines if
; a specific section of the file name is contained in the list of strings
; to be used to eventually get a specific name for a graph
function check_name, file_name, names_so_far
  compile_opt idl2

  temp = strsplit(file_name, '/', /extract)
  cur = strsplit(temp[-1], ' ', /extract)
  temp_inx = where(names_so_far eq cur[1], count)
  if count eq 0 then names_so_far = [names_so_far, cur[1]]

  return, names_so_far
end

; PURPOSE
; take in a set of data and normalize it
; INPUT
; to_norm: structure containing the data to be normalized
; OUTPUT
; to_norm: structure modified to contain the normalized data
function normalize_to, to_norm, norm_basis
  compile_opt idl2

  ; Normalizes via mean
  ;
  ndx_basis = where(norm_basis.avg_x gt 127.5 and norm_basis.avg_x lt 161, basis_count)
  ndx_norm = where(to_norm.avg_x gt 127.5 and to_norm.avg_x lt 161, norm_count)

  to_norm.avg_y *= mean(norm_basis.avg_y[ndx_basis]) / mean(to_norm.avg_y[ndx_norm])

  ; Normalizes via interpolation
  ;
  ; ndx_norm = where(to_norm.avg_x gt 145 and to_norm.avg_x lt 155)
  ; norm_xs = norm_basis.avg_x[ndx_norm]
  ; interp_basis = interpol(norm_basis.avg_y, norm_basis.avg_x, norm_xs)

  ; to_norm.avg_y *= mean(interp_basis / to_norm.avg_y[ndx_norm])

  return, to_norm
end

;
; PURPOSE:
; Take in two folders with csv files containing a set of lines and output their
; respective averages on a plot and normalize them
; Creates a fit line for compounded sensitivities
; INPUTS
; set_1_csvs: array of csv files
; set_2_csvs: array of csv files
; save_path: path to save folder
; KEYWORDS
; fit_type: The type of fit to be used (polynomial, lowess, bspline)
pro sens_line_synthesizer, set_1a_csvs, set_1b_csvs, set_2a_csvs, set_2b_csvs, save_path, $
  fit_type = fit_type, $
  combine_all = combine_all
  compile_opt idl2

  ; USER INPUTS:::
  ; folder_path = '/Users/benjamincondit/Desktop/Sensitivity_Data/Round 7/H2/' ; insert file path here
  ; save_path = '/Users/benjamincondit/Desktop/Sens_Saves/' ; insert save path here
  save_csv = 1

  set_1H2 = avg_csv_data(set_1a_csvs)
  set_1N2 = avg_csv_data(set_1b_csvs)
  set_2H2 = avg_csv_data(set_2a_csvs)
  set_2N2 = avg_csv_data(set_2b_csvs)

  quality_title = '(' + set_1N2.names[0]
  endname = 'Quality'
  for i = 1, n_elements(set_1N2.names) - 1 do begin
    quality_title = quality_title + '/' + set_1N2.names[i] ; Assumes that this set contains all of the qualities that will be present in the run
    endname = 'Qualities'
  endfor
  quality_title = quality_title + ' ' + endname + ')'

  if set_1H2 ne !null and set_1N2 ne !null then $
    temp1 = normalize_to(set_1H2, set_1N2) ; funtion changes set_1H2 so temp1 isn't used
  if set_2H2 ne !null and set_2N2 ne !null then $
    temp2 = normalize_to(set_2H2, set_2N2)

  ; Plots the lines to be meshed together first and then sepeareted by sets
  win1 = window(dim = [1440, 600], buffer = 0)
  xr = [115, 185]
  p1 = plot([115], [1], current = win1, font_size = 16, xr = xr, linestyle = 6, $
    title = 'H2/N2 Seperate Sensititivity Comparisons Combined' + quality_title, font_name = 'times')
  if set_1H2 ne !null then begin
    p2 = plot(set_1H2.avg_x, set_1H2.avg_y, /over, color = 'red', name = 'H2 Pre-')
    leg = legend(/relative, position = [1, 1], font_size = 12, font_name = 'times')
  endif
  if set_1N2 ne !null then begin
    p3 = plot(set_1N2.avg_x, set_1N2.avg_y, /over, color = 'orange', name = 'N2 Pre-')
    leg = legend(/relative, position = [1, 1], font_size = 12, font_name = 'times')
  endif
  if set_2H2 ne !null then begin
    p4 = plot(set_2H2.avg_x, set_2H2.avg_y, /over, color = 'green', name = 'H2 Post-')
    leg = legend(/relative, position = [1, 1], font_size = 12, font_name = 'times')
  endif
  if set_2N2 ne !null then begin
    p5 = plot(set_2N2.avg_x, set_2N2.avg_y, /over, color = 'blue', name = 'N2 Post-')
    leg = legend(/relative, position = [1, 1], font_size = 12, font_name = 'times')
  endif
  win1.save, save_path + 'Uncombined_Sensitivities_Compared_Both.png'

  win2 = window(dim = [1440, 600], buffer = 0)
  xr = [115, 185]
  p1 = plot([115], [1], current = win2, font_size = 16, xr = xr, linestyle = 6, $
    title = 'H2/N2 Seperate Sensititivity Comparisons Pre- ' + quality_title, font_name = 'times')
  if set_1H2 ne !null then begin
    p2 = plot(set_1H2.avg_x, set_1H2.avg_y, /over, color = 'red', name = 'H2 Pre-')
    leg = legend(/relative, position = [1, 1], font_size = 12, font_name = 'times')
  endif
  if set_1N2 ne !null then begin
    p3 = plot(set_1N2.avg_x, set_1N2.avg_y, /over, color = 'orange', name = 'N2 Pre-')
    leg = legend(/relative, position = [1, 1], font_size = 12, font_name = 'times')
  endif
  win2.save, save_path + 'Uncombined_Sensitivities_Compared_-pre.png'

  win3 = window(dim = [1440, 600], buffer = 0)
  xr = [115, 185]
  p1 = plot([115], [1], current = win3, font_size = 16, xr = xr, linestyle = 6, $
    title = 'H2/N2 Seperate Sensititivity Comparisons Post- ' + quality_title, font_name = 'times')
  if set_2H2 ne !null then begin
    p4 = plot(set_2H2.avg_x, set_2H2.avg_y, /over, color = 'green', name = 'H2 Post-')
    leg = legend(/relative, position = [1, 1], font_size = 12, font_name = 'times')
  endif
  if set_2N2 ne !null then begin
    p5 = plot(set_2N2.avg_x, set_2N2.avg_y, /over, color = 'blue', name = 'N2 Post-')
    leg = legend(/relative, position = [1, 1], font_size = 12, font_name = 'times')
  endif
  win3.save, save_path + 'Uncombined_Sensitivities_Compared_-post.png'

  if isa(set_1H2, /null) then set_1H2 = set_1N2
  if isa(set_1N2, /null) then set_1N2 = set_1H2
  if isa(set_2H2, /null) then set_2H2 = set_2N2
  if isa(set_2N2, /null) then set_2N2 = set_2H2

  x_in = [set_1H2.avg_x, set_1N2.avg_x]
  y_in = [set_1H2.avg_y, set_1N2.avg_y]
  x_in2 = [set_2H2.avg_x, set_2N2.avg_x]
  y_in2 = [set_2H2.avg_y, set_2N2.avg_y]

  average_data_points, x_in, y_in, x_out1, y_out1
  average_data_points, x_in2, y_in2, x_out2, y_out2

  data_diff = y_out2 / y_out1

  ; plot the lines
  win = window(dim = [1440, 600], buffer = 0)
  xr = [115, 185]
  yr = [0, 1.5]
  p1 = plot(x_out1, y_out1, current = win, color = 'red', font_size = 16, xr = xr, yr = yr, $
    name = 'Pre-Accident', xtitle = 'Wavelength (nm)', font_name = 'times', title = 'Sensititivity Comparisons ' + quality_title, buffer = 0)
  p2 = plot(x_out2, y_out2, /over, color = 'blue', name = 'Post-Accident')

  ; Line fit section
  ext_leg = 0
  if keyword_set(fit_type) then begin
    ext_leg = 1
    ; Computes polynomial line fit
    if fit_type eq 'polynomial' then begin
      co_1 = poly_fit(double(x_out1), double(y_out1), 4, /double)
      co_2 = poly_fit(double(x_out2), double(y_out2), 4, /double)

      fit_curve_1 = co_1[0] + co_1[1] * x_out1 + co_1[2] * x_out1 ^ 2 $
        + co_1[3] * x_out1 ^ 3 + co_1[4] * x_out1 ^ 4 ; + co_1[5] * x_out1 ^ 5 + co_1[6] * x_out1 ^ 6
      fit_curve_2 = co_2[0] + co_2[1] * x_out2 + co_2[2] * x_out2 ^ 2 $
        + co_2[3] * x_out2 ^ 3 + co_2[4] * x_out2 ^ 4 ; + co_2[5] * x_out2 ^ 5 + co_2[6] * x_out2 ^ 6
      name = 'PolyFit'
    endif

    ; Computes lowless fit
    if fit_type eq 'lowess' then begin
      lowess, x_out1, y_out1, 3, fit_curve_1
      lowess, x_out2, y_out2, 3, fit_curve_2
      name = 'LowessFit'
    endif

    ; Computes the Bspline fit ----- NEED HELP HERE
    if fit_type eq 'bspline' then begin
      invar = make_array(n_elements(x_out1), value = 1)
      fit_curve_1 = bspline_fit(x_out1, double(y_out1), invar)
      fit_curve_2 = bspline_fit(x_out2, double(y_out2), invar)
      name = 'BsplineFit'
    endif

    curve_diff = fit_curve_2 / fit_curve_1
    ; curve_diff = curve_diff / max(curve_diff)
    ; cdco = poly_fit(x_out1, curve_diff, 1, /double)
    ; cd_curve = cdco[0] + cdco[1] * x_out1
    ; p7 = plot(x_out1, cd_curve, color = 'green yellow', /over, name = 'Fit of Ratio of Fits', linestyle = 3)

    p3 = plot(x_out1, fit_curve_1, color = 'dark orange', /over, name = 'Pre-Accident ' + name, linestyle = 1)
    p4 = plot(x_out2, fit_curve_2, color = 'cornflower', /over, name = 'Post-Accident ' + name, linestyle = 1)
    p5 = plot(x_out1, curve_diff, color = 'dark olive green', /over, name = 'Ratio of Fit Lines', linestyle = 2)
  endif

  p6 = plot(x_out1, data_diff, color = 'dark cyan', /over, name = 'Ratio of Two Data Lines', linestyle = 2)
  p7 = plot(xr, [1, 1], color = 'light gray', /over, linestyle = 5)

  if keyword_set(fit_type) then leg = legend(target = [p1, p2, p3, p4, p5, p6], $
    position = [1.1, 1.15], /relative, font_name = 'times', font_size = 7, linestyle = 6) $
  else leg = legend(target = [p1, p2, p6], $
    position = [1.1, 1.15], /relative, font_name = 'times', font_size = 7, linestyle = 6)
  win.save, save_path + 'Sensitivities_Compared.png'
  ; win.close

  if save_csv then begin
    csv_name = save_path + 'All_Averaged_Values_Set1.csv'
    sens_avg_struct = {x_values: x_out1, y_values: y_out1}
    write_csv, csv_name, sens_avg_struct.x_values, sens_avg_struct.y_values, header = ['wl_sens', 'sens']
    csv_name = save_path + 'All_Averaged_Values_Set2.csv'
    sens_avg_struct = {x_values: x_out2, y_values: y_out2}
    write_csv, csv_name, sens_avg_struct.x_values, sens_avg_struct.y_values, header = ['wl_sens', 'sens']
  endif

  if keyword_set(combine_all) then begin
    tot_x = [x_out1, x_out2]
    tot_y = [y_out1, y_out2]
    average_data_points, tot_x, tot_y, final_x, final_y
    win4 = window(dim = [1440, 600], buffer = 0)
    xr = [115, 185]
    p1 = plot(tot_x, tot_y, current = win4, color = 'red', font_size = 16, xr = xr, title = 'H2/N2 Combined Sensitity Curve ' + quality_title)
    win4.save, save_path + 'Combined_Sensitivity_Curve.png'
  endif
end
