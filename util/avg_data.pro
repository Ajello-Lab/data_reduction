; PURPOSE:
; takes a set of data and normalizes and then averages it
; INPUTS
; csv_set: array of csv's to read
; OUTPUTS
; ret: a structure containing the averaged x and y data
pro avg_data, csv_set, ret
  compile_opt idl2

  tot_y = []
  tot_x = []
  tot_err = []
  ; read csv data
  for i = 0, n_elements(csv_set) - 1 do begin
    readcol, csv_set[i], x_out, y_out, err_out, delimiter = ',', format = '(F10.2, F10.6, F10.8)', /silent

    tot_x = [tot_x, x_out]
    tot_y = [tot_y, y_out]
    tot_err = [tot_err, err_out]
  endfor

  if tot_x ne !null then begin
    average_data_points, tot_x, tot_y, avg_x, avg_y, tot_err, err_out
    ret = {avg_x: avg_x, avg_y: avg_y, comb_err: err_out}
  endif else ret = []
end
