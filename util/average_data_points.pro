;
; PURPOSE
; Takes a set of data and averages it for a specific set of points
; Data does not need to match in either length of range
; INPUTS
; two arrays that contain all of the data in an unsorted fashion
; x_in: the x value array
; y_in: the y value array
;
; OUTPUTS
; x_out: the x value array
; y_out: the averaged y value array
;
pro average_data_points, x_in, y_in, x_out, y_out, err_in, err_out
  compile_opt idl2

  wl_sens_index = sort(x_in)
  x_in = x_in[wl_sens_index]
  y_in = y_in[wl_sens_index]
  uniq_index = uniq(x_in)
  x_out = x_in[uniq_index]
  sens_grouped = list()
  err_grouped = list()
  for k = 0, n_elements(x_out) - 1 do begin
    match_idx = where(x_in eq x_out[k])
    sens_grouped.add, y_in[match_idx]
    err_grouped.add, err_in[match_idx]
  endfor
  y_out = fltarr(n_elements(sens_grouped))
  err_out = fltarr(n_elements(sens_grouped))
  for l = 0, n_elements(sens_grouped) - 1 do begin
    y_out[l] = avg(sens_grouped[l])
    err_out[l] = sqrt(total(err_grouped[l] ^ 2))
  endfor
end
