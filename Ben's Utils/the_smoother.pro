;
; PURPOSE
; smooths and then averages the data
;
; INPUT
; structs_in: structuress to be smoothed
;
; OUTPUT
; struct_out: structures that have been smoothed and averaged
;
pro the_smoother, structs_in, struct_out, num_to_use
  compile_opt idl2

  ; x_in = []
  ; y_in = []

  ; for i = 0, n_elements(structs_in) - 1 do begin
  ; y_in = [y_in, smooth(structs_in[i].spec, 3)]
  ; x_in = [x_in, structs_in[i].wl_spec]
  ; endfor

  ; average_data_points, x_in, y_in, x_out, y_out

  ; struct_out = {wl_spec: x_out, spec: y_out}

  struct_out = {wl_spec: structs_in[num_to_use].wl_spec, spec: smooth(structs_in[num_to_use].spec, 1)} ; spec: structs_in[num_to_use].spec}
end
