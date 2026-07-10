; PURPOSE
; Generates the Coefficent of determination (R^2) for a provided set of data
;
; INPUTS
; data_x: x values of data to be fit
; data_y: y values of data to be fit
;
; OUTPUTS
; r_squared: Coefficent of determination for the fit
;
pro calc_r_squared, data_x, data_y, r_squared
  compile_opt idl2
  coeff = poly_fit(data_x, data_y, 1)
  fit_y = poly(data_x, coeff)

  ss_res = total((data_y - fit_y) ^ 2)
  ss_tot = total((data_y - mean(data_y)) ^ 2)
  r_squared = 1 - (ss_res / ss_tot)
end
