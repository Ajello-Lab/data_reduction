;
; PURPOSE
; Takes data and Calibrates it to account for instrument sensitivity based on
; 100eV H2 and 20ev N2 data between Rounds 12 and 14
;
; INPUTS
; struct_in: a structure that contains x and y values of data in 'field1' and 'field2' to be calibrated
;
; OUTPUTS
; struct_out: a structure that contains x and y values of data in 'field1' and 'field2' once calibrated
;
pro calibrator, struct_in, struct_out
  compile_opt idl2

  ; exp_poly fit
  a = 0.036124115
  b = -0.12488699
  c = -110.69664
  calibration_curve = 1 / ((a * (struct_in.wl_spec + c) ^ 2) * exp(b * (struct_in.wl_spec + c)) + 0.049990699)
  calibration_curve /= min(calibration_curve)

  xvals = struct_in.wl_spec
  co = [-1381.7288, 43.180122, -0.53444086, 0.0032813103, -1.0007346 / 10 ^ 5, 1.2140178 / 10 ^ 8]
  co[1] -= 0.0151809589551 - 0.0034857166 - 0.00017062186 ; Correct for linear trend in plot
  co[0] += 2.73269132702 + 0.36892921 - 0.041302309 ; Correct for linear trend

  calibration_curve = 1 / (co[0] + co[1] * xvals + co[2] * xvals ^ 2 $
    + co[3] * xvals ^ 3 + co[4] * xvals ^ 4 + co[5] * xvals ^ 5)

  calibration_curve = smooth(calibration_curve, 3)

  ; xvals = struct_in.wl_spec[where(struct_in.wl_spec lt 175)]
  ; co = [-2612.3804d, 94.049896d, -1.4062934d, 0.011211731d, -5.0385672d-5, 1.2125520d-7, -1.2226647d-10]
  ; calibration_curve = 1 / (co[0] + co[1] * xvals + co[2] * xvals ^ 2 $
  ;   + co[3] * xvals ^ 3 + co[4] * xvals ^ 4 + co[5] * xvals ^ 5 + co[6] * xvals ^ 6)

  ; win = window(dim = [1440, 600], buffer = 0)
  ; p2 = plot(struct_in.wl_spec, calibration_curve, symbol = 24, sym_filled = 1, sym_size = 0.5, /over, color = 'purple', title = 'calibration-curve')

  struct_out = {wl_spec: struct_in.wl_spec, spec: struct_in.spec * calibration_curve}
end
