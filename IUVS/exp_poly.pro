;
; PURPOSE
; To be used in the function CURVEFIT to fit to sensitivity data
;
; INPUTS
;
pro exp_poly, X, A, F, pder
  compile_opt idl2
  aa = A[0]
  bb = A[1]
  cc = A[2]
  Z = X + cc
  F = aa * Z ^ 2.0d * exp(bb * Z)

  if n_params() ge 4 then $
    pder = [[Z ^ 2.0d * exp(bb * Z)], $
      [aa * Z ^ 3.0d * exp(bb * Z)], $
      [aa * Z * exp(bb * Z) * (2.0d + bb * Z)]]
end
