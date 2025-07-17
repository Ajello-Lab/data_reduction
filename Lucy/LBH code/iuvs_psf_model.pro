
pro iuvs_psf_model,x,a,f
  f = A[0] * exp( -0.5 * (x-A[1])^2 / A[2]^2 )
  ;+ $
  ; A[3] / (1.+(x-A[1])^2 / A[4]^2) + $
  ;A[5]
end