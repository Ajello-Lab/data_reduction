
pro ajello_lab_gold_em_psf_model,x,a,f
  f = A[0] * exp( -0.5 * (x-A[1])^2 / A[2]^2 ) 
end
