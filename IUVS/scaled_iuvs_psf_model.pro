;+
; PURPOSE
;  This routine will return the PSF (actually LSF) for MAVEN IUVS when given
;   a wavelength vector.
;   
; INPUTS
;  x: wavelength vector in angstroms
;  
; OUTPUTS
;  ff: PSF (actually LSF)
;  
; NOTES
;  Email from Victoir Veibell on Jul 22, 2025
;-
pro scaled_iuvs_psf_model, x, ff
  ;Gaussian + Lorentzian PSF model

  Compile_opt idl2
  loadct,10,/silent
  ;A = [345.231, 121.524, 0.198492, 24.3185, 0.811196, 0.25399]
  A = [345.23, 121.524, 0.3889, 24.332, 1.0846, 0.6560] ; new numbers bc of python -> idl

  Adist = 0.083129883
  Bdist = x[1]-x[0]
  B = A
  B[1] = median(x)
  B[2] = A[2] * Bdist/Adist
  B[4] = A[4] * Bdist/Adist

  ff = B[0] * exp( -0.5 * (x-B[1])^2 / B[2]^2 ) + B[3] / (1.+(x-B[1])^2 / B[4]^2) + B[5]
end