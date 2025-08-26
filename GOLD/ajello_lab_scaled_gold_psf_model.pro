;+
; PURPOSE
;  This routine will return a line-spread function (called PSF here) for
;   the GOLD EM instrument, found by fitting a Gaussian to the N 1483 angstrom
;   feature in a Round 11 dataset.
; 
; INPUTS
;  wl_ang: wavelength vector in angstroms
;  
; OUTPUTS
;  psf: A Gaussian distribution in unbinned pixel space that can be used to 
;   convolve with a high-spectral-resolution model to compare with 
;   measured data.
;-
pro ajello_lab_scaled_gold_psf_model, wl_ang, psf

; Gaussian fit parameters to the 1493 line in wavelength space
;  center wavelength and standard deviation in angstroms
afit = [ 150880.09, 1493.3298, 1.5506437 ]

; GOLD EM dispersion: 0.1943 angstroms / pixel
;disp = 0.1943  ; angstrom/pixel

disp = mean( deriv( wl_ang ) )

; standard deviation in pixels
sdv_pix = afit[2] / disp

psf = gaussian_function( sdv_pix, /normalize )

end