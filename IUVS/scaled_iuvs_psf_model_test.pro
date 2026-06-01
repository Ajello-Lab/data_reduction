

pro scaled_iuvs_psf_model_test

ajello_lab_set_paths, path_base, path_repo

;
; generate an estimate of the instrument point spread function
;  (actually, this is the line-spread function)
;
file_model = path_repo + '/data/n2/n2_lbh_rot_293K.sav'
restore, file_model, /relax
;  % RESTORE: Recovering incompatible definition of structure LIST using relaxed structure assignment rules.
;  % RESTORE: Recovering incompatible definition of structure HASH using relaxed structure assignment rules.
;  % RESTORE: Restored variable: TROT.
;  % RESTORE: Restored variable: WAVE.
;  % RESTORE: Restored variable: LBH.
;  % RESTORE: Restored variable: LIST.

scaled_iuvs_psf_model, wave[0:301], psf
wave_lbh = wave / 10.

;x = findgen(302)*0.40105999
;x = findgen(302)
x = wave[0:301]*2
scaled_iuvs_psf_model, x, psf2

p1 = plot( x, psf )
p2 = plot( x, psf2, /over, color='red' )
;
;p1 = plot( wave[0:301], psf )
;p2 = plot( x, psf2, /over, color='red' )


x1 = wave[0:301]
v1 = psf
fwhm, x1, v1, x1out, x2out, fwhm_val1, x1in=x1in, x2in=x2in, $
  interpval=interpval, fac=fac, method=method

x2 = x
v2 = psf
fwhm, x2, v2, x1out, x2out, fwhm_val2, x1in=x1in, x2in=x2in, $
  interpval=interpval, fac=fac, method=method

wave_cen = median(x1)
scaled_iuvs_psf_model_gmh, wave_cen, x1, ff ;, wave_wid=wave_wid

x2_step = 0.1
x3 = findgen( (max(x1) - min(x1))/x2_step )*x2_step + min(x1 )
wave_cen = median(x1)
scaled_iuvs_psf_model_gmh, wave_cen, x3, ff3 ;, wave_wid=wave_wid

p1 = plot( x1, ff )
p2 = plot( x1, psf, /over, color='red' )
p3 = plot( x3, ff3, /over, color='blue' )


fwhm, x3, ff3, x1out, x2out, fwhm_val3, x1in=x1in, x2in=x2in, $
  interpval=interpval, fac=fac, method=method

print, fwhm_val1
print, fwhm_val2
print, fwhm_val3

stop

end