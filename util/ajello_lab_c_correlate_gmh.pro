;+
; PURPOSE
;  This routine will find the wavelength shift of one spectrum required to
;   maximize the correlation with another spectrum.
;   
; INPUTS
;  wlv1 : wavelength vector associated with the first spectrum
;  wlv2 : wavelength vector associated with the second spectrum
;  spec1 : first spectrum - spectrum with uncertain wavelength scale
;  spec2 : reference spectrum - this will be shifted in wavelength to match 
;           spec1. The negative of the resulting wavelength offset is then 
;           that required to shift spec1 to match spec2
;  lag : Vector of wavelength shift values
;  sm1 : convolution kernel for spec1.  Set to 0 or 1 for no smoothing.
;  sm2 : convolution kernel for spec2.  Set to 0 or 1 for no smoothing.
;  wlr1 : starting wavelength range used to calculate the correlation coefficient
;  wlr2 : ending wavelength range used to calculate the correlation coefficient
; 
; OUTPUTS
;  c : correlation coefficient at each lag step
;  spec2_sm_max : 
;  
; KEYWORDS
;  plots : set to 1 to show plots
;  
; PROCEDURE:
; iter 1
;  smooth spec1 by sm1
;  smooth spec2 by sm2
;  interpolate spec2 to spec1 with wlv2 shifted by lag[i]
;  normalize the magnitude of the interpolated spec2 across the specified 
;   wavelength range
;  calculate the correlation coefficient between wl1 and wl2
;  repeat for each element of lag
;-
pro ajello_lab_c_correlate_gmh, wlv1, wlv2, spec1, spec2, lag, sm1, sm2, $
  wlr1, wlr2, c, spec2_sm_max, fac_norm_vec, plots=plots

nlag = n_elements(lag)
ndx1 = where( wlv1 gt wlr1 and wlv1 lt wlr2 )

;if keyword_set(plots) then $
;  p = plot( wlv1, spec1, yr=[0,max(spec1)*1.1] )

c = fltarr(nlag)
spec1_sm = smooth( spec1, sm1 )

;spec2_smi_vec = fltarr( n_elements(spec1), nlag )

;fac_norm = max( spec1 ) / max(spec2 )

fac_norm_vec = fltarr(nlag)
for i = 0, nlag - 1 do begin
  spec2_sm = smooth( spec2, sm2 )
  spec2_smi = interpol( spec2_sm, wlv2+lag[i], wlv1 )
  
  fac_norm_i = total( spec2_smi[ndx1] ) / total( spec1_sm[ndx1] )
  fac_norm_vec[i] = fac_norm_i
  
  c[i] = correlate( spec1_sm[ndx1] * fac_norm_i, spec2_smi[ndx1] ) 
  
  ;spec2_smi_vec[*,i] = spec2_smi
  
;  if keyword_set(plots) then begin
;    pi = plot( wlv1, spec2_smi*fac_norm, /overplot )
;    ;stop
;  endif
endfor

;ndx_max = findndx(c,max(c))
c_max = max(c, ndx_max)
spec2_sm = smooth( spec2, sm2 )
spec2_sm_max = interpol( spec2_sm, wlv2+lag[ndx_max], wlv1 )

if keyword_set(plots) then begin
  
  win = window(dim=[800,600])
  p = plot( lag, c, xtitle='spec2 wavelength shift (nm)', $
    ytitle='correlation', font_size=16, current=win )

  win = window(dim=[800,600])
  p1 = plot( wlv1, spec1*fac_norm_vec[ndx_max], current=win, $
    xtitle='wavelength (nm)', ytitle='', font_size=16, name='spec1' )
  p2 = plot( wlv1, spec2_sm_max, /overplot, color='red', name='spec2 (shifted)' )
  leg = legend(target=[p1,p2],font_size=14)
  
  win = window(dim=[800,600])
  p1 = plot( wlv1-lag[ndx_max], spec1*fac_norm_vec[ndx_max], current=win, $
    xtitle='wavelength (nm)', ytitle='', font_size=16, name='spec1 (shifted)' )
  p2 = plot( wlv1, spec2_sm, /overplot, color='red', name='spec2' )
  leg = legend(target=[p1,p2],font_size=14)
  
;  p1 = plot( wlv1, spec1 )
;  p2 = plot( wlv1-2.5, spec2_sm_max*fac_norm, /over, color='red' )
  
  stop
endif

end