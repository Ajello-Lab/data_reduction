;+
; PURPOSE
;  This routine will determine the wavelength offset of a reference MODEL
;   that minimizes the error between it and an observed spectrum.
; 
; INPUTS
;  wave_spec: wavelength estimate for the measured spectrum
;  spec: observed spectrum
;  wave_model: wavelength scale of the model reference
;  model_in: spectrum of the model reference
;  wl1_fit: short-wavelength end of the wavelength range to be compared
;  wl2_fit: long-wavelength end of the wavelength range to be compared
; 
; OUTPUTS
;  wave_model_offset: vector of offset values to evaluate
;  wave_model_offset_opt: the optimal MODEL additive offset value determined
;  
; KEYWORDS
;  show_plots: set to 1 to show comparison plots
;  
; NOTES
;  Upon return, the optimized observed spectrum wavelength scale is:
;   wave_spec_corr = wave_spec - wave_model_offset_opt
;-
pro ajello_lab_spectrum_wavelength_offset, wave_spec, spec_in, wave_model, model_in, $
  wl1_fit, wl2_fit, $
  wave_model_offset, $
  wave_model_offset_opt, $
  show_plots=show_plots

  spec = spec_in - min(spec_in)
  
  ndx_wave_spec_fit = where( (wave_spec gt wl1_fit) and (wave_spec lt wl2_fit), count_spec_fit )
  ndx_wave_model_fit = where( (wave_model gt wl1_fit) and (wave_model lt wl2_fit), count_model_fit )
  model = model_in * mean( spec[ndx_wave_spec_fit] ) / mean( model_in[ndx_wave_model_fit] )
  ;wave_model_offset = (findgen(40)-20)*0.01
  num_wave_model_offset = n_elements(wave_model_offset)
  num_wave_model = n_elements( wave_model )
  num_wave_spec = n_elements( wave_spec )
  err_fit = fltarr( num_wave_model_offset )
  corr_fit = fltarr( num_wave_model_offset )
  for i = 0, num_wave_model_offset - 1 do begin
    wave_modelj = wave_model + wave_model_offset[i]
    modeli = interpol( model, wave_modelj, wave_spec )
    spec_diff = spec - modeli
    err_fit[i] = stddev( spec_diff[ndx_wave_spec_fit] )
    corr_fit[i] = correlate( modeli[ndx_wave_spec_fit], spec[ndx_wave_spec_fit] )
  endfor

  ndx_opt_err_fit = findndx( err_fit, min(err_fit) )
  ndx_opt_corr_fit = findndx( corr_fit, max(corr_fit) )
  wave_model_offset_opt = wave_model_offset[ ndx_opt_corr_fit ]
  
  ;num_wave_model_offset_hi = 
  

  if keyword_set(show_plots) then begin
    win = window(dim=[800,600])
    p1 = plot( wave_model_offset, err_fit, current=win, layout=[1,2,1], title='err', xtitle='wave model offset' )
    pfit_err = poly_fit( wave_model_offset, err_fit, 2, yfit=yfit_err )
    p1f = plot( wave_model_offset, yfit_err, /over, color='red', linestyle=2 )
    markerp,p1,x=wave_model_offset[ndx_opt_err_fit], linestyle=2
    ;
    p2 = plot( wave_model_offset, corr_fit, current=win, layout=[1,2,2], title='correlation' )
    pfit_corr = poly_fit( wave_model_offset, corr_fit, 2, yfit=yfit_corr )
    p2f = plot( wave_model_offset, yfit_corr, /over, color='red', linestyle=2 )
    ;
    gfit = gaussfit( wave_model_offset, corr_fit, nterms=3 )
    p2ff = plot( wave_model_offset, gfit, /over, color='blue' )
    markerp,p2,x=wave_model_offset[ndx_opt_corr_fit], linestyle=2
    
    win = window(dim=[800,600])
    p1 = plot( wave_spec-wave_model_offset_opt, spec, current=win )
    p2 = plot( wave_model, model, /over, color='red' )
    
    stop
  endif
end
