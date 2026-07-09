;+
; PURPOSE
;  This routine will return the FUV sensitivity as determined in Jul 2026
;   using data from ...TBD...
;
; INPUTS
;  wave_out: wavelength vector at which the sensitivity will be returned
;
; OUTPUTS
;  sens_out: sensitivity in arbitrary units at the wavelengths in wave_out
;
; KEYWORDS
;-
pro ajello_lab_sensitivity_fuv_2026_07, wave_out, sens_out

  ;
  ; if no wavelength vector provided, create one
  ;
  if n_params() eq 0 then begin
    ajello_lab_pixel_scale_rot, wlfuv, wlmuv, yfuv, ymuv
    wave_out = wlfuv
    
    wl1 = 105
    wl2 = 200
    wlstep = 0.1
    wave_out = findgen( (wl2-wl1)/wlstep )*wlstep + wl1
  endif
  
  ajello_lab_set_paths, path_base, path_repo

  ;
  ; read in the derived sensitivity at irregular wavelength samples 
  ;
  file_sens = path_repo + '/IUVS/sensitivity/Combined_Sensitivities_Fit.csv'
  data = (read_ascii( file_sens, delimiter=',', data_Start=2 )).(0)
  wave_sens = reform( data[0,*] )
  sens = reform( data[1,*] )

  ;
  ; interpolate the sensitivity to the wavelength vector provided
  ;
  sens_out = 10.^( interpol( alog10(sens), wave_sens, wave_out ) )
  
  if n_params() eq 0 then begin
    p1 = plot( wave_sens, sens, /ylog )
    p2 = plot( wave_out, sens_out, /over, color='red' )
    p3 = plot( wave_sens, sens, /over, thick=2 )
    
    p1 = plot( wave_sens, sens )
    p2 = plot( wave_out, sens_out, /over, color='red' )
    p3 = plot( wave_sens, sens, /over, thick=2, symbol='o', /sym_filled )
    
    stop
  endif

end