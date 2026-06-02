
;+
; PURPOSE
;  This routine will 
;
; INPUTS
;  none
;
;-
pro ajello_lab_calibration_fuv_h2, wl_spec, spec, wl_sens, sens, $
  wl1, wl2, $
  show_plots = show_plots, $
  file_h2_model = file_h2_model

  if n_params() eq 0 then begin
    
    case (get_login_info()).user_name of
      'holsclaw': begin
        file_data = '/Volumes/projects/Phase_Development/MAVEN/IUVS_Data/IUVS_Breadboard/_Big_e-gun_RoundVII/data_reduction/H2_100EV_FUV_TEST12_H2_100EV_ROT_+7_IMAGE1_MED_PRESS_2E-5.idl'
        file_data = '/Volumes/projects/Phase_Development/MAVEN/IUVS_Data/IUVS_Breadboard/_Big_e-gun_RoundVII/data_reduction/H2_100EV_FUV_TEST11_H2_100EV_ROT_+7_IMAGE1_LOW_PRESS_4E-6.idl'
      end
      'benjamincondit': begin
        file_data = ''
        stop
      end
    endcase
    ;restore,file,/ver
  
    if file_test(file_data) eq 0 then begin
      print, 'data file not found:'
      print, file_data
      stop
    endif
  
    ;
    ; to save time, restore only the final background-subtracted image 
    ;
    sObj = OBJ_NEW('IDL_Savefile', file_data)
    sObj->Restore, 'arr'
    OBJ_DESTROY, sObj
  
    ;
    ; add up a subset of spatial rows centered on the emission peak 
    ;  to arrive at one representative spectrum 
    ;
    spat = total( arr, 1 )
    y0 = where( spat eq max(spat) )
    yw = 100
    y1 = y0-yw
    y2 = y0+yw
    spec = total( arr[*,y1:y2], 2 )
  
    ;
    ; retrieve a default wavelength scale for the rotated image
    ;
    ajello_lab_pixel_scale_rot, wlfuv, wlmuv, yfuv, ymuv
  
    ;
    ; TODO: automate the wavelength correction using cross correlation
    ; 
    ; this approach assumes the peak emission is at Lyman alpha
    ;
    wl_spec = wlfuv - wlfuv[ findndx(spec,max(spec)) ] + 121.6
  
    win = window(dim=[800,600])
    p1 = plot( wlfuv, spec, name='orignal data', current=win )
    p2 = plot( wl_spec, spec, /over, color='red', name='shifted' )
    leg = legend(target=[p1,p2])
    markerp,p1,x=121.6,linestyle=2
  
  endif


  case (get_login_info()).user_name of
    'holsclaw': begin
      ajello_lab_set_paths, path_base, path_repo
      path_ref = path_repo + 'data/H2/'
      file_ref = path_ref + 'h2euv100.txt'
      ajello_lab_h2_reference, wave_ref, spec_ref, file_Ref=file_ref
    end
    'benjamincondit': begin
      ajello_lab_set_paths, path_base, path_repo
      path_ref = path_repo + 'data/H2/'
      file_ref = path_ref + 'h2euv100.txt'
      ajello_lab_h2_reference, wave_ref, spec_ref, file_Ref=file_ref
      stop
    end
  endcase
  ;restore,file,/ver

;wl1 = [ ...
;wl2 = [ ...

  win = window(dim=[1200,600])
  xr=[110,170]
  p1 = plot( wave_ref/10., spec_ref-min(spec_ref), yr=[0,5.2e4], name='reference', xtitle='wavelength (nm)', current=win, xr=xr, font_size=14 )
  p2 = plot( wl_spec, spec, /over, color='red', name='data' )
  leg = legend(target=[p1,p2],font_size=14)
  
  ;
  ;  interpolate reference to data wavelength scale
  ;  ratio of data to reference should be the sensitivity, but it is noisy
  ;
  spec_refi = interpol( spec_ref-min(spec_ref), wave_ref/10., wl_spec )
  p = plot( wl_spec, spec / spec_refi )
  
  stop

end