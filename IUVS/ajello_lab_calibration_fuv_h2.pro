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
  compile_opt idl2

  if n_params() eq 0 then begin
    case (get_login_info()).user_name of
      'holsclaw': begin
        file_data = '/Volumes/projects/Phase_Development/MAVEN/IUVS_Data/IUVS_Breadboard/_Big_e-gun_RoundVII/data_reduction/H2_100EV_FUV_TEST12_H2_100EV_ROT_+7_IMAGE1_MED_PRESS_2E-5.idl'
        file_data = '/Volumes/projects/Phase_Development/MAVEN/IUVS_Data/IUVS_Breadboard/_Big_e-gun_RoundVII/data_reduction/H2_100EV_FUV_TEST11_H2_100EV_ROT_+7_IMAGE1_LOW_PRESS_4E-6.idl'
      end
      'benjamincondit': begin
        file_data = '/Users/benjamincondit/idl/dat/H2_100EV_FUV_TEST11_H2_100EV_ROT_+7_IMAGE1_LOW_PRESS_4E-6.idl'
      end
    endcase
    ; restore,file,/ver

    if file_test(file_data) eq 0 then begin
      print, 'data file not found:'
      print, file_data
      stop
    endif

    ;
    ; to save time, restore only the final background-subtracted image
    ;
    sObj = obj_new('IDL_Savefile', file_data)
    sObj.restore, 'arr'
    obj_destroy, sObj

    ;
    ; add up a subset of spatial rows centered on the emission peak
    ; to arrive at one representative spectrum
    ;
    spat = total(arr, 1)
    y0 = where(spat eq max(spat))
    yw = 100
    y1 = y0 - yw
    y2 = y0 + yw
    spec = total(arr[*, y1 : y2], 2)

    ;
    ; retrieve a default wavelength scale for the rotated image
    ;
    ajello_lab_pixel_scale_rot, wlfuv, wlmuv, yfuv, ymuv

    ;
    ; TODO: automate the wavelength correction using cross correlation
    ;
    ; this approach assumes the peak emission is at Lyman alpha
    ;
    wl_spec = wlfuv - wlfuv[findndx(spec, max(spec))] + 121.6

    win = window(dim = [800, 600])
    p1 = plot(wlfuv, spec, name = 'orignal data', current = win)
    p2 = plot(wl_spec, spec, /over, color = 'red', name = 'shifted')
    leg = legend(target = [p1, p2])
    markerp, p1, x = 121.6, linestyle = 2
  endif

  case (get_login_info()).user_name of
    'holsclaw': begin
      ajello_lab_set_paths, path_base, path_repo
      path_ref = path_repo + 'data/H2/'
      file_ref = path_ref + 'h2euv100.txt'
      ajello_lab_h2_reference, wave_ref, spec_ref, file_ref = file_ref
    end
    'benjamincondit': begin
      ajello_lab_set_paths, path_base, path_repo
      path_ref = path_repo + 'data/H2/'
      file_ref = path_ref + 'h2euv100.txt'
      ajello_lab_h2_reference, wave_ref, spec_ref, file_ref = file_ref
    end
  endcase
  ; restore,file,/ver

  ; wl1 = [ ...
  ; wl2 = [ ...

  ; spec_deriv = smooth(deriv(spec, wl_spec), 3, /edge_truncate)
  spec_deriv = fix(deriv(spec, wl_spec) * 10000)

  indicies = where(spec_deriv eq 0)
  peaks = fltarr(n_elements(indicies))
  for i = 0, n_elements(indicies) - 2 do begin
    throw_away = max(spec[indicies[i] : indicies[i + 1]], max_index)
    peaks[i] = indicies[i] + max_index
  endfor
  print, peaks ; TODO: Somehow I'm getting these indicies that don't match up to the actual frequencies. Find a way to connect the two.

  win = window(dim = [1200, 600])
  xr = [110, 170]
  p1 = plot(wave_ref / 10., spec_ref - min(spec_ref), yr = [0, 5.2e4], name = 'reference', xtitle = 'wavelength (nm)', current = win, xr = xr, font_size = 14)
  p2 = plot(wl_spec, spec, /over, color = 'red', name = 'data')
  p3 = plot(wl_spec, spec_deriv * 5000, /over, color = 'pink', name = 'slope')
  leg = legend(target = [p1, p2, p3], font_size = 14)

  ;
  ; interpolate reference to data wavelength scale
  ; ratio of data to reference should be the sensitivity, but it is noisy
  ;
  spec_refi = interpol(spec_ref - min(spec_ref), wave_ref / 10., wl_spec)
  p = plot(wl_spec, spec / spec_refi)

  ; TODO: Added from n2 to be used once peak arrays have been found

  ; win = window(dim=[800,600])
  ; p1 = plot( pos_cen_data_pix, pos_cen_data_wave, symbol='x', current=win, $
  ; font_size=16, name='data', xtitle='pixel', ytitle='wavelength (nm)', $
  ; layout=[1,2,1] )
  ; p2 = plot( pos_cen_data_pix, pos_cen_ref_wave, symbol='o', /over, color='red', name='reference' )
  ; leg = legend(target=[p1,p2],font_size=14)
  ; ;
  ; p3 = plot( pos_cen_data_pix, pos_cen_data_wave-pos_cen_ref_wave, symbol='o', $
  ; current=win, layout=[1,2,2], xtitle='pixel', ytitle='wavelength (nm)', $
  ; font_size=16, title='data - reference' )
  ; markerp,p3,y=0,linestyle=2

  ; ;  p1 = plot( pos_cen_data_wave, sig_data/max(sig_data) )
  ; ;  p2 = plot( pos_cen_ref_wave, sig_ref/max(sig_ref), /over, color='red' )

  ; ;p1 = plot( pos_cen_data_wave, sens/max(sens), symbol='o', /sym_filled )

  ; win = window(dim=[1200,600])
  ; p1 = plot( wl_spec, spec/max(spec[ndx_spec_in]), xr=[110,190], current=win, $
  ; thick=2, yr=[0,1.1], xtitle='wavelength (nm)', font_size=16, name='data' )
  ; p2 = plot( wl_ref, ref/max(ref[ndx_ref_in]), /over, color='red', $
  ; name='reference', thick=2 )
  ; for i = 0, n_peaks - 1 do $
  ; pi = plot_shade( p1, wl1[i], wl2[i], fill_transparency=((i mod 2)*20 + 70), fill_color='blue' )
  ; p3 = plot( pos_cen_data_wave, sens/max(sens), symbol='o', /sym_filled, $
  ; /over, name='sensitivity', linestyle=2 )
  ; leg = legend(target=[p1,p2,p3])

  stop
end
