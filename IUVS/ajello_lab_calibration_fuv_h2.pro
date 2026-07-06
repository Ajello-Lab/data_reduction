;+
; PURPOSE
;  This routine will
;
; INPUTS
; wl_spec: Corrected IUVS wavelength, nm
; spec: IUVS measured spectrum
; OUTPUTS
; wl_sens: wavelength, nm
; sens: sensitivity in arbitrary units
; wl1: starting wavelength of each bandpass used to derive sensitivity
; wl2: ending wavelength of each bandpass used to derive sensitivity
;
; KEYWORDS
; show_plots: set to 1 to show plots at intermediate processing steps
; file_n2_model: If not using the GitHub repo, this must specify
;   the full path to the N2 LBH model file.
;
pro ajello_lab_calibration_fuv_h2, wl_spec, spec, wl_sens, sens, $
  wl1, wl2, sig_ref, $
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
        file_data = '/Users/benjamincondit/Desktop/IUVS_Breadboard/Round 7/data_reduction/H2_100EV_FUV_TEST11_H2_100EV_ROT_+7_IMAGE1_LOW_PRESS_4E-6.idl'
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

    arr = float(arr)
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

  spec_ref -= min(spec_ref)
  wave_ref /= 10
  ; restore,file,/ver
  ndx_spec_in = where(wl_spec gt 125 and wl_spec lt 130)
  ndx_ref_in = where(wave_ref gt 125 and wave_ref lt 130)

  wl1 = [115.4, 116.9, 118.4, 119.6, 122.5, 123.5, 124.3, 126.5, 132.9, 133.9, 134.9, 136.1, 136.7, 138.9, 142.1, 144.9, 147.7, 149.1, 150.8, 152.7, 154.1, 155.6, 159.9, 161.9, 163.1]
  wl2 = [116.9, 118.4, 119.5, 121.0, 123.5, 124.3, 126.5, 129.3, 133.9, 134.9, 136.1, 136.7, 138.4, 140.8, 144.9, 147.0, 149.1, 150.1, 152.1, 153.6, 155.6, 159.9, 161.9, 163.1, 165.0]

  if keyword_set(show_plots) then begin
    win = window(dim = [1200, 600])
    xr = [110, 170]
    p1 = plot(wave_ref / 10., spec_ref - min(spec_ref), yr = [0, 5.2e4], name = 'reference', xtitle = 'wavelength (nm)', current = win, xr = xr, font_size = 14)
    p2 = plot(wl_spec, spec, font_size = 14, /over, color = 'red', name = 'data')
    ; p3 = plot(wl_spec, spec_deriv * 5000, /over, color = 'pink', name = 'slope')
    leg = legend(target = [p1, p2])
  endif

  ;
  ; interpolate reference to data wavelength scale
  ; ratio of data to reference should be the sensitivity, but it is noisy
  ;
  ; spec_refi = interpol(spec_ref - min(spec_ref), wave_ref / 10., wl_spec)
  ; p = plot(wl_spec, spec / spec_refi)

  n_peaks = n_elements(wl1)

  if keyword_set(show_plots) then begin
    win = window(dim = [1200, 600])
    p1 = plot(wl_spec, spec, xr = [110, 190], current = win)
    p2 = plot(wave_ref, spec_ref / max(spec_ref[ndx_ref_in]) * max(spec), /over, color = 'red')
    for i = 0, n_peaks - 1 do $
      pi = plot_shade(p1, wl1[i], wl2[i], fill_transparency = ((i mod 2) * 20 + 70), fill_color = 'blue', yr = [0, 3.5e4])
  endif

  pos_cen_data_pix = fltarr(n_peaks) * !values.f_nan
  pos_cen_data_wave = fltarr(n_peaks) * !values.f_nan
  pos_cen_ref_wave = fltarr(n_peaks) * !values.f_nan
  sig_data = fltarr(n_peaks) * !values.f_nan
  sig_ref = fltarr(n_peaks) * !values.f_nan
  pix_vec = findgen(n_elements(wl_spec))
  for i = 0, n_peaks - 1 do begin
    ndx_speci = where(wl_spec gt wl1[i] and wl_spec lt wl2[i], count_data)
    ndx_refi = where(wave_ref gt wl1[i] and wave_ref lt wl2[i], count_ref)
    if count_data gt 0 then begin
      pos_cen_data_pix[i] = ajello_centroid(pix_vec[ndx_speci], spec[ndx_speci])
      pos_cen_data_wave[i] = ajello_centroid(wl_spec[ndx_speci], spec[ndx_speci])
      pos_cen_ref_wave[i] = ajello_centroid(wave_ref[ndx_refi], spec_ref[ndx_refi])

      sig_data[i] = total(spec[ndx_speci])
      sig_ref[i] = total(spec_ref[ndx_refi])
    endif
  endfor

  wl_sens = pos_cen_ref_wave
  sens = sig_data / sig_ref

  if keyword_set(show_plots) then begin
    win = window(dim = [800, 600])
    p1 = plot(pos_cen_data_pix, pos_cen_data_wave, symbol = 'x', current = win, $
      font_size = 16, name = 'data', xtitle = 'pixel', ytitle = 'wavelength (nm)', $
      layout = [1, 2, 1])
    p2 = plot(pos_cen_data_pix, pos_cen_ref_wave, font_size = 14, symbol = 'o', /over, color = 'red', name = 'reference')
    leg = legend(target = [p1, p2])
    ;
    p3 = plot(pos_cen_data_pix, pos_cen_data_wave - pos_cen_ref_wave, symbol = 'o', $
      current = win, layout = [1, 2, 2], xtitle = 'pixel', ytitle = 'wavelength (nm)', $
      font_size = 16, title = 'data - reference')
    markerp, p3, y = 0, linestyle = 2

    ; p1 = plot( pos_cen_data_wave, sig_data/max(sig_data) )
    ; p2 = plot( pos_cen_ref_wave, sig_ref/max(sig_ref), /over, color='red' )

    ; p1 = plot( pos_cen_data_wave, sens/max(sens), symbol='o', /sym_filled )

    win = window(dim = [1200, 600])
    p1 = plot(wl_spec, spec / max(spec[ndx_spec_in]), xr = [110, 190], current = win, $
      thick = 2, yr = [0, 1.1], xtitle = 'wavelength (nm)', font_size = 16, name = 'data')
    p2 = plot(wave_ref, spec_ref / max(spec_ref[ndx_ref_in]), /over, color = 'red', $
      name = 'reference', thick = 2)
    for i = 0, n_peaks - 1 do $
      pi = plot_shade(p1, wl1[i], wl2[i], fill_transparency = ((i mod 2) * 20 + 70), fill_color = 'blue')
    p3 = plot(pos_cen_data_wave, sens / max(sens), symbol = 'o', /sym_filled, $
      /over, name = 'sensitivity', linestyle = 2)
    leg = legend(target = [p1, p2, p3])
    stop
  endif
end
