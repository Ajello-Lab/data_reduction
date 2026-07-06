;+
; PURPOSE
;  This routine will derive the IUVS FUV sensitivity at the location of
;   multiple prominent spectral features from N2 LBH emission
;
; INPUTS
;  wl_spec: Corrected IUVS wavelength, nm
;  spec: IUVS FUV measured spectrum
;
; OUTPUTS
;  wl_sens: wavelength, nm
;  sens: sensitivity in arbitrary units
;  wl1: starting wavelength of each bandpass used to derive sensitivity
;  wl2: ending wavelength of each bandpass used to derive sensitivity
;
; KEYWORDS
;  show_plots: set to 1 to show plots at intermediate processing steps
;  file_n2_model: If not using the GitHub repo, this must specify
;   the full path to the N2 LBH model file.
;-
pro ajello_lab_calibration_fuv_n2, wl_spec, spec, wl_sens, sens, $
  wl1, wl2, sig_ref, $
  show_plots = show_plots, $
  file_n2_model = file_n2_model
  compile_opt idl2

  ; ajello_lab_set_paths, path_base, path_repo

  ; if n_params() eq 0 then begin
  ;
  ; file_data = '/Volumes/projects/Phase_Development/MAVEN/IUVS_Data/IUVS_Breadboard/Round_12/data_reduction/N2_30EV_FUV_TEST15_IMAGE1.idl'
  ; ;restore,file_data,/ver
  ;
  ; sObj = OBJ_NEW('IDL_Savefile', file_data)
  ; sObj->Restore, 'arr'
  ; OBJ_DESTROY, sObj
  ;
  ; spat = total( arr, 1 )
  ; y0 = where( spat eq max(spat) )
  ; yw = 100
  ; y1 = y0-yw
  ; y2 = y0+yw
  ; spec = total( arr[*,y1:y2], 2 )
  ;
  ; ;
  ; ; retrieve wavelength scale for the rotated image
  ; ;
  ; ajello_lab_pixel_scale_rot, wlfuv, wlmuv, yfuv, ymuv
  ;
  ; ; pixel 159 is at 1200 angstroms
  ; wl_spec = wlfuv - 125.6 + 120.0
  ;
  ; win = window(dim=[800,600])
  ; p1 = plot( wlfuv, spec, name='orignal data', current=win )
  ; p2 = plot( wl_spec, spec, /over, color='red', name='shifted' )
  ; leg = legend(target=[p1,p2])
  ; markerp,p1,x=120.,linestyle=2
  ;
  ; show_plots = 1
  ;
  ; endif

  ajello_lab_n2_reference, wl_ref, ref, file_n2_model = file_n2_model

  ndx_spec_in = where(wl_spec gt 125 and wl_spec lt 135)
  ndx_ref_in = where(wl_ref gt 125 and wl_ref lt 135)

  ; p1 = plot(wl_spec, spec)
  ; p2 = plot(wl_ref, ref / max(ref[ndx_ref_in]) * max(spec[ndx_spec_in]), /over, color = 'red')

  ; area method
  ; Lucy's code
  ; wl1 =  [ 130.7, 132.0, 133.5, 134.8, 136.4, 137.6, 139.3, 140.7, 142.32, 144.0, 145.8, 147.1, 148.5, 149.8, 150.55, 151.3, 152.6, 155.1, 157.0, 158.15, 159.6, 160.72]
  ; wl2 =  [ 131.9, 133.4, 134.7, 136.3, 137.5, 139.2, 140.6, 142.3, 143.90, 145.7, 147.0, 148.4, 149.7, 150.5, 151.25, 152.1, 153.7, 156.7, 158.1, 159.50, 160.7, 162.20]

  ; EMUS code
  ; wl1 = [ 92.0, 93.5, 95.3, 97.4,  99.8, 102.1, 104.1, 105.2, 110.5, 121.0, 125.2, 126.7, 129.3, 131.9, 133.4, 134.7, 136.3, 137.6, 139.2, 140.5, 142.3, 143.9, 145.9, 150.5, 152.5, 154.8, 157.0, 158.1, 159.5, 160.7, 162.2, 163.6, 165.3, 166.7, 168.2, 169.8, 174.7, 176.2, 177.9 ]
  ; wl2 = [ 93.5, 95.3, 96.5, 99.8, 102.1, 104.1, 105.2, 106.3, 111.5, 122.2, 126.7, 128.3, 130.6, 133.4, 134.7, 136.3, 137.6, 139.2, 140.5, 142.3, 143.9, 145.9, 147.1, 151.3, 153.8, 157.0, 158.1, 159.0, 160.7, 162.2, 163.6, 165.3, 166.7, 168.2, 169.8, 171.1, 176.2, 177.9, 179.4 ]

  wl1 = [126.7, 129.3, 131.9, 133.4, 134.7, 137.6, 139.2, 140.5, 142.3, 143.9, 145.9, 150.5, 152.5, 154.8, 157.0, 158.1, 159.5, 160.7, 162.2, 163.6, 165.3, 166.7, 168.2, 169.8, 174.7, 176.2, 177.9, 183.0]
  wl2 = [128.3, 130.6, 133.4, 134.7, 136.3, 139.2, 140.5, 142.3, 143.9, 145.9, 147.1, 151.3, 153.8, 157.0, 158.1, 159.0, 160.7, 162.2, 163.6, 165.3, 166.7, 168.2, 169.8, 171.1, 176.2, 177.9, 179.4, 184.8]

  n_peaks = n_elements(wl1)

  if keyword_set(show_plots) then begin
    win = window(dim = [1200, 600])
    p1 = plot(wl_spec, spec, xr = [110, 190], current = win)
    p2 = plot(wl_ref, ref / max(ref[ndx_ref_in]) * max(spec), /over, color = 'red')
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
    ndx_refi = where(wl_ref gt wl1[i] and wl_ref lt wl2[i], count_ref)
    if count_data gt 0 then begin
      pos_cen_data_pix[i] = ajello_centroid(pix_vec[ndx_speci], spec[ndx_speci])
      pos_cen_data_wave[i] = ajello_centroid(wl_spec[ndx_speci], spec[ndx_speci])
      pos_cen_ref_wave[i] = ajello_centroid(wl_ref[ndx_refi], ref[ndx_refi])

      sig_data[i] = total(spec[ndx_speci])
      sig_ref[i] = total(ref[ndx_refi])
    endif
  endfor

  wl_sens = pos_cen_ref_wave
  sens = sig_data / sig_ref

  if keyword_set(show_plots) then begin
    win = window(dim = [800, 600])
    p1 = plot(pos_cen_data_pix, pos_cen_data_wave, symbol = 'x', current = win, $
      font_size = 16, name = 'data', xtitle = 'pixel', ytitle = 'wavelength (nm)', $
      layout = [1, 2, 1])
    p2 = plot(pos_cen_data_pix, pos_cen_ref_wave, symbol = 'o', /over, color = 'red', name = 'reference')
    leg = legend(target = [p1, p2], font_size = 14)
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
    p2 = plot(wl_ref, ref / max(ref[ndx_ref_in]), /over, color = 'red', $
      name = 'reference', thick = 2)
    for i = 0, n_peaks - 1 do $
      pi = plot_shade(p1, wl1[i], wl2[i], fill_transparency = ((i mod 2) * 20 + 70), fill_color = 'blue')
    p3 = plot(pos_cen_data_wave, sens / max(sens), symbol = 'o', /sym_filled, $
      /over, name = 'sensitivity', linestyle = 2)
    leg = legend(target = [p1, p2, p3])

    stop
  endif
end
