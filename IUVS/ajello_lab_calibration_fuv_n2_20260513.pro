;+
; PURPOSE
;  This routine will
;-
pro ajello_lab_calibration_fuv_n2_20260513
  compile_opt idl2

  ajello_lab_set_paths, path_base, path_repo

  file_n2_model = path_repo + '/data/N2/n2_lbh_rot_293K.sav'

  case (get_login_info()).user_name of
    'holsclaw': begin
      file_data_helper = '/Volumes/projects/Phase_Development/MAVEN/IUVS_Data/IUVS_Breadboard/Round_12/data_reduction/'
      path_save = '/users/holsclaw/Documents/'
    end
    'benjamincondit': begin
      file_data_helper = '/Users/benjamincondit/Desktop/IUVS_Breadboard/Round 12/data_reduction/'
      path_save = '/Users/benjamincondit/idl/data_reduction/IUVS/Sensitivities'
    end
  endcase

  file_data = file_data_helper + 'N2_30EV_FUV_TEST15_IMAGE1' + '.idl'

  ; restore,file_data,/ver
  sObj = obj_new('IDL_Savefile', file_data)
  sObj.restore, 'arr'
  obj_destroy, sObj

  temp = strsplit(file_data, '/', /extract)
  id1 = temp[-3] + '_' + file_basename(temp[-1], '.idl')

  spat = total(arr, 1)
  y0 = where(spat eq max(spat))
  yw = 100
  y1 = y0 - yw
  y2 = y0 + yw
  spec = total(arr[*, y1 : y2], 2)

  ;
  ; retrieve wavelength scale for the rotated image
  ;
  ajello_lab_pixel_scale_rot, wlfuv, wlmuv, yfuv, ymuv

  ; pixel 159 is at 1200 angstroms
  wl_spec = wlfuv - 125.6 + 120.0

  win = window(dim = [800, 600])
  p1 = plot(wlfuv, spec, name = 'orignal data', current = win)
  p2 = plot(wl_spec, spec, /over, color = 'red', name = 'shifted')
  leg = legend(target = [p1, p2])
  markerp, p1, x = 120., linestyle = 2

  ;
  ; read in N2 LBH reference spectrum that has been convolved
  ; with the IUVS LSF
  ;
  ajello_lab_n2_reference, wl_ref, ref, file_n2_model = file_n2_model

  ;
  ; derive the sensitivity
  ;
  ajello_lab_calibration_fuv_n2, wl_spec, spec, wl_sens, sens, wl1, wl2 ; , $
  ; show_plots=show_plots

  ;
  ; identify the wavelength region that will be used for normalization
  ; for the data, reference, and sensitivity
  ;
  wlnc = 135.4
  wln1 = 134.6
  wln2 = 136.2
  ndx_spec = where(wl_spec gt wln1 and wl_spec lt wln2, count_spec)
  ndx_ref = where(wl_ref gt wln1 and wl_ref lt wln2, count_ref)
  ndx_sens = findndx(wl_sens, wlnc)
  sens_norm = sens / sens[ndx_sens]

  ;
  ; write out sensitivity to text file
  ;
  ; file_out = path_save + id1 + '_sensitivity.txt'
  ; num_sens = n_elements(wl_sens)
  ; openw,fid,file_out,/get_lun
  ; for i = 0, num_sens - 1 do $
  ; printf, fid, wl_sens[i], sens_norm[i], format='(F10.2,",",F10.6)'
  ; close,fid
  ; free_lun,fid
  ;
  ; show the normalized data, reference, and derived sensitivity
  ;
  win = window(dim = [1400, 600])
  xr = [110, 190]
  p1 = plot(wl_spec, spec / max(spec[ndx_spec]), yr = [0, 1.2], current = win, $
    thick = 2, font_size = 16, xr = xr, xtitle = 'wavelength (nm)', title = id1, name = 'data')
  p2 = plot(wl_ref, ref / max(ref[ndx_ref]), /over, color = 'red', thick = 2, name = 'reference')
  p3 = plot(wl_sens, sens / sens[ndx_sens] / 2., /over, symbol = 'o', /sym_filled, linestyle = 2, name = 'sensitivity')
  n_peaks = n_elements(wl1)
  for i = 0, n_peaks - 1 do $
    pi = plot_shade(p1, wl1[i], wl2[i], fill_transparency = ((i mod 2) * 20 + 70), fill_color = 'blue')
  leg = legend(target = [p1, p2, p3], position = [0.9, 0.9], /relative, font_size = 14)
  ; win.save,path_save + id1 + '.png'

  stop

  case (get_login_info()).user_name of
    'holsclaw': begin
      file_data_helper2 = '/Volumes/projects/Phase_Development/MAVEN/IUVS_Data/IUVS_Breadboard/NeweGun_round10_after_energy_correction/data_reduction/'
      path_save = '/users/holsclaw/Documents/'
    end
    'benjamincondit': begin
      file_data_helper2 = '/Users/benjamincondit/Desktop/IUVS_Breadboard/Round 10/data_reduction/'
      path_save = '/Users/benjamincondit/idl/data_reduction/data/N2/Sensitivities'
    end
  endcase
  ; array for all files to be read will go on next line
  file_data = file_data_helper2 + 'N2_16EV_FUV_TEST17_IMAGE1_HIPRESS' + '.idl'
  sObj = obj_new('IDL_Savefile', file_data)
  sObj.restore, 'arr'
  obj_destroy, sObj
  spat = total(arr, 1)
  y0 = where(spat eq max(spat))
  yw = 100
  y1 = y0 - yw
  y2 = y0 + yw
  spec2 = total(arr[*, y1 : y2], 2)

  temp = strsplit(file_data, '/', /extract)
  id2 = temp[-3] + ' - ' + temp[-1]

  ;
  ; empirically-derived wavelength shift of data to match the model
  ;
  wl_data_shift = 6.3
  wl_spec2 = wlfuv - wl_data_shift

  p1 = plot(wl_spec2, spec2 / max(spec2))
  p2 = plot(wl_ref, ref / max(ref), color = 'red', /over)

  ajello_lab_calibration_fuv_n2, wl_spec2, spec2, wl_sens2, sens2 ; , $

  ndx_spec2 = where(wl_spec2 gt wln1 and wl_spec2 lt wln2, count_spec2)
  ; ndx_ref = where( wl_ref gt wln1 and wl_ref lt wln2, count_ref )
  ndx_sens2 = findndx(wl_sens2, wlnc)
  sens2_norm = sens2 / sens2[ndx_sens2]

  ;
  ; write out sensitivity to text file
  ;
  ; file_out = path_save + id2 + ' - sensitivity.txt'
  ; num_sens = n_elements(wl_sens)
  ; openw,fid,file_out,/get_lun
  ; for i = 0, num_sens - 1 do $
  ; printf, fid, wl_sens2[i], sens2_norm[i], format='(F10.2,",",F10.6)'
  ; close,fid
  ; free_lun,fid

  win = window(dim = [1400, 600])
  xr = [110, 190]
  p1 = plot(wl_spec2, spec2 / max(spec2[ndx_spec2]), yr = [0, 1.2], current = win, $
    thick = 2, font_size = 16, xr = xr, xtitle = 'wavelength (nm)', title = id2, name = 'data')
  p2 = plot(wl_ref, ref / max(ref[ndx_ref]), /over, color = 'red', thick = 2, name = 'reference')
  p3 = plot(wl_sens2, sens2 / sens2[ndx_sens2] / 2., /over, symbol = 'o', /sym_filled, linestyle = 2, name = 'sensitivity')
  n_peaks = n_elements(wl1)
  for i = 0, n_peaks - 1 do $
    pi = plot_shade(p1, wl1[i], wl2[i], fill_transparency = ((i mod 2) * 20 + 70), fill_color = 'blue')
  leg = legend(target = [p1, p2, p3], position = [0.9, 0.9], /relative, font_size = 14)
  ; win.save,path_save + id2 + '.png'

  stop

  win = window(dim = [1200, 600])
  p1 = plot(wl_spec, spec / max(spec[ndx_spec]), xr = xr, thick = 2, name = id1, current = win, yr = [0, 1.1], xtitle = 'wavelength (nm)', font_size = 16)
  p2 = plot(wl_spec2, spec2 / max(spec2[ndx_spec2]), /over, color = 'red', thick = 2, name = id2)
  leg = legend(target = [p1, p2], font_size = 12)
  ; win.save,path_save+'spectrum_comparison.png'

  win = window(dim = [1200, 600])
  ndx1 = findndx(wl_sens, 135.4)
  ndx2 = findndx(wl_sens2, 135.4)
  p1 = plot(wl_sens, sens / sens[ndx1], name = id1, symbol = 'o', /sym_filled, current = win, thick = 2, font_size = 16, xtitle = 'wavelength (nm)')
  p2 = plot(wl_sens2, sens2 / sens2[ndx2], name = id2, /over, color = 'red', symbol = 'o', /sym_filled, thick = 2)
  leg = legend(target = [p1, p2], font_size = 12)
  ; win.save,path_save+'sensitivity_comparison.png'

  stop
end
