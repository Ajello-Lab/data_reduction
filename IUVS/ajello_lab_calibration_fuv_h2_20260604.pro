;
; PURPOSE
; This routine takes experiemental and reference data and
; outputs graphs of the data and/or a text file containing
; the sensitivities calculated for each trial.
;
pro ajello_lab_calibration_fuv_h2_20260604
  compile_opt idl2

  ajello_lab_set_paths, path_base, path_repo

  file_ref = path_repo + '/data/H2/h2euv100.txt'

  case (get_login_info()).user_name of
    'holsclaw': begin
      file_data_helper = '/Volumes/projects/Phase_Development/MAVEN/IUVS_Data/IUVS_Breadboard/Round_7/data_reduction/'
      path_save = '/users/holsclaw/Documents/'
    end
    'benjamincondit': begin
      file_data_helper = '/Users/benjamincondit/Desktop/IUVS_Breadboard/Round 12/data_reduction/'
      path_save = '/Users/benjamincondit/idl/data_reduction/IUVS/Sensitivities_H2/'
    end
  endcase

  ; ROUND 12
  files_to_read = strarr(1)
  files_to_read[0] = 'H2_100EV_FUV_TEST25_IMAGE1'

  ; ROUND 7
  ; files_to_read = strarr(18)
  ; files_to_read[0] = 'H2_30EV_FUV_TEST13_H2_30EV_ROT_+7_IMAGE1_MED_PRESS_3E-5'
  ; files_to_read[1] = 'H2_30EV_FUV_TEST14_H2_30EV_ROT_+7_IMAGE1_MED_PRESS_1E-5'
  ; files_to_read[2] = 'H2_30EV_FUV_TEST16_H2_30EV_ROT_+7_IMAGE1_MED_PRESS_1E-5'
  ; files_to_read[3] = 'H2_30EV_FUV_TEST17_H2_30EV_ROT_+7_IMAGE3_MED_PRESS_1E-5'
  ; files_to_read[4] = 'H2_30EV_FUV_TEST18_H2_30EV_ROT_+7_IMAGE2_MED_PRESS_1E-5'
  ; files_to_read[5] = 'H2_30EV_MUV_TEST13_H2_30EV_ROT_+7_IMAGE1_MED_PRESS_3E-5'
  ; files_to_read[6] = 'H2_30EV_MUV_TEST14_H2_30EV_ROT_+7_IMAGE1_MED_PRESS_1E-5'
  ; files_to_read[7] = 'H2_30EV_MUV_TEST16_H2_30EV_ROT_+7_IMAGE1_MED_PRESS_1E-5'
  ; files_to_read[8] = 'H2_30EV_MUV_TEST17_H2_30EV_ROT_+7_IMAGE3_MED_PRESS_1E-5'
  ; files_to_read[9] = 'H2_30EV_MUV_TEST18_H2_30EV_ROT_+7_IMAGE2_MED_PRESS_1E-5'
  ; files_to_read[10] = 'H2_100EV_FUV_TEST1_H2_100EV_ROT_+7_IMAGE1_MED_PRESS_2E-5'
  ; files_to_read[11] = 'H2_100EV_FUV_TEST2_H2_100EV_ROT_+7_IMAGE1_LOW_PRESS_4E-6'
  ; files_to_read[12] = 'H2_100EV_FUV_TEST11_H2_100EV_ROT_+7_IMAGE1_LOW_PRESS_4E-6'
  ; files_to_read[13] = 'H2_100EV_FUV_TEST12_H2_100EV_ROT_+7_IMAGE1_MED_PRESS_2E-5'
  ; files_to_read[14] = 'H2_100EV_MUV_TEST1_H2_100EV_ROT_+7_IMAGE1_MED_PRESS_2E-5'
  ; files_to_read[15] = 'H2_100EV_MUV_TEST2_H2_100EV_ROT_+7_IMAGE1_LOW_PRESS_4E-6'
  ; files_to_read[16] = 'H2_100EV_MUV_TEST11_H2_100EV_ROT_+7_IMAGE1_LOW_PRESS_4E-6'
  ; files_to_read[17] = 'H2_100EV_MUV_TEST12_H2_100EV_ROT_+7_IMAGE1_MED_PRESS_2E-5'

  for j = 0, n_elements(files_to_read) - 1 do begin
    print, 'File:' + string(j)
    file_data = file_data_helper + files_to_read[j] + '.idl'

    ; restore,file_data,/ver
    sObj = obj_new('IDL_Savefile', file_data)
    sObj.restore, 'arr'
    obj_destroy, sObj

    arr = float(arr)

    temp = strsplit(file_data, '/', /extract)
    id1 = temp[-3] + '_' + file_basename(temp[-1], '.idl')

    spat = total(arr, 1)
    y0 = where(spat eq max(spat))
    dimensions = size(arr, /dimensions)
    yw = 100
    y1 = y0 - yw
    y2 = y0 + yw
    y1 = y1 > 0
    y2 = y2 < (dimensions[1] - 1)
    spec = total(arr[*, y1 : y2], 2)

    ; Noise Reduction Attempts
    ;
    spec = median(spec, 7)
    spec = smooth(spec, 7)
    under_curve = smooth(spec, 50)
    spec -= min(under_curve)

    ; retrieve wavelength scale for the rotated image
    ajello_lab_pixel_scale_rot, wlfuv, wlmuv, yfuv, ymuv

    wl_spec = wlfuv - wlfuv[findndx(spec, max(spec))] + 121.6

    ; Plot of shifted data
    ;
    ; win = window(dim = [800, 600])
    ; p1 = plot(wlfuv, spec, name = 'orignal data', current = win)
    ; p2 = plot(wl_spec, spec, /over, color = 'red', name = 'shifted')
    ; leg = legend(target = [p1, p2])
    ; markerp, p1, x = 120., linestyle = 2

    ; read in H2 LBH reference spectrum that has been convolved with the IUVS LSF
    ;
    ajello_lab_h2_reference, wl_ref, ref, file_ref = file_ref
    wl_ref /= 10
    ref /= 10
    ref -= min(ref)

    ; derive the sensitivity
    ;
    ajello_lab_calibration_fuv_h2, wl_spec, spec, wl_sens, sens, wl1, wl2 ; , $
    ; show_plots=show_plots

    ; identify the wavelength region that will be used for normalization
    ; for the the data, reference, and sensitivity

    wlnc = 125.4
    wln1 = 124.3
    wln2 = 126.5
    ndx_spec = where(wl_spec gt wln1 and wl_spec lt wln2, count_spec)
    ndx_ref = where(wl_ref gt wln1 and wl_ref lt wln2, count_ref)
    ndx_sens = findndx(wl_sens, wlnc)
    sens_norm = sens / sens[ndx_sens]

    ; write out sensitivity to text file
    ;
    file_out = path_save + id1 + '.txt'
    num_sens = n_elements(wl_sens)
    openw, fid, file_out, /get_lun
    for i = 0, num_sens - 1 do $
      printf, fid, wl_sens[i], sens_norm[i], format = '(F10.2,",",F10.6)'
    close, fid
    free_lun, fid

    win = window(dim = [1400, 600])
    xr = [110, 190]
    p1 = plot(wl_spec, spec / max(spec[ndx_spec]), yr = [0, 1.4], current = win, $
      thick = 2, font_size = 16, xr = xr, xtitle = 'wavelength (nm)', title = id1, name = 'data')
    p2 = plot(wl_ref, ref / max(ref[ndx_ref]), /over, color = 'red', thick = 2, name = 'reference')
    p3 = plot(wl_sens, sens / sens[ndx_sens] / 2., /over, symbol = 'o', /sym_filled, linestyle = 2, name = 'sensitivity')
    n_peaks = n_elements(wl1)
    for i = 0, n_peaks - 1 do $
      pi = plot_shade(p1, wl1[i], wl2[i], fill_transparency = ((i mod 2) * 20 + 70), fill_color = 'blue')
    leg = legend(target = [p1, p2, p3], position = [0.9, 0.9], /relative, font_size = 14)
    win.save, path_save + id1 + '.png'
  endfor
  stop
end
