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
      file_data_helper = '/Users/benjamincondit/Desktop/IUVS_Breadboard/Round 10/data_reduction/'
      path_save = '/Users/benjamincondit/idl/data_reduction/IUVS/Sensitivities/'
    end
  endcase

  ; Files to be analyzed
  files_to_read = strarr(99)
  files_to_read[0] = 'N2_20EV_FUV_TEST25_20EV_ROT_+7_IMAGE1_HI_PRESS_4E-5'
  files_to_read[1] = 'N2_20EV_FUV_TEST26_20EV_ROT_+7_IMAGE1_HI_PRESS_4E-5'
  files_to_read[2] = 'N2_20EV_FUV_TEST60_20EV_ROT_+7_IMAGE1_LOW_PRESS_5E-6'
  files_to_read[3] = 'N2_20EV_FUV_TEST61_20EV_ROT_+7_IMAGE3_LOW_PRESS_5E-6'
  files_to_read[4] = 'N2_20EV_MUV_TEST25_20EV_ROT_+7_IMAGE1_HI_PRESS_4E-5'
  files_to_read[5] = 'N2_20EV_MUV_TEST26_20EV_ROT_+7_IMAGE1_HI_PRESS_4E-5'
  files_to_read[6] = 'N2_20EV_MUV_TEST60_20EV_ROT_+7_IMAGE1_LOW_PRESS_5E-6'
  files_to_read[7] = 'N2_20EV_MUV_TEST61_20EV_ROT_+7_IMAGE3_LOW_PRESS_5E-6'
  files_to_read[8] = 'N2_30EV_FUV_TEST31_30EV_ROT_+7_IMAGE1_HI_PRESS_4E-5'
  files_to_read[9] = 'N2_30EV_FUV_TEST32_30EV_ROT_+7_IMAGE3_HI_PRESS_4E-5'
  files_to_read[10] = 'N2_30EV_FUV_TEST33_30EV_ROT_+7_IMAGE2_HI_PRESS_4E-5'
  files_to_read[11] = 'N2_30EV_FUV_TEST55_30EV_ROT_+7_IMAGE1_LOW_PRESS_5E-6'
  files_to_read[12] = 'N2_30EV_FUV_TEST56_30EV_ROT_+7_IMAGE1_LOW_PRESS_5E-6'
  files_to_read[13] = 'N2_30EV_FUV_TEST57_30EV_ROT_+7_IMAGE3_LOW_PRESS_5E-6'
  files_to_read[14] = 'N2_30EV_FUV_TEST58_30EV_ROT_+7_IMAGE2_LOW_PRESS_5E-6'
  files_to_read[15] = 'N2_30EV_FUV_TEST59_30EV_ROT_+8_IMAGE1_LOW_PRESS_5E-6_CAPTURE'
  files_to_read[16] = 'N2_30EV_MUV_TEST31_30EV_ROT_+7_IMAGE1_HI_PRESS_4E-5'
  files_to_read[17] = 'N2_30EV_MUV_TEST32_30EV_ROT_+7_IMAGE3_HI_PRESS_4E-5'
  files_to_read[18] = 'N2_30EV_MUV_TEST33_30EV_ROT_+7_IMAGE2_HI_PRESS_4E-5'
  files_to_read[19] = 'N2_30EV_MUV_TEST55_30EV_ROT_+7_IMAGE1_LOW_PRESS_5E-6'
  files_to_read[20] = 'N2_30EV_MUV_TEST56_30EV_ROT_+7_IMAGE1_LOW_PRESS_5E-6'
  files_to_read[21] = 'N2_30EV_MUV_TEST57_30EV_ROT_+7_IMAGE3_LOW_PRESS_5E-6'
  files_to_read[22] = 'N2_30EV_MUV_TEST58_30EV_ROT_+7_IMAGE2_LOW_PRESS_5E-6'
  files_to_read[23] = 'N2_30EV_MUV_TEST59_30EV_ROT_+8_IMAGE1_LOW_PRESS_5E-6_CAPTURE'
  files_to_read[24] = 'N2_40EV_FUV_TEST19_40EV_ROT_+7_IMAGE1_HI_PRESS_4E-5'
  files_to_read[25] = 'N2_40EV_FUV_TEST20_40EV_ROT_+7_IMAGE2_HI_PRESS_4E-5'
  files_to_read[26] = 'N2_40EV_FUV_TEST21_40EV_ROT_+7_IMAGE3_HI_PRESS_4E-5'
  files_to_read[27] = 'N2_40EV_MUV_TEST19_40EV_ROT_+7_IMAGE1_HI_PRESS_4E-5'
  files_to_read[28] = 'N2_40EV_MUV_TEST20_40EV_ROT_+7_IMAGE2_HI_PRESS_4E-5'
  files_to_read[29] = 'N2_40EV_MUV_TEST21_40EV_ROT_+7_IMAGE3_HI_PRESS_4E-5'
  files_to_read[30] = 'N2_50EV_FUV_TEST34_50EV_ROT_+7_IMAGE1_HI_PRESS_4E-5'
  files_to_read[31] = 'N2_50EV_FUV_TEST35_50EV_ROT_+7_IMAGE3_HI_PRESS_4E-5'
  files_to_read[32] = 'N2_50EV_FUV_TEST36_50EV_ROT_+7_IMAGE2_HI_PRESS_4E-5'
  files_to_read[33] = 'N2_50EV_MUV_TEST34_50EV_ROT_+7_IMAGE1_HI_PRESS_4E-5'
  files_to_read[34] = 'N2_50EV_MUV_TEST35_50EV_ROT_+7_IMAGE3_HI_PRESS_4E-5'
  files_to_read[35] = 'N2_50EV_MUV_TEST36_50EV_ROT_+7_IMAGE2_HI_PRESS_4E-5'
  files_to_read[36] = 'N2_100EV_FUV_TEST1_IMAGE1'
  files_to_read[37] = 'N2_100EV_FUV_TEST2_IMAGE1'
  files_to_read[38] = 'N2_100EV_FUV_TEST3_IMAGE1'
  files_to_read[39] = 'N2_100EV_FUV_TEST4_IMAGE1_ROT-4'
  files_to_read[40] = 'N2_100EV_FUV_TEST5_IMAGE1_ROT4'
  files_to_read[41] = 'N2_100EV_FUV_TEST6_IMAGE1_ROT8'
  files_to_read[42] = 'N2_100EV_FUV_TEST7_IMAGE1_ROT-8'
  files_to_read[43] = 'N2_100EV_FUV_TEST8_IMAGE1_ROT-6'
  files_to_read[44] = 'N2_100EV_FUV_TEST9_IMAGE1_ROT-4'
  files_to_read[45] = 'N2_100EV_FUV_TEST10_IMAGE1_ROT-2'
  files_to_read[46] = 'N2_100EV_FUV_TEST11_IMAGE1_ROT-7'
  files_to_read[47] = 'N2_100EV_FUV_TEST12_IMAGE1_MED_PRESS'
  files_to_read[48] = 'N2_100EV_FUV_TEST13_IMAGE1_MED_PRES_HI_L2'
  files_to_read[49] = 'N2_100EV_FUV_TEST30_100EV_ROT_+7_IMAGE1_HI_PRESS_4E-5'
  files_to_read[50] = 'N2_100EV_FUV_TEST37_100EV_ROT_+7_IMAGE1_HI_PRESS_4E-5'
  files_to_read[51] = 'N2_100EV_FUV_TEST38_100EV_ROT_+7_IMAGE3_HI_PRESS_4E-5'
  files_to_read[52] = 'N2_100EV_FUV_TEST39_100EV_ROT_+7_IMAGE2_HI_PRESS_4E-5'
  files_to_read[53] = 'N2_100EV_FUV_TEST40_100EV_ROT_+7_IMAGE1_HI_PRESS_4E-5'
  files_to_read[54] = 'N2_100EV_FUV_TEST41_100EV_ROT_+7_IMAGE3_HI_PRESS_4E-5'
  files_to_read[55] = 'N2_100EV_FUV_TEST42_IMAGE2_HI_PRESS_HI_MODE_4E-5'
  files_to_read[56] = 'N2_100EV_FUV_TEST46_100EV_ROT_+7_IMAGE1_LOW_PRESS_5E-6'
  files_to_read[57] = 'N2_100EV_FUV_TEST47_100EV_ROT_+7_IMAGE2_LOW_PRESS_5E-6'
  files_to_read[58] = 'N2_100EV_FUV_TEST48_100EV_IMAGE3_5E-6'
  files_to_read[59] = 'N2_100EV_FUV_TEST48_100EV_ROT_+7_IMAGE3_LOW_PRESS_5E-6'
  files_to_read[60] = 'N2_100EV_FUV_TEST49_100EV_IMAGE2_5E-6'
  files_to_read[61] = 'N2_100EV_FUV_TEST50_100EV_IMAGE3_5E-6'
  files_to_read[62] = 'N2_100EV_FUV_TEST51_100EV_IMAGE1_5E-6'
  files_to_read[63] = 'N2_100EV_FUV_TEST52_100EV_IMAGE2_5E-6'
  files_to_read[64] = 'N2_100EV_FUV_TEST53_100EV_IMAGE3_5E-6'
  files_to_read[65] = 'N2_100EV_MUV_TEST1_IMAGE1'
  files_to_read[66] = 'N2_100EV_MUV_TEST2_IMAGE1'
  files_to_read[67] = 'N2_100EV_MUV_TEST3_IMAGE1'
  files_to_read[68] = 'N2_100EV_MUV_TEST4_IMAGE1_ROT-4'
  files_to_read[69] = 'N2_100EV_MUV_TEST5_IMAGE1_ROT4'
  files_to_read[70] = 'N2_100EV_MUV_TEST6_IMAGE1_ROT8'
  files_to_read[71] = 'N2_100EV_MUV_TEST7_IMAGE1_ROT-8'
  files_to_read[72] = 'N2_100EV_MUV_TEST8_IMAGE1_ROT-6'
  files_to_read[73] = 'N2_100EV_MUV_TEST9_IMAGE1_ROT-4'
  files_to_read[74] = 'N2_100EV_MUV_TEST10_IMAGE1_ROT-2'
  files_to_read[75] = 'N2_100EV_MUV_TEST11_IMAGE1_ROT-7'
  files_to_read[76] = 'N2_100EV_MUV_TEST12_IMAGE1_MED_PRESS'
  files_to_read[77] = 'N2_100EV_MUV_TEST13_IMAGE1_MED_PRES_HI_L2'
  files_to_read[78] = 'N2_100EV_MUV_TEST30_100EV_ROT_+7_IMAGE1_HI_PRESS_4E-5'
  files_to_read[79] = 'N2_100EV_MUV_TEST37_100EV_ROT_+7_IMAGE1_HI_PRESS_4E-5'
  files_to_read[80] = 'N2_100EV_MUV_TEST38_100EV_ROT_+7_IMAGE3_HI_PRESS_4E-5'
  files_to_read[81] = 'N2_100EV_MUV_TEST39_100EV_ROT_+7_IMAGE2_HI_PRESS_4E-5'
  files_to_read[82] = 'N2_100EV_MUV_TEST40_100EV_ROT_+7_IMAGE1_HI_PRESS_4E-5'
  files_to_read[83] = 'N2_100EV_MUV_TEST41_100EV_ROT_+7_IMAGE3_HI_PRESS_4E-5'
  files_to_read[84] = 'N2_100EV_MUV_TEST42_IMAGE2_HI_PRESS_HI_MODE_4E-5'
  files_to_read[85] = 'N2_100EV_MUV_TEST46_100EV_ROT_+7_IMAGE1_LOW_PRESS_5E-6'
  files_to_read[86] = 'N2_100EV_MUV_TEST47_100EV_ROT_+7_IMAGE2_LOW_PRESS_5E-6'
  files_to_read[87] = 'N2_100EV_MUV_TEST48_100EV_IMAGE3_5E-6'
  files_to_read[88] = 'N2_100EV_MUV_TEST49_100EV_IMAGE2_5E-6'
  files_to_read[89] = 'N2_100EV_MUV_TEST50_100EV_IMAGE3_5E-6'
  files_to_read[90] = 'N2_100EV_MUV_TEST51_100EV_IMAGE1_5E-6'
  files_to_read[91] = 'N2_100EV_MUV_TEST52_100EV_IMAGE2_5E-6'
  files_to_read[92] = 'N2_100EV_MUV_TEST53_100EV_IMAGE3_5E-6'
  files_to_read[93] = 'N2_200EV_FUV_TEST43_200EV_ROT_+7_IMAGE1_HI_PRESS_4E-5'
  files_to_read[94] = 'N2_200EV_FUV_TEST44_200EV_ROT_+7_IMAGE2_HI_PRESS_4E-5'
  files_to_read[95] = 'N2_200EV_FUV_TEST45_200EV_ROT_+7_IMAGE3_HI_PRESS_4E-5'
  files_to_read[96] = 'N2_200EV_MUV_TEST43_200EV_ROT_+7_IMAGE1_HI_PRESS_4E-5'
  files_to_read[97] = 'N2_200EV_MUV_TEST44_200EV_ROT_+7_IMAGE2_HI_PRESS_4E-5'
  files_to_read[98] = 'N2_200EV_MUV_TEST45_200EV_ROT_+7_IMAGE3_HI_PRESS_4E-5'

  ; iterates through all provided files and returns their sensitivies
  ;
  for j = 0, n_elements(files_to_read) - 1 do begin
    ; for j = 0, 2 do begin ; Testing with limited files
    print, j
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
    spec = median(spec, 5)
    spec = smooth(spec, 7)
    under_curve = smooth(spec, 50)
    spec -= min(under_curve)
    ;
    ; retrieve wavelength scale for the rotated image
    ;
    ajello_lab_pixel_scale_rot, wlfuv, wlmuv, yfuv, ymuv

    ; Prints a plot of the shifted data
    ;
    ; win = window(dim = [800, 600])
    ; p1 = plot(wlfuv, spec, name = 'orignal data', current = win)
    ; p2 = plot(wl_spec, spec, /over, color = 'red', name = 'shifted')
    ; leg = legend(target = [p1, p2])
    ; markerp, p1, x = 120., linestyle = 2

    ;
    ; read in N2 LBH reference spectrum that has been convolved
    ; with the IUVS LSF
    ;
    ajello_lab_n2_reference, wl_ref, ref, file_n2_model = file_n2_model

    ; Find shift for spec
    ;

    ; finds the three maximum by blocking out 2nm spaces around them
    spec_subset = spec
    spec_subset[where(wlfuv lt 128)] = 0.0 ; Lower bound for peaks to be found
    index = lonarr(3)
    for k = 0, 2 do begin
      index[k] = (where(spec_subset eq max(spec_subset)))[0]
      peak_wl = wlfuv[index[k]]
      zero_window = where(abs(wlfuv - peak_wl) lt 1.5, count) ; Width of 2.4nm around the peak is blocked out for finding the next peak
      if (count gt 0) then spec_subset[zero_window] = 0.0
      print, wlfuv[index[k]]
    endfor
    ; ctc = [[1, 0, 0], [1, 1, 0], [1, 0, 1], [0, 1, 1], [1, 1, 1]] ; combinations to check
    ; peaks = [135.4, 138.4, 132.6]
    ; ctc = ctc * peaks
    ctc = [[135.4, 0, 0], [135.4, 138.4, 0], [135.4, 0, 132.6], [0, 138.4, 132.6], [135.4, 138.4, 132.6]] ; combinations to check with main reference wavelengths
    best_so_far = 100000
    bsf_index = 0
    for k = 0, 4 do begin
      active_peaks = where(ctc[*, k] ne 0.0, active_count)
      if (active_count gt 0) then begin
        diff_sum = 0.0
        for p = 0, active_count - 1 do begin
          peak_col = active_peaks[p]
          ind = where(ctc ne 0.0, count)
          diff_sum += abs(ctc[peak_col, k] - wlfuv[index[peak_col]]) / (2 + (2 - ind[0]) * 0.25) ; Weighting factor to prefer main peaks
        endfor
        sum_avg = diff_sum / active_count
        if sum_avg lt best_so_far then begin
          best_so_far = sum_avg
          bsf_index = k
        endif
        print, sum_avg
      endif
    endfor
    if ctc[0, bsf_index] ne 0 then begin
      wl_spec = wlfuv - wlfuv[index[0]] + 135.4
    endif else if ctc[1, bsf_index] ne 0 then begin
      wl_spec = wlfuv - wlfuv[index[1]] + 138.4
    endif else if ctc[2, bsf_index] ne 0 then begin
      wl_spec = wlfuv - wlfuv[index[2]] + 132.6
    endif

    ; mapped highest point of the reference data as calibration point for exactness
    ; Old Code:
    ; wl_spec = wlfuv - wlfuv[findndx(spec, max(spec_subset)] + 135.4 ; where(ref eq max(ref))
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
    file_out = path_save + id1 + '.txt'
    num_sens = n_elements(wl_sens)
    openw, fid, file_out, /get_lun
    for i = 0, num_sens - 1 do $
      printf, fid, wl_sens[i], sens_norm[i], format = '(F10.2,",",F10.6)'
    close, fid
    free_lun, fid
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
    win.save, path_save + id1 + '.png'
  endfor

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
