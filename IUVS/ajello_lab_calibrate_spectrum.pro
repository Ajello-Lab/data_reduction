;+
; PURPOSE
;
;
; INPUTS
;
; OUTPUTS
;
; KEYWORDS
;-
pro ajello_lab_calibrate_spectrum
  compile_opt idl2

  ajello_lab_set_paths, path_base, path_repo

  case (get_login_info()).user_name of
    'holsclaw': begin
      ; file_data_helper = '/Volumes/projects/Phase_Development/MAVEN/IUVS_Data/IUVS_Breadboard/Round_12/data_reduction/'
      path_save = '/users/holsclaw/Documents/'
      file_path = '/Volumes/projects/Phase_Development/MAVEN/IUVS_Data/IUVS_Breadboard/Ajello_Round14/Data_Reduction/'
      file_model = path_repo + 'data/N2/n2_lbh_rot_293K.sav'
    end
    'benjamincondit': begin
      ; file_data_helper = '/Users/benjamincondit/Desktop/IUVS_Breadboard/Round 12/data_reduction/'
      path_save = '/Users/benjamincondit/Desktop/Data_Reduction_copy/'
      file_path = '/Users/benjamincondit/Desktop/Data_Reduction_copy/'
      file_model = path_repo + 'data/N2/n2_lbh_rot_293K.sav'
    end
  endcase

  ; file_names = ['']
  folder_searcher, file_path, '.idl', 'FUV', file_names, keyword_2 = 'N2'

  if file_test(file_model) eq 0 then begin
    print, 'model file not found or not defined'
    stop
  endif

  for i = 0, n_elements(file_names) - 1 do begin
    if file_names[i].contains('Calibrated') then continue

    file_data = file_names[i]

    if file_test(file_data) eq 0 then begin
      print, 'data file not found or not defined'
      stop
    endif

    file_name = file_basename(file_data, '.idl') + '_Calibrated'
    print, 'Processing: ' + file_data

    sObj = obj_new('IDL_Savefile', file_data)
    sObj.restore, 'arr'
    obj_destroy, sObj

    spat = total(arr, 1, /nan)

    ;
    ; create a single spectrum
    ;
    y1_key = 130 ; outside key hole
    y2_key = 940

    if file_name.contains('IMAGE1') then begin
      ndx_spat_peak = findndx(spat, max(spat))
      yw = 100
      y1 = (ndx_spat_peak - yw) > y1_key
      y2 = (ndx_spat_peak + yw) < y2_key
    endif else if file_name.contains('IMAGE2') or file_name.contains('IMAGE3') then begin
      y1 = y1_key
      y2 = y2_key
    endif

    spec = total(arr[*, y1 : y2], 2, /nan) ; for rotated image

    spec_modded = spec
    spec_modded[0 : 330] = 0
    spec_modded[360 : n_elements(spec_modded) - 1] = 0
    ndx_spec_peak = findndx(spec_modded, max(spec_modded))

    ;
    ; retrieve a notional wavelength scale
    ;
    ajello_lab_pixel_scale_rot, wlfuv, wlmuv, yfuv, ymuv

    wave_shift = wlfuv[ndx_spec_peak] + 135.4 ; to be saved
    wave_spec = wlfuv - wlfuv[ndx_spec_peak] + 135.4
    ; print, ndx_spec_peak

    ; Subtract residual background
    ;
    spec_left = mean(spec[0 : long(0.1 * n_elements(spec))])
    spec_right = mean(spec[long(0.9 * n_elements(spec)) : n_elements(spec) - 1])
    background_slope = (spec_right - spec_left) / (wave_spec[long(0.95 * n_elements(wave_spec))] - wave_spec[long(0.05 * n_elements(wave_spec))])
    spec_significant = spec - (background_slope * (wave_spec - wave_spec[long(0.05 * n_elements(wave_spec))]) + spec_left)
    slope_start = [wave_spec[long(0.05 * n_elements(wave_spec))], spec_left]

    ;
    ; retrieve sensitivity
    ;
    ajello_lab_sensitivity_fuv_2026_07, wave_spec, sens

    spec_cal = spec_significant / sens

    ndx_imporant = where(wave_spec lt 160 and wave_spec gt 115)
    yr = [1.05 * min(spec_cal[ndx_imporant]), 1.05 * max(spec_cal[ndx_imporant])]
    win = window(dimensions = [1000, 500])
    p1 = plot(wave_spec, spec, xtitle = 'wavelength scale corrected (nm)', title = file_name, $
      ytitle = 'Intensity (arb. units)', current = win, yr = yr, color = 'gray', font_name = 'times', font_size = 12, name = 'Raw Data')
    p2 = plot(wave_spec, spec_significant, color = 'green', /over, name = 'Background Subtracted')
    p3 = plot(wave_spec, spec_cal, color = 'purple', /over, name = 'Calibrated')
    markerp, p1, x = 120.0, linestyle = 2, color = 'red' ; 120nm feature
    markerp, p1, x = 135.4, linestyle = 2, color = 'blue' ; scale aligning point
    markerp, p1, y = 0
    leg = legend(target = [p1, p2, p3], font_size = 9, font_name = 'times', linestyle = 6, /relative, position = [1.1, 1.15], sample_width = 0.1)
    win.save, path_save + file_name + '.png'

    desc = [ $
      'arr: data loaded from savefile, an average, dark-subtracted image in units of DN per readout', $
      'background_slope: slope component for calculating the residual offset value, starting from coords in slope_start', $
      'slope_start: [x , y] coordinates for starting point of background slope subtraction. X is in nm and y in arb units of dataset', $
      'sens: sensitivity curve used to calibrate data', $
      'spec: uncalibrated signal from summation of arr between y1 and y2', $
      'spec_cal: calibrated spectrum using sensitivity curve for IUVS instrument', $
      'spat: the spatial distribution of the signal', $
      'wave_shift: amount that wlfuv is shifted to match the data to prominent figure at 120nm', $
      'wave_spec: shifted wave length spread that matches data', $
      'y1: lower bound of used spatial data', $
      'y2: upper bound of used spatial data']
    file_name = file_basename(file_data, '.idl')
    save, filename = path_save + file_name + '_Calibrated.idl', arr, y1, y2, spec, $
      wave_spec, wave_shift, spat, spec_cal, background_slope, slope_start, sens, desc
  endfor
  stop
end
