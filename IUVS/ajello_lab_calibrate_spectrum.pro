;+
; PURPOSE
; This routine will turn a set of IUVS data into a calibrated spectrum
;   using the instrument sensitivity curve produced in July 2026
;
; INPUTS
; arr: [NW, NW] input data array
; image_type: what kind of image the arr is from (image 1, 2, or 3)
;
; OUTPUTS
; wave_spec: [NW] corrected wavelength scale, nm
; spec_cal: [NW] observed and calibrated N2 spectrum
;
; KEYWORDS
;
; NOTES
; [NW] = number of wavelengths
; -
pro ajello_lab_calibrate_spectrum, arr, image_type, $
  wave_spec, $
  spec_cal, $
  show_plots = show_plots, $
  save_data = save_data
  compile_opt idl2

  if n_params() eq 0 then begin
    show_plots = 1
    save_data = '/Users/benjamincondit/Desktop/Data_Reduction copy/N2_16EV_FUV_TEST27_IMAGE1.idl'
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
        path_save = '/Users/benjamincondit/Desktop/Data_Reduction copy/'
        file_data = '/Users/benjamincondit/Desktop/Data_Reduction copy/N2_16EV_FUV_TEST27_IMAGE1.idl'
        file_model = path_repo + 'data/N2/n2_lbh_rot_293K.sav'
      end
    endcase

    ; file_name = ['']
    ; folder_searcher, file_path, '.idl', 'FUV', file_names, keyword_2 = 'N2'

    if file_test(file_model) eq 0 then begin
      print, 'model file not found or not defined'
      stop
    endif

    if file_test(file_data) eq 0 then begin
      print, 'data file not found or not defined'
      stop
    endif

    file_name = file_basename(file_data, '.idl') + '_Calibrated'
    print, 'Processing: ' + file_data

    sObj = obj_new('IDL_Savefile', file_data)
    sObj.restore, 'arr'
    obj_destroy, sObj

    if file_name.contains('IMAGE1') then image_type = 1 $
    else image_type = 2
  endif

  spat = total(arr, 1, /nan)

  ;
  ; create a single spectrum
  ;
  y1_key = 130 ; outside key hole
  y2_key = 940

  if image_type eq 1 then begin
    ndx_spat_peak = findndx(spat, max(spat))
    yw = 100
    y1 = (ndx_spat_peak - yw) > y1_key
    y2 = (ndx_spat_peak + yw) < y2_key
  endif else begin
    y1 = y1_key
    y2 = y2_key
  endelse

  spec = total(arr[*, y1 : y2], 2, /nan) ; for rotated image

  spec_modded = [replicate(0.0, 336), spec[336 : 358], replicate(0.0, n_elements(spec) - 359)]
  modded_wl = findgen(n_elements(spec_modded))
  gfit = gaussfit(modded_wl, spec_modded, A, nterms = 4)
  ndx_spec_peak = (where(gfit eq max(gfit)))[0]

  ;
  ; retrieve a notional wavelength scale
  ;
  ajello_lab_pixel_scale_rot, wlfuv, wlmuv, yfuv, ymuv

  wave_shift = wlfuv[ndx_spec_peak] + 135.44 ; to be saved
  wave_spec = wlfuv - wlfuv[ndx_spec_peak] + 135.44 ; from Ajello et al. 1985
  ; print, ndx_spec_peak

  ; Subtract residual background
  ;
  ; left_pt = mean(spec[0 : long(0.1 * n_elements(spec))])
  ; right_pt = mean(spec[long(0.85 * n_elements(spec)) : n_elements(spec) - 1])
  left_end = 0.05
  right_begin = 0.95
  spec_left = spec[0 : long(left_end * n_elements(spec))]
  spec_right = spec[long(right_begin * n_elements(spec)) : n_elements(spec) - 1]
  left_ndx = spec_left[sort(spec_left)]
  right_ndx = spec_right[sort(spec_right)]
  left_pt = left_ndx[long(0.25 * n_elements(left_ndx))]
  right_pt = right_ndx[long(0.25 * n_elements(right_ndx))]

  background_slope = (left_pt - right_pt) / (wave_spec[0] - wave_spec[-1])
  spec_significant = spec - (background_slope * (wave_spec - wave_spec[0]) + left_pt)
  slope_start = [wave_spec[1], left_pt]

  ;
  ; retrieve sensitivity
  ;
  ajello_lab_sensitivity_fuv_2026_07, wave_spec, sens

  spec_cal = spec_significant / sens

  if keyword_set(show_plots) then begin
    if keyword_set(save_data) then $
      file_name = file_basename(save_data, '.idl') + '_Calibrated' else file_name = ''

    ndx_imporant = where(wave_spec lt 160 and wave_spec gt 115)
    yr = [1.05 * min(spec_cal[ndx_imporant]), 1.05 * max(spec_cal[ndx_imporant])]

    win = window(dimensions = [1000, 500])
    win.refresh, /disable

    p1 = plot(wave_spec, spec, xtitle = 'Wavelength Scale Corrected (nm)', title = file_name, $
      ytitle = 'Intensity (arb. units)', current = win, yr = yr, color = 'gray', font_name = 'times', font_size = 12, name = 'Raw Data')
    p2 = plot(wave_spec, spec_significant, color = 'green', /over, name = 'Background Subtracted')
    p3 = plot(wave_spec, spec_cal, color = 'purple', /over, name = 'Calibrated')
    p4 = plot(wave_spec, background_slope * (wave_spec - wave_spec[0]) + left_pt, color = 'pink', /over, name = 'Background slope')
    markerp, p1, x = 120.0, linestyle = 2, color = 'red' ; 120nm feature
    markerp, p1, x = 135.4, linestyle = 2, color = 'blue' ; scale aligning point
    markerp, p1, y = 0
    markerp, p1, x = wave_spec[long(left_end * n_elements(spec))]
    markerp, p1, x = wave_spec[long(right_begin * n_elements(spec))]
    leg = legend(target = [p1, p2, p3], font_size = 9, font_name = 'times', linestyle = 6, /relative, position = [1.1, 1.15], sample_width = 0.1)
    win.refresh

    if keyword_set(save_data) then $
      win.save, file_dirname(save_data) + path_sep() + file_name + '.png'
  endif

  if keyword_set(save_data) then begin
    path_save = file_dirname(save_data) + path_sep()
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
    file_name = file_basename(save_data, '.idl')
    save, filename = path_save + file_name + '_Calibrated.idl', arr, y1, y2, spec, $
      wave_spec, wave_shift, spat, spec_cal, background_slope, slope_start, sens, desc
  endif

  if n_params() eq 0 then stop
end
