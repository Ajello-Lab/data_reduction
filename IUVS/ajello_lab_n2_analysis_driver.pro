;+
; PURPOSE
;  Iterates through IUVS data and calibrates it, fits data to image 1, and combines triads of data into a combined image
;-
pro ajello_lab_n2_analysis_driver
  compile_opt idl2

  show_plots = 1

  case (get_login_info()).user_name of
    'holsclaw': begin
      ; file_data_helper = '/Volumes/projects/Phase_Development/MAVEN/IUVS_Data/IUVS_Breadboard/Round_12/data_reduction/'
      path_save = '/users/holsclaw/Documents/'
      file_path = '/Volumes/projects/Phase_Development/MAVEN/IUVS_Data/IUVS_Breadboard/Ajello_Round14/Data_Reduction/N2_30EV_FUV_TEST19_IMAGE1.idl'
    end
    'benjamincondit': begin
      ; file_data_helper = '/Users/benjamincondit/Desktop/IUVS_Breadboard/Round 12/data_reduction/'
      path_save = '/Users/benjamincondit/Desktop/Data_Reduction copy/'
      file_path = '/Users/benjamincondit/Desktop/Data_Reduction copy/'
    end
  endcase

  ; in order of Image 1, Image 2, Image 3
  data_list = ['N2_30EV_FUV_TEST19_IMAGE1.idl', 'N2_30EV_FUV_TEST20_IMAGE2.idl', 'N2_30EV_FUV_TEST21_IMAGE3.idl']
  data_list = ['N2_20EV_FUV_TEST31_IMAGE1.idl', 'N2_20EV_FUV_TEST32_IMAGE2.idl', 'N2_20EV_FUV_TEST30_IMAGE3.idl']
  data_list = ['N2_14EV_FUV_TEST33_IMAGE1.idl', 'N2_14EV_FUV_TEST34_IMAGE2.idl', 'N2_14EV_FUV_TEST35_IMAGE3.idl']
  data_list = ['N2_14EV_FUV_TEST33_IMAGE1.idl', 'N2_14EV_FUV_TEST34_IMAGE2.idl', 'N2_14EV_FUV_TEST35_IMAGE3.idl']
  data_list = ['N2_16EV_FUV_TEST27_IMAGE1.idl', 'N2_16EV_FUV_TEST28_IMAGE2.idl', 'N2_16EV_FUV_TEST29_IMAGE3.idl']

  sz = size(data_list, /dim)
  num_data = sz[0]

  for i = 0, num_data - 1 do begin
    ; LOAD IN DATA ***********************
    wave_spec_data = fltarr(3, 1024)
    spec_cal_data = fltarr(3, 1024)
    for j = 0, 2 do begin
      file_data = file_path + data_list[j, i]
      save_data = path_save + data_list[j, i]

      sObj = obj_new('IDL_Savefile', file_data)
      sObj.restore, 'arr'
      obj_destroy, sObj

      ; calibrate data
      ajello_lab_calibrate_spectrum, arr, j + 1, temp_wave_spec, temp_spec_cal, show_plots = show_plots, save_data = save_data

      wave_spec_data[j, *] = temp_wave_spec
      spec_cal_data[j, *] = temp_spec_cal
    endfor

    print, 'Calibrated: ' + data_list[0, i] + ', ' + data_list[1, i] + ', ' + data_list[2, i]
    stop

    save_data = path_save + data_list[i, 0]

    ; ajello_lab_n2_model_fit, wave_spec_data[0, *], spec_cal_data[0, *], param_fit, $
    ; param_id, fcf_fit, model_fit_arr, spec_fit, show_plots = show_plots, save_data = save_data ; , wl2_fit = 170

    ; print, 'Data fit to LBH lines'
    ; stop

    ajello_lab_combine_images, wave_spec_data[0, *], spec_cal_data[0, *], wave_spec_data[1, *], $
      spec_cal_data[1, *], wave_spec_data[2, *], spec_cal_data[2, *], $
      wave_spec, total_spec, show_plots = show_plots, save_data = save_data

    print, 'Images 1,2,3 combined'
    stop
  endfor
  stop
end
