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
pro ajello_lab_regression_driver
  compile_opt idl2

  case (get_login_info()).user_name of
    'holsclaw': begin
      ; file_data_helper = '/Volumes/projects/Phase_Development/MAVEN/IUVS_Data/IUVS_Breadboard/Round_12/data_reduction/'
      path_save = '/users/holsclaw/Documents/'
      file_path = '/Volumes/projects/Phase_Development/MAVEN/IUVS_Data/IUVS_Breadboard/Ajello_Round14/Data_Reduction/'
    end
    'benjamincondit': begin
      ; file_data_helper = '/Users/benjamincondit/Desktop/IUVS_Breadboard/Round 12/data_reduction/'
      path_save = '/Users/benjamincondit/idl/data_reduction/IUVS/Sensitivities/'
      file_path = '/Users/benjamincondit/Desktop/IUVS_Breadboard/Ajello_Round14/Data_Reduction/'
    end
  endcase

  files = ['N2_30EV_FUV_TEST19_IMAGE1.idl', '', '']

  spec_norm = !null
  lbh_norm = !null
  regg_fits = list()
  regg_data = list()
  for i = 0, n_elements(files) - 1 do begin
    ajello_lab_regression_example, file_path + files[i], fit, data, wave_spec, spec_norm, lbh_norm
    regg_fits.add, fit
    regg_data.add, data
  endfor
end
