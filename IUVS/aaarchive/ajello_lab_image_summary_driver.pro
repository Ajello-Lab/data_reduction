
;+
; NAME:
; AJELLO_LAB_IMAGE_SUMMARY_DRIVER
; 
; PURPOSE:
; Create summary image files for multiple reduced datasets 
;-
pro ajello_lab_image_summary_driver

ajello_lab_set_paths

;path_data = '/Users/holsclaw/MAVEN/Ajello_lab/_Big_e-gun_RoundII/data_reduction/'
path_data = !path_base +  '_Big_e-gun_RoundII/'
path_save = path_data

file_list = file_search( path_save, '*.idl', count=nfiles )
;file_list = path_save + ['CO_30eV_FUV_test_5_image3.idl','CO_30eV_MUV_test_5_image3.idl']
;file_list = path_save + ['CO2_100eV_FUV_test_8_image1.idl','CO2_100eV_MUV_test_8_image1.idl'] 

nfiles = n_elements(file_list)
for i = 0, nfiles - 1 do begin
  file = file_list[i]
  ajello_lab_image_summary_rot, file, win
  win.save,path_save+file_basename(file,'.idl')+'.png'
endfor

stop

end