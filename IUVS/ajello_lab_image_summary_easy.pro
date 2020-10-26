;+
; NAME:
; AJELLO_LAB_IMAGE_SUMMARY_EASY
; 
; PURPOSE:
; This routine will query the user for one or more IDL save files and an output destination, then 
;   produce summary plots.   
;-
pro ajello_lab_image_summary_easy
  ;
  ; Define local paths based on current user  
  ;
  ajello_lab_set_paths
  ;
  ; Ask user to specify data path for processing 
  ;
  file      = dialog_pickfile(path=!path_base, title='Select a file containing data to process',/MULTIPLE_FILES,filter='*.idl')
  if n_elements(file) eq 1 then begin
    if file eq '' then begin
      print, 'no file selected'
      stop
    endif
  endif
  ;
  ; Ask user to specify path to save processed data
  ;
  path_save = dialog_pickfile(path=!path_save3,/directory, title='Select a path to store the reduced data')
  ;
  ; create a summary image
  ;
  for i = 0, n_elements(file) - 1 do begin
    ajello_lab_image_summary_rot, file[i], win
    win.save,path_save+file_basename(file[i],'.idl')+'.png'
  end
  ;stop
end
