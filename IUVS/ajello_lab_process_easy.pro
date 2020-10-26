;+
; NAME:
; AJELLO_LAB_PROCESS_EASY
; 
; PURPOSE:
;  This routine will query the user for a path to a single IUVS dataset, reduce the data, 
;    place the result in an IDL save file, create a summary plot for each channel,
;    and calibrate the data.   
; 
; INPUTS:
; none.
; 
; OUTPUTS:
; none.
;-
pro ajello_lab_process_easy
  ;
  ; Define local paths based on current user  
  ;
  ajello_lab_set_paths
  ;
  ; Define the spatial rows to consider when forming the summary spectral plot and the calibrated data range. 
  ;
  yr_spec = [150,900]
  ;
  ; Ask user to specify data path for processing 
  ;
  path_data = dialog_pickfile(path=!path_base,/directory, title='Select a path containing data to process')
  if path_data eq '' then begin
    print, 'Dialog canceled.'
    stop
  endif
  ;
  ; error handling
  ;
  pos = strpos( path_data, !path_base )
  if pos eq -1 then begin
    print, 'Error: '
    print, '  Selected file path of: ', path_data
    print, '  Must be below the base file path of: ', !path_base
    stop
  endif
  temp1 = strmid( path_Data, strlen(!path_base), strlen(path_data) )
  temp2 = strsplit( temp1, path_sep(), /extract )
  ;
  ; Based on selected dataset, define subfolders where reduced and calibrated data will be stored 
  ;
  path_ref = !path_base + temp2[0] + path_sep()
  path_save = path_ref + 'data_reduction' + path_sep()
  ;path_cal = path_ref + 'data_calibration' + path_sep()
  ; 
  ; check to make sure the required paths exist
  ;
  path_ref_test = file_search( path_ref, /mark )
  path_save_test = file_search( path_save, /mark )
  ;path_cal_test = file_search( path_cal, /mark )
  if path_ref_test eq '' then begin
    print, 'Expected this path to exist: ', path_ref_test
    stop
  endif
  if path_save_test eq '' then begin
    print, 'Expected this path to exist: ', path_save_test
    stop
  endif
  
  print, 'path base:      ', !path_base
  print, 'path reference: ', path_ref_test
  print, 'path save:      ', path_save
  print, 'path data:      ', path_data
  
  ;stop
  
;  if path_cal_test eq '' then begin
;    print, 'Expected this path to exist: ', path_cal_test
;    stop
;  endif
  ;
  ; Ask user to specify path to save processed data
  ;
  ;path_save = dialog_pickfile(path=!path_save,/directory, title='Select a path to store the reduced data')
  ;
  ; process the data
  ;
  print, 'processing: ', path_data
  tic
  ajello_lab_process, path_data, path_save, fsave_vec
  toc
  ;
  ; create a summary image
  ;
  for i = 0, n_elements(fsave_vec) - 1 do begin
    ajello_lab_image_summary_rot, fsave_vec[i], win, yr=yr_spec
    win.save,path_save+file_basename(fsave_vec[i],'.idl')+'.png'
  endfor
;  ;
;  ; Calibrate the data
;  ;
;  print, 'calibrating: ', path_data
;  ajello_lab_calibrate, fsave_vec[0], fsave_cal1, path_cal=path_cal, yr=yr_spec
;  ajello_lab_calibrate, fsave_vec[1], fsave_cal2, path_cal=path_cal, yr=yr_spec
;  
  stop
end

