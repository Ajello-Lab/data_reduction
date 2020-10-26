
;+
; NAME:
; AJELLO_LAB_PROCESS_DRIVER
; 
; PURPOSE:
; This routine iterates through each of the IUVS lab datasets and creates a single dark-subtracted array stored in an IDL save file
;-  
pro ajello_lab_process_driver
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
  path_base = !path_base
  ;path_base = '/Volumes/projects/Phase_Development/MAVEN/IUVS_Data/IUVS_Breadboard/'  
  
  ;
;  dataset_id = '_Big_e-gun'
;  path_data1 = file_search( path_base+dataset_id, 'CO_*', /test_dir )
;  path_data2 = file_search( path_base+dataset_id, 'CO2_*', /test_dir )
;  path_data3 = file_search( path_base+dataset_id, 'H2_*', /test_dir )
;  path_data4 = file_search( path_base+dataset_id, 'N2_*', /test_dir )
;  path_data = [path_data1,path_data2,path_data3,path_data4]
;  pos = strpos( path_data, 'Junk_Play' )
;  ndx = where( pos eq -1, /null )
;  path_data = path_data[ndx]
;  ndir = n_elements(path_data)

  ;dataset_id = '_Big_e-gun_roundIV'
  ;dataset_id = '_Big_e-gun_roundIII'
  ;path_data = file_search( path_base+dataset_id+'/H2', 'test*', /test_dir )
  
  dataset_id = '_Big_e-gun_roundVII'
  ;path_data = file_search( path_base+dataset_id+path_sep()+'CO2', 'test*', /test_dir )  
  ;path_data = file_search( path_base+dataset_id+path_sep()+'CO2/100eV', 'test*', /test_dir )
  ;path_data = file_search( path_base+dataset_id+path_sep()+'N2/200eV', 'test*', /test_dir )
  path_data = file_search( path_base+dataset_id+path_sep()+'N2/30eV', 'test*', /test_dir )
  
 ; path_data = '/Volumes/projects/Phase_Development/MAVEN/IUVS_Data/IUVS_Breadboard/_Big_e-gun_roundVII/N2/100eV/test48_100eV_image3_5e-6'
  ;path_data = path_data[24:*]
  
  ndir = n_elements(path_data)

  ;
  ;
  print, 'data to process:'
  for i = 0, n_elements(path_data) - 1 do print, i, ' ', path_data[i]

  
  
  stop
  
;  ;
;  ; exclude one dataset from Round3 because it crashes IDL, presumably because there is too much data 
;  ;
;  pos = strpos( path_data, 'N2/100eV/test_8_image1' )
;  ndx = where( pos eq -1, /null )
;  path_data = path_data[ndx]
;  ndir = n_elements(path_data)
;  
;  pos = strpos( path_data, 'Base_pressure' )
;  ndx = where( pos eq -1, /null )
;  path_data = path_data[ndx]
  ndir = n_elements(path_data)
  
  ; 
  ; 
  print, 'data to process:'
  for i = 0, n_elements(path_data) - 1 do print, i, ' ', path_data[i]

  
  
  ;
  ; Based on selected dataset, define subfolders where reduced and calibrated data will be stored
  ;
  path_ref = path_base + dataset_id + path_sep()
  path_save = path_ref + 'data_reduction' + path_sep()
  ;
  ; check to make sure the required paths exist
  ;
  path_ref_test = file_search( path_ref, /mark )
  path_save_test = file_search( path_save, /mark )
  if path_ref_test eq '' then begin
    print, 'Expected this path to exist: ', path_ref_test
    stop
  endif
  if path_save_test eq '' then begin
    print, 'Expected this path to exist: ', path_save_test
    stop
  endif
  
  stop
  
  ;
  ; process the data and create summary plots
  ;
  for i = 0, ndir - 1 do begin
    print, i, ' processing: ', path_data[i]
    tic
    fsave_vec = ''
    ajello_lab_process, path_data[i], path_save, fsave_vec  
    ;stop
    if fsave_vec ne !NULL then begin
      if fsave_vec[0] ne '' then begin
        ;
        ajello_lab_image_summary_rot, fsave_vec[0], win, yr=yr_spec
        win.save,path_save+file_basename(fsave_vec[0],'.idl')+'.png'
        ;
        if n_elements(fsave_vec) gt 1 then begin
          ajello_lab_image_summary_rot, fsave_vec[1], win, yr=yr_spec
          win.save,path_save+file_basename(fsave_vec[1],'.idl')+'.png'
        endif
        toc
      endif
    endif
     ;stop
  endfor





stop

  close_windows

  ajello_lab_set_paths

  ;path_data = '/Volumes/projects/Phase_Development/MAVEN/IUVS_Data/IUVS_Breadboard/_BIG_e-gun/'
  path_data = '/Volumes/projects/Phase_Development/MAVEN/IUVS_Data/IUVS_Breadboard/_BIG_e-gun_RoundII/'

  ;
  ; collect all datasets4.7
  ;
  path_CO  = file_search( path_data + '/CO/CO_*',   /test_directory, count=count_CO )
  path_CO2 = file_search( path_data + '/CO2/CO2_*', /test_directory, count=count_CO2 )
  path_N2  = file_search( path_data + '/N2/N2_*',   /test_directory, count=count_N2 )
  path_H2  = file_search( path_data + '/H2/H2_*',   /test_directory, count=count_H2 )

  ;
  ; All data will be saved here
  ;
  ;path_save = '/Volumes/projects/Phase_Development/MAVEN/IUVS_Data/IUVS_Breadboard/_BIG_e-gun/data_reduction/'
  path_save = path_data + 'data_reduction/'
  
  ;
  ; CO
  ; Duration: 324 sec
   tic
  for i = 0, count_CO - 1 do begin
    print, 'processing: ', path_CO[i]
    ajello_lab_process, path_CO[i], path_save, fsave_vec
  endfor
  toc
    
  ;
  ; CO2
  ; Duration: 230 sec (3.8min)
;  tic
;  for i = 0, count_CO2 - 1 do begin
;    print, 'processing: ', path_CO2[i]
;    ajello_lab_process, path_CO2[i], path_save, fsave_vec
;  endfor
;  toc
  
  ;
  ; N2
  ; Duration: 325 sec
;  tic
;  for i = 0, count_N2 - 1 do begin
;    print, 'processing: ', path_N2[i]
;    ajello_lab_process, path_N2[i], path_save, fsave_vec
;  endfor
;  toc

  ;
  ; H2
  ; Duration: 140 sec
;  tic
;  for i = 0, count_H2 - 1 do begin
;    print, 'processing: ', path_H2[i]
;    ajello_lab_process, path_H2[i], path_save, fsave_vec
;  endfor
;  toc

  
stop

end