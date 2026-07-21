;+
; NAME:
; AJELLO_LAB_PROCESS_DRIVER
;
; PURPOSE:
; This routine iterates through each of the IUVS lab datasets and creates a single dark-subtracted array stored in an IDL save file
;-
pro ajello_lab_process_driver
  compile_opt idl2
  ;
  ; Define local paths based on current user
  ;
  ajello_lab_set_paths
  ;
  ; Define the spatial rows to consider when forming the summary spectral plot and the calibrated data range.
  ;
  yr_spec = [150, 900]
  ;
  ; Ask user to specify data path for processing
  ;
  path_base = !path_base
  ; path_base = '/Volumes/projects/Phase_Development/MAVEN/IUVS_Data/IUVS_Breadboard/'

  ;
  ; dataset_id = '_Big_e-gun'
  ; path_data1 = file_search( path_base+dataset_id, 'CO_*', /test_dir )
  ; path_data2 = file_search( path_base+dataset_id, 'CO2_*', /test_dir )
  ; path_data3 = file_search( path_base+dataset_id, 'H2_*', /test_dir )
  ; path_data4 = file_search( path_base+dataset_id, 'N2_*', /test_dir )
  ; path_data = [path_data1,path_data2,path_data3,path_data4]
  ; pos = strpos( path_data, 'Junk_Play' )
  ; ndx = where( pos eq -1, /null )
  ; path_data = path_data[ndx]
  ; ndir = n_elements(path_data)

  ; dataset_id = '_Big_e-gun_roundIV'
  ; dataset_id = '_Big_e-gun_roundIII'
  ; path_data = file_search( path_base+dataset_id+'/H2', 'test*', /test_dir )

  ; dataset_id = '_Big_e-gun_roundVII'
  ; path_data = file_search( path_base+dataset_id+path_sep()+'CO2', 'test*', /test_dir )
  ; path_data = file_search( path_base+dataset_id+path_sep()+'CO2/100eV', 'test*', /test_dir )
  ; path_data = file_search( path_base+dataset_id+path_sep()+'N2/200eV', 'test*', /test_dir )
  ; path_data = file_search( path_base+dataset_id+path_sep()+'N2/30eV', 'test*', /test_dir )

  dataset_id = 'Big_e-gun_roundVIII'
  dataset_id = 'Big_e-gun_RoundVIII_final'
  dataset_id = 'NeweGun_round10_after_energy_correction'
  dataset_id = 'Round_12'
  dataset_id = 'Ajello_Round13'
  dataset_id = 'Ajello_Round14'
  ; path_data = file_search( path_base+dataset_id+path_sep()+'N2/30eV', 'test*', /test_dir )

  path_data_exp = path_base + dataset_id + path_sep() + 'N2+O2/30eV/'
  path_data_exp = path_base + dataset_id + path_sep() + 'N2+O2/100eV/'
  path_data_exp = path_base + dataset_id + path_sep() + 'N2/100eV/'
  path_data_exp = path_base + dataset_id + path_sep() + 'N2/30eV/'
  path_data_exp = path_base + dataset_id + path_sep() + 'N2/20eV/'
  path_data_exp = path_base + dataset_id + path_sep() + 'N2/16eV/'
  path_data_exp = path_base + dataset_id + path_sep() + 'N2/100eV/test20_image1_hiPress/'
  path_data_exp = path_base + dataset_id + path_sep() + 'N2/100eV/test21_image2_hiPress/'
  path_data_exp = path_base + dataset_id + path_sep() + 'N2/100eV/test22_image3_hiPress/'
  path_data_exp = path_base + dataset_id + path_sep() + 'N2/30eV/'
  ; path_data_exp = path_base+dataset_id+path_sep() + 'N2/100eV/'
  path_data_exp = path_base + dataset_id + path_sep() + 'N2/20eV/'
  ; path_data_exp = path_base+dataset_id+path_sep() + 'N2/12eV/'
  ; path_data_exp = path_base+dataset_id+path_sep() + 'N2/16eV/'
  path_data_exp = path_base + dataset_id + path_sep() + 'H2/100eV/'
  path_data_exp = path_base + dataset_id + path_sep() + 'CO2/30eV/'

  ; path_data_exp = path_base+dataset_id+path_sep() + 'CO2/16eV/test5_image1/'
  ; path_data_exp = path_base+dataset_id+path_sep() + 'CO2/30eV/test4_image1/'
  ; path_data_exp = path_base+dataset_id+path_sep() + 'CO2/30eV/test7_image2/'
  ; path_data_exp = path_base+dataset_id+path_sep() + 'CO2/30eV/test8_image1/'
  ; path_data_exp = path_base+dataset_id+path_sep() + 'CO2/100eV/test6_image1/'

  ; Run by ben starting on 6/29 : <start>
  ; path_data_exp = path_base + dataset_id + path_sep() + 'H2/100eV/Test1_Image1/'
  ; path_data_exp = path_base + dataset_id + path_sep() + 'H2/100eV/test2_image2/'
  ; ; path_data_exp = path_base + dataset_id + path_sep() + 'H2/100eV/test3_image3/'
  ; path_data_exp = path_base + dataset_id + path_sep() + 'H2/100eV/test4_image2/'
  ; path_data_exp = path_base + dataset_id + path_sep() + 'H2/100eV/test5_image1/'
  ; path_data_exp = path_base + dataset_id + path_sep() + 'H2/100eV/test6_image3/'

  ; path_data_exp = path_base + dataset_id + path_sep() + 'N2/30eV/test19_image1/'
  ; path_data_exp = path_base + dataset_id + path_sep() + 'N2/30eV/test20_image2/'
  ; path_data_exp = path_base + dataset_id + path_sep() + 'N2/30eV/test21_image3/'

  ; path_data_exp = path_base + dataset_id + path_sep() + 'N2/12eV/test17_image1/'
  ; path_data_exp = path_base + dataset_id + path_sep() + 'N2/12eV/test18_image2/'

  ; path_data_exp = path_base + dataset_id + path_sep() + 'N2/100eV/test7_image1/'
  ; path_data_exp = path_base + dataset_id + path_sep() + 'N2/100eV/test8_image2/'
  ; path_data_exp = path_base + dataset_id + path_sep() + 'N2/100eV/test9_image3/'
  ; path_data_exp = path_base + dataset_id + path_sep() + 'N2/100eV/test10_image3/'
  ; path_data_exp = path_base + dataset_id + path_sep() + 'N2/100eV/test11_image1/'
  ; path_data_exp = path_base + dataset_id + path_sep() + 'N2/100eV/test12_image2/'
  ; path_data_exp = path_base + dataset_id + path_sep() + 'N2/100eV/test14_image1/'
  ; path_data_exp = path_base + dataset_id + path_sep() + 'N2/100eV/test15_image3/'
  ; path_data_exp = path_base + dataset_id + path_sep() + 'N2/100eV/test16_image2/'

  ; path_data_list = ['H2/100eV/test3_image3/', 'H2/100eV/test4_image2/', 'H2/100eV/test5_image1/', 'H2/100eV/test6_image3/', 'N2/30eV/test19_image1/', $
  ; 'N2/30eV/test20_image2/', 'N2/30eV/test21_image3/', 'N2/12eV/test17_image1/', 'N2/12eV/test18_image2/', 'N2/100eV/test7_image1/', $
  ; 'N2/100eV/test8_image2/', 'N2/100eV/test9_image3/', 'N2/100eV/test10_image3/', 'N2/100eV/test11_image1/', 'N2/100eV/test12_image2/', $
  ; 'N2/100eV/test14_image1/', 'N2/100eV/test15_image3/', 'N2/100eV/test16_image2/']
  ; path_data_list = ['N2/10eV/test23_image2/']
  ; path_data_list = ['N2/9eV/test26_image1/', 'N2/12eV/test24_image2/', 'N2/12eV/test25_image2/', 'N2/16eV/test27_image1/']
  ; path_data_list = ['N2/16eV/test28_image2/']
  ; path_data_list = ['N2/16eV/test29_image3/', 'N2/20eV/test30_image3/']
  ; path_data_list = ['N2/20eV/test31_image1/', 'N2/20eV/test32_image2/', 'N2/14eV/test33_image1/']
  ; path_data_list = ['N2/14eV/test34_image2/']
  ; path_data_list = ['N2/14eV/test35_image3/', 'N2/11eV/test36_image1/']
  ; path_data_list = ['N2/18eV/test37_image3/']
  ; path_data_list = ['N2/18eV/test38_image1/', 'N2/18eV/test39_image2/']
  ; path_data_list = ['N2/9eV/test40_image1/']
  path_data_list = ['N2/11eV/test41_image1/', 'N2/11eV/test42_image2/']
  ; <end>

  ; '/Volumes/projects/Phase_Development/MAVEN/IUVS_Data/IUVS_Breadboard/Round 12/data_reduction'

  ; path_data_exp = path_base+dataset_id+path_sep() + 'N2/35eV/'
  ; path_data_exp = path_base+dataset_id+path_sep() + 'N2/100eV_image1_test4_hi_4e-5Torr_9-30-2024/'
  ; path_data_exp = path_base+dataset_id+path_sep() + 'N2/100eV_image2_test5_hi_4e-5Torr_9-30-2024/'
  ; path_data_exp = path_base+dataset_id+path_sep() + 'N2/100eV_image3_test6_hi_4e-5Torr_9-30-2024/'
  ; path_data_exp = path_base+dataset_id+path_sep() + 'N2/100eV_setup/'
  ; path_data_exp = path_base+dataset_id+path_sep() + 'O2/30eV/'
  ; path_data_exp = path_base+dataset_id+path_sep() + 'O2/100eV/'
  ; path_data_exp = path_base+dataset_id+path_sep() + 'N2/100eV/'
  ; path_data_exp = path_base+dataset_id+path_sep() + 'N2/30eV/'

  for k = 0, n_elements(path_data_list) - 1 do begin
    print, 'File #: ' + string(k)
    path_data_exp = path_base + dataset_id + path_sep() + path_data_list[k]

    ;
    ; find all subdirectories
    tic
    path_data = file_search(path_data_exp + '*', /test_dir, count = count_dir)
    toc

    ; n = where( file_basename(path_data) ne 'here-files have wrong name need to edit' )
    ; path_data = path_data[n]
    ; count_dir = n_elements(path_data)

    ;
    ; if there are no subdirectories, assume the data is located in the base path
    ;
    if count_dir eq 0 then begin
      path_data = file_search(path_data_exp, /test_dir, count = count_dir)
      if count_dir eq 0 then begin
        print, 'path not found: ', path_data_exp
        stop
      endif
    endif

    ; path_data = '/Volumes/projects/Phase_Development/MAVEN/IUVS_Data/IUVS_Breadboard/_Big_e-gun_roundVII/N2/100eV/test48_100eV_image3_5e-6'
    ; path_data = path_data[24:*]

    ; ;
    ; ; exclude one dataset from Round3 because it crashes IDL, presumably because there is too much data
    ; ;
    ; pos = strpos( path_data, 'N2/100eV/test_8_image1' )
    ; ndx = where( pos eq -1, /null )
    ; path_data = path_data[ndx]
    ; ndir = n_elements(path_data)
    ;
    ; pos = strpos( path_data, 'Base_pressure' )
    ; ndx = where( pos eq -1, /null )
    ; path_data = path_data[ndx]

    ;
    ; print out the dataset paths that will be processed
    ;
    ndir = n_elements(path_data)
    print, 'data to process:'
    for i = 0, ndir - 1 do print, i, ' ', path_data[i]
    print, ' '

    ;
    ; Based on selected dataset, define subfolders where reduced and calibrated data will be stored
    ;
    path_ref = path_base + dataset_id + path_sep()
    path_save = path_ref + 'data_reduction' + path_sep()
    ;
    ; check to make sure the required paths exist
    ;
    path_ref_test = file_search(path_ref, /mark)
    path_save_test = file_search(path_save, /mark)
    if path_ref_test eq '' then begin
      print, 'Expected this path to exist: ', path_ref
      stop
    endif
    if path_save_test eq '' then begin
      print, 'Expected this path to exist: ', path_save
      stop
    endif

    ; stop

    ;
    ; set this flag to 1 to overwrite a previously processed dataset if it exists
    ;
    reprocess_flag = 0
    experimental_flag = 0

    ;
    ; process the data and create summary plots
    ;
    for i = 0, ndir - 1 do begin
      print, i, ' processing: ', path_data[i]
      tic
      fsave_vec = ''
      ajello_lab_process, path_data[i], path_save, fsave_vec, reprocess_flag = reprocess_flag, experimental_flag = experimental_flag
      print, fsave_vec
      if fsave_vec ne !null then begin
        if fsave_vec[0] ne '' then begin
          for j = 0, n_elements(fsave_vec) - 1 do begin
            ajello_lab_image_summary_rot, fsave_vec[j], win, yr = yr_spec
            win.save, path_save + file_basename(fsave_vec[j], '.idl') + '.png'
          endfor

          toc
        endif
      endif
      ; stop
    endfor
  endfor
  stop
end
