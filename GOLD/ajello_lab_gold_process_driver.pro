;+
; NAME:
; AJELLO_LAB_GOLD_PROCESS_DRIVER
; 
; PURPOSE:
; This routine will process multiple GOLD datasets.
;
; INPUTS
; None.
;
; OUTPUTS
; None.
;
;-
pro ajello_lab_gold_process_driver

  source_routine = file_basename(routine_filepath(),'.pro')

  ajello_lab_set_paths
  path_base = !path_base+'GOLD' + path_sep()

  data_set_id = 'big_e-gun_round_2'
  data_set_id = 'big_e-gun_round_3'
  data_set_id = 'kimball_egun_round_11'
  path_data = file_search( path_base + data_set_id + path_sep(), 'test*', /test_dir, count=num_data_sets )
  if num_data_sets eq 0 then begin
    print, 'no data found'
    stop
  endif

;  ;
;  ; eliminate "backup" data sets, which were terminated early due to some anomaly 
;  ;
;  pos_backup = strpos( path_data, 'backup' )
;  ndx_good = where( pos_backup eq -1 )
;  path_data = path_data[ndx_good]
;  num_data_sets = n_elements(path_data)
;  
;  ; /Volumes/projects/Phase_Development/MAVEN/IUVS_Data/IUVS_Breadboard/GOLD/kimball_egun_round_11/N2/100eV/hi-pres/test2_image1
;
;  ;path1 = '/Volumes/projects/Phase_Development/MAVEN/IUVS_Data/IUVS_Breadboard/GOLD/big_e-gun_round_2/N2/30eV/low_pres/'
;  ;path1 = '/Volumes/projects/Phase_Development/MAVEN/IUVS_Data/IUVS_Breadboard/GOLD/big_e-gun_round_2/N2/100eV/low_pres/'
;  ;path1 = '/Volumes/projects/Phase_Development/MAVEN/IUVS_Data/IUVS_Breadboard/GOLD/big_e-gun_round_3/CO2_CO/'
;  path1 = '/Volumes/projects/Phase_Development/MAVEN/IUVS_Data/IUVS_Breadboard/GOLD/big_e-gun_round_3/CO2/'
;  path1 = '/Volumes/projects/Phase_Development/MAVEN/IUVS_Data/IUVS_Breadboard/GOLD/kimball_egun_round_11/N2/100eV/hi-pres/'
;  path_data = file_search( path1, 'test*', /test_dir, count=num_data_sets )
;  


;  path_data = '/Volumes/projects/Phase_Development/MAVEN/IUVS_Data/IUVS_Breadboard/GOLD/big_e-gun_round_1/N2/40eV/med_pres/test1_image1/'
;  num_data_sets = n_elements(path_data)

  ;
  ; Define the path where summary images and save files will be stored. 
  ;
  path_save = path_base + data_set_id + path_sep() + 'data_reduction' + path_sep()
  
  ;
  ; pulse height filter thresholds
  ; only pmin < p_event < pmax will be used to aggregate into 2D images 
  ;
  pmin = 0
  pmax = 250
  pmin = 112
  
  ; incomplete:
  ;/Volumes/projects/Phase_Development/MAVEN/IUVS_Data/IUVS_Breadboard/GOLD/big_e-gun_round_2/N2/15eV/med_pres/test3_image3/GOLD_15eV_N2_1e-5_test3_image3_20200128_211015.fits-001.fits
  ;/Volumes/projects/Phase_Development/MAVEN/IUVS_Data/IUVS_Breadboard/GOLD/big_e-gun_round_2/N2/30eV/high_pres/test2_image2_dark_before/GOLD_30eV_N2_5e-5_test2_image2_dark_20200131_134923.fits-001.fits
  ;/Volumes/projects/Phase_Development/MAVEN/IUVS_Data/IUVS_Breadboard/GOLD/big_e-gun_round_2/N2/30eV/med_pres/test2_image2/GOLD_100eV_N2_1e-5_test2_image2_20200126_190311.fits-017.fits
  ;/Volumes/projects/Phase_Development/MAVEN/IUVS_Data/IUVS_Breadboard/GOLD/big_e-gun_round_2/N2/40eV/med_pres/test2_image2_dark_before/GOLD_40eV_N2_1e-5_test2_image1_dark_20200130_185315.fits-003.fits
  
  ;i = 24, 41 are incomplete or have corrupt files
  
  tic
  for i = 0, num_data_sets - 1 do begin
    print, i+1, ' of ', num_data_sets, ' datasets'
    print, 'path = ', path_data[i]
    print, ' '
    ajello_lab_gold_process, path_data[i], wl, xp, yp, cbin, phd, hdr_list, $
      plt, plt_phd_bin, pmin=pmin, pmax=pmax, buffer=1
    
    ;ajello_lab_gold_phd_segmentation, path_data[i], plt_phd_bin
    
    str = strsplit(path_data[i],path_sep(),/extract)

    ;path_save = '/Users/holsclaw/GOLD/data/big_e-gun_round_1/data_reduction/'
    ;path_save = path_base+path_sep()+data_set_id+path_sep()+'data_reduction'+path_sep()
    
    file_phdbin_png  = path_save + str[-4] + '_' + str[-3] + '_' + str[-2] + '_' + str[-1] + '_pbin.png'
    file_summary_png = path_save + str[-4] + '_' + str[-3] + '_' + str[-2] + '_' + str[-1] + '_pmax_' + string(pmax,format='(I03)') + '.png'
    file_summary_sav = path_save + str[-4] + '_' + str[-3] + '_' + str[-2] + '_' + str[-1] + '_pmax_' + string(pmax,format='(I03)') + '.sav'

    plt_phd_bin.save,file_phdbin_png

    plt.save,file_summary_png

    ;int_time_per_file = sxpar( hdr_list[0], 'INT-TIME' )
    ;duration = int_time_per_file * n_elements(hdr_list)
    
    if n_elements(hdr_list) gt 0 then begin
      int_time_file = fltarr( n_elements(hdr_list) )
      for j = 0, n_elements(hdr_list) - 1 do int_time_file[j] = sxpar( hdr_list[j], 'INT-TIME' )
      duration = total( int_time_file )
      
      exp_desc = { data_set_id:str[-5], gas:str[-4], energy:str[-3], pressure_id:str[-2], test_id:str[-1], path:path_data[i] }
      
      var_desc = [ $
        'wl: wavelength in nanometers', $
        'xp: detector pixel value in the spectral dimension', $
        'yp: detector pixel value in the spatial dimension', $
        'cbin: 2D summed and binned array', $
        'phd: pulse-height distribution of the data region, without the PH filter applied', $
        'hdr_list: list data type with each element containing the contents of the FITS header', $
        'duration: total duration of the dataset in seconds', $
        'exp_desc: data set id, gas, energy, pressure id, test id, full data path' ]
      
      process_time = systime()
      save,filename=file_summary_sav, wl, xp, yp, cbin, phd, hdr_list, duration, exp_desc, var_desc, source_routine,process_time    
    endif else begin
      print, 'no elements found in hdr_list, skipping this dataset:'
      print, path_data[i]
    endelse
    
;stop

  endfor
  toc

stop

end