
;+
; NAME:
; AJELLO_LAB_SUMMARY_PLOTS
; 
; PURPOSE:
; This routine will regenerate the summary plots for a specified path containing a set of IDL save files 
;-
pro ajello_lab_summary_plots

  path = '/Volumes/projects/Phase_Development/MAVEN/IUVS_Data/IUVS_Breadboard/_Big_e-gun_RoundVII/data_reduction/'
  file = file_search(path,'*.idl',count=nfiles)
  
  ;path_save = '/users/holsclaw/temp/ajello_roundVII_plots/'
  path_save = path
  
  ;
  ; iterate through each IDL save file and create summary plot
  ;
  tic
  for i = 0, nfiles - 1 do begin
    print, i, ' processing: ', file[i]
    ajello_lab_image_summary_rot, file[i], win ;, yr=yr_spec
    win.save,path_save+file_basename(file[i],'.idl')+'.png'
    ;stop
  endfor
  toc
  
  stop
  
end