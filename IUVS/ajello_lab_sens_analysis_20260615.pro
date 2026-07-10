;
; PURPOSE
; Take in a folder containing folders containing sens_data files that can be compounded
; into a single sensitivity comparision
;
; INPUTS
; sens_set_1: first set of data to be compared containing folders with various sets of data
; sens_set_2: same as above, just the second set
; sens_saved: the folder to which final products will be presented
;
pro ajello_lab_sens_analysis_20260615
  compile_opt idl2

  ; USER INPUTS
  set_1_path = '/Users/benjamincondit/Desktop/Sens_Main/Sens_set_1/'
  set_2_path = '/Users/benjamincondit/Desktop/Sens_Main/Sens_set_2/'
  save_path = '/Users/benjamincondit/Desktop/Sens_Main/sens_saved/'
  qualities = 'High/Medium' ; enter with '/' and uppercase first letter

  set_1_folder_indicator = 0
  set_2_folder_indicator = 0

  set_1_fpaths = file_search(set_1_path + '*.txt', count = count1files)
  if count1files eq 0 then begin
    set_1_folder_indicator = 1
    set_1_fpaths = file_search(set_1_path + '*', /test_directory, count = count1folders)
    if count1folders eq 0 then $
      message, 'Sens_set_1 contains no folders or .txt files'
  endif else $
    set_1_fpaths = [set_1_path]

  set_2_fpaths = file_search(set_2_path + '*.txt', count = count2files)
  if count2files eq 0 then begin
    set_2_folder_indicator = 1
    set_2_fpaths = file_search(set_2_path + '*', /test_directory, count = count2folders)
    if count2folders eq 0 then $
      message, 'Sens_set_2 contains no folders or .txt files'
  endif else $
    set_2_fpaths = [set_2_path]

  for i = 0, n_elements(set_1_fpaths) - 1 do $
    sens_file_compounder, set_1_fpaths[i], set_1_path
  for i = 0, n_elements(set_2_fpaths) - 1 do $
    sens_file_compounder, set_2_fpaths[i], set_2_path

  if set_1_folder_indicator then begin
    folder_searcher, set_1_path, '.csv', 'H2', set_1_H2_csvs
    folder_searcher, set_1_path, '.csv', 'N2', set_1_N2_csvs
  endif else begin
    folder_searcher, set_1_path, '.csv', '', set_1_H2_csvs
    folder_searcher, set_1_path, '.csv', '', set_1_N2_csvs
  endelse

  if set_2_folder_indicator then begin
    folder_searcher, set_2_path, '.csv', 'H2', set_2_H2_csvs
    folder_searcher, set_2_path, '.csv', 'N2', set_2_N2_csvs
  endif else begin
    folder_searcher, set_2_path, '.csv', '', set_2_H2_csvs
    folder_searcher, set_2_path, '.csv', '', set_2_N2_csvs
  endelse

  sens_line_synthesizer, set_1_H2_csvs, set_1_N2_csvs, set_2_H2_csvs, set_2_N2_csvs, save_path, $
    fit_type = 'polynomial', quality_types = qualities, combine_all = 1
end
