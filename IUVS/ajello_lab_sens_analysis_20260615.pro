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

  if file_test(set_1_path + '*') then begin
    set_1_fpaths = file_search(set_1_path + '*')
    for i = 0, n_elements(set_1_fpaths) - 1 do begin
      if set_1_fpaths[i].contains('.') then begin
        set_1_fpaths[i] = ''
      endif
    endfor
    set_1_fpaths = set_1_fpaths[where(set_1_fpaths ne '')]
  endif else begin
    print, 'Sens_Set_1 contains no folders'
    stop
  endelse

  set_2_fpaths = file_search(set_2_path + '*', /test_directory, count = count)

  if count eq 0 then $
    message, 'Sens_Set_2 contains no folders'

  for i = 0, n_elements(set_1_fpaths) - 1 do $
    sens_file_compounder, set_1_fpaths[i], set_1_path
  for i = 0, n_elements(set_2_fpaths) - 1 do $
    sens_file_compounder, set_2_fpaths[i], set_2_path

  folder_searcher, set_1_path, '.csv', 'H2', set_1_H2_csvs
  folder_searcher, set_1_path, '.csv', 'N2', set_1_N2_csvs
  folder_searcher, set_2_path, '.csv', 'H2', set_2_H2_csvs
  folder_searcher, set_2_path, '.csv', 'N2', set_2_N2_csvs

  sens_line_synthesizer, set_1_H2_csvs, set_1_N2_csvs, set_2_H2_csvs, set_2_N2_csvs, save_path, $
    fit_type = 'polynomial'
end
