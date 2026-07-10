;
; PURPOSE
; Combines the provided files into a single plot displaying the before and after of the combination
;
pro plot_combiner
  compile_opt idl2

  ; USER INPUTS:
  folder_paths = ['/Users/benjamincondit/Desktop/Final Sens/']
  ; folder_paths = ['/Users/benjamincondit/Desktop/Sens_Main/Sens_set_1/', '/Users/benjamincondit/Desktop/Sens_Main/Sens_set_2/']
  save_path = '/Users/benjamincondit/Desktop/Plot_combiner_Outputs/'

  N2file_paths = []
  H2file_paths = []
  H2file_paths_new = []

  for i = 0, n_elements(folder_paths) - 1 do begin
    folder_searcher, folder_paths[i], '.txt', 'N2', N2_files
    N2file_paths = [N2file_paths, N2_files]
    folder_searcher, folder_paths[i], '.txt', 'H2_', H2_files
    H2file_paths = [H2file_paths, H2_files]
  endfor

  if n_elements(N2file_paths) ne 0 then begin
    avg_data, N2_files, N2_avg

    for i = 0, n_elements(H2file_paths) - 1 do begin
      avg_data, H2file_paths[i], H2_struct ; takes in a file and returns a structure of said file but doesn't average since only one file is inputted
      normalize_to, H2_struct, N2_avg
      csv_name = save_path + file_basename(H2file_paths[i]) + '_Normalized.csv'
      write_csv, csv_name, H2_struct
      H2file_paths_new = [H2file_paths_new, csv_name]
    endfor

    print, 'Normalization Finished'
    file_paths = [H2file_paths_new, N2file_paths]
  endif else $
    file_paths = [H2file_paths, N2file_paths]

  sens_file_compounder, file_paths, save_path, actually_files = 1, save_csv = 1, title = 'Final Curve'
end
