;
; PURPOSE
; Routine takes in a folder path and searches all elements
; of a certain type for certain keywords
;
; INPUTS
; folder_path: path to the folder to search (must contain final "/")
; file_type: type of files to search (ex .txt) include '.'
; keyword_1: first keyword to search
; keyword_2: second keyword to search (if only one keyword, repeat keyword 1 )
;
; OUTPUTS
; files: a string array of files that match search criteria
;
pro folder_searcher, folder_path, file_type, keyword_1, files, $
  keyword_2 = keyword_2, $
  keyword_3 = keyword_3
  compile_opt idl2

  ; if not keyword_set(keyword_2) then keyword_2 = keyword_1
  ; if not keyword_set(keyword_3) then keyword_3 = keyword_1

  if not file_test(folder_path) then begin
    print, 'Error: Folder Not Found'
    stop
  endif

  files = file_search(folder_path + '*' + file_type, count = file_count)

  if file_count eq 0 then message, 'No files of ' + file_type + ' found in ' + folder_path

  contains_k1 = intarr(file_count)
  contains_k2 = intarr(file_count)
  contains_k3 = intarr(file_count)

  for i = 0, file_count - 1 do begin
    file = files[i]
    contains_k1[i] = file.contains(keyword_1)
    contains_k2[i] = file.contains(keyword_2)
    contains_k3[i] = file.contains(keyword_3)
  endfor

  cont = contains_k1 and contains_k2 and contains_k3

  temp = where(cont, count)
  if count ne 0 then begin
    files = files[where(cont)]
  endif else files = !null
end
