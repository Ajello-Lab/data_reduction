;
; PURPOSE
; takes in an array of file names and turns the file names into structures of data
;
; INPUTS
; file_arr: The array of file names that are to be read
; file_type: type of file to be read (use .type eg. '.idl')
;
; OUTPUT
; arrs_out: list of outputted hash tables
;
pro data_retriever, file_arr, file_type, arrs_out
  compile_opt idl2

  case file_type of
    '.idl': begin
      arrs_out = list()
      for i = 0, n_elements(file_arr) - 1 do begin
        print, 'File ' + string(i + 1)

        sObj = obj_new('IDL_Savefile', file_arr[i])
        sObj.restore, sObj.names()
        obj_destroy, sObj
        ; arr = float(arr)
        arr_median = float(arr_median)

        ; sig = 0.1 * arr_light_mad
        ; sig_data = where(arr_median gt sig)

        arrs_out.add, arr_median
        ; arrs_out.add, sig_data
      endfor
    end
  endcase
end
