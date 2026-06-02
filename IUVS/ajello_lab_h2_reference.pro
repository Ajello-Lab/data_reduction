;+
; PURPOSE
;  This routine will return a reference H2 emission spectrum that has
;   been convolved with the IUVS FUV line-spread function
; 
; INPUTS
;  file: full path filename of the data file "h2euv100.txt"
; 
; OUTPUTS
;  wave: wavelength, nm
;  spec: spectrum, arbitrary units
;  
; KEYWORDS
;  refresh: set to 1 to read in the line list and convolve with the PSF
;   even if the save file exists
;   
;-
pro ajello_lab_h2_reference, wave, spec, $
  refresh = refresh, $
  show_plots = show_plots, $
  file_ref = file_ref

if n_params() eq 0 then begin
  ajello_lab_set_paths, path_base, path_repo
  path_ref = path_repo + 'data/H2/'
  file_ref = path_ref + 'h2euv100.txt'

;  file_1998 = path_ref + 'OUT_1998.DAT'
;  file_2011 = path_ref + 'OUT_20N_2011.DAT'
;  file_liu = path_ref + 'h2ag11_liu.cal'
;  file_liu2 = path_Ref + 'h2euv100.txt'

  ;data = (read_ascii(file_liu)).(0)
  ;wl_liu = reform(data[0,*])
  ;spec_liu = reform(data[1,*])
  ;
  ;data = (read_ascii(file_1998,data_start=2)).(0)
  ;wl_1998 = reform(data[0,*])
  ;spec_1998 = reform(data[1,*])
endif

if file_test(file_ref) eq 0 then begin
  print, 'file not found: '
  print, file_ref
  stop
endif

file_path = file_dirname( file_ref, /mark_directory )

file_save = file_path + file_basename( file_ref, '.txt' ) + '.sav'

if (file_test(file_save) eq 0) or keyword_set(refresh) then begin

  ajello_lab_read_h2euv100, wave_line, mag_line, file=file_ref

  wave = findgen(1250)+750  ;
  spec = fltarr(n_elements(wave))
  nwave_ref = n_elements(wave_line)
  for i = 0, nwave_ref - 1 do begin
    scaled_iuvs_psf_model_gmh, wave_line[i], wave, ff
    spec += ff * mag_line[i]
  endfor
  ;
  ; Because the above takes a long time, store the result in an IDL save
  ;  file for quick reload at next execution. 
  ;
  routine_name = routine_filepath()
  save, filename=file_save, file_ref, wave, spec, routine_name

endif else begin
  
  restore, file_save
  
endelse

if keyword_set(show_plots) then begin
  p1 = plot( wave, spec )
  stop
endif

end