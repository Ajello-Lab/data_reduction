;+
; PURPOSE
;  This routine will return a reference H2 emission spectrum that has
;   been convolved with the IUVS FUV line-spread function
; 
; INPUTS
;  none
; 
; OUTPUTS
;  wave: wavelength, nm
;  spec: spectrum, arbitrary units
;-
pro ajello_lab_h2_reference, wave, spec, $
  show_plots=show_plots

ajello_lab_set_paths, path_base, path_repo

path_ref = path_repo + '/data/H2/'
file_1998 = path_ref + 'OUT_1998.DAT'
file_2011 = path_ref + 'OUT_20N_2011.DAT'
file_liu = path_ref + 'h2ag11_liu.cal'
file_liu2 = path_Ref + 'h2euv100.txt'

;data = (read_ascii(file_liu)).(0)
;wl_liu = reform(data[0,*])
;spec_liu = reform(data[1,*])
;
;data = (read_ascii(file_1998,data_start=2)).(0)
;wl_1998 = reform(data[0,*])
;spec_1998 = reform(data[1,*])

ajello_lab_read_h2euv100, file_liu2, wave_line, mag_line

wave = findgen(1250)+750  ;
spec = fltarr(n_elements(wli))
nwave_ref = n_elements(wave_line)
for i = 0, nwave_ref - 1 do begin
  scaled_iuvs_psf_model_gmh, wave_line[i], wave, ff
  spec += ff * mag_line[i]
endfor

if keyword_set(show_plots) then begin
  p1 = plot( wave, spec )
  stop
endif

end