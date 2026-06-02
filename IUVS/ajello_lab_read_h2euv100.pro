;+
; PURPOSE
;  This routine will read in the H2 emission line list (wavelength, magnitude)
;   produced by Xianming Liu, acquired from Joe Ajello in May 2026
;   
; INPUTS
;  none
; 
; OUTPUTS
;  wave: sorted wavelength, angstroms
;  mag: magnitude, units unknown
;
; KEYWORDS
;  file: (optional) full path filename of the data file "h2euv100.txt",
;   must be provided if not using GitHub repo
;-
pro ajello_lab_read_h2euv100, wave, mag, file=file

;
; **** DO NOT EDIT THE PATH BELOW ****
;
if keyword_set(file) eq 0 then begin
  ajello_lab_set_paths, path_base, path_repo  
  file = path_Ref + 'h2euv100.txt'
endif

if file_test(file) eq 0 then begin
  print, 'file not found: '
  print, file
  stop
endif

nlines = file_lines(file)
nhdr = 2
nrow = nlines - nhdr
wave = dblarr(nrow)
mag = fltarr(nrow)
str = ''
openr,fid,file,/get_lun
for i = 0, nhdr - 1 do $
  readf,fid,str
c1 = 1-1
c2 = 15-1
len1 = c2-c1+1
c3 = 16-1
c4 = 28-1
len2 = c4-c3+1
for i = 0, nrow - 1 do begin
  readf,fid,str
  wave[i] = double( strmid( str, c1, len1 ) )
  mag[i] = float( strmid( str, c3, len2 ) )
endfor
close,fid
free_lun,fid

ndx_sort = sort( wave )
wave = wave[ndx_sort]
mag = mag[ndx_sort]

end