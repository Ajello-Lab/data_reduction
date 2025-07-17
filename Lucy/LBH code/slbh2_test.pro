 pro slbh2_test

file= "C:\Users\lufa5942\Documents\GOLD\gold_spectra_diff_alt.txt"

dat=read_ascii(file)
rdat=dat.FIELD001

wavelengths=rdat[*,0] ;wavelengths
ndx_138 = where( wavelengths gt 138.3-0.6 and wavelengths lt 138.3+1.9 )
data=(rdat[*,2]);; uncertainity 0-100

print,ndx_138
temperature = 300 ; K

result = slbh2(temperature, wavelengths*10D)
 
; checks
 print, 'Returned result dimensions: ', size(result)
 print, min(result[*,0]), max(result[*,0])
 print, min(wavelengths), max(wavelengths)


; gold fit
  p = [temperature]
  x = wavelengths[1:*]*10D
  ;print,x

;FUNCTION MYFUNCT, p,dp, X=x, Y=y, ERR=err
  v=[0.0710558,     0.133515,     0.162337,     0.170232,     0.159273,     0.109679,    0.0719076]; vib pop of may 14-16 13-17 UT 60-70sza
  aa=slbh2(p[0],x,population_in=v)
  shp=size(aa)
  model=fltarr(shp[1])
  A=[1, 0, 0.8,       22.9622]
  ;apply psf and sum over different vibrational spectra
  for w=0, shp[1]-1 do begin;all wl
    for n=0,shp[2]-1 do begin
      a[0]=aa[w,n]
      a[1]=x[w]
      iuvs_psf_model,x+0,a,f
      ;print, size(f)
      model=model+f
    endfor
  endfor
  

; find norm index 
range_indices = where((x GE 1400) AND (x LE 1500), count) ; x range

y_subset = model[range_indices]
max_val = max(y_subset, local_idx)
norm_idx = range_indices[local_idx]

print, 'index: ', norm_idx   ; 298
print, 'Wavelength: ', x[norm_idx]  ;  1464.9934 Ang
print, 'Pre-normalization value: ', model[norm_idx]  ; 258.11024

; Normalize
model_norm = model / model[norm_idx]
data_norm = data / data[norm_idx]

print, 'Post-normalization value: ', model_norm[norm_idx] ; 1.0000000

 plot, x, model_norm
 oplot, x, data_norm, LINESTYLE = 2

 
 end