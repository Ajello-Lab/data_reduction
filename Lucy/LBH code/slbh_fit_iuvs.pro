pro slbh_fit_iuvs

  ; IUVS calibration data
  restore,filename="D:\SSD_I-drive\MAVEN\data_flight\fuv_sonal\fuv_hifi_data.sav";flight data
  help
  ;stop
  wavelength_fuv=wavenm
  calibrated_fuv=fuv_AIRGLOW_IMG[*,30]/dn_conversion;150 km 30*5=150 km
  calibrated_fuv=calibrated_fuv/max(calibrated_fuv[580:600]);-range of 154.7 to 156.3 where !c is 598 and wavelenght [598] is 156.2nm
  n2_lbh = fitvec[3,*]
  ni1494=fitvec[6,*]
  n2_lbh_BB=[fltarr(124),n2_lbh[0:195],fltarr(240)]
  

  
;stop
  ; plot,wavenm[120:*],calibrated_fuv[120:*]
  ;plot, wavelengths, result[0,*],
  ;oplot, wavelengths, result[2,*], color='blue'
  ;oplot, wavelengths, result[4,*], color='green'
  ;oplot, wavelengths, result[6,*], color='red'
  
  temperature = 300 ; K
  
  p = [temperature]
  x = wavenm*10D

  v=[0.0710558,     0.133515,     0.162337,     0.170232,     0.159273,     0.109679,    0.0719076]; vib pop of may 14-16 13-17 UT 60-70sza
  aa=slbh2(p[0],x,population_in=v)
  shp=size(aa)
  model=fltarr(shp[1])
  A=[1, 0, 0.80,       22.9622]
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

;plot, x, model

; find norm index
range_indices = where((x GE 1400) AND (x LE 1500), count) ; x range

y_subset = model[range_indices]
max_val = max(y_subset, local_idx)
norm_idx = range_indices[local_idx]

print, 'Normalizing to index: ', norm_idx   ; 482
print, 'Wavelength: ', x[norm_idx]  ;  1467.2454 Ang
print, 'Pre-normalization value: ', model[norm_idx]  ; 242.02842

; Normalize
model_norm = model / model[norm_idx]
data_norm =   n2_lbh_BB /   n2_lbh_BB[norm_idx]

print, 'Post-normalization value: ', model_norm[norm_idx] ; 1.0000000

plot ,x, model_norm
oplot, x, n2_lbh_BB, LINESTYLE = 2


end