pro iuvs_data_fit

; real spectra data
  
  file1= "Z:\round10\NeweGun_round10_after_energy_correction\data_reduction\N2_20EV_FUV_TEST13_IMAGE1_HIPRESS.idl"


  fbase = file_basename(file1)
  pos_fuv = strpos(fbase,'FUV')
  pos_muv = strpos(fbase,'MUV')
  if pos_fuv gt -1 then channel='FUV'
  if pos_muv gt -1 then channel='MUV'

  restore,file1,/verbose
  help
  print,'integration time  ',int_time

  ; rotate image
  case channel of
    'FUV': begin
      arr_orig = arr
      arr1=arr*60/int_time
    end
    'MUV': begin
      arr_orig = arr
      arrr=arr
    end
  Endcase

  y1 = 130;outside key hole
  y2 = 940

  spec_mean = mean( arr[*,y1:y2], dimension=2, /nan )  ; for rotated image
  spat_mean = mean( arr         , dimension=1, /nan )  ; for rotated image


  delta_shift=69
  waveuncal = wl
  ;waveuncal=shift(wl,delta_shift); ;

  waveuncal=waveuncal[75:1023]
  ;119.484      119.567      119.650      119.733      119.817      119.900
  ;119.983      120.066      120.149      120.232      120.315      120.398
  ;120.482      120.565      120.648      120.731      120.814      120.897
  ;120.980      121.064      121.147
  ;waveuncal[1200] = 95

  siguncal=spec_mean
  siguncal=siguncal[75:1023];
  ;waveuncal=shift(waveuncal,+9)
  ;stop
  yspa1=yspa;0:1023

  yspa_new1=yspa1-yspa1[!c+130];shift of -5.872 mm
  yspa_new1_correct=yspa_new1[130:940]

  ;need to subtract background again
  aa=total(siguncal[900:930])/31.
  bb=total(siguncal[0:20])/21.
  back=0.5*(aa+bb)
  back=aa

  siguncal=siguncal-back

  ;restore,filename="D:\SSD_I-drive\MAVEN\data_flight\calibration\model\2017\"+'fuv_22sept2017_calibration.save'
  close,1

;  cal=fltarr(949)
;  cal_un=INVSENS_BB*siguncal;un: means unnormalized
;  cal=cal_un/max(smooth(cal_un[260:290],1));max is waveuncal[525=155.98nm];normlaized
; 
  arr1_correct=arr1[75:1023,130:940]
  
; plot, waveuncal, siguncal
 
 
 ; model spectra
 
  temperature = 300 ; K
 p = [temperature]
 x =waveuncal*10D

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

; ;plot, x, model
;
; ; find norm index
; range_indices = where((x GE 1200) AND (x LE 1400), count) ; x range
;
; y_subset = model[range_indices]
; max_val = max(y_subset, local_idx)
; norm_idx = range_indices[local_idx]
;
; print, 'Normalizing to index: ', norm_idx   ; 140
; print, 'Wavelength: ', x[norm_idx]  ;   1237.2368 Ang
; print, 'Pre-normalization value: ', model[norm_idx]  ; 54.176118
;
; ; Normalize
; model_norm = model / model[norm_idx]
; data_norm =    siguncal / siguncal[norm_idx]
;
; print, 'Post-normalization value: ', model_norm[norm_idx] ; 1.0000000
;
;;plot ,x, model_norm
;;oplot, x, data_norm, LINESTYLE = 2
;
; ;plot, x, siguncal
;  print, siguncal[norm_idx]


x_model = waveuncal * 10D  ; model wavelengths in Angstroms
x_data = waveuncal * 10.0  ; data wavelengths in Angstroms

; Normalize model and data in overlapping region
range_indices = where((x_model GE 1400) AND (x_model LE 1500), count)
y_subset = model[range_indices]
max_val = max(y_subset, local_idx)
norm_idx = range_indices[local_idx]

print, 'Norm wavelength (Å): ', x_model[norm_idx]
print, 'siguncal at norm_idx: ', siguncal[norm_idx]

model_norm = model / model[norm_idx]
data_norm  = siguncal / siguncal[norm_idx]

print, x_model
print, x_data

 
;plot, x_model, model_norm, xrange = [1300,1700]
plot, x_data, data_norm, linestyle=2, xrange = [1300,1700]

p1 = plot( x_data, shift(data_norm,-75), linestyle=0, xr=[1100,1800], xtitle='wavelength (angstroms)', font_size=16 )
p2 = plot( x_model, model_norm*5., /over, color='red' )

stop

  end
  
 ;pick two peaks and caclculate the delta x between them so you can go forward pixel by pixel. dx = x2-x1 / d wavelengths . need starting point (initial refernce point (use about 120 nm on the 100ev plot big spike)
 ;plot gold data should lign up?