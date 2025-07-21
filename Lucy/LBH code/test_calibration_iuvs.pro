pro test_calibration_iuvs

; calibration, normalization, and plotting with the model and shifted


; ***** DATA *****
lab_data= "Z:\round10\NeweGun_round10_after_energy_correction\data_reduction\N2_20EV_FUV_TEST13_IMAGE1_HIPRESS.idl"
restore, filename="C:\Users\lufa5942\flight_data_fuv"
calibration_file = "C:\Users\lufa5942\2017fuv_calibration"

restore, filename=lab_data
restore, filename=calibration_file
help

fbase = file_basename(lab_data)
pos_fuv = strpos(fbase,'FUV')
pos_muv = strpos(fbase,'MUV')
if pos_fuv gt -1 then channel='FUV'
if pos_muv gt -1 then channel='MUV'

restore,lab_data,/verbose
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

waveuncal = wl
waveuncal=waveuncal[75:1023]
siguncal=spec_mean
siguncal=siguncal[75:1023]

aa=total(siguncal[900:930])/31.
bb=total(siguncal[0:20])/21.
back=0.5*(aa+bb)
back=aa
siguncal=siguncal-back

;calibrate
sigcal = (1/INVSENS_BB) * siguncal ; calibrated, unnormalized
;cal = sigcal / max(smooth(sigcal[260:290], 1))  ; normalized

; debug
print, 'Min/max of siguncal:', min(siguncal), max(siguncal) ; max=96.9352
print, 'Min/max of INVSENS_BB:', min(1/INVSENS_BB), max(1/INVSENS_BB)  ; max=1.1876664
print, 'Min/max of sigcal:', min(sigcal), max(sigcal) ; max=96.935234

temperature = 300 ; K
p = [temperature]
x =waveuncal*10D


; ***** MODEL *****
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


; ***** NORMALIZE *****
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
data_norm  = sigcal / sigcal[norm_idx]

;print, x_model
;print, x_data

; ***** PLOT *****
p1 = plot( x_data, shift(data_norm,-75), linestyle=0, xtitle='wavelength (angstroms)', font_size=16 ) ;xr=[1100,1800]
p2 = plot( x_model, model_norm*5., /over, color='red' )

;p1 = plot( x_data, shift(sigcal,-75), linestyle=0, xtitle='wavelength (angstroms)', font_size=16 ) ; calibrated, unnormalized

stop

end
