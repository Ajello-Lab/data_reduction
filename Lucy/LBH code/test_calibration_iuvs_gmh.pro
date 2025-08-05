pro test_calibration_iuvs_GMH

; calibration, normalization, and plotting with the model and shifted

ajello_lab_set_paths
path_iuvs = !path_repo + 'IUVS' + path_sep()

; ***** DATA *****
user_name = (get_login_info()).user_name
case user_name of
  'holsclaw': begin
    path_lab_data = '/Volumes/projects/Phase_Development/MAVEN/IUVS_Data/IUVS_Breadboard/NeweGun_round10_after_energy_correction/data_reduction/'
    ;file_calibration = '/Users/holsclaw/Downloads/2025-07-17_joe/fuv_22sept2017_calibration.save'
  end
  'lufa5942': begin
    path_lab_data = "Z:\round10\NeweGun_round10_after_energy_correction\data_reduction\"
    ;file_calibration = "C:\Users\lufa5942\2017fuv_calibration"
  end
endcase
file_calibration = path_iuvs + 'fuv_22sept2017_calibration.save'
file_lab_data = path_lab_data + 'N2_20EV_FUV_TEST13_IMAGE1_HIPRESS.idl'
;lab_data= "Z:\round10\NeweGun_round10_after_energy_correction\data_reduction\N2_20EV_FUV_TEST13_IMAGE1_HIPRESS.idl"
;restore, filename="C:\Users\lufa5942\flight_data_fuv"
;calibration_file = "C:\Users\lufa5942\2017fuv_calibration"

;restore, filename=lab_data
;restore, filename=calibration_file
;help

restore,file_lab_data,/ver
;% RESTORE: Restored variable: ARR.
;% RESTORE: Restored variable: WL.
restore,file_calibration
;RESTORE: Restored variable: LAMBDA_FLIGHT.
;% RESTORE: Restored variable: SENS_BB.
;% RESTORE: Restored variable: INVSENS_BB.
;% RESTORE: Restored variable: SENS_IUVS_1024_MARS_FLIGHT.
;% RESTORE: Restored variable: FINAL_WAVE_2015.
;% RESTORE: Restored variable: FINAL_SENS_2015.


;fbase = file_basename(lab_data)
;pos_fuv = strpos(fbase,'FUV')
;pos_muv = strpos(fbase,'MUV')
;if pos_fuv gt -1 then channel='FUV'
;if pos_muv gt -1 then channel='MUV'

;restore,lab_data,/verbose
help
print,'integration time  ',int_time

;; rotate image
;case channel of
;  'FUV': begin
;    arr_orig = arr
;    arr1=arr*60/int_time
;  end
;  'MUV': begin
;    arr_orig = arr
;    arrr=arr
;  end
;Endcase

y1 = 130;outside key hole
y2 = 940

spec_mean = mean( arr[*,y1:y2], dimension=2, /nan )  ; for rotated image
spat_mean = mean( arr         , dimension=1, /nan )  ; for rotated image

;
; estimate the background signal by calculating the mean value at the
;  short- and long-wavelength ends of the spectrum where emission is minimal 
;
x1 = 0
x2 = 100
x3 = 1023-100
x4 = 1023
background = ( mean( spec_mean[x1:x2] ) + mean( spec_mean[x3:x4] ) ) / 2.

;
; show the spectrum, defined boundary values, and background signal 
;
p = plot( spec_mean, /ylog )
markerp,p,x=x1, linestyle=2
markerp,p,x=x2, linestyle=2
markerp,p,x=x3, linestyle=2
markerp,p,x=x4, linestyle=2
markerp,p,y=0,  linestyle=2
markerp,p,y=background, linestyle=2

sens_bb_i = interpol( sens_bb-min(sens_bb), LAMBDA_FLIGHT, wl )
n = where( sens_bb_i lt 0. )
sens_bb_i[n] = !values.f_nan

win = window(dim=[800,600])
p1 = plot( LAMBDA_FLIGHT, sens_bb, name='original', current=win, font_size=16, $
  xtitle='wavelength (nm)' )
;p2 = plot( wl, sens_bb_i, /over, color='blue', thick=3, name='interpolated and truncated' )
;leg = legend(target=[p1,p2]) 
markerp,p1,y=0,linestyle=2
path_save = '/Users/holsclaw/Downloads/2025-07-17_joe/'
;win.save,path_save + 'iuvs_bb_sensitivity_joe_Save_file.png'


cal = ( spec_mean - background ) / sens_bb_i

p = plot( wl, cal )

;p1 = plot( LAMBDA_FLIGHT, (sens_bb-min(sens_bb))*30. )
;p2 = plot( wl, spec_mean, /over, color='red' )

win = window(dim=[800,600])
p1 = plot( LAMBDA_FLIGHT, (sens_bb)*40., thick=2, current=win )
p2 = plot( wl, spec_mean, /over )
;path_save = '/Users/holsclaw/Downloads/2025-07-17_joe/'
;win.save,path_save + 'iuvs_bb_sensitivity.png'


;stop
;
;waveuncal = wl
;waveuncal=waveuncal[75:1023]
;siguncal=spec_mean
;siguncal=siguncal[75:1023]
;
;aa=total(siguncal[900:930])/31.
;bb=total(siguncal[0:20])/21.
;back=0.5*(aa+bb)
;back=aa
;siguncal=siguncal-back
;
;sigcal = INVSENS_BB * siguncal  ; calibrated, unnormalized
;;cal = sigcal / max(smooth(sigcal[260:290], 1))  ; normalized

temperature = 300 ; K
p = [temperature]
;x =waveuncal*10D
x = wl*10.

; ***** MODEL *****
v=[0.0710558,     0.133515,     0.162337,     0.170232,     0.159273,     0.109679,    0.0719076]; vib pop of may 14-16 13-17 UT 60-70sza
aa=slbh2(p[0],x,population_in=v)
shp=size(aa)
model=fltarr(shp[1])
A=[1, 0, 0.80,       22.9622]
a[2] = 2.0
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

wlv1 = wl
wlv2 = wl
spec1 = spec_mean
spec2 = model * sens_bb_i
lag = findgen(60)-30
lag = findgen(200)/10-10
sm1 = 1
sm2 = 1
wlr1 = 135.
wlr2 = 170.
;ajello_lab_c_correlate_gmh, wlv1, wlv2, spec1, spec2, lag, sm1, sm2, wlr1, wlr2, c, spec2_sm_max, spec1_norm_vec, plots=0
c_correlate_interpolate_reference, wlv1, wlv2, spec1, spec2, lag, sm1, sm2, wlr1, wlr2, c, spec2_sm_max, spec1_norm_vec, lag_max, plots=0


lag_step = indgen(200)-100
c_correlate_interpolate_lag, wlv1, wlv2, spec1, spec2, lag_step, sm1, sm2, $
  wlr1, wlr2, r, lag_vec_root, $
  no_plots=no_plots, $
  step_interp=step_interp

print, 'c_correlate_interpolate_reference: ', lag_max
print, 'c_correlate_interpolate_lag:       ', lag_vec_root * mean(deriv(wlv1))

ndx_wave = where( (wlv1 gt wlr1) and (wlv1 lt wlr2) )
spec1_norm = spec1 / total(spec1[ndx_Wave])
spec2_norm = spec2 / total(spec2[ndx_Wave])

p1 = plot( wlv1-lag_max, spec1_norm )
p2 = plot( wlv2, spec2_norm, /over, color='red' )

spec2_normi = interpol( spec2_norm, wlv2+lag_max, wlv1 )

p1 = plot( wlv1, spec1_norm )
p2 = plot( wlv2, spec2_normi, /over, color='red' )

p1 = plot( wlv1, spec1_norm / spec2_normi, yr=[-1,5] )

stop




win = window(dim=[1200,600])
;p1 = plot( wlv1-lag[ndx_max], spec1*spec1_norm_vec[ndx_max], current=win, $
;  xtitle='wavelength (nm)', ytitle='', font_size=16, name='data (shifted)', $
;  xr=[120,190], thick=2 )
p1 = plot( wlv1, spec1_shifted*spec1_norm_vec[ndx_max], current=win, $
  xtitle='wavelength (nm)', ytitle='', font_size=16, name='data (shifted)', $
  xr=[120,190], thick=2 )
p2 = plot( wlv1, spec2, /overplot, color='red', name='model', thick=2 )
leg = legend(target=[p1,p2],font_size=14)
;path_save = '/Users/holsclaw/Downloads/2025-07-17_joe/'
;win.save,path_save+file_basename(file_lab_data,'.idl')+'_comparison_to_model.png'

win = window(dim=[1200,600])
p1 = plot( wlv1, spec1_shifted*spec1_norm_vec[ndx_max], current=win, $
  xtitle='wavelength (nm)', ytitle='', font_size=16, name='data (shifted)', $
  xr=[130,140], thick=2 )
p2 = plot( wlv1, spec2, /overplot, color='red', name='model', thick=2 )
leg = legend(target=[p1,p2],font_size=14)
markerp,p1,y=0,linestyle=2
markerp,p1,x=131.9,linestyle=2
markerp,p1,x=133.4,linestyle=2
;path_save = '/Users/holsclaw/Downloads/2025-07-17_joe/'
;win.save,path_save+file_basename(file_lab_data,'.idl')+'_comparison_to_model_xzoom.png'

;131,9

stop

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

stop

end
