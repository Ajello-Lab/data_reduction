pro scaled_iuvs_psf_model, x, ff
  ;Gaussian + Lorentzian PSF model
  
  Compile_opt idl2
  loadct,10,/silent
  ;A = [345.231, 121.524, 0.198492, 24.3185, 0.811196, 0.25399]
  A = [345.23, 121.524, 0.3889, 24.332, 1.0846, 0.6560] ; new numbers bc of python -> idl
  
  Adist = 0.083129883
  Bdist = x[1]-x[0]
  B = A
  B[1] = median(x)
  B[2] = A[2] * Bdist/Adist
  B[4] = A[4] * Bdist/Adist

  ff = B[0] * exp( -0.5 * (x-B[1])^2 / B[2]^2 ) + B[3] / (1.+(x-B[1])^2 / B[4]^2) + B[5]
end

pro data_and_model

  ajello_lab_set_paths, path_base, path_repo

  ; ***** DATA *****
  user_name = (get_login_info()).user_name
  case user_name of
    'holsclaw': begin
      path_lab_data = '/Volumes/projects/Phase_Development/MAVEN/IUVS_Data/IUVS_Breadboard/NeweGun_round10_after_energy_correction/data_reduction/'
    end
    'lufa5942': begin
      path_lab_data = "Z:\round10\NeweGun_round10_after_energy_correction\data_reduction\"
    end
  endcase
  file_calibration = path_repo + 'IUVS' + path_sep() + 'fuv_22sept2017_calibration.save'
  ;file_lab_data = path_lab_data + 'N2_20EV_FUV_TEST13_IMAGE1_HIPRESS.idl'
  file_data = path_lab_data + 'N2_16EV_FUV_TEST17_IMAGE1_HIPRESS.idl'
  file_model = path_repo + 'Lucy' + path_sep() + 'LBH code' + path_sep() + 'calibration_victor' + path_sep() + 'n2_lbh_rot_293K.sav'
  path_plots = path_repo + 'IUVS' + path_sep() + 'LBH_calibration' + path_sep() + 'plots' + path_sep()

  ;
  ; check to make sure all data files and paths exist'
  ;
  flag_calibration = file_test(file_calibration)
  flag_data = file_test(file_data)
  flag_model = file_test(file_model)
  flag_path_plots = file_test(path_plots,/directory)
  if flag_calibration eq 0 then begin
    print, 'file does not exist: ', file_calibration
    stop
  endif
  if flag_data eq 0 then begin
    print, 'file does not exist: ', file_data
    stop
  endif
  if flag_model eq 0 then begin
    print, 'file does not exist: ', file_model
    stop
  endif
  if flag_path_plots eq 0 then begin
    print, 'file does not exist: ', path_plots
    stop
  endif

  ;
  ; generate an estimate of the instrument point spread function
  ;  (actually, this is the line-spread function)
  ; 
  ;restore, "C:\Users\lufa5942\Documents\data_reduction\Lucy\LBH code\calibration_victor\n2_lbh_rot_293K.sav",/relax
  restore, file_model, /relax
;  % RESTORE: Recovering incompatible definition of structure LIST using relaxed structure assignment rules.
;  % RESTORE: Recovering incompatible definition of structure HASH using relaxed structure assignment rules.
;  % RESTORE: Restored variable: TROT.
;  % RESTORE: Restored variable: WAVE.
;  % RESTORE: Restored variable: LBH.
;  % RESTORE: Restored variable: LIST.
  scaled_iuvs_psf_model, wave[0:301], psf
  wave_lbh = wave / 10.
  
  lbh_orig = lbh
  ;
  ; convolve each rotational band by the PSF
  ; 
  ; TODO: multiply the high-resolution model by the instrument sensitivity
  ;  prior to convolving by the PSF
  ;
  for i=0, ((size(lbh))[2]-1) do begin   ; joe added this loop?
    lbh[*, i] = convol(lbh[*, i], psf)
  endfor
  
  p1 = plot( wave, lbh_orig[*,0] )
  p2 = plot( wave, lbh[*,0]/total(psf), /over, color='red' )

  lbh_orig_tot = total( lbh_orig, 2 )  
  lbh_tot = total( lbh, 2 ) / total(psf)
  
  p1 = plot( wave, lbh_orig_tot )
  p2 = plot( wave, lbh_tot, /over, color='red' )
  
  
  stop
  
  ;done
  ;
  ; LOAD IN DATA ***********************
  ;restore, "Z:\round10\NeweGun_round10_after_energy_correction\data_reduction\N2_16EV_FUV_TEST17_IMAGE1_HIPRESS.idl", /verbose
  restore, file_data, /verbose
  y1 = 130;outside key hole
  y2 = 940
  spec_mean = mean( arr[*,y1:y2], dimension=2, /nan )  ; for rotated image
  spat_mean = mean( arr         , dimension=1, /nan )
  siguncal=spec_mean
  x1 = 0
  x2 = 100
  x3 = 1023-x2
  x4 = 1023
  wlv1 = wl
  wlv2 = wl
 ; spec1 = spec_mean
  ;spec2 = lbh * sens_bb_i
  ;lag = findgen(60)-30
  ;lag = findgen(200)/10-10
  sm1 = 1
  sm2 = 1
  wlr1 = 135.
  wlr2 = 170

  norm_wavelength = 135.5
;signorm = siguncal / max(smooth(siguncal[260:290], 1))  ; normalized

;
; estimate residual detector background by calculating the average value
;  at the extreme ends of the detector with no expected signal 
;
background = ( mean( spec_mean[x1:x2] ) + mean( spec_mean[x3:x4] ) ) / 2.
sig_back_sub = siguncal - background
sig_back_sub = sig_back_sub > 0.0  ; clip all negative values to 0

signorm = sig_back_sub / max(sig_back_sub)
sig = signorm

;  ; *************************************
;  ;stop
;  ;need print file
;  close,4
;  folder= 'C:\Users\lufa5942\Documents\data_reduction\IUVS\LBH_calibration\save\'
;  ;LETS print file of intensiites of uncalibrated N2
;  openw,4,folder+'N2_LBH_intensities_07_27_2025.txt'
;  printf,4,'n2_LBH__intensities v prime from 0-6'
;  printf,4,'number,  wavelength, int[0], int[1],  int[2], int[3],  int[4],  int[5],  int[6]
;  ;read in file old
;  FOR I=0,1799 DO BEGIN
;    ;for j=0,4 do begin
;    printF,4,'$(I4,1x,f10.4,7(2x,f8.3))',i,wave[i],lbh[i,0]/lbh[385,3],lbh[i,1]/lbh[385,3],lbh[i,2]/lbh[385,3],lbh[i,3]/lbh[385,3],lbh[i,4]/lbh[385,3],lbh[i,5]/lbh[385,3],lbh[i,6]/lbh[385,3]
;
;    ;ENDFOR
;  endfor
;  ;stop
;  close,4
;  wdir_plots=  'C:\Users\lufa5942\Documents\data_reduction\IUVS\LBH_calibration\plots\'
;  ;wdir_plots='G:\SSD_I-drive\Cassini\2025\library_plots\regression\2025\png_173k\18march2025\april14\'
;  ;win5=window(window_title='#2 Calibrated unsmoothed-normalized 2009 DOY 173 Titan ! c limb Spectra with Altitude 700km & CH4 Absorp')
  
  ; PLOT SEPERATE v' ***
  
  p0 = plot( wave[0:1799]/10.0,lbh[0:1799,0]/lbh[385,3], xr=[120,180],name="v'=0",yr=[0,1.5], color='black',layout=[1,1,1],XTITLE='Wavelength (nm)', YTITLE=" Relative Intensity of v'  [arb units]",title=" LBH relative intensity with v'")
  p1 = plot( wave[0:1799]/10.0, lbh[0:1799,1]/lbh[385,3], /over, color='red',name='v1' )
  p2 = plot( wave[0:1799]/10.0,  LBH[0:1799,2]/lbh[385,3], /over, color='blue' ,name='v2')
  p3 = plot( wave[0:1799]/10.0,  LBH[0:1799,3]/lbh[385,3], /over, color='coral' ,name='v3')
  p4 = plot( wave[0:1799]/10.0,  LBH[0:1799,4]/lbh[385,3], /over, color='green' ,name='v4')
  p5 = plot( wave[0:1799]/10.0,  LBH[0:1799,5]/lbh[385,3], /over, color='violet' ,name='v5')
  p6 = plot( wave[0:1799]/10.0,  LBH[0:1799,6]/lbh[385,3], /over, color='orange' ,name='v6')
  
  ;p7 = plot(wl-6.25,  signorm, /over, color='black', linestyle=5, name='data')

  leg = LEGEND(TARGET=[p0,p1,p2,p3,p4,p5,p6], POSITION=[177,1.1], $
    /DATA, /AUTO_TEXT_COLOR)
  ;print,'this is wdir_plots   ',wdir_plots
 ; win5.save,wdir_plots+'lbh_intensities.png'
  ;win5.save,wdir_plots+'#2 fuv_Round6_nov10_700km_CH4_greg.png'
  ;p0.close
  
  
 ; sun bakground

  ;
  ; empirically-derived wavelength shift of data to match the model
  ;
  wl_data_shift = 6.3  
  wl_shifted = wl - wl_data_shift
  
 ; PLOT SUM v' ***
  lbh_total = total(lbh[0:1799, *], 2)  ; Sum over vibrational levels
  
  p0 = plot(wave[0:1799]/10.0, lbh_total / lbh[385,3], $
          xr=[120,180], color='red', linestyle=0, $
          XTITLE='Wavelength (nm)', YTITLE="Total LBH Intensity (arb units)", $
          title="LBH Model (Summed)", name='model sum')
          
   p1 = plot( wl_shifted,  signorm, /over, color='black', linestyle=5, name='data' )
   
   leg = legend(target=[p0, p1], position=[175,1.0], /data, /auto_text_color)
  ; win6.save,wdir_plots+'lbh_intensities_sum.png'

stop

; CALIBRATION ********************************   STILL WORKING ON THIS....

; why not take avg difference between model and data heights per peak to get calibration number then add it?

; area method
wl_start = [126.0, 128.8, 131.7, 134.4, 137.3, 140.3, 143.7, 145.4, 152.1, 154.4, 162.0, 168.1]
wl_end =   [128.2, 130.5, 134.2, 136.2, 139.1, 142.1, 145.4, 148.0, 154.0, 156.8, 163.5, 169.5]
n_peaks = n_elements(wl_start)



p0 = plot(wave_lbh, lbh_total / lbh[385,3], $
  xr=[120,180], color='red', linestyle=0, $
  XTITLE='Wavelength (nm)', YTITLE="Total LBH Intensity (arb units)", $
  title="LBH Model (Summed)", name='model sum')
p1 = plot( wl_shifted,  signorm, /over, color='black', linestyle=5, name='data' )
for i = 0, n_peaks - 1 do $
  plot_handle_shade = plot( [wl_start[i],wl_end[i]], [1,1]*p0.yrange[0], /over, fill_level=p0.yrange[1], /fill_background, fill_transparency=70 )



; initialize arrays
area_data  = fltarr(n_peaks)  ;filtarr = arrays of 0 (like initializing)
area_model = fltarr(n_peaks)
wavemean   = fltarr(n_peaks)

; integrated areas per peak
area_data = fltarr(n_peaks)
area_model = fltarr(n_peaks)
wavemean = fltarr(n_peaks)
wavecen = fltarr(n_peaks)
sinv = fltarr(n_peaks)
sens = fltarr(n_peaks)
;sig = fltarr(n_peaks)
for i = 0, n_peaks - 1 do begin
  ndx_data = where( (wl_shifted gt wl_start[i]) and (wl_shifted lt wl_end[i]) )
  area_data[i] = total( sig[ndx_data] )
  
  ndx_model = where( (wave_lbh gt wl_start[i]) and (wave_lbh lt wl_end[i]) )
  area_model[i] = total( lbh_total[ndx_model] )
  
  ; average wavelength
  wavemean[i] = mean( wl_shifted[ndx_data] )
  
  ; centroid wavelength
  wavecen[i] = total( (wave_lbh[ndx_model]) * (lbh_total[ndx_model]) ) / total( lbh_total[ndx_model] )
endfor

sinv=area_model/area_data
sinv=sinv/min(sinv)
sens=1./sinv

p1 = plot( wavecen, area_data / area_model, symbol='o' )


;; Save to file
;openw,1,'n2_IUVS_inverse_sens_calculated.txt'
;printf,1,'# index   wavemean[nm]   inverse_sens   sensitivity'
;FOR i = 0, n_peaks-1 DO printf,1,i, wavemean[i], sinv[i], sens[i]
;close,1

; Plot sensitivity curves
;aaa = max(sinv)
;xvector = [120, 125]
;yvector1 = [aaa*0.95, aaa*0.95]
;yvector2 = [aaa*0.90, aaa*0.90]

p0 = plot(wavemean, sinv, $
  xrange=[120,175], xtitle='Wavelength (nm)', ytitle='Inverse Sensitivity (arb units)', $
  color='black', thick=2, title='IUVS BB Inverse Sensitivity')

p1 = plot(wavemean, sens * 5.0, /over, color='red', linestyle=2, thick=2)


; Optional: Apply interpolated sensitivity to full data
sens_interp = interpol(sens, wavemean, wl_shifted, /spline) > 0
sigcal = siguncal / sens_interp  ; calibrated spectrum

ndx_bad = where( finite(sigcal) eq 0 )
sigcal[ndx_bad] = 0.

p1 = plot( wavemean, sens, symbol='o' )
p2 = plot( wl_shifted, sens_interp, color='red', /over )

; idx
 max_val = max(sigcal, max_cal_idx)
 ;max_cal_idx = where(sigcal EQ max_val, count)

; Plot LBH model (normalized)
p0 = plot(wave[0:1799]/10.0, lbh_total / lbh[385,3], $
  color='red', linestyle=0, thick=2, $
  xr=[120,180], $
  xtitle='Wavelength (nm)', $
  ytitle='Total LBH Intensity (arb units)', $
  title='Model vs. Calibrated Data', yr=[0,1.5])

; Plot calibrated data
p1 = plot(wl_shifted, sigcal/sigcal[max_cal_idx]*30., /over, color='black', linestyle=2, thick=2)

stop
end


