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

pro GOLD_vs_IUVS_16eV_calibration

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

  ; greg plots (checks)
;  p1 = plot( wave, lbh_orig[*,0] )
;  p2 = plot( wave, lbh[*,0]/total(psf), /over, color='red' )

  lbh_orig_tot = total( lbh_orig, 2 )
  lbh_tot = total( lbh, 2 ) / total(psf)

;  p1 = plot( wave, lbh_orig_tot )
;  p2 = plot( wave, lbh_tot, /over, color='red' )


  ;stop

  ;done

  ; LOAD IN DATA ***********************
  restore, file_data, /verbose
  ;  % RESTORE: IDL version 9.0.0 (darwin, arm64).
  ;  % RESTORE: Restored variable: ARR.
  ;  % RESTORE: Restored variable: ARR_LIGHT_AVG.
  ;  % RESTORE: Restored variable: ARR_DARK_AVG.
  ;  % RESTORE: Restored variable: ARR_LIGHT_SDV.
  ;  % RESTORE: Restored variable: ARR_DARK_SDV.
  ;  % RESTORE: Restored variable: ARR_MEDIAN.
  ;  % RESTORE: Restored variable: ARR_LIGHT_MEDIAN.
  ;  % RESTORE: Restored variable: ARR_DARK_MEDIAN.
  ;  % RESTORE: Restored variable: ARR_LIGHT_MAD.
  ;  % RESTORE: Restored variable: ARR_DARK_MAD.
  ;  % RESTORE: Restored variable: SIG_LIGHT.
  ;  % RESTORE: Restored variable: SIG_DARK.
  ;  % RESTORE: Restored variable: JD_DARK.
  ;  % RESTORE: Restored variable: JD_LIGHT.
  ;  % RESTORE: Restored variable: DESC.
  ;  % RESTORE: Restored variable: SOURCE_ROUTINE.
  ;  % RESTORE: Restored variable: GAS.
  ;  % RESTORE: Restored variable: ENERGY.
  ;  % RESTORE: Restored variable: CHANNEL.
  ;  % RESTORE: Restored variable: TEMP_LIGHT.
  ;  % RESTORE: Restored variable: TEMP_DARK.
  ;  % RESTORE: Restored variable: FILE_DESC_LIGHT.
  ;  % RESTORE: Restored variable: FILE_DESC_DARK.
  ;  % RESTORE: Restored variable: PATH.
  ;  % RESTORE: Restored variable: INT_TIME.
  ;  % RESTORE: Restored variable: WL.
  ;  % RESTORE: Restored variable: YSPA.

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

  ; empirically-derived wavelength shift of data to match the model
  ;
  wl_data_shift = 6.3
  wl_shifted = wl - wl_data_shift

  ; PLOT SUM v' *** with uncalib data
  lbh_total = total(lbh[0:1799, *], 2)  ; Sum over vibrational levels


  ; PLOT SEPERATE v' ***
;  p0 = plot( wave[*]/10.0,lbh[0:1799,0]/lbh[385,3], xr=[120,180],name="v'=0",yr=[0,1.5], color='red',layout=[1,1,1],XTITLE='Wavelength (nm)', YTITLE=" Relative Intensity of v'  [arb units]",title=" LBH relative intensity with v'")
;  p1 = plot( wave[*]/10.0, lbh[0:1799,1]/lbh[385,3], /over, color='cyan',name='v1' )
;  p2 = plot( wave[*]/10.0,  LBH[0:1799,2]/lbh[385,3], /over, color='blue' ,name='v2')
;  p3 = plot( wave[*]/10.0,  LBH[0:1799,3]/lbh[385,3], /over, color='coral' ,name='v3')
;  p4 = plot( wave[*]/10.0,  LBH[0:1799,4]/lbh[385,3], /over, color='green' ,name='v4')
;  p5 = plot( wave[*]/10.0,  LBH[0:1799,5]/lbh[385,3], /over, color='violet' ,name='v5')
;  p6 = plot( wave[*]/10.0,  LBH[0:1799,6]/lbh[385,3], /over, color='orange' ,name='v6')
;
;  p7 = plot( wave[*]/10.0, lbh_total / lbh[385,3], /over, color='black', name = 'v sum', thick = 2)
;  ;p7 = plot(wl-6.25,  signorm, /over, color='black', linestyle=5, name='data')
;
;  leg = LEGEND(TARGET=[p0,p1,p2,p3,p4,p5,p6,p7], POSITION=[177,1.1], $
;    /DATA, /AUTO_TEXT_COLOR)
;  print, lbh_total
  ;print,'this is wdir_plots   ',wdir_plots
  ; win5.save,wdir_plots+'lbh_intensities.png'
  ;win5.save,wdir_plots+'#2 fuv_Round6_nov10_700km_CH4_greg.png'
  ;p0.close

 ; p0.save, "Z:\Lucy's codes, plots\PS plots\vib_model_2025.png"
;
;  p0 = plot(wave[*]/10.0, lbh_total / lbh[385,3], $
;    xr=[120,180], color='red', linestyle=0, $
;    XTITLE='Wavelength (nm)', YTITLE="Total LBH Intensity (arb units)", $
;    title="LBH Model (Summed)", name='model sum')
;
;  p1 = plot( wl_shifted,  signorm, /over, color='black', linestyle=5, name='data' )
;
;  leg = legend(target=[p0, p1], position=[175,1.0], /data, /auto_text_color)
;  ; win6.save,wdir_plots+'lbh_intensities_sum.png'

 ; stop

  ; CALIBRATION ********************************

  ; area method
  wl_start = [126.0, 128.8, 131.9, 134.7, 137.5, 140.4, 143.9, 145.7, 152.2, 155, 159.7, 162.2, 165.4, 168.3, 173, 174.5, 177.9]
  wl_end =   [128.2, 130.5, 133.4, 136.3, 139.3, 142.3, 145.5, 148.4, 154.0, 156.8, 160.7, 164, 166.7, 169.8, 174.4, 176.2, 179.5]
  n_peaks = n_elements(wl_start)


  ; plot model, data, and bands (ranges)
;  p0 = plot(wave_lbh, lbh_total / lbh[385,3], $
;    xr=[120,180], color='red', linestyle=0, $
;    XTITLE='Wavelength (nm)', YTITLE="Total LBH Intensity (arb units)", $
;    title="LBH Model (Summed)", name='model sum')
;  p1 = plot( wl_shifted,  signorm, /over, color='black', linestyle=5, name='data' )
;  for i = 0, n_peaks - 1 do $
;    plot_handle_shade = plot( [wl_start[i],wl_end[i]], [1,1]*p0.yrange[0], /over, fill_level=p0.yrange[1], /fill_background, fill_transparency=70 )

  ;p0.save, "Z:\Lucy's codes, plots\PS plots\data_uncal_band_ranges_2025.png"


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


  ; plot areas
;  p0 = plot(wavemean, area_data/max(area_data), title='Areas')
;  p1 = plot(wavemean, area_model/max(area_model), /over, color='red')
;
;
;  ; plot interp
;  p1 = plot( wavecen, area_data / area_model, symbol='o' )

  ; Optional: Apply interpolated sensitivity to full data
  sens_interp = interpol(sens, wavemean, wl_shifted, /spline) > 0
  invsens_interp = interpol(1/sens, wavemean, wl_shifted, /spline) > 0
  sigcal = signorm / sens_interp  ; calibrated spectrum

  ndx_bad = where( finite(sigcal) eq 0 )
  sigcal[ndx_bad] = 0.

  ; plot interpolated curve fit
;  p1 = plot( wavemean, sens, symbol='o', thick=2 )
;  p2 = plot( wl_shifted, sens_interp, color='red', /over )
;  p1.save, "Z:\Lucy's codes, plots\PS plots\cal_interp_curvefit_2025.png"
;  ; plot inv interp curve fit
;  p1 = plot( wavemean, 1/sens, symbol='o',  yrange=[0,100], thick=2)
;  p2 = plot( wl_shifted, invsens_interp, color='red', /over )
;  ;p1.save, "Z:\Lucy's codes, plots\PS plots\cal_invinterp_curvefit_2025.png"



  ;stop
  ;
  ; idx
  max_val = max(sigcal, max_cal_idx)
  ;max_cal_idx = where(sigcal EQ max_val, count)

  ; Plot LBH model (normalized) and calibrated data
  cal_idx = where(abs(wl_shifted - 135.5) lt 0.2, count)
  model_idx = where(abs(wave/10.0 - 135.5) lt 0.2, count)
  model_peak = max(lbh_total[model_idx])
  data_peak = max(sigcal[cal_idx])

  model_norm = lbh_total / model_peak
  data_norm = sigcal / data_peak


;  p0 = plot(wave[*]/10.0, model_norm, $
;    color='red', linestyle=0, thick=2, $
;    xr=[120,180], $
;    xtitle='Wavelength (nm)', $
;    ytitle='Total LBH Intensity (arb units)', $
;    title='Model vs. Calibrated Data', yr=[0,1.5])
;
;  p1 = plot(wl_shifted, data_norm, /over, color='black', linestyle=2, thick=2)

  ;p0.save, "Z:\Lucy's codes, plots\PS plots\iuvs_model_caldata__2025.png"

wl_IUVS = wl_shifted
data_IUVS = data_norm


; ***************************************GOLD**********************************************************************************************************



; PURPOSE: this code reades in data from GOLD insturment and uses the LBH model (with vibrational bands) and the point spread function (PSF)
; to create a sensitivity curve to calibrate the data.


  ; ============= READ IN =============

  user_name = (get_login_info()).user_name
  case user_name of
    'lufa5942': begin
      path_lab_data = "\\lasp-store\projects\Phase_Development\MAVEN\IUVS_Data\IUVS_Breadboard\GOLD\kimball_egun_round_11\data_reduction\"
      file_model = "C:\Users\lufa5942\Documents\data_reduction\Lucy\LBH code\calibration_victor\n2_lbh_rot_293K.sav" ; temporary lucy
    end
    'holsclaw': begin
      ajello_lab_set_paths, path_base, path_repo
      path_lab_data =  '/Volumes/projects/Phase_Development/MAVEN/IUVS_Data/IUVS_Breadboard/GOLD/kimball_egun_round_11/data_reduction/'
      file_model = path_repo + 'Lucy' + path_sep() + 'LBH code' + path_sep() + 'calibration_victor' + path_sep() + 'n2_lbh_rot_293K.sav'   ; LBH model
      ;'/Users/holsclaw/Ajello-Lab/data_reduction/Lucy/LBH code/calibration_victor/n2_lbh_rot_293K.sav'
    end
    else:begin
      print, 'Unknown user: Create a new entry of path_lab_data for this case statement'
      stop
    end
  endcase

  file_r = routine_filepath()
  path_repo = file_dirname(file_r,/MARK_DIRECTORY)

  file_data = path_lab_data + "N2_16ev_hi_pres_test19_image1_pmax_250.sav"  ; 16eV img1     ; 'N2_100eV_hi-pres_test2_image1_pmax_250.sav' (100eV img1)

  ; =============  DATA =============

  restore, file_data, /ver
  ;  % RESTORE: Restored variable: WL.
  ;  % RESTORE: Restored variable: XP.
  ;  % RESTORE: Restored variable: YP.
  ;  % RESTORE: Restored variable: CBIN.
  ;  % RESTORE: Restored variable: PHD.
  ;  % RESTORE: Restored variable: HDR_LIST.
  ;  % RESTORE: Restored variable: DURATION.
  ;  % RESTORE: Restored variable: EXP_DESC.
  ;  % RESTORE: Restored variable: VAR_DESC.
  ;  % RESTORE: Restored variable: SOURCE_ROUTINE.
  ;  % RESTORE: Restored variable: PROCESS_TIME.
  wl_data = wl



  ajello_lab_gold_wavelength_scale, wl1

  spat = total( cbin, 1 )
  spec = total( cbin, 2 )

  ;
  ; isolate the spatial region with high signal and low contamination
  ;
  spat_max = max( spat, ndx_spat_max )
  w_spat = 150
  y1 = ndx_spat_max - w_spat
  y2 = ndx_spat_max + w_spat

;  p = plot( spat, xtitle='spatial pixel' )
;  markerp,p,x=y1,linestyle=2
;  markerp,p,x=y2,linestyle=2

  ;
  ; create a spectrum limited to the identified spatial region
  ;
  spec = total( cbin[*,y1:y2], 2 )
;  p = plot( wl, spec, xtitle='wavelength (nm)' )

  ;
  ; isolate the 1493 emission line
  ;
  spec_max = max( spec, ndx_spec_max )
  w_spec = 30
  x1 = ndx_spec_max - w_spec
  x2 = ndx_spec_max + w_spec

;  p = plot( spec[x1:x2] )

  wl2 = 0.1943 * findgen(4096) + 1187 + 145.70251
  wl3 = 0.1943 * findgen(4096) + 1332.7025

  wl = wl1 - wl1[ndx_spec_max] + 1493.

;  p = plot( wl, spec )

  img = image( cbin, min_value=0, max_value=100 )

;  p = plot( wl - wl2 )


  x = wl[x1:x2]
  y = spec[x1:x2]

  g = gaussfit( x, y, afit, nterms=3 )

;  p = plot( x, y )
;  p2 = plot( x, g, /over, color='red' )

  ajello_lab_gold_em_psf_model, x, afit, f

;  p = plot( x, f )

  g2 = gaussian_function( afit[2], /normalize )

;  p = plot( g2 )


  ; ============= LBH MODEL =============

  restore, file_model, /relax
  ;  % RESTORE: Recovering incompatible definition of structure LIST using relaxed structure assignment rules.
  ;  % RESTORE: Recovering incompatible definition of structure HASH using relaxed structure assignment rules.
  ;  % RESTORE: Restored variable: TROT.
  ;  % RESTORE: Restored variable: WAVE.
  ;  % RESTORE: Restored variable: LBH.
  ;  % RESTORE: Restored variable: LIST.

  wave_nm = wave/10.  ; angstroms to nm
  wl_nm = wl/10

  ; ============= PSF =============

  ajello_lab_scaled_gold_psf_model, wave, psf_mod

  ;xp = findgen(x2-x1+1)
  ;
  ;p = plot( xp, y )
  ;p2 = plot( psf*max(y)/max(psf), /over, color='red' )


  for i=0, ((size(lbh))[2]-1) do begin   ; joe added this loop?
    lbh[*, i] = convol(lbh[*, i], psf_mod)
  endfor

  lbh_total = total(lbh, 2)  ; Sum over vibrational levels
  ;lbh_total = total( lbh, 2 ) / total(psf)

  ; plot v' 1-6 and sum
;  p0 = plot( wave_nm, lbh[*,0]/lbh[385,3], xr=[120,180],name="v'=0",yr=[0,1.5], color='red',layout=[1,1,1],XTITLE='Wavelength (nm)', YTITLE=" Relative Intensity of v' [arb units]",title=" LBH relative intensity with v'")
;  p1 = plot( wave_nm, lbh[*,1]/lbh[385,3], /over, color='cyan',name='v1' )
;  p2 = plot( wave_nm, lbh[*,2]/lbh[385,3], /over, color='blue' ,name='v2')
;  p3 = plot( wave_nm, lbh[*,3]/lbh[385,3], /over, color='coral' ,name='v3')
;  p4 = plot( wave_nm, lbh[*,4]/lbh[385,3], /over, color='green' ,name='v4')
;  p5 = plot( wave_nm, lbh[*,5]/lbh[385,3], /over, color='violet' ,name='v5')
;  p6 = plot( wave_nm, lbh[*,6]/lbh[385,3], /over, color='orange' ,name='v6')
;  p7 = plot( wave_nm, lbh_total/lbh[385,3], /over, color='black', name = 'v sum', thick = 2)
;  leg = LEGEND(TARGET=[p0,p1,p2,p3,p4,p5,p6,p7], POSITION=[177,1.1], $
;    /DATA, /AUTO_TEXT_COLOR)
;
;  ;print, lbh_total
;
;  ; plot model over uncal data
;  p8 = plot(wave_nm, lbh_total / lbh[385,3],xr=[120,180], color='red', linestyle=0, $
;    XTITLE='Wavelength (nm)', YTITLE="Total LBH Intensity (arb units)", title="LBH Model (Summed)", name='model sum')
;  p9 = plot(wave_nm, spec/8.1, /over, color='black', linestyle=5, name='data' )
;  leg = legend(target=[p8, p9], position=[175,1.0], /data, /auto_text_color)

  lbh_total_max = max(lbh_total, ndx_lbh_max)
  spec_max = max(spec, ndx_spec_max)
  wl_data_shift = wl_data - wl_data[ndx_spec_max] + wave_nm[ndx_lbh_max]


;  ; Plot uncal data vs model
;  p1 = plot( wave_nm, lbh_total / max(lbh_total), color='red', title='uncal data vs model', name = 'model')
;  p2 = plot( wl_data_shift, spec / max(spec), /over, linestyle=2, name= 'data')
;  leg = legend(target=[p1, p2], position=[180,0.8], /data, /auto_text_color)


  ; plot zoomed in model to get channel nums
;  p = plot( wave_nm, lbh_total / max(lbh_total), color='red', title='model zoom', xr = [130,165])



  ; ============= CALIBRATION =============

  ; area method
  wl_start = [130.7, 132.0, 133.5, 134.8, 137.6, 140.7, 142.32, 145.8, 147.1, 149.8, 151.3, 152.6, 157.0, 159.6, 160.72]
  wl_end =  [131.9, 133.4, 134.7, 136.3, 139.2, 142.3, 143.9, 147.0, 148.4, 150.5, 152.1, 153.7, 158.1, 160.7, 162.2]
  n_peaks = n_elements(wl_start)

  ; plot model, data, and bands (ranges)
;  p0 = plot(wave_nm, lbh_total / max(lbh_total),  xr=[120,180], color='red', linestyle=0, $
;    XTITLE='Wavelength (nm)', YTITLE="Total LBH Intensity (arb units)", title="peak ranges")
;  p1 = plot(wl_data_shift, spec / max(spec), /over, color='black', linestyle=2)
;  for i = 0, n_peaks - 1 do $
;    plot_handle_shade = plot( [wl_start[i],wl_end[i]], [1,1]*p0.yrange[0], /over, fill_level=p0.yrange[1], /fill_background, fill_transparency=70 )

 ; p0.save, "Z:\Lucy's codes, plots\GOLD\16eV\data_uncal_band_ranges.png"


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
  ;spec = spec[19:1643]

  for i = 0, n_peaks - 1 do begin
    ndx_data = where( (wl_data_shift gt wl_start[i]) and (wl_data_shift lt wl_end[i]) )
    area_data[i] = total( spec[ndx_data] )

    ndx_model = where( (wave_nm gt wl_start[i]) and (wave_nm lt wl_end[i]) )
    area_model[i] = total( lbh_total[ndx_model] )

    ; average wavelength
    wavemean[i] = mean(wl_data_shift[ndx_data] )

    ; centroid wavelength
    wavecen[i] = total( (wave_nm[ndx_model]) * (lbh_total[ndx_model]) ) / total( lbh_total[ndx_model] )
  endfor

  sinv=area_model/area_data
  sinv=sinv/min(sinv)
  sens=1./sinv

  ; plot interp fit
  ; Apply interpolated sensitivity to full data
  sens_interp = interpol(sens, wavemean, wl_data_shift, /spline) > 0
  invsens_interp = interpol(1/sens, wavemean, wl_data_shift, /spline) > 0
  sigcal = spec / sens_interp  ; calibrated spectrum
  ndx_bad = where( finite(sigcal) eq 0 )
  sigcal[ndx_bad] = 0.

  ; plot interpolated curve fit and invinterp
;  p1 = plot( wavemean, sens, symbol='o', thick=2, title = 'interp' )
;  p2 = plot( wl_data_shift, sens_interp, color='red', /over )
; ; p1.save, "Z:\Lucy's codes, plots\GOLD\16eV\cal_interp_curvefit.png"
;
;  p1 = plot( wavemean, 1/sens, symbol='o', thick=2, title = 'inv interp')
;  p2 = plot( wl_data_shift, invsens_interp, color='red', /over )
  ;p1.save, "Z:\Lucy's codes, plots\GOLD\JPG plots\cal_invinterp_curvefit.png"

  ; normalize
  max_val = max(sigcal, max_cal_idx)
  cal_idx = where(abs(wl_data_shift - 135.3) lt 0.2, count)
  model_idx = where(abs(wave_nm- 135.3) lt 0.2, count)
  model_peak = max(lbh_total[model_idx])
  data_peak = max(sigcal[cal_idx])

  model_norm = lbh_total / model_peak
  data_norm = sigcal / data_peak

  ; plot final cal data and model
;  p0 = plot(wave_nm, model_norm, color='red', linestyle=0, thick=2, xr=[120,180], yr = [0,1.1], $
;    xtitle='Wavelength (nm)', ytitle='Total LBH Intensity (arb units)', title='Model vs. Calibrated Data', name = 'model')
;  p1 = plot(wl_data_shift, data_norm, /over, color='black', linestyle=2, thick=2, name = 'calibrated data')
;  leg = legend(target=[p0, p1], position=[170,0.8], /data, /auto_text_color)

 ; p0.save, "Z:\Lucy's codes, plots\GOLD\16eV\caldata_and_model.png"


wl_GOLD = wl_data_shift
data_GOLD = data_norm



; plot 16eV calibrated data GOLD vs IUVS
p0 = plot(wl_GOLD, data_GOLD, color = 'orange', title = 'GOLD vs IUVS 16eV calibrated data', name = 'GOLD', $
  xtitle='Wavelength (nm)', ytitle='Total LBH Intensity (arb units)', xr = [125,170], yr = [0,1.2])
p1 = plot(wl_IUVS, data_IUVS, /over, color = 'blue', name = 'IUVS')
leg = legend(target=[p0, p1], position=[160,1], /data, /auto_text_color)
p0.save, "Z:\Lucy's codes, plots\GOLD\IUVS_v_GOLD_16eV_cal.png"

stop
end