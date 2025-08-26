
; PURPOSE: this code reades in 100eV img1 data from GOLD insturment and uses the LBH model (with vibrational bands) and the point spread function (PSF) 
; to create a sensitivity curve to calibrate the data.


pro GOLD_data_and_model

; ============= READ IN =============

user_name = (get_login_info()).user_name
case user_name of
  'lufa5942': begin
    path_lab_data = "\\lasp-store\projects\Phase_Development\MAVEN\IUVS_Data\IUVS_Breadboard\GOLD\kimball_egun_round_11\data_reduction\"
  end
  'holsclaw': begin
    path_lab_data =  '/Volumes/projects/Phase_Development/MAVEN/IUVS_Data/IUVS_Breadboard/GOLD/kimball_egun_round_11/data_reduction/'
  end
  else:begin
    print, 'Unknown user: Create a new entry of path_lab_data for this case statement'
    stop
  end
endcase

file_r = routine_filepath()
path_repo = file_dirname(file_r,/MARK_DIRECTORY)

file_data = path_lab_data + "N2_16ev_hi_pres_test19_image1_pmax_250.sav"  ; 16eV img1     ; 'N2_100eV_hi-pres_test2_image1_pmax_250.sav' (100eV img1)
file_model = path_repo + 'Lucy' + path_sep() + 'LBH code' + path_sep() + 'calibration_victor' + path_sep() + 'n2_lbh_rot_293K.sav'   ; LBH model
file_model = "C:\Users\lufa5942\Documents\data_reduction\Lucy\LBH code\calibration_victor\n2_lbh_rot_293K.sav" ; temporary lucy

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


  ajello_lab_gold_wavelength_scale, wl1

  spat = total( cbin, 1 )
  spec = total( cbin, 2 )

  ;
  ; isolate the spatial region with high signal and low contamination
  ;
  spat_max = max( spat, ndx_spat_max )
  w_spat = 30
  y1 = ndx_spat_max - w_spat
  y2 = ndx_spat_max + w_spat

  p = plot( spat, xtitle='spatial pixel' )
  markerp,p,x=y1,linestyle=2
  markerp,p,x=y2,linestyle=2

  ;
  ; create a spectrum limited to the identified spatial region
  ;
  spec = total( cbin[*,y1:y2], 2 )
  p = plot( wl, spec, xtitle='wavelength (nm)' )

  ;
  ; isolate the 1493 emission line
  ;
  spec_max = max( spec, ndx_spec_max )
  w_spec = 30
  x1 = ndx_spec_max - w_spec
  x2 = ndx_spec_max + w_spec

  p = plot( spec[x1:x2] )

  wl2 = 0.1943 * findgen(4096) + 1187 + 145.70251
  wl3 = 0.1943 * findgen(4096) + 1332.7025

  wl = wl1 - wl1[ndx_spec_max] + 1493.

  p = plot( wl, spec )

  img = image( cbin, min_value=0, max_value=100 )

  p = plot( wl - wl2 )  ; blank


  x = wl[x1:x2]
  y = spec[x1:x2]

  g = gaussfit( x, y, afit, nterms=3 )

  p = plot( x, y )
  p2 = plot( x, g, /over, color='red' )

;  ajello_lab_gold_em_psf_model, x, afit, f
;
;  p = plot( x, f )
;
;  g2 = gaussian_function( afit[2], /normalize )
;
;  p = plot( g2 )


; ============= PSF =============

  ajello_lab_scaled_gold_psf_model, psf

  xp = findgen(x2-x1+1)
  p = plot( xp, y )
  p2 = plot( psf, /over )

  ;stop


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

  for i=0, ((size(lbh))[2]-1) do begin   ; joe added this loop?
    lbh[*, i] = convol(lbh[*, i], psf)
  endfor
  
  lbh_total = total(lbh, 2)  ; Sum over vibrational levels
  ;lbh_total = total( lbh, 2 ) / total(psf)

  ; plot v' 1-6 and sum 
  p0 = plot( wave_nm, lbh[*,0]/lbh[385,3], xr=[120,180],name="v'=0",yr=[0,1.5], color='red',layout=[1,1,1],XTITLE='Wavelength (nm)', YTITLE=" Relative Intensity of v' [arb units]",title=" LBH relative intensity with v'")
  p1 = plot( wave_nm, lbh[*,1]/lbh[385,3], /over, color='cyan',name='v1' )
  p2 = plot( wave_nm, lbh[*,2]/lbh[385,3], /over, color='blue' ,name='v2')
  p3 = plot( wave_nm, lbh[*,3]/lbh[385,3], /over, color='coral' ,name='v3')
  p4 = plot( wave_nm, lbh[*,4]/lbh[385,3], /over, color='green' ,name='v4')
  p5 = plot( wave_nm, lbh[*,5]/lbh[385,3], /over, color='violet' ,name='v5')
  p6 = plot( wave_nm, lbh[*,6]/lbh[385,3], /over, color='orange' ,name='v6')
  p7 = plot( wave_nm, lbh_total/lbh[385,3], /over, color='black', name = 'v sum', thick = 2)
  leg = LEGEND(TARGET=[p0,p1,p2,p3,p4,p5,p6,p7], POSITION=[177,1.1], $
    /DATA, /AUTO_TEXT_COLOR)
    
;print, lbh_total

; plot model over uncal data
p8 = plot(wave_nm, lbh_total / lbh[385,3],xr=[120,180], color='red', linestyle=0, $
  XTITLE='Wavelength (nm)', YTITLE="Total LBH Intensity (arb units)", title="LBH Model (Summed)", name='model sum')
p9 = plot(wave_nm, spec/8.1, /over, color='black', linestyle=5, name='data' )
leg = legend(target=[p8, p9], position=[175,1.0], /data, /auto_text_color)


; ============= CALIBRATION =============

; area method
wl_start = [143.9, 145.7, 152.2, 155, 159.7, 162.2, 165.4, 168.3, 173, 174.5]
wl_end =   [145.5, 148.4, 154.0, 156.8, 160.7, 164, 166.7, 169.8, 174.4, 176.2]
n_peaks = n_elements(wl_start)

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
  ndx_data = where( (wl_nm gt wl_start[i]) and (wl_nm lt wl_end[i]) )
  area_data[i] = total( spec[ndx_data] )

  ndx_model = where( (wave_nm gt wl_start[i]) and (wave_nm lt wl_end[i]) )
  area_model[i] = total( lbh_total[ndx_model] )

  ; average wavelength
  wavemean[i] = mean( wl_nm[ndx_data] )

  ; centroid wavelength
  wavecen[i] = total( (wave_nm[ndx_model]) * (lbh_total[ndx_model]) ) / total( lbh_total[ndx_model] )
endfor

sinv=area_model/area_data
sinv=sinv/min(sinv)
sens=1./sinv

; plot interp fit
; Apply interpolated sensitivity to full data
sens_interp = interpol(sens, wavemean, wl_nm, /spline) > 0
invsens_interp = interpol(1/sens, wavemean, wl_nm, /spline) > 0
sigcal = spec / sens_interp  ; calibrated spectrum

ndx_bad = where( finite(sigcal) eq 0 )
sigcal[ndx_bad] = 0.

; plot interpolated curve fit and invinterp
p1 = plot( wavemean, sens, symbol='o', thick=2, title = 'interp' )
p2 = plot( wl_nm, sens_interp, color='red', /over )

p1 = plot( wavemean, 1/sens, symbol='o',  yrange=[0,20], xrange = [140,180], thick=2, title = 'inv interp')
p2 = plot( wl_nm, invsens_interp, color='red', /over )
;p1.save, "Z:\Lucy's codes, plots\GOLD\JPG plots\cal_invinterp_curvefit.png"

; normalize
max_val = max(sigcal, max_cal_idx)
cal_idx = where(abs(wl_nm - 135.5) lt 0.2, count)
model_idx = where(abs(wave_nm- 135.5) lt 0.2, count)
model_peak = max(lbh_total[model_idx])
data_peak = max(sigcal[cal_idx])

model_norm = lbh_total / model_peak
data_norm = sigcal / data_peak

; plot final cal data and model
p0 = plot(wave_nm, model_norm, $
  color='red', linestyle=0, thick=2, $
  xr=[120,180], $
  xtitle='Wavelength (nm)', $
  ytitle='Total LBH Intensity (arb units)', $
  title='Model vs. Calibrated Data', yr=[0,1.5])
p1 = plot(wl_nm, data_norm, color='black', linestyle=2, thick=2, xr = [140,230])

; p0.save, "Z:\Lucy's codes, plots\GOLD\JPG plots\caldata_and_model"


stop
end

