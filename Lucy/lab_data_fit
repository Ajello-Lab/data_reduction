; lab fir
 
 

; calibrate

pro ajello_lab_calibrate, fsave_data, fsave_cal, yr=yr_spec, path_cal=path_cal

  if n_params() eq 0 then begin
    fsave_data = "Z:\round10\NeweGun_round10_after_energy_correction\data_reduction\N2_20EV_FUV_TEST13_IMAGE1_HIPRESS.idl"
    ;fsave_data = '/Users/holsclaw/MAVEN/Ajello_lab/_Big_e-gun_roundIII/data_reduction/H2_100eV_FUV_test_4_image1.idl'
  endif

  if keyword_set(path_cal) eq 0 then begin
    fsave_cal = file_dirname(fsave_data) + path_sep() + file_basename(fsave_data,'.idl') + '_cal.idl'
  endif else begin
    fsave_cal = path_cal + file_basename(fsave_data,'.idl') + '_cal.idl'
  endelse

  if keyword_set(yr_spec) eq 0 then begin
    yr_spec = [150,900]
  endif

  fsave_data_check = file_search(fsave_data)
  if fsave_data_check eq '' then begin
    print, 'Specified file not found: ', fsave_data
    stop
  endif
  restore,fsave_data,/ver

  ;
  ; read in sensitivity curve
  ;
  file_sens = !path_base + 'IUVS/IUVSFlightSensitivity.sav'
  file_sens_check = file_search(file_sens)
  if file_sens_check eq '' then begin
    print, 'IUVS flight sensitivity IDL save file not found: ', file_sens
    stop
  endif
  restore, file_sens
  ;restore,'/Users/holsclaw/MAVEN/IDL/ajello_lab/IUVSFlightSensitivity.sav',/ver
  fuv_wave = fuv_wave / 10.
  muv_wave = muv_wave / 10.

  ;
  ; retrieve wavelength scale for the rotated image
  ;
  ajello_lab_pixel_scale_rot, wlfuv, wlmuv, yfuv, ymuv
  ;
  ; Scale atmosphere slit sensitivity to a single row
  ;   (This assumes sensitivity in file is for the entire narrow slit area)
  ;   Atmosphere slit is 0.1 x 19.8mm in size
  ;
  y_scale_fuv = mean( deriv( yfuv ) )
  y_scale_muv = mean( deriv( ymuv ) )
  fuv_sens = fuv_sens * y_scale_fuv / 19.8
  muv_sens = muv_sens * y_scale_muv / 19.8

  ;
  ; adjust wavelength scale for the particular observation under study
  ;
  wlfuv = wlfuv - (128.-121.6)

  ;
  ; Rotate the image so wavelength increases with increasing pixel, and the big keyhole is down
  ;   Interpolate sensitivity to wavelength scale of data.
  ;   Set sensitivity values to NaN that are undefined.
  ;
  case channel of
    'FUV': begin
      wl = wlfuv
      yspa = yfuv
      ;    xr_wave = [100, 200]
      ;    wl_param = linfit( wlfuv, findgen(1024) )
      ;    y_param = linfit( yspa, findgen(1024) )

      arr_orig = arr
      arr_rot = rotate( arr_orig, 6 )
      arr = arr_rot

      sens = interpol( fuv_sens, fuv_wave, wlfuv )

      ndx_good_short = where( fuv_sens gt 1.e-5 and fuv_wave lt 150. )
      ndx_good_long = where( fuv_sens gt 1.e-5 and fuv_wave gt 150. )

      wl1 = fuv_wave[ndx_good_short[0]]
      wl2 = fuv_wave[ndx_good_long[-1]]

      ndx_undef = where( (wlfuv lt wl1) OR (wlfuv gt wl2), count_undef )
      sens[ndx_undef] = !values.f_nan

    end
    'MUV': begin
      wl = wlmuv
      yspa = ymuv
      ;    xr_wave = [170, 350]
      ;    wl_param = linfit( wlmuv, findgen(1024) )
      ;    y_param = linfit( yspa, findgen(1024) )

      arr_orig = arr
      arr_rot = rotate( arr_orig, 1 )
      arr = arr_rot

      sens = interpol( muv_sens, muv_wave/10., wlmuv )

      ndx_good_short = where( muv_sens gt 1.e-5 and muv_wave lt 250. )
      ndx_good_long = where( muv_sens gt 1.e-5 and muv_wave gt 250. )

      wl1 = muv_wave[ndx_good_short[0]]
      wl2 = muv_wave[ndx_good_long[-1]]

      ndx_undef = where( (wlmuv lt wl1) OR (wlmuv gt wl2), count_undef )
      sens[ndx_undef] = !values.f_nan

    end
  endcase

  ;
  ; Calibrate the data to something proportional to kR/nm
  ;
  arr_cal = fltarr(1024,1024)
  for i = yr_spec[0], yr_spec[1] do begin
    arr_cal[*,i] = arr[*,i] / int_time / sens
  endfor

  source_routine = 'ajello_lab_calibrate'
  arr_rate = arr / int_time
  save,filename=fsave_cal, arr_cal, arr_rate, gas, energy, channel, int_time, wl, yspa, temp_light, temp_dark, jd_light, jd_dark, path, yr_spec, source_routine

  return

  img1 = image( arr, min_value=minval, max_value=maxval, rgb_table=34, title=file_basename(fsave_in), xtickdir=1, ytickdir=1 )
  img2 = image( arrcal, min_value=minval, max_value=max(arrcal)*0.5, rgb_table=34, title=file_basename(fsave_in), xtickdir=1, ytickdir=1 )

  v = findgen(yr_spec[1]-yr_spec[0]+1)+yr_spec[0]
  spat = total( arr[*,yr_spec[0]:yr_spec[1]], 1 )
  xc = centroid( v, spat-min(spat) )

  p = plot( v, spat )
  markerp,p,x=yr_spec[0]
  markerp,p,x=yr_spec[1]
  markerp,p,x=xc

  stop


  w = 20
  y0  = 550
  spec_raw = total( arr[*,y0-w:y0+w], 2 )
  spec_cal = total( arrcal[*,y0-w:y0+w], 2 )

  xr = [110,170]
  p1 = plot( wlfuv, spec_raw, layout=[1,2,1], title='raw', xr=xr )
  p2 = plot( wlfuv, spec_cal, layout=[1,2,2], /current, title='calibrated', xtitle='wavelength (nm)', xr=xr ) ;, yr=[0,3.e8]

  p = plot( fuv_wave, fuv_sens )


  stop

end