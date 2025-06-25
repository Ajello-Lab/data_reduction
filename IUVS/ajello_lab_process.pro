;+
; NAME:
; AJELLO_LAB_PROCESS
; 
; PURPOSE:
; For one set of IUVS data, identified in the given path 'path_data', read in all dark scans and all light scans, 
;  average each, compute the difference, and record in an IDL save file for later processing.
;
; INPUTS:
; path_data: path to the data, string
; path_save: path where data should be saved, string
;
; OUTPUTS:
; fsave_vec: full path to the IDL save file, string
; 
; KEYWORDS
; reprocess_flag: set to 1 to reprocess the data, overwriting an existing save file if it exists 
; 
; NOTES:
; The processing is repeated on FUV and MUV datasets, which are assumed to be located in the same given path, path_data.
; This routine is called iteratively by "ajello_lab_process_driver" in order to process multiple datasets.
;  
;-
pro ajello_lab_process, path_data, path_save, fsave_vec, reprocess_flag=reprocess_flag, $
  experimental_flag=experimental_flag

if keyword_set(reprocess_flag) eq 0 then reprocess_flag = 0
if keyword_set(experimental_flag) eq 0 then experimental_flag = 0

if n_params() eq 0 then begin
  close_windows  
  gas = 'CO'
  energy = '30eV'
  exp_desc = 'CO_30eV_3'
  ;exp_desc = 'CO_30eV_4'
  ;exp_desc = 'CO_30eV_5'
  path_data = '/Volumes/projects/Phase_Development/MAVEN/IUVS_Data/IUVS_Breadboard/_BIG_e-gun/'+gas+'/' + exp_desc + '/'
endif
;
; Assumptions made regarding the voltages used for light and dark images
;
volt_light = 910
volt_dark_max = 100
;
; Saturation value for 12 bit sensor
;
sat_value = 2.^12 - 1.

;
; There are three datasets: one acquired in 2015, and two starting in 2017
;   the 2015 dataset is in a folder called "_Big_e-gun"
;   the 2017 dataset is in a folder called "_Big_e-gun_RoundII" and "_Big_e-gun_RoundIII" 
; Let's use the "RoundII" string as a unique indicator of the 2017 datasets
;
;pos = strpos(strupcase(path_data),'ROUNDII')
;case pos of
;  -1:begin
;    temp = strsplit( path_data, path_sep(), /extract )
;    gas = temp[-2]
;    ;exp_desc = temp[-1]
;    ;
;    temp2 = strsplit( temp[-1], '_', /extract )
;    energy = temp2[1]
;    extra_id = temp2[2]
;  end
;  else: begin
;    temp = strsplit( path_data, path_sep(), /extract )
;    match2 = strmatch(temp,'_Big_e-gun_RoundII',/fold_case)
;    match3 = strmatch(temp,'_Big_e-gun_RoundIII',/fold_case)
;    ndx_match = where( match2 or match3, count )
;    if count eq 0 then begin
;      print, 'this path does not contain the anticipated string'
;      stop
;    endif
;    gas = temp[ndx_match+1]
;    energy = temp[ndx_match+2]
;    extra_id = temp[ndx_match+3]
;  end
;endcase

path_comp = strupcase( strsplit( path_data, path_sep(), /extract ) )
;ndx_match = where( strmatch( strmid(path_comp,0,10), '_BIG_E-GUN' ), count )
ndx_match = where( strpos(path_comp,'BIG_E-GUN') ne -1, count )  ; Oct 2, 2024
if count eq 0 then begin

  ndx_match = where( strpos(path_comp,'NEWEGUN') ne -1, count )  ; Oct 2, 2024
  if count eq 0 then begin    
    print, 'dataset not found'
    stop
  endif
endif
if count gt 1 then begin
  print, 'too many fields satisfy criteria'
  stop
endif
ndx_match = ndx_match[0]
;path_comp[ndx_match]
gas = path_comp[ndx_match+1]
extra_id = ''
case path_comp[ndx_match] of 
  '_BIG_E-GUN':begin
    temp = strsplit( path_comp[ndx_match+2], '_', /extract )
    energy = temp[1]
    extra_id = temp[2]
  end
  '_BIG_E-GUN_ROUNDII':begin
    energy = path_comp[ndx_match+2]
    extra_id = path_comp[ndx_match+3]
  end
  '_BIG_E-GUN_ROUNDIII':begin
    energy = path_comp[ndx_match+2]
    extra_id = path_comp[ndx_match+3]
  end
  '_BIG_E-GUN_ROUNDIV':begin
    temp = path_comp[ndx_match+2]
    pos = strpos(temp,'EV')
    energy = strmid(temp,0,pos+2)
    ;energy = path_comp[ndx_match+2]
    extra_id1 = strmid(temp,pos+3,strlen(temp))
    extra_id2 = path_comp[ndx_match+3]
    if extra_id1 eq '' then extra_id = extra_id2 else extra_id = extra_id1 + '_' + extra_id2
    if strmatch(temp,'Y-DISPLACEMENT') then begin
      energy = ''
      extra_id = temp + '_' + extra_id2
    endif
  end
  '_BIG_E-GUN_ROUNDV':begin
    temp = path_comp[ndx_match+2]
    pos = strpos(temp,'EV')
    energy = strmid(temp,0,pos+2)
    ;energy = path_comp[ndx_match+2]
    extra_id1 = strmid(temp,pos+3,strlen(temp))
    extra_id2 = path_comp[ndx_match+3]
    if extra_id1 eq '' then extra_id = extra_id2 else extra_id = extra_id1 + '_' + extra_id2
    if strmatch(temp,'Y-DISPLACEMENT') then begin
      energy = ''
      extra_id = temp + '_' + extra_id2
    endif    
  end
  '_BIG_E-GUN_ROUNDVI':begin
    temp = path_comp[ndx_match+2]
    pos = strpos(temp,'EV')
    energy = strmid(temp,0,pos+2)
    ;energy = path_comp[ndx_match+2]
    extra_id1 = strmid(temp,pos+3,strlen(temp))
    extra_id2 = path_comp[ndx_match+3]
    if extra_id1 eq '' then extra_id = extra_id2 else extra_id = extra_id1 + '_' + extra_id2
    if strmatch(temp,'Y-DISPLACEMENT') then begin
      energy = ''
      extra_id = temp + '_' + extra_id2
    endif
  end
  '_BIG_E-GUN_ROUNDVII':begin
    temp = path_comp[ndx_match+2]
    pos = strpos(temp,'EV')
    energy = strmid(temp,0,pos+2)
    ;energy = path_comp[ndx_match+2]
    extra_id1 = strmid(temp,pos+3,strlen(temp))
    extra_id2 = path_comp[ndx_match+3]
    if extra_id1 eq '' then extra_id = extra_id2 else extra_id = extra_id1 + '_' + extra_id2
    if strmatch(temp,'Y-DISPLACEMENT') then begin
      energy = ''
      extra_id = temp + '_' + extra_id2
    endif
  end
  'BIG_E-GUN_ROUNDVIII':begin
    temp = path_comp[ndx_match+2]
    ;temp = path_comp[ndx_match:*]
    pos = strpos(temp,'EV')
    energy = strmid(temp,0,pos+2)
    ;energy = path_comp[ndx_match+2]
    extra_id1 = strmid(temp,pos+3,strlen(temp))
    if n_elements(path_comp[ndx_match:*]) gt 3 then $
      extra_id2 = path_comp[ndx_match+3] else $
      extra_id2 = ''
    if extra_id1 eq '' and extra_id2 ne '' then extra_id = extra_id2 ;else extra_id = extra_id1 + '_' + extra_id2
    if extra_id1 ne '' and extra_id2 eq '' then extra_id = extra_id1
    if extra_id1 ne '' and extra_id2 ne '' then extra_id = extra_id1 + '_' + extra_id2
    if extra_id1 eq '' and extra_id2 eq '' then extra_id = '' 
    if strmatch(temp,'Y-DISPLACEMENT') then begin
      energy = ''
      extra_id = temp + '_' + extra_id2
    endif
  end
  'BIG_E-GUN_ROUNDVIII_FINAL':begin
    temp = path_comp[ndx_match+2]
    ;temp = path_comp[ndx_match:*]
    pos = strpos(temp,'EV')
    energy = strmid(temp,0,pos+2)
    ;energy = path_comp[ndx_match+2]
    extra_id1 = strmid(temp,pos+3,strlen(temp))
    if n_elements(path_comp[ndx_match:*]) gt 3 then $
      extra_id2 = path_comp[ndx_match+3] else $
      extra_id2 = ''
    if extra_id1 eq '' and extra_id2 ne '' then extra_id = extra_id2 ;else extra_id = extra_id1 + '_' + extra_id2
    if extra_id1 ne '' and extra_id2 eq '' then extra_id = extra_id1
    if extra_id1 ne '' and extra_id2 ne '' then extra_id = extra_id1 + '_' + extra_id2
    if extra_id1 eq '' and extra_id2 eq '' then extra_id = ''
    if strmatch(temp,'Y-DISPLACEMENT') then begin
      energy = ''
      extra_id = temp + '_' + extra_id2
    endif
  end
  'NEWEGUN_ROUND10_AFTER_ENERGY_CORRECTION':begin
    temp = path_comp[ndx_match+2]
    ;temp = path_comp[ndx_match:*]
    pos = strpos(temp,'EV')
    energy = strmid(temp,0,pos+2)
    ;energy = path_comp[ndx_match+2]
    extra_id1 = strmid(temp,pos+3,strlen(temp))
    if n_elements(path_comp[ndx_match:*]) gt 3 then $
      extra_id2 = path_comp[ndx_match+3] else $
      extra_id2 = ''
    if extra_id1 eq '' and extra_id2 ne '' then extra_id = extra_id2 ;else extra_id = extra_id1 + '_' + extra_id2
    if extra_id1 ne '' and extra_id2 eq '' then extra_id = extra_id1
    if extra_id1 ne '' and extra_id2 ne '' then extra_id = extra_id1 + '_' + extra_id2
    if extra_id1 eq '' and extra_id2 eq '' then extra_id = ''
    if strmatch(temp,'Y-DISPLACEMENT') then begin
      energy = ''
      extra_id = temp + '_' + extra_id2
    endif
  end
endcase

print, 'gas: ', gas
print, 'energy: ', energy
print, 'extra: ', extra_id
print, ' '

;stop

f = file_search( path_data, '*.fits', count=nfiles )
if nfiles eq 0 then begin
  print, 'no files found for within this path:'
  print, path_data
  return
endif
;
; create a structure containing parameters from the filename
;
;struct = { channel:'', time:'', volt:0, index:0 }
;fp = replicate(struct,nfiles)
;for i = 0, nfiles - 1 do begin
;  temp = strsplit( file_basename(f[i]), '_', /extract )
;  fp[i].channel = temp[0]
;  fp[i].time = temp[2]
;  pos = strpos( temp[3], 'V' )
;  fp[i].volt = fix( strmid(temp[3], 3, pos-1 ) )
;  fp[i].index = fix( strmid( temp[5], 0, 3 )  )
;endfor
;fp1 = fp

path = file_dirname(f[0])

;
; create a structure containing parameters from the filename
;
struct = { name:'', channel:'', time:'', volt:0, index:0, int_time:0. }
file_desc = replicate(struct,nfiles)
for i = 0, nfiles - 1 do begin
  file_desc[i].name = file_basename(f[i])
  
  temp = strsplit( file_basename(f[i]), '_', /extract )
  file_desc[i].channel = temp[0]
  file_desc[i].time = temp[2]

  fbase = file_basename(f[i])

  len = strlen(fbase)
  pos = strpos(fbase,'MCP')
  temp = strmid(fbase,pos,len-1)
  pos = strpos(temp,'V')
  file_desc[i].volt = fix( strmid(temp,3,pos-3) )

  pos = strpos(fbase,'Exp')
  temp = strmid(fbase,pos,len-1)
  pos = strpos(temp,'ms')
  file_desc[i].int_time = long( strmid(temp,3,pos-3) ) / 1000.

  len = strlen(fbase)
  pos = strpos(fbase,'.fits')
  file_desc[i].index = fix( strmid(fbase,pos-3,3) )
endfor

channel_vec = ['FUV','MUV']

;
; repeat for the two channels
;
fsave_vec = []
mem1 = memory(/current)
foreach channel, channel_vec do begin

  case strmatch(extra_id,'') of
    0: id = gas + '_' + energy + '_' + channel + '_' + extra_id
    1: id = gas + '_' + energy + '_' + channel
  endcase

  fsave = path_save + id + '.idl'

  if (file_test(fsave) eq 0) or (reprocess_flag eq 1) then begin 
    ;
    ; identify light and dark data files 
    ;
    ndx_light = where( file_desc.channel eq channel and file_desc.volt eq volt_light    and file_desc.index ne 0, nlight )
    ndx_dark =  where( file_desc.channel eq channel and file_desc.volt le volt_dark_max and file_desc.index ne 0, ndark )
    ;
    ; only start processing if both light and dark data found
    ;
    if (nlight eq 0) or (ndark eq 0) then begin
      if (nlight eq 0) then begin
        print, 'no light data found for the channel ', channel, ' in the following path:'
        print, path_data
        print, ' '
      endif
      if (ndark eq 0) then begin
        print, 'no dark data found for the channel ', channel, ' in the following path:'
        print, path_data
        print, ' '
      endif
    endif else begin
      ;
      ; identify and read in each light image, then create an average
      ;
      arr_light = fltarr(1024,1024,nlight)
      ;time_light = strarr(nlight)
      jd_light = dblarr(nlight)
      struct_temps = { fuv:!values.f_nan, muv:!values.f_nan }
      temps_light = replicate(struct_temps,nlight)
      for i = 0, nlight - 1 do begin
        d = iuvs_read_fits( f[ ndx_light[i] ] )
        arr_light[*,*,i] = float( d.image )
        ;time_light[i] = strmid( d.image_header[8], 11, 23 )
        str = strmid( d.image_header[8], 11, 23 )
;        year = fix( strmid(str,0,4) )
;        month = fix( strmid(str,5,2) )
;        day = fix( strmid(str,8,2) )
;        hour = fix( strmid(str,11,2) )
;        minute = fix( strmid(str,14,2) )
;        sec = float( strmid(str,17,strlen(str)-17) )        
        TIMESTAMPTOVALUES, str+'Z', $  ; note that the trailing Z is required if the date/time is assumed UTC.  
          YEAR=year, MONTH=month, DAY=day, $
          HOUR=hour, MINUTE=minute, $
          SECOND=second, OFFSET=offset
        ;Result = JULDAY(Month, Day, Year, Hour, Minute, Second)
        jd_light[i] = julday( month, day, year, hour, minute, second )
        fuv_temp = d.telemetry_data[10].telemetry_data
        muv_temp = d.telemetry_data[11].telemetry_data
        temps_light[i].fuv = fuv_temp
        temps_light[i].muv = muv_temp
      endfor
      ;
      ; check for saturation in the raw data
      ; if found, replace pixel value with NaN
      ;
      ndx_sat_light = where( arr_light ge sat_value, count_sat_light )
      if count_sat_light gt 0 then begin
        arr_light[ndx_sat_light] = !values.f_nan
        print, 'warning - fraction of light data found to have saturated values: ', float(count_sat_light) / float( n_elements(arr_light) ) 
      endif
      ;
      arr_light_avg = mean( arr_light, dimension=3, /nan )
      arr_light_sdv = stddev( arr_light, dimension=3, /nan )
      sig_light = total( total( arr_light, 1, /nan ), 1, /nan )
      file_desc_light = file_desc[ndx_light]
      
      ;
      ; identify and read in each dark image, then create an average
      ;
      ;ndx_dark = where( file_desc.channel eq channel and file_desc.volt le volt_dark_max and file_desc.index ne 0, ndark )
;      if ndark eq 0 then begin
;        print, 'no dark data found for the channel ', channel, ' in the following path:'
;        print, path_data
;        return
;      endif
      arr_dark = fltarr(1024,1024,ndark)
      jd_dark = dblarr(ndark)
      temps_dark = replicate(struct_temps,ndark)
      for i = 0, ndark - 1 do begin
        d = iuvs_read_fits( f[ ndx_dark[i] ] )
        arr_dark[*,*,i] = float( d.image )
        str = strmid( d.image_header[8], 11, 23 )
;        year = fix(strmid(str,0,4))
;        month = fix(strmid(str,5,2))
;        day = fix(strmid(str,8,2))
;        hour = strmid(str,11,2)
;        minute = strmid(str,14,2)
;        second = strmid(str,17,strlen(str)-17)
        TIMESTAMPTOVALUES, str+'Z', $  ; note that the trailing Z is required if the date/time is assumed UTC.
          YEAR=year, MONTH=month, DAY=day, $
          HOUR=hour, MINUTE=minute, $
          SECOND=second, OFFSET=offset
        ;Result = JULDAY(Month, Day, Year, Hour, Minute, Second)
        jd_dark[i] = julday( month, day, year, hour, minute, second )
        fuv_temp = d.telemetry_data[10].telemetry_data
        muv_temp = d.telemetry_data[11].telemetry_data
        temps_dark[i].fuv = fuv_temp
        temps_dark[i].muv = muv_temp
      endfor
      ;
      ; check for saturation in the raw data
      ; if found, replace pixel value with NaN
      ;
      ndx_sat_dark = where( arr_dark ge sat_value, count_sat_dark )
      if count_sat_dark gt 0 then begin
        arr_dark[ndx_sat_dark] = !values.f_nan
        print, 'warning - fraction of dark data found to have saturated values: ', float(count_sat_dark) / float( n_elements(arr_dark) )
      endif
      ;
      arr_dark_avg = mean( arr_dark, dimension=3, /nan )
      arr_dark_sdv = stddev( arr_dark, dimension=3, /nan )
      sig_dark = total( total( arr_dark, 1, /nan ), 1, /nan )
      file_desc_dark = file_desc[ndx_dark]
      
      ;
      ; subtract average dark from average light
      ;
      arr = arr_light_avg - arr_dark_avg
    
      ; 
      ; calculate median light and dark images across time 
      ;
      arr_light_median = median(arr_light,dim=3)
      arr_dark_median = median(arr_dark,dim=3)
      ;
      ; subtract median dark from median light 
      ;
      arr_median = arr_light_median - arr_dark_median

;      img = image( arr, rgb_table=34)
;      img = image( arr_median, rgb_table=34)

      ;
      ; calculate the median absolute deviation, a measure of spread similar to the standard deviation
      ;   "The constant 1.483 is a correction factor that makes the MAD unbiased at the normal distribution"
      ;   Ref: https://i2pc.es/coss/Docencia/SignalProcessingReviews/Rousseeuw2011.pdf
      ;   2011, Robust statistics for outlier detection
      ;
      arr_light_median_diff = fltarr(1024,1024,nlight)
      for i = 0, nlight - 1 do $
        arr_light_median_diff[*,*,i] = arr_light[*,*,i] - arr_light_median
      arr_light_mad = 1.483 * median( abs(arr_light_median_diff), dim=3 )

      ;
      ; calculate the median absolute deviation, a measure of spread similar to the standard deviation
      ;
      arr_dark_median_diff = fltarr(1024,1024,ndark)
      for i = 0, ndark - 1 do $
        arr_dark_median_diff[*,*,i] = arr_dark[*,*,i] - arr_dark_median
      arr_dark_mad = 1.483 * median( abs(arr_dark_median_diff), dim=3 )

      ;title = exp_desc + '_' + channel
    
    ;  case strmatch(extra_id,'') of
    ;    0: id = gas + '_' + energy + '_' + channel + '_' + extra_id
    ;    1: id = gas + '_' + energy + '_' + channel
    ;  endcase
      
      case channel of 
        'FUV': begin
          temp_light = temps_light.fuv
          temp_dark = temps_dark.fuv
        end
        'MUV': begin
          temp_light = temps_light.muv
          temp_dark = temps_dark.muv
        end
      endcase
      
      ;
      ; rotate image so that big keyhole pointed down, and wavelength increasing with pixel to the right
      ;
      case channel of
        'FUV': begin
          rot_dir = 6
        end
        'MUV': begin
          rot_dir = 1
        end
      endcase
      arr = rotate( arr, rot_dir )
      arr_light_avg = rotate( arr_light_avg, rot_dir )
      arr_dark_avg = rotate( arr_dark_avg, rot_dir )
      arr_light_sdv = rotate( arr_light_sdv, rot_dir )
      arr_dark_sdv = rotate( arr_dark_sdv, rot_dir )
      arr_median = rotate( arr_median, rot_dir )
      arr_light_median = rotate( arr_light_median, rot_dir )
      arr_dark_median = rotate( arr_dark_median, rot_dir )
      arr_light_mad = rotate( arr_light_mad, rot_dir )
      arr_dark_mad = rotate( arr_dark_mad, rot_dir )

      ;
      ; retrieve wavelength scale
      ;
      ajello_lab_pixel_scale_rot, wlfuv, wlmuv, yfuv, ymuv
      case channel of
        'FUV': begin
          wl = wlfuv
          yspa = yfuv
        end
        'MUV': begin
          wl = wlmuv
          yspa = ymuv
        end
      endcase
    
      ;
      ; Special processing intended to look for any dependency of the acquired spectra on detector temperature (actually time, which in this case was correlated with temperature)
      ; Essentially, this produces a spectrum at the first and last hour of an observation set 
      ;
      H2_special_process = 0
      if H2_special_process then begin
        ;
        ; Special processing for H2
        ;
        ndx1_light = where( (jd_light ge jd_light[0]) and (jd_light lt jd_light[0]+1./24.), count_light )
        ndx1_dark = where( (jd_dark ge jd_dark[0]) and (jd_dark lt jd_dark[0]+1./24.), count_dark )
        
        ndx2_light = where( (jd_light ge jd_light[-1]-1./24) and (jd_light le jd_light[-1]), count_light )
        ndx2_dark = where( (jd_dark ge jd_dark[-1]-1./24) and (jd_dark le jd_dark[-1]), count_dark )
        
        p1 = plot( jd_light, temp_light, symbol='x' )
        p2 = plot( jd_dark, temp_dark, /over, symbol='x', color='red' )
        
        arr_dark_mean1 = mean( arr_dark[*,*,ndx1_dark], dimension=3 )
        arr_light_mean1 = mean( arr_light[*,*,ndx1_light], dimension=3 )
        arr_diff1 = arr_light_mean1 - arr_dark_mean1
        temp_max1 = max(temp_light[ndx1_light])
        temp_min1 = min(temp_light[ndx1_light])
      
        arr_dark_mean2 = mean( arr_dark[*,*,ndx2_dark], dimension=3 )
        arr_light_mean2 = mean( arr_light[*,*,ndx2_light], dimension=3 )
        arr_diff2 = arr_light_mean2 - arr_dark_mean2
        temp_max2 = max(temp_light[ndx2_light])
        temp_min2 = min(temp_light[ndx2_light])
        
        img1 = image( arr_diff1, min_value=0., max_value=100. )
        img2 = image( arr_diff2, min_value=0., max_value=100. )
        
        h1 = histogram(arr_diff1, locations=x1)
        h2 = histogram(arr_diff2, locations=x2)
        p1 = plot( x1, h1, color='red' )
        p2 = plot( x2, h2, /over, color='blue' )
        
        w = 10
        x0 = 552
        x1 = x0-w
        x2 = x0+w
        spec1 = total( arr_diff1[x1:x2,*], 1 )
        spec2 = total( arr_diff2[x1:x2,*], 1 )
        
        ajello_lab_pixel_scale_rot, wlfuv, wlmuv, yfuv, ymuv
        
        p1 = plot( wlfuv-(127.8-121.6), reverse(spec1), color='blue', name='cool', /ylog, xtitle='wavelength (nm)', ytitle='DN', title=file_basename(fsave,'.idl') )
        p2 = plot( wlfuv-(127.8-121.6), reverse(spec2), /over, color='red', name='hot' )
        leg = legend(target=[p1,p2])
        format = '(F5.1)'
        t1 = text( 0.35, 0.80, 'cool range: '+string(temp_min1,format=format)+' to '+string(temp_max1,format=format)+' degC', font_name='Courier' )
        t2 = text( 0.35, 0.75, ' hot range: '+string(temp_min2,format=format)+' to '+string(temp_max2,format=format)+' degC', font_name='Courier' )
        p1.save,'/users/holsclaw/desktop/idl_temp/'+file_basename(fsave,'.idl')+'_spectrum.png'
        
        print, temp_min1, temp_max1
        print, temp_min2, temp_max2
        
        stop
      end
      
      ;experimental_flag = 1
      if experimental_flag then begin
        p1 = plot( jd_light, arr_light[512,512,*], symbol='o', /sym_filled )
        p2 = plot( jd_dark, arr_dark[512,512,*], symbol='o', /sym_filled, sym_color='red', color='red', /over )
        
        arr_diff_mean = mean(arr_light,dim=3) - mean(arr_dark,dim=3)
        arr_diff_median = median(arr_light,dim=3) - median(arr_dark,dim=3)
        
        yr_spec = [150,900]
        ;spec_mean = total(arr_diff_mean[*,yr_spec[0]:yr_spec[1]], 1 )
        ;spec_median = total( arr_diff_median[*,yr_spec[0]:yr_spec[1]], 1 )
        spec_mean = total(arr[*,yr_spec[0]:yr_spec[1]], 2 )
        spec_median = total( arr_median[*,yr_spec[0]:yr_spec[1]], 2 )
        
        img = image( arr_light_sdv / mean(arr_light,dim=3), rgb_table=34 )
  
        img = image( arr_dark_sdv, rgb_table=34, min_value=0, max_value=100 )
        
        win = window(dim=[800,600])
        min_value = 0
        max_value = max(arr)
        img = image( arr, layout=[2,1,1], rgb_table=34, current=win, title='mean', min_value=min_value, max_value=max_value )
        img = image( arr_median, layout=[2,1,2], rgb_table=34, current=win, title='median', min_value=min_value, max_value=max_value )
        win.save,path_save+'ajello_lab_mean_median_comparison_images.png'
  
        win = window(dim=[800,600])
        min_value = 0
        max_value = max(arr-arr_dark_avg)
        img = image( arr_light_avg - arr_dark_avg, layout=[2,1,1], rgb_table=34, current=win, title='mean', min_value=min_value, max_value=max_value )
        img = image( arr_light_median - arr_dark_avg, layout=[2,1,2], rgb_table=34, current=win, title='median', min_value=min_value, max_value=max_value )
        win.save,path_save+'ajello_lab_mean_median_comparison_images.png'
  
        win = window(dim=[800,600])
        p1 = plot( spec_mean, current=win, thick=2, font_size=16, name='mean' )
        p2 = plot( spec_median, color='red', /over, thick=2, name='median' )
        leg = legend(target=[p1,p2])
        win.save,path_save+'ajello_lab_mean_median_comparison_spectra.png'
  
        win = window(dim=[800,600])
        img = image( arr_light_sdv, current=win, rgb_table=34 )
        win.save,path_save+'ajello_lab_sdv_light_image.png'
  
        win = window(dim=[800,600])      
        p1 = plot( (spec_mean - spec_median)/spec_median, current=win )
        
        min_value = 0
        max_value = max(arr_diff_median)
        img = image( arr_diff_mean, rgb_table=34, title='mean', min_value=min_value, max_value=max_value )
        img = image( arr_diff_median, rgb_table=34, title='median', min_value=min_value, max_value=max_value )
        
        img = image( arr_diff_mean / arr_diff_median, min_value=0, max_value=2 )
        
        arr_light_sdv_to_mean = arr_light_sdv / (mean(arr_light,dim=3) - mean(arr_dark,dim=3))
        h = histogram( arr_light_sdv_to_mean, locations=xh, binsize=0.1, min=-20, max=20 )
        p = plot( xh, h, /ylog )
        
        img = image( arr_light_sdv_to_mean, rgb_table=34, min_value=-5, max_value=5 )
        
        arr1 = fltarr(1024,1024)
        ndx = where( arr_light_sdv_to_mean gt 100 )
        arr1[ndx] = 255
        
        img = image( arr1 )
        
        xval = 595
        yval = 195
        ;xval = 848
        ;yval = 123
        xval = 460
        yval = 595
        p = plot( arr_light[xval,yval,*] )
        print, mean(arr_light[xval,yval,*])
        print, median(arr_light[xval,yval,*])
        markerp,p,y=mean(arr_light[xval,yval,*])
        markerp,p,y=median(arr_light[xval,yval,*]), color='red'
        markerp,p,y=stddev(arr_light[xval,yval,*]), color='green'
        markerp,p,y=arr_light_mad[xval,yval], color='violet'
        
        v = reform(arr_light[xval,yval,*])
        vm = 1.483*median( abs(v-median(v)) )
        print, median(v)
        print, stddev(v)
        print, stddev([v[0:21],v[23:*]])
        print, vm 
        
;        img = image( mean(arr_dark,dim=3), min_value=0, max_value=100 )
;        img = image( median(arr_dark,dim=3), min_value=0, max_value=100 )
;        
;        img = image( mean(arr_dark,dim=3) - median(arr_dark,dim=3), min_value=-20, max_value=20 )
;  
;        img = image( mean(arr_light,dim=3) - median(arr_light,dim=3), min_value=-20, max_value=20 )
;  
;        img = image( arr_light_sdv )
        
        
        ;
        ; attempt to identify and filter out discrete background events
        ;
        ;FIND, image, [ x, y, flux, sharp, round, hmin, fwhm, roundlim, sharplim
        arr_dark_avg1 = mean( arr_dark, dim=3 )
        hmin = 3000.
        fwhm = 4.
        roundlim = [-1.0,1.0]*5.
        sharplim = [0.2,1.0]        
        arr_light_filt = arr_light*0.
        w = fwhm/2.
        xvec = []
        yvec = []
        fluxvec = []
        for k = 0, nlight - 1 do begin
          ;k = 10
          arrdiff = arr_light[*,*,k] - arr_dark_avg1
          FIND, arrdiff, x, y, flux, sharp, round, hmin, fwhm, roundlim, sharplim  
          ;img = image( arrdiff ) ;, rgb_table=34
          ;p = plot( x, y, linestyle=6, symbol='o', /over, sym_color='red' )
          
          xvec = [xvec,x]
          yvec = [yvec,y]
          fluxvec = [fluxvec,flux]
          arr_light_filt[*,*,k] = arrdiff
          for i = 0, n_elements(x) - 1 do $
            arr_light_filt[x[i]-w:x[i]+w,y[i]-w:y[i]+w,k] = !values.f_nan
          
          ;img = image( arrdiff2 )
        endfor
        arr_light_filt_avg = mean( arr_light_filt, dim=3, /nan )
        arr_light_filt_avg_rot = rotate( arr_light_filt_avg, rot_dir )
        
        img = image( arr, title='arr', min_value=0, max_value=max(arr)*0.2 )
        img = image( arr_light_filt_avg_rot, title='arr_light_filt_avg_rot', min_value=0, max_value=max(arr)*0.2 )
        
        img = image( arr - arr_light_filt_avg_rot )
        
        spec = total( arr[*,yr_spec[0]:yr_spec[1]], 2 )
        spec_filt = total( arr_light_filt_avg_rot[*,yr_spec[0]:yr_spec[1]], 2 )
        
        p1 = plot( spec )
        p2 = plot( spec_filt, /over, color='red' )
        
        p = plot( spec - spec_filt )
        
        k = 525
        p1 = plot( arr_light_filt_avg_rot[*,k], thick=3 )
        p2 = plot( arr[*,k], /over, color='red' )
        
        stop
        
      endif
      
      ;  fsave = path_save + id + '.idl'
      ;stop
      source_routine = 'ajello_lab_process'
      desc = [$
        'arr: average, dark-subtracted image in units of DN per readout', $
        'arr_light_avg: average of light images in units of DN per readout', $
        'arr_dark_avg: average of dark images in units of DN per readout',$
        'arr_light_sdv: standard deviation of each pixel value in light images, units of DN', $
        'arr_dark_sdv: standard deviation of each pixel value in dark images, units of DN', $
        'arr_median: median, dark subtracted image in units of DN per readout', $
        'arr_light_median: median of light images in units of DN per readout', $
        'arr_dark_median: median of dark images in units of DN per readout', $
        'arr_light_mad: median absolute deviation of light images, units of DN, includes multiplier of 1.483', $
        'arr_dark_mad: median absolute deviation of dark images, units of DN, includes multiplier of 1.483', $
        'sig_light: total signal in DN of each light image', $
        'sig_dark: total signal in DN of each dark iamge', $
        'jd_light: Julian date time stamp of light images', $
        'jd_dark: Julian date time stamp of dark images', $
        'count_light: number of light images acquired', $
        'count_dark: number of dark images_acquired', $
        'source_routine: routine that produced this file', $
        'gas: target gas', $
        'energy: electron energy', $
        'channel: FUV or MUV', $
        'temp_light: vector of temperatures for the light observations', $
        'temp_dark: vector of temperatures for the dark observations', $
        'file_desc_light: list of files contributing to light signal', $
        'file_desc_dark: list of files contributing to dark signal', $
        'int_time: integration time in seconds for each readout', $
        'wl: estimated wavelength for each pixel', $
        'yspa: estimated spatial position for each pixel']

      int_time = file_desc[0].int_time
      save, filename=fsave, $
        arr, arr_light_avg, arr_dark_avg, arr_light_sdv, arr_dark_sdv, $
        arr_median, arr_light_median, arr_dark_median, arr_light_mad, arr_dark_mad, $
        sig_light, sig_dark, $
        jd_dark, jd_light, $
        count_light, count_dark, $
        desc, source_routine, $
        gas, energy, channel, $
        temp_light, temp_dark, $
        file_desc_light, file_desc_dark, path, $
        int_time, $ 
        wl, yspa
      
      if (reprocess_flag eq 1) then begin
        print, 'IDL save file has been regenerated, and the older version overwritten: ', fsave
      endif else begin
        print, 'IDL save file has been generated: ', fsave
      endelse
      fsave_vec = [ fsave_vec, fsave ]
    endelse  ; else completed if both light and dark data found
    
  endif else begin
    if (file_test(fsave) eq 1) and (reprocess_flag eq 0) then begin
      print, 'The following IDL save file already exists, and thus processing skipped: ', fsave
      fsave_vec = [ fsave_vec, fsave ]
    endif
  endelse
  print, ' '
  
endforeach
mem2 = memory(/current)

print, 'memory in current use:'
print, 'start of loop: ', mem1/1.e6
print, 'end of loop:  ', mem2/1.e6
print, 'diff: ', (mem2-mem1)/1.e6

;stop

end