;+
; NAME:
; AJELLO_LAB_PROCESS
; 
; PURPOSE:
; For one set of IUVS data, identified in the given path 'path_data', read in all dark scans and all light scans, 
;  average each, compute the difference, and record in an IDL save file for later processing.
;
; INPUTS:
; path_data: path to the data
; path_save: path where data should be saved
;
; OUTPUTS:
; fsave_vec: full path to the IDL save file
; 
; NOTES:
; The processing is repeated on FUV and MUV datasets, which are assumed to be located in the same given path, path_data.
; This routine is called iteratively by "ajello_lab_process_driver" in order to process multiple datasets.
;  
; TODO: Modify so it saves rotated image
;-
pro ajello_lab_process, path_data, path_save, fsave_vec

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
ndx_match = where( strmatch( strmid(path_comp,0,10), '_BIG_E-GUN' ) )
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
endcase

print, 'gas: ', gas
print, 'energy: ', energy
print, 'extra: ', extra_id

;stop

f = file_search( path_data, '*.fits', count=nfiles )
if nfiles eq 0 then begin
  print, 'no files found for this path:'
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
  ;
  ; identify and read in each light image, then create an average
  ;
  ndx_light = where( file_desc.channel eq channel and file_desc.volt eq volt_light and file_desc.index ne 0, nlight )
  if nlight eq 0 then begin
    print, 'no light data found for the channel ', channel, ' in the following path:'
    print, path_data
    return
  endif
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
    year = strmid(str,0,4)
    mnth = strmid(str,5,2)
    day = strmid(str,8,2)
    hour = strmid(str,11,2)
    min = strmid(str,14,2)
    sec = strmid(str,17,strlen(str)-17)
    ;Result = JULDAY(Month, Day, Year, Hour, Minute, Second)
    jd_light[i] = julday( mnth, day, year, hour, min, sec )
    fuv_temp = d.telemetry_data[10].telemetry_data
    muv_temp = d.telemetry_data[11].telemetry_data
    temps_light[i].fuv = fuv_temp
    temps_light[i].muv = muv_temp
  endfor
  arr_light_avg = mean( arr_light, dimension=3 )
  arr_light_sdv = stddev( arr_light, dimension=3 )
  sig_light = total( total( arr_light, 1 ), 1 )
  file_desc_light = file_desc[ndx_light]
  
  ;
  ; identify and read in each dark image, then create an average
  ;
  ndx_dark = where( file_desc.channel eq channel and file_desc.volt le volt_dark_max and file_desc.index ne 0, ndark )
  if ndark eq 0 then begin
    print, 'no dark data found for the channel ', channel, ' in the following path:'
    print, path_data
    return
  endif
  arr_dark = fltarr(1024,1024,ndark)
  jd_dark = dblarr(ndark)
  temps_dark = replicate(struct_temps,ndark)
  for i = 0, ndark - 1 do begin
    d = iuvs_read_fits( f[ ndx_dark[i] ] )
    arr_dark[*,*,i] = float( d.image )
    str = strmid( d.image_header[8], 11, 23 )
    year = strmid(str,0,4)
    mnth = strmid(str,5,2)
    day = strmid(str,8,2)
    hour = strmid(str,11,2)
    min = strmid(str,14,2)
    sec = strmid(str,17,strlen(str)-17)
    ;Result = JULDAY(Month, Day, Year, Hour, Minute, Second)
    jd_dark[i] = julday( mnth, day, year, hour, min, sec )
    fuv_temp = d.telemetry_data[10].telemetry_data
    muv_temp = d.telemetry_data[11].telemetry_data
    temps_dark[i].fuv = fuv_temp
    temps_dark[i].muv = muv_temp
  endfor
  arr_dark_avg = mean( arr_dark, dimension=3 )
  arr_dark_sdv = stddev( arr_dark, dimension=3 )
  sig_dark = total( total( arr_dark, 1 ), 1 )
  file_desc_dark = file_desc[ndx_dark]
  
  arr = arr_light_avg - arr_dark_avg

  ;title = exp_desc + '_' + channel

  case strmatch(extra_id,'') of
    0: id = gas + '_' + energy + '_' + channel + '_' + extra_id
    1: id = gas + '_' + energy + '_' + channel
  endcase
  
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
  
  
;  for i = 0, 1023 do begin
;    for j = 0, 1023 do begin
;      stop
;
;    endfor
;  endfor
;
;  p1 = plot( jd_light, arr_light[512,512,*], symbol='x', color='red', linestyle='' )
;  p2 = plot( jd_dark, arr_dark[512,512,*], symbol='o', /over, color='blue' )
;
;  stop

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
  
  fsave = path_save + id + '.idl'
  ;stop
  source_routine = 'ajello_lab_process'
  desc = ['arr: average, dark-subtracted image in units of DN.', $
    'arr_light_avg: average of light images', $
    'arr_dark_avg: average of dark images',$
    'arr_light_sdv: standard deviation of each pixel value in light images', $
    'arr_dark_sdv: standard deviation of each pixel value in dark images', $
    'temp_light: vector of temperatures for the light observations', $
    'temp_dark: vector of temkperatures for the dark observations', $
    'jd_dark: Julian date of dark images', $
    'sig_dark: total signal in DN of each dark iamge', $
    'jd_light: Julian date of light images', $
    'sig_light: total signal in DN of each light image', $
    'file_desc_light: list of files contributing to light signal', $
    'file_desc_dark: list of files contributing to dark signal', $
    'int_time: integration time in seconds', $
    'wl: estimated wavelength for each pixel', $
    'yspa: estimated spatial position for each pixel']
    
  int_time = file_desc[0].int_time
  save, filename=fsave, arr, arr_light_avg, arr_dark_avg, arr_light_sdv, arr_dark_sdv, desc, $
    source_routine, gas, energy, channel, temp_light, temp_dark, path, $
    jd_dark, sig_dark, jd_light, sig_light, $
    file_desc_light, file_desc_dark, int_time, $ ;, mask
    wl, yspa
  
  fsave_vec = [ fsave_vec, fsave ]
  
endforeach
mem2 = memory(/current)

print, 'memory in current use:'
print, 'start of loop: ', mem1/1.e6
print, 'end of loop:  ', mem2/1.e6
print, 'diff: ', (mem2-mem1)/1.e6

;stop

end