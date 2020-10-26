;+
; NAME:
; AJELLO_LAB_IMAGE_SUMMARY_ROT
; 
; PURPOSE:
; This routine will produce a summary plot based on the reduced data contained in the specified IDL save file.
; 
; INPUTS:
; file: FUll path to IDL save file produced by "ajello_lab_process.pro".
; 
; OUTPUTS:
; win: IDL object graphics handle to the generated window.  This can be used to save the graphic (e.g. win.save,'file.png').
; 
; KEYWORDS:
; yr: (optional) Starting and ending spatial row numbers considered when averaging together data to produce the spectrum.
;-
pro ajello_lab_image_summary_rot, file, win, yr=yr_spec

  ;
  ; Determine which detector channel
  ;
  fbase = file_basename(file)
  pos_fuv = strpos(fbase,'FUV')
  pos_muv = strpos(fbase,'MUV')
  if pos_fuv gt -1 then channel='FUV'
  if pos_muv gt -1 then channel='MUV'
  ;
  ; retrieve wavelength scale
  ;
;  ajello_lab_pixel_scale_rot, wlfuv, wlmuv, yfuv, ymuv
;  case channel of
;    'FUV': begin
;      wl = wlfuv
;      yspa = yfuv
;      xr_wave = [100, 200]
;      wl_param = linfit( wlfuv, findgen(1024) )
;      y_param = linfit( yspa, findgen(1024) )
;    end
;    'MUV': begin
;      wl = wlmuv
;      yspa = ymuv
;      xr_wave = [170, 350]
;      wl_param = linfit( wlmuv, findgen(1024) )
;      y_param = linfit( yspa, findgen(1024) )
;    end
;  endcase

  if keyword_set(yr_spec) eq 0 then begin
    yr_spec = [150,900]
  endif

  ;
  ; Read in the processed data.
  ;
  restore,file

  wl_param = linfit( wl, findgen(1024) )
  y_param = linfit( yspa, findgen(1024) )
  
  ;
  ; rotate image
  ;
;  case channel of
;  'FUV': begin  
;    arr_orig = arr
;    arr_rot = rotate( arr_orig, 6 )
;    arr = arr_rot    
;  end
;  'MUV': begin
;    arr_orig = arr
;    arr_rot = rotate( arr_orig, 1 )
;    arr = arr_rot
;  end
;  endcase
  
;  mn_diff = mean(arr[x1:x2,*])
;  sd_diff = stddev(arr[x1:x2,*])

  mn_diff = mean(arr[*,yr_spec[0]:yr_spec[1]])
  sd_diff = stddev(arr[*,yr_spec[0]:yr_spec[1]])

  font_name = 'Courier'
  
  ;
  ; determine start time of experiment and duration
  ;
  jd_start = min( [jd_dark,jd_light] )
  CALDAT, jd_start, Month, Day, Year, Hour, Minute, Second
  str_start = string(year,format='(I4)')+'-'+string(month,format='(I02)')+'-'+string(day,format='(I02)')+'T'+string(hour,format='(I02)')+':'+string(minute,format='(I02)')
  ;
  t_duration = ( max( [jd_dark,jd_light] ) - jd_start ) * 24. * 60.  ; duration in minutes
  str_duration = string(t_duration,format='(I5)')+' min'
  
  ;
  ; create a mean spectrum over the entire narrow slit region
  ; create a mean spatial profile by averaging across the spectral dimension for the entire detector
  ;
;  spec_mean = mean( arr[x1:x2,*], dimension=1 )  ; for original image
;  spat_mean = mean( arr         , dimension=2 )  ; for original image
  spec_mean = mean( arr[*,yr_spec[0]:yr_spec[1]], dimension=2 )  ; for rotated image
  spat_mean = mean( arr         , dimension=1 )  ; for rotated image
  
  win = window(dimensions=[800,800])
  minval = mn_diff-sd_diff
  minval = 0
  maxval = mn_diff+sd_diff*4
  ;maxval = 300
  img1 = image( arr, min_value=minval, max_value=maxval, layout=[2,2,1], /current, rgb_table=34, title=file_basename(file), xtickdir=1, ytickdir=1, position=[0.1,0.55,0.5,0.95]  ) ; position=[0.1,0.5,0.5,0.9] , axis_style=2
  xax = AXIS('X', LOCATION='bottom', TICKDIR=1,target=img1) ;, MINOR=0
  yax = AXIS('Y', LOCATION='left', TICKDIR=1,target=img1)
  cb = colorbar(orientation=1,/border,target=img1,range=[minval,maxval])
  ;
  title='mean spectrum in narrow slit ['+string(yr_spec[0],format='(I3)')+':'+string(yr_spec[1],format='(I3)')+']'
  p1 = plot( wl,   spec_mean, layout=[2,2,3], /current, xtitle='wavelength (nm)', ytitle='DN', xr=xr_wave )
  p1['axis2'].ticklen = 0  ; remove tick markers from top axis so it can be replaced by a pixel axis
  t1 = text( 0.1, 0.9, title, target=p1, /relative )
  ;p1['axis2'].title = 'test'
  ;p1['axis2'].showtext = 1
  xaxis = AXIS('X', LOCATION='top', TITLE='pixel', target=p1, coord_transform=wl_param, textpos=1)  ; TICKFORMAT='(C(CHI),"h")' MINOR=1
  ; 
  title = 'mean spatial'
  p2 = plot( yspa, spat_mean, layout=[2,2,4], /current, xtitle='distance from beam center (mm)', ytitle='DN', xstyle=1 )
  p2['axis2'].ticklen = 0  ; remove tick markers from top axis so it can be replaced by a pixel axis
  t1 = text( 0.1, 0.9, title, target=p2, /relative )
  xaxis = AXIS('X', LOCATION='top', TITLE='pixel', target=p2, coord_transform=y_param, textpos=1)  ; TICKFORMAT='(C(CHI),"h")' MINOR=1
  markerp,p2,x=yspa[yr_spec[0]],linestyle=2  ; for rotated image
  markerp,p2,x=yspa[yr_spec[1]],linestyle=2  ; for rotated image
  
  ;
  p0 = 0.95
  pd = 0.02
  ;
  ; create a formatted table for the temperatures
  ;
  formath = '(A8,A6,  A6,  A6)'
  format =  '(A8,F6.1,F6.1,F6.2)'
  desch = string('(degC)', 'max', 'min', 'diff', format=formath)
  desc1 = string( 'T_light', max(temp_light), min(temp_light), max(temp_light)-min(temp_light), format=format )
  desc2 = string( ' T_dark', max(temp_dark) , min(temp_dark) , max(temp_dark) - min(temp_dark), format=format )
  desc = desch + '!C' + desc1 + '!C' + desc2
  text_temptable = text( 0.60, p0-pd*1, desc, font_name=font_name )
  ;
  div = (1024.*1024.)
  sig_light_avg = sig_light / div
  sig_dark_avg = sig_dark / div
  formath = '(A10,A7,  A7,  A7)'
  format =  '(A10,F7.1,F7.1,F7.1)'
  desch = string('(DN)', 'max', 'min', 'diff', format=formath)
  desc1 = string( 'Sig_light', max(sig_light_avg), min(sig_light_avg), max(sig_light_avg)-min(sig_light_avg), format=format )
  desc2 = string( ' Sig_dark', max(sig_dark_avg) , min(sig_dark_avg) , max(sig_dark_avg) - min(sig_dark_avg), format=format )
  desc = desch + '!C' + desc1 + '!C' + desc2
  text_sigtable = text( 0.60, p0-pd*5, desc, font_name=font_name )
  ;
  text_darkavg = text( 0.65, p0-pd*8, 'dark_avg (DN) = '+string(mean(arr_dark_Avg),format='(F7.1)'), font_name=font_name )
  t2ext_darksdv = text( 0.65, p0-pd*9, '    _sdv (DN) = '+string(stddev(arr_dark_Avg),format='(F7.1)'), font_name=font_name )
  ;
  ;
  text_maxsignal = text( 0.65, p0-pd*12, 'Max signal (DN) = '+string(max(arr),format='(I5)'), font_name=font_name )
  ;
  text_numlight = text( 0.65, p0-pd*14, 'Num_light = '+string(n_elements(temp_light),format='(I3)'), font_name=font_name )
  text_numdark = text( 0.65, p0-pd*15, ' Num_dark = '+string(n_elements(temp_dark),format='(I3)'), font_name=font_name )
  ;
  text_timestart = text( 0.6, p0-pd*17, 'time start = ' + str_start, font_name=font_name )
  ;
  text_duration = text( 0.6, p0-pd*18, 'duration = ' + str_duration, font_name=font_name )
  ;
  text_inttime = text( 0.6, p0-pd*19, 'int_time = ' + string(int_time,format='(I3)')+ ' sec', font_name=font_name )

;
;p1 = plot( wl, mean( arr[x1:x2,*], dimension=1 ), layout=[2,2,3], /current, xr=[0,1023], title='mean spectrum in narrow slit ['+string(x1,format='(I3)')+':'+string(x2,format='(I3)')+']', xtitle='spectral pixel', ytitle='DN' )
;p2 = plot( wl, mean( arr         , dimension=2 ), layout=[2,2,4], /current, xr=[0,1023], title='mean spatial', xtitle='spatial pixel', ytitle='DN' )

;arr_orig = arr
;;arr_rot = transpose(arr_orig)
;arr_rot = rotate( arr_orig, 6 )
;
;img1 = image( arr_orig, rgb_table=34, title='original' )
;img2 = image( arr_rot, rgb_table=34, title='rotate' )
;
;stop
  

;  t1 = text( 0.65, p0     , 'T_light_max='+string(max(temp_light),format='(F5.2)'), font_name=font_name )
;  t2 = text( 0.65, p0-pd  , '       _min='+string(min(temp_light),format='(F5.2)'), font_name=font_name )
;  t3 = text( 0.65, p0-pd*2, ' T_dark_max='+string(max(temp_dark),format='(F5.2)'), font_name=font_name )
;  t4 = text( 0.65, p0-pd*3, '       _min='+string(min(temp_dark),format='(F5.2)'), font_name=font_name )
  
  
  ;t1 = text( 0.1, 0.60, 'x1='+string(x1,format='(I4)') )
  ;t2 = text( 0.1, 0.55, 'x2='+string(x2,format='(I4)') )
  ;t3 = text( 0.2, 0.46, file_basename(file) )
  ;temp = strsplit(path,path_sep(),/extract)
  ;t3 = text( 0.2, 0.41, temp[-1] )
  ;win.save,'/users/holsclaw/desktop/idl_temp/'+file_basename(file)+'.png'


;stop

;  win = window(dim=[800,800])
;  
;  h = histogram(arr,locations=x,binsize=0.1)
;
;  mn = mean(arr)
;  sdv = stddev(arr)
;  
;  min_value = mn-sdv
;  max_value = mn+sdv
;
;  img = image(arr,min_value=min_value,max_value=max_value, layout=[2,2,1], /current)
;  ;
;  xr = [0,1023]
;  p1 = plot( total(arr, 1), xtitle='pixel', ytitle='DN', title='Sum across horizontal', xr=xr, layout=[2,2,2], /current )
;  ;
;  p2 = plot( total(arr, 2), xtitle='pixel', ytitle='DN', title='Sum across vertical', xr=xr, layout=[2,2,3], /current )
;  ;
;  ph = plot(x,h,xr=[mn-sdv*3,mn+sdv*3], xtitle='value', title='histogram', layout=[2,2,4], /current )
;  t1 = text( 0.6, 0.60, 'mean = '+string(mn,format='(F5.2)'), font_name='Courier' )
;  t2 = text( 0.6, 0.55, 'stdv = '+string(sdv,format='(F5.2)'), font_name='Courier' )

end
