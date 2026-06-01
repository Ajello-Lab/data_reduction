
pro ajello_lab_gold_background_analysis

path_data = '/Volumes/projects/Phase_Development/MAVEN/IUVS_Data/IUVS_Breadboard/GOLD/kimball_egun_round_11/data_reduction/'
path_save = path_data

file = file_search( path_data, '*.sav', count=nfiles )
fbase = file_basename(file)

energy = fltarr(nfiles)
pressure_raw = strarr(nfiles)
int_time = fltarr(nfiles)
background = fltarr(nfiles)
for i = 0, nfiles - 1 do begin
  print, i+1, ' of ', nfiles
  
  pos = strpos( fbase[i], '_' )
  
  temp = strsplit( fbase[i], '_', /extract )
  
  pos = strpos( strlowcase(temp[1]), 'ev' )
  energy[i] = strmid( temp[1], 0, pos )
  
  pressure_raw[i] = temp[2]

  restore,file[i] ;,/ver
  
  if n_elements(cbin_back) eq 1 then begin
    background[i] = !values.f_nan
  endif else begin
    background[i] = total(cbin_back)
  endelse
  
  int_time[i] = sxpar( hdr_list[0], 'INT-TIME' )
  
  if i eq 0 then begin
    sz = size(cbin_back,/dim)
    cbin_back_arr = fltarr(sz[0],sz[1],nfiles)
  endif
  cbin_back_arr[*,*,i] = cbin_back
  
  if energy[i] eq 0 then stop
  
endfor
energy_str = string(energy,format='(I3)')

pressure = strarr(nfiles)
ndx_hi = where( pressure_raw eq 'hi-pres' or pressure_raw eq 'hi', count_hi )
pressure[ndx_hi] = 'hi'
ndx_med = where( pressure_raw eq 'med-pres' or pressure_raw eq 'med', count_md )
pressure[ndx_med] = 'med'
ndx_lo = where( pressure_raw eq 'lo-pres' or pressure_raw eq 'lo', count_md )
pressure[ndx_lo] = 'lo'

;if count_hi+count_md ne nfiles then begin
;  print, 'number of hi and med pressure files less than total found'
;  stop
;endif


;p1 = plot( energy, background/int_time, symbol='o', /sym_filled )

win = window(dim=[800,600])
p1 = plot( energy[ndx_hi], (background)[ndx_hi], symbol='o', /sym_filled, $
  xr=[0,110], font_size=16, xtitle='energy (eV)', ytitle='count rate', $
  title='background total count rate', current=win, name='hi pressure' )
p2 = plot( energy[ndx_med], (background)[ndx_med], symbol='o', /sym_filled, $
  color='red', /over, name='med pressure' )
p3 = plot( energy[ndx_lo], (background)[ndx_lo], symbol='o', /sym_filled, $
  color='blue', /over, name='lo pressure' )  
leg = legend(target=[p1,p2,p3],position=[0.8,0.8],font_size=14)
win.save,path_save+'ajello_lab_background_vs_energy.png'

nx = ceil(sqrt(nfiles))
ny = round(sqrt(nfiles))
while nx*ny lt nfiles do $ 
  ny +=1

ndx_sort = sort(energy)

win = window(dim=[1200,1200])
minval = 0
maxval = max(cbin_back_arr)
for i = 0, nfiles - 1 do $
  im = image( cbin_back_arr[*,*,ndx_sort[i]], layout=[nx,ny,i+1], $
  current=win, title=energy_str[ndx_sort[i]]+' eV, '+pressure[ndx_sort[i]], $
  min_value=minval, max_value=maxval )

win = window(dim=[1200,1200])
minval = 0
maxval = max(cbin_back_arr)
for i = 0, nfiles - 1 do $
  im = image( cbin_back_arr[*,*,ndx_sort[i]], layout=[nx,ny,i+1], $
  current=win, title=energy_str[ndx_sort[i]]+' eV, '+pressure[ndx_sort[i]] )

win = window(dim=[1200,1200])
minval = 0
maxval = max(cbin_back_arr)
minval = min(alog10(cbin_back_arr),/nan)
maxval = max(alog10(cbin_back_arr),/nan)
for i = 0, nfiles - 1 do $
  im = image( alog10(cbin_back_arr[*,*,ndx_sort[i]]), layout=[nx,ny,i+1], $
  current=win, title=energy_str[ndx_sort[i]]+' eV, '+pressure[ndx_sort[i]], $
  min_value=minval, max_value=maxval )
;win.save,path_save+'ajello_lab_background_images_vs_energy_vs_pressure.png'

stop

end