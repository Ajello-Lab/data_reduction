
;
; by default, returns the binned-averaged vector (or array)
; if sum=1, returns the binned-sum vector (or array)
;
pro ajello_lab_bin_gold, cbin, binx, biny, cbin_win_bin, sum=sum
  sz = size(cbin,/dim)
  winx = floor(sz[0]/binx)*binx
  ; 1D vector
  if n_elements(sz) eq 1 then begin
    cbin_win = cbin[0:winx-1]
    cbin_win_bin = rebin(cbin_win,winx/binx)
    if keyword_set(sum) then cbin_win *= binx
  endif
  ; 2D array
  if n_elements(sz) eq 2 then begin
    winy = floor(sz[1]/biny)*biny
    cbin_win = cbin[0:winx-1,0:winy-1]
    cbin_win_bin = rebin(cbin_win,winx/binx,winy/biny)
    if keyword_set(sum) then cbin_win_bin *= binx * biny
  endif
end

;+
; NAME:
; AJELLO_LAB_GOLD_BACKGROUND_SUBTRACTION
; 
; PURPOSE:
;   This routine will use the processed data from the GOLD EM to calculate a 
;   background-subtracted image and store the result in an IDL save file.  
;-
pro ajello_lab_gold_background_subtraction

;'/Volumes/projects/Phase_Development/MAVEN/IUVS_Data/IUVS_Breadboard/GOLD/big_e-gun_round_2/data_reduction'


source_routine_background_subtraction = file_basename(routine_filepath(),'.pro')

ajello_lab_set_paths
path_base = !path_base + 'GOLD' + path_sep()

data_set_id = 'big_e-gun_round_1'
data_set_id = 'big_e-gun_round_2'
data_set_id = 'big_e-gun_round_3'

; Default detector window
wind = [ 1011, 1369, 2667, 2641 ]

case data_set_id of
  'big_e-gun_round_1':begin
    ;
    ; Wavelength scale from Alan (see forwarded email from Joe Jan 24, 2020)
    ; Lambda (in Angstroms) = 0.1943 * Detector Column + 1187
    ;
    wl_full = 0.1943 * findgen(4096) + 1187
    wl_data_set_id = wl_full[ wind[0] : wind[2] ] / 10.    
  end
  'big_e-gun_round_2':begin
    ;
    ; Modification of the above scale where the brightest line was assumed to be 1493
    ;
    wl_full = 0.1943 * findgen(4096) + 1187 -1554+1493
    wl_data_set_id = wl_full[ wind[0] : wind[2] ] / 10.
  end
  'big_e-gun_round_3':begin
    ;
    ; Modification of the above scale where the brightest line was assumed to be 1493
    ;
    wl_full = 0.1943 * findgen(4096) + 1187 -1554+1493
    wl_data_set_id = wl_full[ wind[0] : wind[2] ] / 10.
  end
endcase



path_data_reduction = path_base + data_set_id + path_sep() + 'data_reduction/'
path_data_reduction_background_subtraction = path_base + data_set_id + path_sep() + 'data_reduction_background_subtraction/'

file = file_search( path_data_reduction, '*.sav', count=nfiles )
;filebase = file_basename(file)


fbase = strarr(nfiles)
dark_flag = bytarr(nfiles)
for i = 0, nfiles - 1 do begin
  fbasei = file_Basename(file[i])
  pos1 = strpos(fbasei,'image')
  pos2 = strpos(fbasei,'_',pos1)
  fbase[i] = strmid(fbasei,0,pos2)
  pos_dark = strpos(fbasei,'dark')
  if pos_dark ne -1 then dark_flag[i] = 1
endfor

fbase_uniq = fbase[ sort(fbase) ]

fbase_uniq = fbase[ UNIQ(fbase, SORT(fbase)) ]
num_uniq = n_elements(fbase_uniq)

buffer = 1

for i = 0, num_uniq - 1 do begin
;for i = 0, 19 - 1 do begin
  
  print, 'processing ', i, ' of ', num_uniq
  
  ndx_light = where( fbase eq fbase_uniq[i] and dark_flag eq 0, count_lighti )
  
  ndx_dark = where( fbase eq fbase_uniq[i] and dark_flag eq 1, count_darki )
  
  if count_lighti eq 0 then begin
    print, 'no light datasets found'
    stop
  endif
  if count_lighti gt 1 then begin
    print, 'more than one light dataset found'
    stop
  endif
  if (count_darki eq 0) or (count_darki gt 2) then begin
    print, 'unexpected number of dark datasets found'
    stop
  endif
  if count_darki eq 1 then begin
    print, 'only one dark dataset found'
  endif
  
  
  restore,file[ndx_dark[0]]
  save_file_dark1 = file[ndx_dark[0]]
  cr_dark1 = cbin / duration

  if count_darki eq 2 then begin
    restore,file[ndx_dark[1]]
    save_file_dark2 = file[ndx_dark[1]]
    cr_dark2 = cbin / duration
  endif

  restore,file[ndx_light[0]] ;,/ver
  save_file_light = file[ndx_light[0]]
  cr_light = cbin / duration
  phd_light = phd
  exp_desc_light = exp_desc
  hdr_list_light = hdr_list

  case count_darki of
    1: cr_light_diff = cr_light - cr_dark1
    2: cr_light_diff = cr_light - (cr_dark1 + cr_dark2)/2.
  endcase
  
  plt = window(dim=[1536,1024],buffer=buffer)
  img1 = image( cr_dark1, layout=[3,2,1], title='dark before', current=plt )
  if count_darki eq 2 then $
    img2 = image( cr_dark2, layout=[3,2,2], title='dark after', current=plt )
  img3 = image( cr_light, layout=[3,2,4], title='raw', current=plt )
  img4 = image( cr_light_diff, layout=[3,2,5], title='dark subtracted', current=plt )

  spec_light = total( cr_light, 2 )
  spec_light_diff = total( cr_light_diff, 2 )
  
  wl = wl_data_set_id
  
  ;win = window(dim=[800,600])
  p0 = plot( wl, spec_light, margin=[0.05,0.,0.,0.1], current=plt, name='raw', title=fbase_uniq[i], ytitle='spatial summed count rate', xtitle='wavelength (nm)', layout=[3,2,3] )
  p1 = plot( wl, spec_light_diff, color='red', /over, name='background-subtracted' )
  leg = legend(target=[p0,p1],position=[0.9,0.4])
  plt.save,path_data_reduction_background_subtraction + fbase_uniq[i] + '_background_subtracted.png'

  process_time_background_subtraction = systime()
  file_background_subtraction_save = path_data_reduction_background_subtraction + fbase_uniq[i] + '_background_subtracted.sav'
  
  case count_darki of
    1: begin
      var_desc = [ $
        'wl: wavelength in nanometers', $
        'xp: detector pixel value in the spectral dimension', $
        'yp: detector pixel value in the spatial dimension', $
        'cr_light_diff: Background-subtracted 2D array', $
        'cr_light: Original light image', $
        'cr_dark1: Original dark1 image', $
        'phd_light: pulse-height distribution of the data region from the light dataset, without the PH filter applied', $
        'hdr_list_light: list data type with each element containing the contents of the FITS headers for each light file', $
        'exp_desc_light: light data set id, gas, energy, pressure id, test id, full data path ', $
        'save_file_light: IDL save file from which the light data was obtained', $
        'save_file_dark1: IDL save file from which first set of dark data was obtained', $
        'process_time_background_subtraction: date and time at which this file was produced' ]
      save,filename=file_background_subtraction_save, wl, xp, yp, cr_light_diff, cr_light, cr_dark1, phd_light, exp_desc_light, source_routine_background_subtraction, $
        save_file_light, save_file_dark1, process_time_background_subtraction, var_desc
    end
    2: begin
      var_desc = [ $
        'wl: wavelength in nanometers', $
        'xp: detector pixel value in the spectral dimension', $
        'yp: detector pixel value in the spatial dimension', $
        'cr_light_diff: Background-subtracted 2D array', $
        'cr_light: Original light image', $
        'cr_dark1: Original dark1 image', $
        'cr_dark2: Original dark2 image', $
        'phd_light: pulse-height distribution of the data region from the light dataset, without the PH filter applied', $
        'hdr_list_light: list data type with each element containing the contents of the FITS headers for each light file', $
        'exp_desc_light: light data set id, gas, energy, pressure id, test id, full data path ', $
        'save_file_light: IDL save file from which the light data was obtained', $
        'save_file_dark1: IDL save file from which first set of dark data was obtained', $
        'save_file_dark2: IDL save file from which second set of dark data was obtained', $
        'process_time_background_subtraction: date and time at which this file was produced' ]
      save,filename=file_background_subtraction_save, wl, xp, yp, cr_light_diff, cr_light, cr_dark1, cr_dark2, phd_light, exp_desc_light, source_routine_background_subtraction, $
        save_file_light, save_file_dark1, save_file_dark2, process_time_background_subtraction, var_desc
    end
  endcase

  ;stop
    
endfor


stop


restore,'/Volumes/projects/Phase_Development/MAVEN/IUVS_Data/IUVS_Breadboard/GOLD/big_e-gun_round_2/data_reduction/N2_100eV_med_pres_test3_image3_dark_after_pmax_250.sav',/ver
cr_dark_after = cbin / duration
print, duration

restore,'/Volumes/projects/Phase_Development/MAVEN/IUVS_Data/IUVS_Breadboard/GOLD/big_e-gun_round_2/data_reduction/N2_100eV_med_pres_test3_image3_dark_before_pmax_250.sav'
cr_dark_before = cbin / duration
print, duration

restore,'/Volumes/projects/Phase_Development/MAVEN/IUVS_Data/IUVS_Breadboard/GOLD/big_e-gun_round_2/data_reduction/N2_100eV_med_pres_test3_image3_pmax_250.sav'
cr_light = cbin / duration
print, duration

cr_light_diff = cr_light - (cr_dark_before + cr_dark_after)/2.

plt = window(dim=[1024,1024])
img1 = image( cr_dark_before, layout=[2,2,1], title='dark before', current=plt )
img2 = image( cr_dark_after, layout=[2,2,2], title='dark after', current=plt )
img3 = image( cr_light, layout=[2,2,3], title='raw', current=plt )
img4 = image( cr_light_diff, layout=[2,2,4], title='dark subtracted', current=plt )

binx = 8
biny = 8
ajello_lab_bin_gold, cr_dark_before, binx, biny, cr_dark_before_bin, /sum
ajello_lab_bin_gold, cr_dark_after, binx, biny, cr_dark_after_bin, /sum
ajello_lab_bin_gold, cr_light, binx, biny, cr_light_bin, /sum
ajello_lab_bin_gold, wl, binx, 1, wl_bin
cr_light_diff_bin = cr_light_bin - (cr_dark_before_bin + cr_dark_after_bin)/2.

plt = window(dim=[1024,1024])
img1 = image( cr_dark_before_bin, layout=[2,2,1], title='dark before (binned)', current=plt )
img2 = image( cr_dark_after_bin, layout=[2,2,2], title='dark after (binned)', current=plt )
img3 = image( cr_light_bin, layout=[2,2,3], title='raw (binned)', current=plt )
img4 = image( cr_light_diff_bin, layout=[2,2,4], title='dark subtracted (binned)', current=plt )

spec_light = total( cr_light, 2 )
spec_light_diff = total( cr_light_diff, 2 )
spec_light_diff_bin = total( cr_light_diff_bin, 2 )

win = window(dim=[800,600])
p0 = plot( wl, spec_light, current=win )
p1 = plot( wl, spec_light_diff, color='blue', /over )
p2 = plot( wl_bin, spec_light_diff_bin/binx, /over, color='red', thick=3 )


stop

;struct = { gas:'', energy:'', pressure_id:'', test_id:'', dark_flag:0b }
;exp_desc = replicate( struct, nfiles )

exp_desc_vec = []
num_test_id_components = intarr(nfiles)
dark_flag = bytarr(nfiles)
for i = 0, nfiles - 1 do begin
  ;str = strsplit( file_basename(file[i]), '_', /extract )
  
  ; Create a savefile object.
  sObj = OBJ_NEW('IDL_Savefile', file[i])
  sObj->Restore, 'exp_desc'
  
  exp_desc_vec = [ exp_desc_vec, exp_desc ]
  OBJ_DESTROY, sObj
  
  str = strsplit( exp_desc.test_id, '_', /extract )
  num_test_id_components[i] = n_elements(str)
  
  pos = strpos( exp_desc.test_id, 'dark' )
  
  if pos ne -1 then dark_flag[i] = 1
   
  ;stop
  
endfor

energy_uniq = exp_desc_vec[ uniq( exp_desc_vec.energy ) ].energy

num_energy = n_elements( energy_uniq )
for i = 0, num_energy - 1 do begin
  ndxi = where( (exp_desc_vec.energy eq energy_uniq[i]) and dark_flag eq 0, counti )
  num_datasets = counti
  
  for j = 0, num_datasets - 1 do begin
    pos = strpos( exp_desc_vec[ndxi].test_id, exp_desc_vec[ndxi[j]].test_id ) 
    
    
    stop
  endfor
  
  stop
  
endfor


ndx_datasets = where( dark_flag eq 0, count )
num_datasets = count

pos_vec = intarr(num_datasets)
for i = 0, num_datasets - 1 do begin
  pos_vec[i] = strpos( exp_desc_vec.test_id, exp_desc_vec[ndx_datasets[i]].test_id )
   
endfor



stop

path_data = file_search( path_data_reduction, 'test*', /test_dir, count=num_data_sets )
if num_data_sets eq 0 then begin
  print, 'no data found'
  stop
endif


stop

end