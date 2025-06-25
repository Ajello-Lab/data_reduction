;+
; PURPOSE
;  This routine will fit a low-order polynomial across all spatial columns 
;   based on the ranges provided in order to provide an estimate of 
;   interpolated values.
;   
; INPUTS
;  arr: 2D array, assumed axes of [spectral,spatial]
; 
; KEYWORDS
;  yr_spec: 2-element array defining the spatial pixel range of the narrow 
;    slit, avoiding the keyholes. Default is [150,900]
;    
;  yr_bump: 2-element array defining the spatial pixel range of the anomalous 
;    feature to be interpolated across. Default is [300,450]
;    Constraints: yr_bump[0] > yr_spec[0]; yr_bump[1] < yr_spec[1]
;            
;  poly_order: order of the polynomial used to fit to the valid data
; 
; OUTPUTS
;  arr_interp: 2D array, same dimensions as arr 
;  
; EXAMPLE
;  restore,IDL_save_file
;  yr_spec = [150,900] ; spatial range of narrow slit, that avoids the keyholes
;  yr_bump = [300,450] ; spatial range of feature to interpolate across
;  ajello_lab_interpolate_spatial_feature, arr, arr_interp, yr_spec=yr_spec, $
;    yr_bump=yr_bump
;  
;-
pro ajello_lab_interpolate_spatial_feature, arr, arr_interp, $
  yr_spec=yr_spec, yr_bump=yr_bump, $
  poly_order=poly_order, show_plots=show_plots, ylog=ylog

;
; set default spatial range values
;
if isa(yr_spec) eq 0 then yr_spec = [200,900]
if isa(yr_bump) eq 0 then yr_bump = [300,450]
if isa(poly_order) eq 0 then poly_order = 1
if isa(show_plots) eq 0 then show_plots = 0
if isa(ylog) eq 0 then ylog = 0

;
; check range parameters
;
if yr_bump[0] lt yr_spec[0] then begin
  print, 'yr_bump[0] must be larger than yr_spec[0]
  stop
endif
if yr_bump[1] gt yr_spec[1] then begin
  print, 'yr_bump[1] must be less than than yr_spec[1]
  stop
endif

;
; restore test data if no parameters present
;
if n_params() eq 0 then begin
  file = '/Volumes/projects/Phase_Development/MAVEN/IUVS_Data/IUVS_Breadboard/Big_e-gun_RoundVIII_final/data_reduction/N2_100EV_FUV_TEST18_IMAGE2_HIPRES.idl'
  restore,file,/ver
  show_plots = 1
endif

;
; create output array
; 
arr_interp = arr
;
; define valid region that will be used for polynomial fit
;
sz = size(arr,/dim)
y = findgen(sz[1])
ndx_good = where( ((y gt yr_spec[0]) and (y lt yr_bump[0])) or $
                  ((y lt yr_spec[1]) and (y gt yr_bump[1])) )
;
; iterate through each wavelength
;
if ylog eq 0 then begin
  for i = 0, sz[0] - 1 do begin
    ;
    ; fit polynomial to valid data
    ;
    v = reform(arr_interp[i,*])
    param = poly_fit( y[ndx_good], v[ndx_good], poly_order, yfit=yfit )
    ;
    ; replace anomalous data with the polynomial values 
    ;
    arr_interp[i,yr_bump[0]:yr_bump[1]] = poly(y[yr_bump[0]:yr_bump[1]],param)
  endfor
endif else begin
  for i = 0, sz[0] - 1 do begin
    ;
    ; fit polynomial to valid data
    ;
    v = reform(arr_interp[i,*])
    vmin = min(v[ndx_good])
    vf = alog10(v-vmin)
    n = where( finite(vf[ndx_good]) )
    param = poly_fit( y[ndx_good[n]], vf[ndx_good[n]], poly_order, yfit=yfit )
    ;
    ; replace anomalous data with the polynomial values
    ;
    arr_interp[i,yr_bump[0]:yr_bump[1]] = 10.^poly(y[yr_bump[0]:yr_bump[1]],param) + vmin
  endfor  
endelse


;
; plot test data if no parameters are present
;
if keyword_set(show_plots) then begin
  min_value = 0
  max_value = max(arr_interp)
  img = image( arr, rgb_table=34, min_value=min_value, max_value=max_value )
  img = image( arr_interp, rgb_table=34, min_value=min_value, max_value=max_value )

  k = 747
  win = window(dim=[800,600])
  p1 = plot( arr[k,*], current=win )
  p2 = plot( arr_interp[k,*], /over, color='red' )

  spec = mean( arr[*,yr_spec[0]:yr_spec[1]], dim=2 )
  spec_interp = mean( arr_interp[*,yr_spec[0]:yr_spec[1]], dim=2 )
  
  p1 = plot( spec )
  p2 = plot( spec_interp, /over, color='red' )
  
  p1 = plot( mean(arr,dim=1), /ylog )
  p2 = plot( mean(arr_interp,dim=1), /over, color='red' )

  stop
  
endif

end


pro ajello_lab_interpolate_spatial_feature_example

  @'qualcolors'
  loadcv, 77, rgb_table=rgb_table, /noqual

file = '/Volumes/projects/Phase_Development/MAVEN/IUVS_Data/IUVS_Breadboard/NeweGun_round10_after_energy_correction/data_reduction/N2_100EV_FUV_TEST7_IMAGE2_HIPRESS_WITHH20.idl'
restore,file,/ver

img = image( arr, rgb_table=rgb_table, min_value=0., max_value=max(arr)*0.2 )

p = plot( total( arr, 1 ) )

stop

end