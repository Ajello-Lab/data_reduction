;+
; NAME:
; AJELLO_LAB_GOLD_READ_DATA
; 
; PURPOSE:
; This routine will read in a FITS-format data file from the GOLD breadboard, and return a spatially binned 2D array.
;
; INPUTS:
; file: filename of the data file to read
;   
; KEYWORD PARAMETERS:
; wind: window subregion as a four element vector, [ x1, y1, x2, y2 ]
; 
; OUTPUTS:
; cbin: binned counts 2D image
; phd: pulse-height distribution of the unfiltered data
; plt: plot handle, which can be used to save the contents.
;-
pro ajello_lab_gold_read_data, file, data, hdr, cbin, phd, plt, wind=wind, pmin=pmin, pmax=pmax, noplot=noplot, err=err

err = 0

if n_params() eq 0 then begin
  file = '/Volumes/projects/Phase_Development/MAVEN/IUVS_Data/IUVS_Breadboard/GOLD/big_e-gun_round_1/N2/100eV/medium_press/test1_image1/GOLD_100eV_N2_1e-5_test1_image1_20200119_085250.fits-001.fits'
  ;file = '/Volumes/projects/Phase_Development/MAVEN/IUVS_Data/IUVS_Breadboard/GOLD/big_e-gun_round_1/N2/100eV/medium_press/test1_image1/GOLD_100eV_N2_1e-5_test1_image1_20200119_085250.fits-002.fits'
  file = '/Users/holsclaw/GOLD/data/big_e-gun_round_1/N2/100eV/medium_press/test1_image1/GOLD_100eV_N2_1e-5_test1_image1_20200119_085250.fits-001.fits'
  pmin = 0
  pmax = 255
endif

if keyword_set(wind) eq 0 then begin
  ;wind = [ x1, y1, x2, y2 ]
  ;wind = [ 1369, 1011, 2641, 2667 ]  ; Taken from Alan's plots
  wind = [ 1011, 1369, 2667, 2641 ]  ; Taken from Alan's plots
;  x1 = 1369
;  x2 = 2641
;  y1 = 1011
;  y2 = 2667
endif

if keyword_set(pmin) eq 0 then pmin = 0
if keyword_set(pmax) eq 0 then pmax = 255

x1 = wind[0]
y1 = wind[1]
x2 = wind[2]
y2 = wind[3]

;
; read in the photon list data
; 
;data0 = mrdfits(file,0,hdr0)
data = mrdfits(file,1,hdr)  ; data.x, data.y, data.p
sz = size(data,/str)
if sz.type_name eq 'LONG' then begin
  print, 'No date in file: ', file
  err = 1
  return
endif
;obs_time = sxpar(hdr_data, 'OBSTIME')

ndx_spat_only = where( (data.x gt x1) and (data.x lt x2) and (data.y gt y1) and (data.y lt y2), count )

ndx = where( (data.x gt x1) and (data.x lt x2) and (data.y gt y1) and (data.y lt y2) and (data.p ge pmin) and (data.p le pmax), count )

;
;
; Create a 2D image for the selected window subregion
;  
;cbin = float( hist_2d(data.x, data.y, min1=0, min2=0, max1=4095, max2=4095) )
;cbin_win = float( hist_2d(data.x, data.y, min1=x1, min2=y1, max1=x2, max2=y2) )
cbin = float( hist_2d(data[ndx].x, data[ndx].y, min1=x1, min2=y1, max1=x2, max2=y2 ) )

h = histogram( cbin, locations=xh )
;p1 = plot( xh, h, /ylog, xr=[0,50], color='blue' )

phd = histogram( data[ndx_spat_only].p, locations=x_phd, min=0, max=255 )

if keyword_set(noplot) eq 0 then noplot=0

if noplot eq 0 then begin

  font_size = 14
  thick = 2
  
  plt = window(dim=[1000,1000])
  
  TVLCT, red, green, blue, /GET
  red = reverse(red)
  green = reverse(green)
  blue = reverse(blue)
  red[0] = 0
  green[0] = 0
  blue[0] = 0
  newct = [ transpose(red), transpose(green), transpose(blue) ]
  img = image( bytscl(cbin,0,30), rgb_table=newct, aspect_ratio=1, current=plt, layout=[2,2,1] )
  
  margin = [0.15,0.1,0.1,0.1]
  p = plot( x_phd, phd, xr=[0,255], title='PHD', xtitle='pulse height', ytitle='', /ylog, font_size=font_size, thick=thick, /current, layout=[2,2,2], margin=margin )
  markerp,p,x=pmin,linestyle=2
  markerp,p,x=pmax,linestyle=2
  
  spec = total(cbin,2)
  
  margin = [0.15,0.1,0.1,0.1]
  p = plot( spec, current=plt, layout=[2,2,3], font_size=font_size, thick=thick, margin=margin )
  
  ; GOLD pixel spacing
  ;w_pixel = 1./57.8  ; mm, average distance between photoevent locations in the spectral dimension
  ;h_pixel = 1./54  ; mm, average distance between photoevent locations in the spatial dimension
  
  loadct,74
  ;loadct,34
  
  ;img = image( alog10(cbin) > 0.1, min=1, max=4, rgb_table=newct )

endif

if n_params() eq 0 then stop

;stop

end