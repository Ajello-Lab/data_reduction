;+
; NAME:
; AJELLO_LAB_GOLD_PHD_SEGMENTATION
; 
; PURPOSE:
; This routine will create images from a photon-list dataset in a binned range of pulse-height values.
; 
; INPUTS:
; path: A full path to set of data.
; 
; OUTPUTS:
; none (creates plot)
; 
;-
pro ajello_lab_gold_phd_segmentation, path, plt

if n_params() eq 0 then begin
  path = '/Volumes/projects/Phase_Development/MAVEN/IUVS_Data/IUVS_Breadboard/GOLD/big_e-gun_round_1/N2/100eV/medium_press/test1_image1/'
  path = '/Volumes/projects/Phase_Development/MAVEN/IUVS_Data/IUVS_Breadboard/GOLD/big_e-gun_round_1/N2/100eV/medium_press/test2_image2/'
endif

wind = [ 1011, 1369, 2667, 2641 ]  ; Taken from Alan's plots
x1 = wind[0]
y1 = wind[1]
x2 = wind[2]
y2 = wind[3]

file = file_search(path,'*.fits', count=nfiles)
if nfiles eq 0 then begin
  print, 'no files found'
  return
  stop
endif

test_flag = bytarr(nfiles)
pos = intarr(nfiles)
for i = 0, nfiles -1  do pos[i] = strpos( file_basename(file[i]), 'fits-' )
ndx_valid = where( pos ne -1, count )
if count eq 0 then begin
  print, 'no files left after filter'
  return
  stop
endif
file = file[ndx_valid]
nfiles = n_elements(file)

wind = [ 1011, 1369, 2667, 2641 ]
;ajello_lab_read_gold_data, file[0], plt, cbini, wind=wind, pmin=pmin, pmax=pmax ;, noplot=noplot
cbin = 0.
phd = 0.
data = []
for i = 0, nfiles - 1 do begin
  data = [data, mrdfits(file[i],1,hdr)]  ; data.x, data.y, data.p
endfor

binsize = 16
bin = findgen(256./binsize)*binsize
nbin = n_elements(bin)

cbin = fltarr( x2-x1+1, y2-y1+1, nbin )
for i = 0, nbin - 1 do begin
  pmin = bin[i]
  pmax = pmin + binsize - 1
  ndx = where( (data.x gt x1) and (data.x lt x2) and (data.y gt y1) and (data.y lt y2) and (data.p ge pmin) and (data.p le pmax), count )
  cbin[*,*,i] = float( hist_2d(data[ndx].x, data[ndx].y, min1=x1, min2=y1, max1=x2, max2=y2 ) )
endfor

cbin_sum = total(cbin,3)

;loadct,74
;TVLCT, red, green, blue, /GET
;red = reverse(red)
;green = reverse(green)
;blue = reverse(blue)
;red[0] = 0
;green[0] = 0
;blue[0] = 0
;red[1:*] = 255
;green[1:*] = 255
;blue[1,*] = 255
;newct = [ transpose(red), transpose(green), transpose(blue) ]

plt = window(dim=[1024,1024])
for i = 0, nbin - 1 do begin
  title = string(bin[i],format='(I3)')+' : '+string([bin[i]+binsize-1],format='(I3)')
  ;img = image( bytscl(cbin[*,*,i],0,max(cbin[*,*,i])),layout=[4,4,i+1],current=plt,title=title, rgb_table=newct )
  arr = cbin[*,*,i]
  ndx = where( arr gt 0 )
  arr[ndx] = 255
  img = image( arr, layout=[4,4,i+1], current=plt, title=title, rgb_table=0 )
endfor

stop

end