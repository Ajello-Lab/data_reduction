;+
; NAME:
; AJELLO_LAB_GOLD_PROCESS
; 
; PURPOSE:
; This routine will co-add all data in a given path, returning a binned 2D array
;   
; INPUTS:
; path: Full path to one set of data files collected using the GOLD engineering model.
; 
; KEYWORDS:
; pmin: (optional) Photoevents less than this value will be excluded from cbin.  Default is 0.
; pmax: (optional) Photoevents greater than this value will be excluded from cbin.  Default is 255.
; buffer: (optional) Set to one to prevent the summary image from being displayed to the screen.
;   
; OUTPUTS:
; cbin: 2D array containing the binned and co-added data.
; phd: Summed pulse height vector, 256 elements.
; hdr_list: list data type with each element containing the contents of the FITS header
; plt: plot handle, which can be used to save the contents.
; plt_pbin:plot handle, which can be used to save the contents.
;-
pro ajello_lab_gold_process, path, wl, xp, yp, cbin, phd, hdr_list, plt, plt_pbin, pmin=pmin, pmax=pmax, buffer=buffer

if n_params() eq 0 then begin
  path = '/Volumes/projects/Phase_Development/MAVEN/IUVS_Data/IUVS_Breadboard/GOLD/big_e-gun_round_1/N2/100eV/medium_press/test1_image1/'
  ;path = '/Users/holsclaw/GOLD/data/big_e-gun_round_1/N2/100eV/medium_press/test1_image1/'
  
  pmin = 0.
  pmax = 250 
  ;pmax = 255
endif

if keyword_set(noplot) eq 0 then noplot = 0
if keyword_set(pmin) eq 0 then pmin = 0
if keyword_set(pmax) eq 0 then pmax = 255


file = file_search(path,'*.fits', count=nfiles)
if nfiles eq 0 then begin
  print, 'no files found'
  return
  stop
endif

print, file[0]

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
hdr_list = list()
for i = 0, nfiles - 1 do begin
  ajello_lab_gold_read_data, file[i], datai, hdri, cbini, phdi, plti, wind=wind, pmin=pmin, pmax=pmax, /noplot, err=err
  if err eq 0 then begin
    cbin += cbini
    phd += phdi
    data = [data,datai]
    hdr_list.add,hdri
  endif
endfor

x1 = wind[0]
y1 = wind[1]
x2 = wind[2]
y2 = wind[3]

binsize = 16
bin = findgen(256./binsize)*binsize
nbin = n_elements(bin)
cbin_pbin = fltarr( x2-x1+1, y2-y1+1, nbin )
for i = 0, nbin - 1 do begin
  pmini = bin[i]
  pmaxi = pmini + binsize - 1
  ndx = where( (data.x gt x1) and (data.x lt x2) and (data.y gt y1) and (data.y lt y2) and (data.p ge pmini) and (data.p le pmaxi), count )
  if count gt 0 then $
    cbin_pbin[*,*,i] = float( hist_2d(data[ndx].x, data[ndx].y, min1=x1, min2=y1, max1=x2, max2=y2 ) )
endfor


plt_pbin = window(dim=[1024,1024],buffer=buffer)
for i = 0, nbin - 1 do begin
  title = string(bin[i],format='(I3)')+' : '+string([bin[i]+binsize-1],format='(I3)')
  ;img = image( bytscl(cbin[*,*,i],0,max(cbin[*,*,i])),layout=[4,4,i+1],current=plt,title=title, rgb_table=newct )
  arr = cbin_pbin[*,*,i]
  ndx = where( arr gt 0 )
  arr[ndx] = 255
  img = image( arr, layout=[4,4,i+1], current=plt_pbin, title=title, rgb_table=0 )
endfor


;
; Wavelength scale from Alan (see forwarded email from Joe Jan 24, 2020)
; Lambda (in Angstroms) = 0.1943 * Detector Column + 1187
;   
wl_full = 0.1943 * findgen(4096) + 1187
wl = wl_full[ wind[0] : wind[2] ] / 10.

;p = plot( wl, total(cbin,2) )
;markerp,p,x=149.3,linestyle=2

sz = size(cbin,/dim)
xp = findgen(sz[0])+wind[0]
yp = findgen(sz[1])+wind[1]

  
  font_size = 10
  thick = 2
  
  plt = window(dim=[1000,1000],buffer=buffer)
  
  ; plot position keyword format: [X1, Y1, X2, Y2]
  plt_dim = [0.42, 0.42]
  position = fltarr(4,4)
  xs = [0.1, 0.57]
  ys = [0.1, 0.57]
  position[*,0] = [ xs[0], ys[1], xs[0]+plt_dim[0], ys[1]+plt_dim[1]]
  position[*,1] = [ xs[1], ys[1], xs[1]+plt_dim[0], ys[1]+plt_dim[1]]
  position[*,2] = [ xs[0], ys[0], xs[0]+plt_dim[0], ys[0]+plt_dim[1]]
  
  margin = [0,0,0,0]
  
  loadct,74
  TVLCT, red, green, blue, /GET
  red = reverse(red)
  green = reverse(green)
  blue = reverse(blue)
  red[0] = 0
  green[0] = 0
  blue[0] = 0
  newct = [ transpose(red), transpose(green), transpose(blue) ]
  img = image( bytscl(cbin,0,max(cbin)), xp, yp, rgb_table=newct, layout=[2,2,1], current=plt, axis_style=1, $
    xtitle='column (pixel)', ytitle='row (pixel)', position=position[*,0], margin=margin, $
    xmajor=-1, ymajor=-1, xtickdir=1, ytickdir=1 )
  
  ;Result = graphic.CONVERTCOORD( X, [Y [, Z]] [, /DATA] [, /DEVICE] [, /NORMAL, /RELATIVE] [, /TO_DATA] [, /TO_DEVICE] [, /TO_NORMAL, /TO_RELATIVE]) 
  
  ;
  ; get the vertical position of the image, to use in scaling the spatial plot to match
  ;
  r0 = img.convertcoord( xp[ 0], yp [0], /data, /to_normal )
  r1 = img.convertcoord( xp[-1], yp[-1], /data, /to_normal )
  position[1,1] = r0[1]
  position[3,1] = r1[1]
  
  spat = total(cbin,1)
  ndx_y_peak = where(spat eq max(spat))
  y_peak = yp[ndx_y_peak]
  title = 'Peak at pixel '+string(y_peak,format='(I4)')
  p_spat = plot( spat, yp, layout=[2,2,2], current=plt, xtitle='counts', ytitle='row (pixel)', title=title, yr=[min(yp),max(yp)], position=position[*,1], margin=margin, $
    font_size=font_size, thick=thick )
  ;txt_spat1 = text( 0.2, 0.2, 'Peak at pixel '+string(y_peak,format='(I4)'), target=p_spat, /relative, font_size=font_size )

  spec = total(cbin,2)
  p_spec = plot( wl, spec, layout=[2,2,3], current=plt, xtitle='wavelength (nm)', ytitle='total counts', xr=[min(wl),max(wl)], position=position[*,2], margin=margin, $
     font_size=font_size, thick=thick )
  
  pos_vec = strsplit(path,path_sep())
  pos = pos_vec[-5]
  str1 = strmid(path,pos-1,strlen(path)-pos+1)
  str2 = 'pmax = '+string(pmax,format='(I3)')
  
;  txt1 = text( 0.55, 0.45, str1, font_size=font_size)
;  txt2 = text( 0.55, 0.40, str2, font_size=font_size)
  
  str = strsplit(path,path_sep(),/extract)
  txt = [str[-5],str[-4],str[-3],str[-2],str[-1]]
  txt3 = text( 0.55, 0.44, txt, font_size=font_size )
  
  txt_phd = ['pmin='+string(pmin,format='(I)'), 'pmax='+string(pmax,format='(I)')]
  txt4 = text( 0.75, 0.44, txt_phd, font_size=font_size )
  
  p_phd = plot( phd, layout=[2,2,4], font_size=font_size, thick=thick, xtitle='pulse height', current=plt, title='pulse-height distribution' ) 
  markerp,p_phd,x=pmin,linestyle=2,thick=thick
  markerp,p_phd,x=pmax,linestyle=2,thick=thick
  
if n_params() eq 0 then begin  
  stop
endif

end