
;+
; NAME:
; AJELLO_LAB_COMBINE
; 
; PURPOSE:
;  This routine will produce a spatially binned and combined "super-image" composed of all three instrument pointings.
;
; INPUTS:
;   f1: full path filename of the IDL save file containing the dark-subtracted image when the instrument was pointed at the e-beam
;   f2: same as f1 but for the second instrument poining
;   f3: same as f1 but for the third instrument pointing, farthest from the e-beam
;
; OUTPUTS:
;   ybin: spatial binning vector
;   arrbin: Spatially binned and combined image
;
;-
pro ajello_lab_combine, f1, f2, f3, wl, ybin, arrbin

if n_params() eq 0 then begin
  f1 = '/Users/holsclaw/MAVEN/Ajello_lab/_Big_e-gun_RoundII/data_reduction/CO_30eV_MUV_test_4_image1.idl'
  f2 = '/Users/holsclaw/MAVEN/Ajello_lab/_Big_e-gun_RoundII/data_reduction/CO_30eV_MUV_test_6_image2.idl'
  f3 = '/Users/holsclaw/MAVEN/Ajello_lab/_Big_e-gun_RoundII/data_reduction/CO_30eV_MUV_test_5_image3.idl'
  
  f1 = '/Volumes/projects/Phase_Development/MAVEN/IUVS_Data/IUVS_Breadboard/_BIG_e-gun/data_reduction/CO_30eV_MUV_3.idl'
  f2 = '/Volumes/projects/Phase_Development/MAVEN/IUVS_Data/IUVS_Breadboard/_BIG_e-gun/data_reduction/CO_30eV_MUV_4.idl'
  f3 = '/Volumes/projects/Phase_Development/MAVEN/IUVS_Data/IUVS_Breadboard/_BIG_e-gun/data_reduction/CO_30eV_MUV_6.idl'

  f1 = '/Volumes/projects/Phase_Development/MAVEN/IUVS_Data/IUVS_Breadboard/Big_e-gun_RoundVIII_final/data_reduction/N2+O2_100EV_FUV_TEST5_IMAGE1.idl'
  f2 = '/Volumes/projects/Phase_Development/MAVEN/IUVS_Data/IUVS_Breadboard/Big_e-gun_RoundVIII_final/data_reduction/N2+O2_100EV_FUV_TEST6_IMAGE2.idl'
  f3 = '/Volumes/projects/Phase_Development/MAVEN/IUVS_Data/IUVS_Breadboard/Big_e-gun_RoundVIII_final/data_reduction/N2+O2_100EV_FUV_TEST7_IMAGE3.idl'

  f1 = '/Volumes/projects/Phase_Development/MAVEN/IUVS_Data/IUVS_Breadboard/Big_e-gun_RoundVIII_final/data_reduction/O2_100EV_FUV_TEST26_IMAGE1_HIPRESS.idl'
  f2 = '/Volumes/projects/Phase_Development/MAVEN/IUVS_Data/IUVS_Breadboard/Big_e-gun_RoundVIII_final/data_reduction/O2_100EV_FUV_TEST27_IMAGE2_HIPRESS.idl'
  f3 = '/Volumes/projects/Phase_Development/MAVEN/IUVS_Data/IUVS_Breadboard/Big_e-gun_RoundVIII_final/data_reduction/O2_100EV_FUV_TEST32_IMAGE3_MEDPRES.idl'

;  f1 = '/Volumes/projects/Phase_Development/MAVEN/IUVS_Data/IUVS_Breadboard/Big_e-gun_RoundVIII_final/data_reduction/O2_100EV_FUV_TEST30_IMAGE1_MEDPRES.idl'
;  f2 = '/Volumes/projects/Phase_Development/MAVEN/IUVS_Data/IUVS_Breadboard/Big_e-gun_RoundVIII_final/data_reduction/O2_100EV_FUV_TEST31_IMAGE2_MEDPRES.idl'
;  f3 = '/Volumes/projects/Phase_Development/MAVEN/IUVS_Data/IUVS_Breadboard/Big_e-gun_RoundVIII_final/data_reduction/O2_100EV_FUV_TEST32_IMAGE3_MEDPRES.idl'
  
  
  f1 = '/Volumes/projects/Phase_Development/MAVEN/IUVS_Data/IUVS_Breadboard/NeweGun_round10_after_energy_correction/data_reduction/N2_100EV_FUV_TEST6_IMAGE1_HIPRESS_WITHH20.idl'
  f2 = '/Volumes/projects/Phase_Development/MAVEN/IUVS_Data/IUVS_Breadboard/NeweGun_round10_after_energy_correction/data_reduction/N2_100EV_FUV_TEST7_IMAGE2_HIPRESS_WITHH20.idl'
  f3 = '/Volumes/projects/Phase_Development/MAVEN/IUVS_Data/IUVS_Breadboard/NeweGun_round10_after_energy_correction/data_reduction/N2_100EV_FUV_TEST8_IMAGE3_HIPRESS_WITHH20.idl'
  desc_exp = 'N2 100ev TEST-06-07-08 HIPRESS WITHH20'
  
  f1 = '/Volumes/projects/Phase_Development/MAVEN/IUVS_Data/IUVS_Breadboard/NeweGun_round10_after_energy_correction/data_reduction/N2_100EV_FUV_TEST20_IMAGE1_HIPRESS.idl'
  f2 = '/Volumes/projects/Phase_Development/MAVEN/IUVS_Data/IUVS_Breadboard/NeweGun_round10_after_energy_correction/data_reduction/N2_100EV_FUV_TEST21_IMAGE2_HIPRESS.idl'
  f3 = '/Volumes/projects/Phase_Development/MAVEN/IUVS_Data/IUVS_Breadboard/NeweGun_round10_after_energy_correction/data_reduction/N2_100EV_FUV_TEST22_IMAGE3_HIPRESS.idl'
  desc_exp = 'N2 100ev TEST-20-21-22 HIPRESS'
  
  path_save = '/Users/holsclaw/MAVEN/Ajello_lab/_Big_e-gun_RoundVIII/'
  path_save = '/Users/holsclaw/MAVEN/Ajello_lab/Round10/'
  
  ; something wrong with this set:
  ;f1 = '/Volumes/projects/Phase_Development/MAVEN/IUVS_Data/IUVS_Breadboard/_BIG_e-gun/data_reduction/CO2_30eV_FUV_5.idl'
  ;f2 = '/Volumes/projects/Phase_Development/MAVEN/IUVS_Data/IUVS_Breadboard/_BIG_e-gun/data_reduction/CO2_30eV_FUV_4.idl'
  ;f3 = '/Volumes/projects/Phase_Development/MAVEN/IUVS_Data/IUVS_Breadboard/_BIG_e-gun/data_reduction/CO2_30eV_FUV_3.idl'
endif

;
; restore the data
;
restore,f1
arr1 = arr
int_time1 = int_time
restore,f2
arr2 = arr
int_time2 = int_time
restore,f3
arr3 = arr
int_time3 = int_time

yr_spec = [150,900] ; spatial range of narrow slit, that avoids the keyholes
yr_spec = [200,900] ; this avoids some anomalous emission in image2 and image3
yr_bump = [300,450] ; spatial range of feature to interpolate across
ajello_lab_interpolate_spatial_feature, arr2, arr2i, yr_spec=yr_spec, yr_bump=yr_bump
arr2 = arr2i

arr1 /= int_time1
arr2 /= int_time2
arr3 /= int_time3

;
; retrieve wavelength scale for the rotated image
;
ajello_lab_pixel_scale_rot, wlfuv, wlmuv, yfuv, ymuv

; adjust default waelength scale for round8
wlfuv = wlfuv - 136.6 + 130.4 - 0.2

;
; approximate spatial range of narrow slit
;
ylim1 = 140
ylim2 = 940
ylim1 = yr_spec[0]
ylim2 = yr_spec[1]

;
; determine the spatial center of image1
;
y = findgen(1024)
sig = total(arr1,1)
nterms = 4
ag_est = fltarr(nterms)
ag_est[0] = max(sig[ylim1:ylim2]) - min(sig[ylim1:ylim2])
ag_est[1] = centroid(y[ylim1:ylim2], sig[ylim1:ylim2])
ag_est[2] = 20
ag_est[3] = min(sig[ylim1:ylim2])
gaussvec,y[ylim1:ylim2], gest, ag_est
gfit = gaussfit( y[ylim1:ylim2], sig[ylim1:ylim2], ag, nterms=nterms, estimates=ag_est )
image1_spatial_pixel_center = ag[1]

p1 = plot( y, sig )
p2 = plot( y[ylim1:ylim2], gest, /over, color='blue' )
p3 = plot( y[ylim1:ylim2], gfit, /over, color='red' )
markerp,p1,x=ylim1,linestyle=2
markerp,p1,x=ylim2,linestyle=2
markerp,p1,x=ag[1],linestyle=2


ajello_lab_round_number, f1, round_number

;ajello_lab_wavelength_def, round_number, wlfuv, wlmuv


; Alan's notes:
;   distance from beam in mm = -0.2192 * col + 131.503
;   row to wavelength conversion
;   wavelength in angstroms = -1.636 * row + 3431.9
;   Translation offsets for each instrument re-pointing: 0, -161mm, -322mm  ; Round 1
;   Translation offsets for each instrument re-pointing: 0, -6inches (-152.4mm), -12inches (-304.8mm)  ; Round 2
;
; other notes:
;  distance from instrument to target: 41 inches
;  distance from telescope mirror to slit: 110.62 mm
;
; h/(41.*25.4) = hp/(110.62)
mag = 110.62 / (41.*25.4)
; fiber taper reformats 24 mm at input to 18.4 mm at output (ref: 2014 instrument paper)
; CMOS pixels are 18 micron square
; Thus, pixels appear to be: 18. * 24./18.4 = 23.5 micron in size at the input of the intensifier
pixel_size_at_target = 18. * 24./18.4 / mag / 1000.  ; mm

;case round_number of
;  1: begin
;    image2_offset = -161  ; mm
;    image2_offset = -322  ; mm
;  end
;  2: begin
;    image2_offset = -152.4  ; mm
;    image2_offset = -304.8  ; mm
;  end
;  else: image2_offset = -300.  ; mm
;endcase

y1 = pixel_size_at_target*( findgen(1024) - image1_spatial_pixel_center )
y2 = y1 + 152.4
y3 = y1 + 304.8

; mm to cm
y1 /= 10.
y2 /= 10.
y3 /= 10.

xsum1 = total( arr1, 1, /nan )
xsum2 = total( arr2, 1, /nan )
xsum3 = total( arr3, 1, /nan )

win = window(dim=[800,600])
yr = [1,1e4]
p1 = plot( y1, xsum1, /ylog, current=win, font_size=14, title=desc_exp, xtitle='distance (cm)', yr=yr )
p2 = plot( y2, xsum2, color='red', /over )
p3 = plot( y3, xsum3, color='blue', /over )
;win.save,path_save+'round10_test-6-7-8_total_rate_vs_distance.png'
win.save,path_save+'round10_test-20-21-22_total_rate_vs_distance.png'


win = window(dim=[800,600])
p1 = plot( xsum1, current=win, layout=[1,3,1], yr=[0,max(xsum1)*0.1] )
markerp,p1,x=ylim1,linestyle=2
markerp,p1,x=ylim2,linestyle=2
p2 = plot( xsum2, current=win, layout=[1,3,2], yr=[0,max(xsum2)*0.2] )
markerp,p2,x=ylim1,linestyle=2
markerp,p2,x=ylim2,linestyle=2
p3 = plot( xsum3, current=win, layout=[1,3,3], yr=[0,max(xsum3)*0.2] )
markerp,p3,x=ylim1,linestyle=2
markerp,p3,x=ylim2,linestyle=2


y1 = y1[ylim1:ylim2]
y2 = y2[ylim1:ylim2]
y3 = y3[ylim1:ylim2]
xsum1 = xsum1[ylim1:ylim2]
xsum2 = xsum2[ylim1:ylim2]
xsum3 = xsum3[ylim1:ylim2]
arr1 = arr1[*,ylim1:ylim2]
arr2 = arr2[*,ylim1:ylim2]
arr3 = arr3[*,ylim1:ylim2]

ybin1 = -100  ; mm
ybin2 =  420  ; mm
ybinsize = 10  ; mm
; mm to cm
ybin1 /= 10.
ybin2 /= 10.
ybinsize /= 10.
ybin = findgen( (ybin2-ybin1)/ybinsize + 1 ) * ybinsize + ybin1 ;+ ybinsize/2.
nbin = n_elements(ybin)

arrbin = fltarr(1024,nbin)
for i = 0, nbin - 1 do begin
  ;y1i = ybin1 + ybinsize*i
  ;y2i = y1i + ybinsize
  y1i = ybin[i] - ybinsize/2.
  y2i = ybin[i] + ybinsize/2.
  ndx1 = where( y1 ge y1i and y1 lt y2i, count1 )
  ndx2 = where( y2 ge y1i and y2 lt y2i, count2 )
  ndx3 = where( y3 ge y1i and y3 lt y2i, count3 )
  
  arrbin1 = 0.
  arrbin2 = 0.
  arrbin3 = 0.

  if count1 gt 1 then begin
    arrbin1 = mean( arr1[*,ndx1], dimension=2 )
  endif else begin
    if count1 eq 1 then arrbin1 = arr1[*,ndx1]
  endelse
  
  if count2 gt 1 then begin
    arrbin2 = mean( arr2[*,ndx2], dimension=2 )
  endif else begin
    if count2 eq 1 then arrbin2 = arr2[*,ndx2]
  endelse
  
  if count3 gt 1 then begin
    arrbin3 = mean( arr3[*,ndx3], dimension=2 )
  endif else begin
    if count3 eq 1 then arrbin3 = arr3[*,ndx3]
  endelse
  
  numavg = (count1 gt 0) + (count2 gt 0) + (count3 gt 0)
  arrbin[*,i] = ( arrbin1 + arrbin2 + arrbin3 ) / numavg
  
;  print, numavg
;  stop
  
endfor


arrbin_regrid = congrid(arrbin,1024,nbin*ybinsize*4)

img = image( alog10(arrbin_regrid-min(arrbin_regrid)) )

win = window(dim=[800,600])
p1 = plot( y1, xsum1, current=win, /ylog )
p2 = plot( y2, xsum2, color='red', /over )
p3 = plot( y3, xsum3, color='blue', /over )
p4 = plot( ybin, total( arrbin,1), color='green', /over, symbol='x', thick=3, histogram=1 )



p = plot( total(arr1,1) )

p = plot( wlfuv, total(arr1[*,400:600],2) )
w = 0.6
markerp,p,x=130.4,linestyle=2
markerp,p,x=130.4-w,linestyle=2
markerp,p,x=130.4+w,linestyle=2
markerp,p,x=135.6,linestyle=2
markerp,p,x=135.6-w,linestyle=2
markerp,p,x=135.6+w,linestyle=2

ndx_1304 = where( wlfuv gt 130.4-w and wlfuv lt 130.4+w )
ndx_1356 = where( wlfuv gt 135.6-w and wlfuv lt 135.6+w )

sigbin_1304 = total( arrbin[ndx_1304,*], 1 )
sig1_1304 = total( arr1[ndx_1304,*], 1 )
sig2_1304 = total( arr2[ndx_1304,*], 1 )
sig3_1304 = total( arr3[ndx_1304,*], 1 )

sigbin_1356 = total( arrbin[ndx_1356,*], 1 )
sig1_1356 = total( arr1[ndx_1356,*], 1 )
sig2_1356 = total( arr2[ndx_1356,*], 1 )
sig3_1356 = total( arr3[ndx_1356,*], 1 )

win = window(dim=[800,600])
p1 = plot( y1, sig1_1304, /ylog, current=win, xtitle='distance (cm)', ytitle='signal rate (DN/sec)', name='130.4 nm', $
  font_size=16, margin=[0.2,0.1,0.1,0.1] )
p2 = plot( y2, sig2_1304, /over )
p3 = plot( y3, sig3_1304, /over )
p4 = plot( y1, sig1_1356, color='red', /over, name='135.6 nm' )
p5 = plot( y2, sig2_1356, color='red', /over )
p6 = plot( y3, sig3_1356, color='red', /over )
p7 = plot( ybin, sigbin_1304, /over, color='green', thick=3, /stairstep )
p8 = plot( ybin, sigbin_1356, /over, color='green', thick=3, /stairstep )
leg = legend(target=[p1,p4])
;win.save,path_save+'round8_o_vs_distance_combine.png'

ndx_bin = where( ybin ge 0. )
print, total(sigbin_1304[ndx_bin],/nan) /  total(sigbin_1356[ndx_bin],/nan)

win = window(dim=[800,600])
p = plot( ybin, sigbin_1304 / sigbin_1356, /stairstep, current=win, xtitle='distance (cm)', ytitle='ratio', $
  font_size=16, margin=[0.2,0.1,0.1,0.1], yr=[0,5.1], title='ratio 1304 to 1356' )
;win.save,path_save+'round8_o_ratio_vs_distance_combine.png'


stop

p1 = plot( wlmuv, arrbin[*,0], xr=[170,350] )
for i = 0, nbin - 1 do pi=plot( wlmuv, arrbin[*,i], /over )

ndx = where( wlmuv gt 220. and wlmuv lt 270. )
sig_cameron = total( arrbin[ndx,*], 1 )

ndx1 = where( wlmuv gt 280 and wlmuv lt 285 )
sig_unknown = total( arrbin[ndx1,*], 1 )

p1 = plot( ybin, sig_cameron/max(sig_cameron) )
p2 = plot( ybin, sig_unknown/max(sig_unknown), /over, color='red' )



stop

end