
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
restore,f2
arr2 = arr
restore,f3
arr3 = arr

;
; approximate spatial range of narrow slit
;
ylim1 = 140
ylim2 = 940

;
; determine the spatial center of image1
;
y = findgen(1024)
sig = total(arr1,1)
nterms = 4
ag_est = fltarr(nterms)
ag_est[0] = max(sig[ylim1:ylim2]) - min(sig[ylim1:ylim2])
ag_est[1] = centroid(y[ylim1:ylim2], sig[ylim1:ylim2])
ag_est[2] = 200
ag_est[3] = min(sig[ylim1:ylim2])
gfit = gaussfit( y[ylim1:ylim2], sig[ylim1:ylim2], ag, nterms=nterms, estimates=ag_est )
image1_spatial_pixel_center = ag[1]

p1 = plot( y, sig )
p2 = plot( y[ylim1:ylim2], gfit, /over, color='red' )
markerp,p1,x=ylim1,linestyle=2
markerp,p1,x=ylim2,linestyle=2
markerp,p1,x=ag[1],linestyle=2


ajello_lab_round_number, f1, round_number

ajello_lab_wavelength_def, round_number, wlfuv, wlmuv


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

case round_number of
  1: begin
    image2_offset = -161  ; mm
    image2_offset = -322  ; mm
  end
  2: begin
    image2_offset = -152.4  ; mm
    image2_offset = -304.8  ; mm
  end
endcase

y1 = pixel_size_at_target*( findgen(1024) - image1_spatial_pixel_center )
y2 = y1 + 152.4
y3 = y1 + 304.8

xsum1 = total( arr1, 1 )
xsum2 = total( arr2, 1 )
xsum3 = total( arr3, 1 )

p1 = plot( y1, xsum1, /ylog )
p2 = plot( y2, xsum2, color='red', /over )
p3 = plot( y3, xsum3, color='blue', /over )

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
ybin = findgen( (ybin2-ybin1)/ybinsize + 1 ) * ybinsize + ybin1 ;+ ybinsize/2.
nbin = n_elements(ybin)

arrbin = fltarr(1024,nbin)
for i = 0, nbin - 1 do begin
  y1i = ybin1 + ybinsize*i
  y2i = y1i + ybinsize
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


arrbin_regrid = congrid(arrbin,1024,nbin*ybinsize)

img = image( alog10(arrbin_regrid) )

p1 = plot( y1, xsum1 )
p2 = plot( y2, xsum2, color='red', /over )
p3 = plot( y3, xsum3, color='blue', /over )
p4 = plot( ybin, total( arrbin,1), color='green', /over, symbol='x', thick=3, histogram=1 )

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