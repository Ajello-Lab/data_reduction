
pro ajello_lab_gold_20250820_wavelength_scale

file = '/Volumes/projects/Phase_Development/MAVEN/IUVS_Data/IUVS_Breadboard/GOLD/kimball_egun_round_11/data_reduction/N2_100eV_hi-pres_test2_image1_pmax_250.sav'
restore, file, /ver

ajello_lab_gold_wavelength_scale, wl1

spat = total( cbin, 1 )
spec = total( cbin, 2 )

;
; isolate the spatial region with high signal and low contamination 
;
spat_max = max( spat, ndx_spat_max )
w_spat = 30
y1 = ndx_spat_max - w_spat
y2 = ndx_spat_max + w_spat
 
p = plot( spat, xtitle='spatial pixel' )
markerp,p,x=y1,linestyle=2
markerp,p,x=y2,linestyle=2

;
; create a spectrum limited to the identified spatial region
;
spec = total( cbin[*,y1:y2], 2 )
p = plot( wl, spec, xtitle='wavelength (nm)' )

;
; isolate the 1493 emission line 
;
spec_max = max( spec, ndx_spec_max )
w_spec = 30
x1 = ndx_spec_max - w_spec
x2 = ndx_spec_max + w_spec

p = plot( spec[x1:x2] )

wl2 = 0.1943 * findgen(4096) + 1187 + 145.70251
wl3 = 0.1943 * findgen(4096) + 1332.7025

wl = wl1 - wl1[ndx_spec_max] + 1493.

p = plot( wl, spec )

img = image( cbin, min_value=0, max_value=100 )

p = plot( wl - wl2 )


x = wl[x1:x2]
y = spec[x1:x2] 

g = gaussfit( x, y, afit, nterms=3 )

p = plot( x, y )
p2 = plot( x, g, /over, color='red' )

ajello_lab_gold_em_psf_model, x, afit, f

p = plot( x, f )

g2 = gaussian_function( afit[2], /normalize )

p = plot( g2 )

ajello_lab_scaled_gold_psf_model, psf

xp = findgen(x2-x1+1)

p = plot( xp, y )
p2 = plot( psf*max(y)/max(psf), /over, color='red' )

stop

end