;+
; NAME:
; AJELLO_LAB_PIXEL_SCALE_ROT
; 
; PURPOSE:
; This routine defines the wavelength and spatial scales of the rotated images 
;   for both FUV and MUV channels of the IUVS breadboard.
; 
; INPUTS:
; none
;
; OUTPUTS:
; wlfuv: FUV channel wavelength scale
; wlmuv: MUV channel wavelength scale
; yfuv: FUV channel spatial scale
; ymuv: MUV channel spatial scale
; 
; NOTES:
; It has been found that both the wavelength scale and spatial scale vary between
;   observation sets.
;-
pro ajello_lab_pixel_scale_rot, wlfuv, wlmuv, yfuv, ymuv

; Alan's notes:
;   distance from beam in mm = -0.2192 * col + 131.503
;   row to wavelength conversion
;   wavelength in angstroms = -1.636 * row + 3431.9
;   Translation offsets for each instrument re-pointing: 0, -161mm, -322mm
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
;
; There are three images, with each at a different translation of the instrument along a direction perpendicular to the beam line
; The "offsets" are 0, -161mm, and -322mm
;
wlmuv = (-1.636 * findgen(1024) + 3431.9)/10.  ; wavelength in nm
wlmuv = reverse(wlmuv)
;y0 = -0.2192 * findgen(1024) + 131.503  ; distance from beam in mm for the "centered" image
y0 = -pixel_size_at_target * findgen(1024) + 131.503  ; distance from beam in mm for the "centered" image
ymuv = y0 - 38.12  ; empirical adjustment to match the Round2 data
ymuv = reverse(ymuv)

;restore,'/Users/holsclaw/MAVEN/IDL/ajello_lab/IUVSbreadboardSensitivity_FUV.sav',/ver
;v = findgen(n_elements(final_wave))
;pf = linfit( v, final_wave, yfit=yfit )
;
; The FUV wavelength scale was found from the IUVS breadboard sensitivity file above
;
wlfuv =  (0.8313 * findgen(1024) + 1073.3487)/10.  ; nm
wlfuv = reverse(wlfuv) + (121.6-116.6)
wlfuv = reverse(wlfuv)

yfuv = y0 ;- 25.  ; empirical adjustment to match the Round2 data
yfuv = y0 - 12.25 - 25
yfuv = reverse(yfuv)

end