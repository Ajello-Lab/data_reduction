;+
; NAME:
;  mkspace
; PURPOSE:   (one line only)
;  Make spacing file for creating occultation tracks
; DESCRIPTION:
; CATEGORY:
;  Occultations
; CALLING SEQUENCE:
;  mkspace, ntracks, spread, [prefix], spacing=spacing
; INPUTS:
;  ntracks  :Integer, number of tracks to generate
;  spread   :Float,   spread of deployment in km
; OPTIONAL INPUT PARAMETERS:
;  prefix   :String,  prefix to use before track number
; KEYWORD INPUT PARAMETERS:
;  spacing  :Float,   space between tracks in meters
; OUTPUTS:
;  A file is created with track names and cross-track positions in km.
; KEYWORD OUTPUT PARAMETERS:
; CONFIGURATION:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
;  If the SPACING keyword is provided, its value is used as the track spacing instead
;  of the value inferred from SPREAD.
; MODIFICATION HISTORY:
;  Written by Brian Keeney, Southwest Research Institute, August 2019
;    2023-08-25, BAK: Modified to use the spread to drive spacing by default
;    2023-10-23, BAK: Modified to create .info file for tracking purposes
;-
pro mkspace, ntracks, spread, prefix, spacing=spacein

  self='mkspace: '
  if badpar(ntracks, [2,3], 0, caller=self+'(ntracks) ') then return
  if badpar(spread, [2,3,4,5], 0, caller=self+'(spread) ') then return
  if badpar(prefix, [0,7], 0, caller=self+'(prefix) ', default='B') then return
  if badpar(spacein, [0,2,3,4,5], 0, caller=self+'(spacein) ',default=-1) then return

  spacing = (spacein eq -1) ? spread*1000./(ntracks-1) : spacein
  fnout = 'space'+strtrim(round(spacing),2)

  openw, lun, fnout+'.info', /get_lun

  printf, lun, ntracks, ' tracks requested.'
  
  if (spacein eq -1) then begin
     printf, lun, 'Spread of ', spread, ' km requested.'
     printf, lun, 'Derived spacing is ', spacing, ' m.'
  endif else begin
     spread = spacing/1000.*(ntracks-1)
     printf, lun, 'Spacing of ', spacing, ' m requested.'
     printf, lun, 'Derived spread is ', spread, ' km.'
  endelse

  free_lun, lun

  
  openw, lun, fnout+'.dat', /get_lun
  
  dx = spacing / 1000d ; in km
  minx = -0.5*(ntracks-1)*dx
  fmt = (ntracks lt 100) ? '(i02)' : '(i03)'

  for i=0,ntracks-1 do begin
     track = prefix + string(f=fmt,i+1)
     posn  = minx + i*dx
    
     printf, lun, f='(a,x,f7.3)', track, posn
  endfor
  
  free_lun, lun
end
