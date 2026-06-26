;+
; NAME:
;  timecsv
; PURPOSE:   (one line only)
;  Create CSV file with locations and event times
; DESCRIPTION:
; CATEGORY:
;  Occultations
; CALLING SEQUENCE:
;  timecsv, cfile
; INPUTS:
;  cfile   :String, center-line file created by ocmapex
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
; OUTPUTS:
;  A file named times.csv containing the latitude, longitude, and
;  event time at that location in one minute intervals.
; KEYWORD OUTPUT PARAMETERS:
; CONFIGURATION:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  Written by Brian Keeney, Southwest Research Institute, August 2022
;-
pro timecsv, cfile

  self='timecsv: '
  if badpar(cfile, 7, 0, caller=self+'(cfile) ') then return
  
  if nofile(cfile, 'Centerline') then return
  readcol, cfile, f='d,d,d,f', jd, lat, lon, alt, /silent

  ; Convert from Larry's convention (west longitudes) to Google's
  lon *= -1

  openw,  lun, 'times.csv', /get_lun
  printf, lun, 'Lat, Lon, Time'
  
  for i=0,n_elements(jd)-1 do begin
     jdstr,jd[i],0,datetime
     time = (strsplit(datetime,/extract))[-1]
     sec  = (strsplit(time,':',/extract))[-1]

     if (sec eq '00') then $
        printf, lun, format='(f10.6,",",f12.6,",",x,a)', $
                lat[i], lon[i], time+' UT'
  endfor

  close, lun

end
