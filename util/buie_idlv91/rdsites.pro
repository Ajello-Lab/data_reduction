;+
; NAME:
;  rdsites
; PURPOSE:   (one line only)
;  Read an occultation event file
; DESCRIPTION:
; CATEGORY:
;  Occultations
; CALLING SEQUENCE:
;  rdsites,fn,team,flag,lat,lon,alt
; INPUTS:
;  fn - Name of file to read (usually just sites.dat)
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
; OUTPUTS:
;  team - Name of team (short string, no blanks)  if there is nothing in the
;           file or no file exists, then team=!null and nsites=0
;  flag - string, usually one character, meaningful values are:
;          y - data are good and constraint the result
;          n - There are no constraining data for this site, records attempt
;          x - data not yet processed
;         other values are ignored
;  lat  - Latitute of station in decimal degrees, WGS84 datum
;  lon  - East longitude of station in decimal degrees, WGS84 datum
;  alt  - Altitude of station in meters relative to WGS84 datum
; KEYWORD OUTPUT PARAMETERS:
;  NSITES - number of valid site entries read
; CONFIGURATION:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  Written by Marc W. Buie, Southwest Research Institute, 2022/12/07
;-
pro rdsites,fn,team,flag,lat,lon,alt,NSITES=nsites

   team=!null
   nsites=0

   self='rdsites: '
   if badpar(fn,7,0,caller='(fn) ') then return

   if nofile(fn,'input event file') then return

   readcol,fn,team,flag,lat,lon,alt,format='a,a,d,d,f',count=nread
   if nread eq 0 then return

   z=where(flag eq 'y' or flag eq 'n' or flag eq 'x',nsites)

   if nsites gt 0 and nsites ne nread then begin
      team = team[z]
      flag = flag[z]
      lat  = lat[z]
      lon  = lon[z]
      alt  = alt[z]
   endif

end
