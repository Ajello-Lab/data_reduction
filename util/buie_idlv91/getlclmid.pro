;+
; NAME:
;  getlclmid
; PURPOSE:   (one line only)
;  Compute the time of the next local midnight
; DESCRIPTION:
; CATEGORY:
;  Astronomy
; CALLING SEQUENCE:
;  jdlclmid = getlclmid(jd,lat,lon)
; INPUTS:
;  jd - Julian date or calendar date-time string (see jdparse.pro)
;        If the string is empty or the date is negative, then the system
;          clock is used to set the time.
;  lat - Latitude of observatory (radians or sexegesimal string)
;  lon - West longitude of observatory (radians or sexegesimal string)
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
; OUTPUTS:
;  Return value is the JD (UTC) of the nearest local midnight.  This may be in
;   the past.
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  2018/05/22, Written by Marc W. Buie, Southwest Research Institute
;-
function getlclmid,in_jd,in_lat,in_lon,SUNRA=sunra,SUNDEC=sundec

   self='getlclmid: '
   if badpar(in_jd,[4,5,7],0,caller=self+'(jd) ', $
                             type=jd_type) then return,!null
   if badpar(in_lat,[4,5,7],0,caller=self+'(lat) ', $
                              type=lat_type) then return,!null
   if badpar(in_lon,[4,5,7],0,caller=self+'(lon) ', $
                              type=lon_type) then return,!null

   if jd_type eq 7 then begin
      if in_jd eq '' then jd=systime(/julian,/ut) $
      else jd=jdparse(in_jd)
   endif else begin
      if in_jd le 0 then jd=systime(/julian,/ut) $
      else jd=in_jd
   endelse

   if lat_type eq 7 then begin
      cvtsixty,in_lat,-1.0*!dpi/2.0,!dpi/2.0,0,['N','S'],lat
   endif else begin
      lat=in_lat
   endelse

   if lon_type eq 7 then begin
      cvtsixty,in_lon,0.0d0,2.0d0*!dpi,1,['W','E'],lon
   endif else begin
      lon=in_lon
   endelse

   sunpos,jd,sunra,sundec
   sunra  = sunra/!radeg
   sundec = sundec/!radeg

   hangle,jd,sunra,sundec,lat,lon,sunha,lst

;   hastr,sunha,0,sunhas

   hatojd,!dpi,sunra,lst,jd,jdlclmid

   return,jdlclmid

end
