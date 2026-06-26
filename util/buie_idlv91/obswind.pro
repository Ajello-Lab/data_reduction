;+
; NAME:
;  obswind
; PURPOSE:
;  Compute possible observing window for a celestial object.
; DESCRIPTION:
;
; This routines determines the possible observing window for an object.
; The times of sunrise and sunset are left as input parameters to help
; speed up the computations.  It is expected that this routine will be called
; many times for a given date (LST0), but for different objects.
; RTIME and STIME denote the earliest time and the latest time that the object
; can be observed.  Note that if RTIME > STIME then the range passes through
; 0 hours.  RKIND and SKIND tell what limits the ends of the window as follows:
;        RKIND  will be either 'sunset' or 'rises' for TYPE = 0
;        RKIND  will be 'sunset' for TYPE =  1
;        RKIND  is undefined for TYPE = -1
;        SKIND  will be either 'sunrise' or 'sets' for TYPE = 0
;        SKIND  will be 'sunrise' for TYPE = 1
;        SKIND  is undefined for TYPE = -1
;        if TYPE = 2 then object is not available when sun is down.
;
; CATEGORY:
;  Astronomy
;
; CALLING SEQUENCE:
;  obswind,lst0,lat,ra,dec,srise,sset,rtime,rkind,stime,skind,type
; INPUTS:
;  lst0  - Local Sidereal Time at local midnight (radians).
;  lat   - Latitude of observatory (radians).
;  ra    - right ascension of object (radians), should be of date.
;  dec   - declination of object (radans), should be of date.
;  srise - Time of sunrise (JD).
;  sset  - Time of sunset (JD).
;  
; OPTIONAL INPUT PARAMETERS:
;
; KEYWORD INPUT PARAMETERS:
;  AMLIMIT - Air Mass limit to denote rise and set of object.  Default=3.0
;
; OUTPUTS:
;  rtime - Time of opening of observing window (at rise or sunset)
;  rkind - String that identifies limit, either 'sunset' or 'rises'
;  stime - Time of closing of observing window (at rise or sunset)
;  skind - String that identifies limit, either 'sunset' or 'rises'
;  type  - Indicator of type of window
;           2 - Object not available when sun is down.
;           1 - Object always up.
;           0 - Object rises and sets.
;          -1 - Object always down.
;
; KEYWORD OUTPUT PARAMETERS:
;  JDTRANS - JD of object transit
;
; COMMON BLOCKS:
;
; SIDE EFFECTS:
;
; RESTRICTIONS:
;
; PROCEDURE:
;
; MODIFICATION HISTORY:
;  1997/9/3, Written by Marc W. Buie, Lowell Observatory
;  2002/03/27, MWB, fixed AMLIMIT bug.
;  2012/01/28, MWB, validation changed to enforce only scalar input
;  2018/05/23, MWB, added JDTRANS output keyword
;-
PRO obswind,lst0,lat,ra,dec,srise,sset,rtime,rkind,stime,skind,type, $
   AMLIMIT=amlimit,JDTRANS=jdtrans

   if badpar(lst0, [4,5],0,CALLER='OBSWIND: (lst0) ' ) then return
   if badpar(lat,  [4,5],0,    CALLER='OBSWIND: (lat) '  ) then return
   if badpar(ra,   [4,5],0,CALLER='OBSWIND: (ra) '   ) then return
   if badpar(dec,  [4,5],0,CALLER='OBSWIND: (dec) '  ) then return
   if badpar(srise,[4,5],0,CALLER='OBSWIND: (srise) ') then return
   if badpar(sset, [4,5],0,CALLER='OBSWIND: (sset) ' ) then return

   ; Set the critical altitude and airmass for observability.
   if keyword_set(amlimit) then $
      amcrit=amlimit $
   else $
      amcrit=3.0
   crital = 0.5*!pi - acos(1.0/amcrit)
   altoha,crital,dec,lat,ha,type
   jdlclmid = (srise+sset)/2.0d0
   hatojd,0.0d0,ra,lst0,jdlclmid,jdtrans

   if type eq 1 then begin
      rtime = sset
      rkind = 'sunset'
      stime = srise
      skind = 'sunrise'
   endif else if type eq 0 then begin
      rtime = jdtrans - ha/2.0d0/!dpi
      stime = jdtrans + ha/2.0d0/!dpi
      rkind = 'rises'
      skind = 'sets'
      if srise ge sset then begin

         if rtime le stime then begin
            if rtime lt sset then begin
               if stime lt sset then begin
                  type=2
               endif else if stime gt srise then begin
                  rtime=sset
                  rkind='sunset'
                  stime=srise
                  skind='sunrise'
               endif else begin
                  rtime=sset
                  rkind='sunset'
               endelse
            endif else if rtime gt srise then begin
               type=2
            endif else begin
               if stime gt srise then begin
                  stime=srise
                  skind='sunrise'
               endif
            endelse
         endif else begin
            if rtime lt sset then begin
               rtime=sset
               rkind='sunset'
               stime=srise
               skind='sunrise'
            endif else if rtime gt srise then begin
               if stime lt sset then begin
                  type=2
               endif else if stime gt srise then begin
                  rtime=sset
                  rkind='sunset'
                  stime=srise
                  skind='sunrise'
               endif else begin
                  rtime=sset
                  rkind='sunset'
               endelse
            endif else begin
               stime=srise
               skind='sunrise'
            endelse
         endelse
      endif else begin
         if rtime le stime then begin
            if rtime lt srise then begin
               if stime gt srise then begin
                  stime=srise
                  skind='sunrise'
               endif
            endif else if rtime gt sset then begin
               ; nop
            endif else begin
               if stime ge sset then begin
                  rtime=sset
                  rkind='sunset'
               endif else begin
                  type=2
               endelse
            endelse
         endif else begin
            if rtime lt srise then begin
               sset=srise
            endif else if rtime gt sset then begin
               if stime gt srise then begin
                  stime=srise
                  skind='sunrise'
               endif
            endif else begin
               if stime gt srise then begin
                  stime=srise
                  skind='sunrise'
                  rtime=sset
                  rkind='sunset'
               endif else begin
                  rtime=sset
                  rkind='sunset'
               endelse
            endelse
         endelse
      endelse
   endif

end


