;+
; NAME:
;    getobloc
; PURPOSE: (one line)
;    Fetch location of observatory given its code
; DESCRIPTION:
;
; CATEGORY:
;  Astronomy
; CALLING SEQUENCE:
;    getobloc,obscode,obs
;
; INPUTS:
;  obscode - observatory code (integer or string)
;
; OPTIONAL INPUT PARAMETERS:
;
; KEYWORD INPUT PARAMETERS:
;  OBSFILE - Name of observatory code file to read.  Default=obscode.dat
;
; OUTPUTS:
;  obs - anonymous structure that contains information for the requested
;          observatory.  Four tags are defined:
;             name - string name of observatory
;             lat  - Latitude of observatory (radians)
;             lon  - Longitude of observatory (radians)
;             alt  - Altitude of observatory (meters), [always zero for now]
;             obscode - observatory code (input returned always as string)
;
; COMMON BLOCKS:
;  getobloc_com - used to cache the contents of the observatory coordinate
;                   file.  The file is only read the first time it is needed
;                   and then the memory copy is use from then on.
;
; SIDE EFFECTS:
;  Note, if the observatory code file is NOT found, '688' (Lowell Observatory)
;    is used.
;
;  If the file is okay but the code is not recognized, the observatory
;    name is returned as 'unknown' and lat=lon=alt=0.0
;
;  Observatory coordinate file is read each time this procedure is called.
;    This really needs to add code to read the file once and store its
;    contents in non-volatile memory (anonymous stucture in a common block)
;    and then re-read the file only when it's date changes.
;
; RESTRICTIONS:
;
; PROCEDURE:
;
; MODIFICATION HISTORY:
;  2002/03/14, Written by David Tucker
;  2002/12/09, MWB, converted to separate standalone routine.
;  2016/02/05, MWB, forced lat/lon to be double
;  2021/09/08, MWB, added cache of file read and added alt to structure
;  2024/07/04, MWB, added decoding of altitude, this led to a few hidden bugs (errors
;                     that cancelled) and other cleanup and documentation.
;-
pro getobloc,obscode,obs,OBSFILE=obsfile

   common getobloc_com, info

   self='getobloc: '
   if badpar(obscode,[1,2,3,7],0,caller=self+'(obscode) ', $
                                             type=codetype) then return

   if badpar(obsfile,[0,7],0,caller=self+'(OBSFILE) ', $
                                             default='obscode.dat') then return

   sz_info=size(info)
   ; If no structure yet, the data must be loaded fresh
   if sz_info[n_elements(sz_info)-2] ne 8 then begin
      reload=1
   endif else begin
      reload=0
      valid=1
   endelse

   if codetype ne 7 then begin
      obscode = strn(obscode,length=3,padchar='0')
   endif else begin
      obscode = strupcase(obscode)
   endelse

   ; pre-define output structure
   obs = { $
      name:"",        $ ;Name of observatory
      lat:0.0d0,      $ ;Latitude of observitory
      lon:0.0d0,      $ ;Longitude of observatory
      alt:0.0d0,      $ ;Altitude of observatory in meters
      rhosinp:0.0d0,      $ ;rho sin(lat)
      rhocosp:0.0d0,      $ ;rho cos(lat)
      obscode:obscode $ ;Unique observatory code
      }

   ; the altitude is not populated yet but is provide in case something
   ;   else can fill it in.
  
   ; Try to load file coordinates, if failed, set to Lowell
   if reload then begin
      rdobscod, code,alllon,rhosinp,rhocosp,obsname,valid,FILE=obsfile
      if valid then begin
          info = { $
             file: obsfile, $
             code: code, $
             lon:  alllon, $
             rhosinp: rhosinp, $
             rhocosp: rhocosp, $
             obsname: obsname }
      endif else begin
         print,'Observatory code file ',obsfile,' not found.'
         print,'Using Lowell Observatory (688) built in default values.'
         obscode = '688'
         obs.name=''
      endelse
   endif else valid=1

   if valid then begin
      idx=where(obscode eq info.code,count)
      idx=idx[0]
      if (count eq 1) then begin

         ; this converts from east to west longitude
         obs.lon = (360.0-info.lon[idx])/180.0*!pi

         ; easier to type
         rcp=info.rhocosp[idx]
         rsp=info.rhosinp[idx]
         obs.rhocosp=rcp
         obs.rhosinp=rsp

         ; some constants for the Earth
         re = 6378136.6d0 ; meters
         f  = 1.0d0/298.257d0 ; J2000 flattening

         ; first guess
         lat=atan(rsp,rcp)
         h=0.0d0

         clat=cos(lat)
         slat=sin(lat)

         repeat begin
            lasth = h
            c = sqrt(clat^2+((1+f)*slat)^2)
            h = re*(rcp/clat-c)

            slat = rsp / (c*(1-f)^2 + h/re)
            lat = asin(slat)
            clat = cos(lat)
;   print,h,lat*180.0d0/!dpi
         endrep until abs(h-lasth) lt 0.2 

         obs.lat = lat
         obs.name=strtrim(info.obsname[idx],2)
         obs.alt = h

      endif else begin
         obs.name='unknown'
      endelse
   endif

   ; Hardcoded position for 42" to get the program running on failure.
   if obscode eq '688' and obs.name eq '' then begin
      ; This is the GPS position for the 42", derived 1993 Sep 08
      obs.lat = (35.0d0+5.0/60.0+48.740/3600.0)/180.0*!pi
      obs.lon = (-111.0d0+32.0/60.0+10.601/3600.0)/180.0*!pi
      obs.name= 'Lowell Observatory - Anderson Mesa Station'
      obs.alt = 2209.0
      obs.rhocosp = 0.57193
      obs.rhosinp = 0.81938
      obs.obscode = obscode
   endif

end
