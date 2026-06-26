;+
; NAME:
;  refcorr
; PURPOSE:   (one line only)
;  Compute a differential refraction correction to an apparent position
; DESCRIPTION:
;  Compute the amount of refraction relative to a target wavelength for
;    light passing through the Earth's atmosphere.
; CATEGORY:
;  Astrometry
; CALLING SEQUENCE:
;  refcorr,jd,obs,ra,dec,refwave,targwave,ra1,dec1
; INPUTS:
;  jd       - Julian date (must be double precision to get nearest second).
;  obs      - Observatory code or strucgture (see obsinfo)
;  ra       - J2000 right ascension of apparent location [radians]
;  dec      - J2000 declination of apparent location [radians]
;  refwave  - Reference wavelength [microns]
;  targwave - Wavelength of target [microns]
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;  PRESSURE - atmospheric pressure in mm of Hg (default=760.0)
;  TEMP     - atmospheric temperature in degrees C (default=0.0)
;  RELHUM   - Relative humidity (in percent) (default=0.0)
; OUTPUTS:
;  ra1      - corrected J2000 coordinate in right ascension [radians]
;  dec1     -  correctedJ2000 coordinate in declination [radians]
; KEYWORD OUTPUT PARAMETERS:
;  delra    - JDnow right ascension offset in arcsec, includes cos(dec)
;  deldec   - JDnow declination offset in arcsec
;  offset   - total refraction in arcsec along great circle including zenith
;                and normal to local horizon, this is the amount of the
;                differential refraction.  If targwave>refwave the amount will
;                be toward the zenith (negative).  If targwave<refwave the
;                amount will be toward the horizon (positive).
;  alt      - sky altitude of position (angle from horizons)
;  zd       - zenith distance
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  Written by Marc W. Buie, Southwest Research Institute, 2019/10/21
;  2024/07/06, MWB, rework for new obs tools, dropped OBSINFO keyword
;-
pro refcorr,in_jd,in_obs,in_ra,in_dec,in_refwave,in_targwave,ra1,dec1, $
   PRESSURE=pressure,TEMP=temp,RELHUM=relhum, $
   DELRA=delra,DELDEC=deldec,OFFSET=offset,ALT=alt,ZD=zd

   self='refcorr: '
   if badpar(in_jd,[4,5],[0,1],caller=self+'(jd) ',npts=njd) then return
   if badpar(in_ra,[4,5],[0,1],caller=self+'(ra) ',npts=nra) then return
   if badpar(in_dec,[4,5],[0,1],caller=self+'(dec) ',npts=ndec) then return
   if badpar(in_obs,[2,3,7,8],[0,1],caller=self+'(obs) ',type=codetype) then return
   if badpar(in_refwave,[4,5],[0,1],caller=self+'(refwave) ', $
                                 npts=nrefwave) then return
   if badpar(in_targwave,[4,5],[0,1],caller=self+'(targwave) ', $
                                  npts=ntargwave) then return
   if badpar(pressure,[0,4,5],[0,1],caller=self+'(PRESSURE) ', $
                                  default=760.0) then return
   if badpar(temp,[0,4,5],[0,1],caller=self+'(TEMP) ', $
                                  default=0.0) then return
   if badpar(relhum,[0,4,5],[0,1],caller=self+'(RELHUM) ', $
                                  default=0.0) then return

   if codetype eq 8 then begin
      obs = in_obs
   endif else begin
      obsinfo,in_obs,obs
   endelse

   refwave=double(in_refwave)
   targwave=double(in_targwave)

   npts=max([njd,nra,ndec,nrefwave,ntargwave])
   if njd eq npts then jd=in_jd else jd=replicate(in_jd,npts)
   if nra eq npts then ra=in_ra else ra=replicate(in_ra,npts)
   if ndec eq npts then dec=in_dec else dec=replicate(in_dec,npts)
   if nrefwave eq npts then refwave=in_refwave $
   else refwave=replicate(in_refwave,npts)
   if ntargwave eq npts then targwave=in_targwave else $
   targwave=replicate(in_targwave,npts)

   pressure=double(pressure)
   temp=double(temp)
   relhum=double(relhum)

   jd2year,jd,year
   inyear=replicate(2000.0d0,npts)

   ; precess coordinates to JD
   rajd=ra
   decjd=dec
   for i=0,npts-1 do begin
      ra0=rajd[i]
      dec0=decjd[i]
      precess,ra0,dec0,inyear[i],year[i],/radian
      rajd[i]=ra0
      decjd[i]=dec0
   endfor

   ; compute local coordinates
   hangle,jd,rajd,decjd,obs.lat,obs.wlon,ha,lst
;plot,(jd-jd[0])*24.0d0,ha*12.0d0/!dpi,psym=3
   altaz,ha,obs.lat,decjd,alt,az
;plot,(jd-jd[0])*24.0d0,alt*180.0d0/!dpi,psym=3
;plot,(jd-jd[0])*24.0d0,az*180.0d0/!dpi,psym=3
   zd=!dpi/2.0-alt
;plot,(jd-jd[0])*24.0d0,zd*180.0d0/!dpi,psym=3
;print,'ALT ',alt*!radeg,'  AZ',az*!radeg,' ZD',zd*!radeg
   zref = refrac(zd,refwave,pressure,temp,relhum)
;plot,(jd-jd[0])*24.0d0,zref*180.0d0/!dpi,psym=3
   ztar = refrac(zd,targwave,pressure,temp,relhum)
;plot,(jd-jd[0])*24.0d0,ztar*180.0d0/!dpi,psym=3
;print,'zref',zref,'  ztar',ztar,' zref-ztar',zref-ztar
;print,'diff',(zref-ztar)*180.0d0/!dpi*3600.0d0,' arcsec'

;plot,(jd-jd[0])*24.0d0,(zref-zd)*180.0d0/!dpi*3600.0d0,psym=3
;plot,(jd-jd[0])*24.0d0,(ztar-zd)*180.0d0/!dpi*3600.0d0,psym=3
;plot,(jd-jd[0])*24.0d0,(zref-ztar)*180.0d0/!dpi*3600.0d0,psym=3

; option 1, my first attempt but it looks backwards wrt residuals
;   offset=zref-ztar
;   alt1=alt+(zref-ztar)

; option 2, reverse of option 1
   offset=ztar-zref
   alt1=alt+(ztar-zref)

   lcltoeq,alt1,az,obs.lat,ha1,dec1
;plot,(jd-jd[0])*24.0d0,(alt1-alt)*180.0d0/!dpi*3600.0d0,psym=3
;plot,(jd-jd[0])*24.0d0,(ha1-ha)*180.0d0/!dpi*3600.0d0,psym=3
;plot,(jd-jd[0])*24.0d0,(dec1-decjd)*180.0d0/!dpi*3600.0d0,psym=3
   ra1=lst-ha1
   for i=0,npts-1 do begin
      ra0=ra1[i]
      dec0=dec1[i]
      precess,ra0,dec0,year[i],inyear[i],/radian
      ra1[i]=ra0
      dec1[i]=dec0
   endfor
;plot,(jd-jd[0])*24.0d0,(dec1-dec)*180.0d0/!dpi*3600.0d0,psym=3
;plot,(jd-jd[0])*24.0d0,(ra1-ra)*180.0d0/!dpi*3600.0d0,psym=3


end
