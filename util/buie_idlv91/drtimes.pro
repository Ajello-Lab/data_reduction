;+
; NAME:
;  drtimes
; PURPOSE:   (one line only)
;  Compute disappearance and reappearance times for an occultation given a shape
; DESCRIPTION:
; CATEGORY:
;  Occultations
; CALLING SEQUENCE:
;  drtimes,jd0,rastar,decstar,shape,obscode,objcode,jd_d,jd_r
; INPUTS:
;  jd0 - Julian date of the occultation.  Can be a geocentric time or topocentric,
;           it is used as a starting point to find the minimum separation for the
;           given site.
;  rastar - Right ascension of the occultation star, J2000, at the epoch of jd0
;  decstar - Declination of the occultation star, J2000, at the epoch of jd0
;  shape  - [N,2] array of xi,eta coordinates for the shape of the object.
;             [*,0] are the xi coordinates, [*,1] are the eta coordinates.
;             xi,eta are to be given in km for the projected outline of the
;             object and should be a closed figure.  It is assumed that a given
;             line across this shape can intersect the figure in at most two
;             points.
;  obscode - observatory code or structure (see ephem.pro)
;  objcode - object code (see ephem.pro)
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
; OUTPUTS:
;  jd_d - Julian date of the time of disappearance (=0 if no occultation)
;  jd_r - Julian date of the time of re-appearance (=0 if no occultation)
; KEYWORD OUTPUT PARAMETERS:
; CONFIGURATION:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  Written by Marc W. Buie, Southwest Research Institute, 2022/01/27
;  2023/09/12, MWB, added missing default value on DEBUG
;-
pro drtimes,jd0,rastar,decstar,shape,obscode,objcode,jd1,jd2,DEBUG=debug

   self='drtimes: '
   if badpar(jd0,5,0,caller=self+'(jd0) ') then return
   if badpar(rastar,5,0,caller=self+'(rastar) ') then return
   if badpar(decstar,5,0,caller=self+'(decstar) ') then return
   if badpar(shape,[4,5],2,caller=self+'(shape) ') then return
   if badpar(obscode,[1,2,3,7,8],[0,1],caller=self+'(obscode) ', $
                                 type=obscodetype) then return
   if badpar(objcode,7,0,caller=self+'(object) ') then return
   if badpar(debug,[0,1,2],0,caller=self+'(DEBUG) ',default=0) then return

   xpoly=trimrank(shape[*,0])
   ypoly=trimrank(shape[*,1])

   spawn,'geteph',unit=pipe

   appuldis,objcode,obscode,jd0,rastar,decstar,jdmin,sep,pa,info=info,pipe=pipe
;jdstr,info.jd,3,jds
;print,jds,' midtime at site'

   ; determine if the star is interior to the object at the time of minimum
   ;   separation
   ephem,info.jd,obscode,72,objcode,eph,pipe=pipe
   ssgeom,eph,sun,earth,phang,elong,kscale
   ra=eph[0]
   dec=eph[1]
   astrd2sn,rastar,decstar,ra,dec,xi,eta,/arcsec
   xi = xi*kscale
   eta = eta*kscale
   in = interior(xpoly,ypoly,xi,eta)
   if debug then begin
      jdstr,jdmin,0,jds
      print,'minsep of ',sep*kscale,' at ',jds
      print,'xi,eta ',xi,eta
   endif

   if in eq 0 then begin
      if debug then print,'no occultation at jd minsep'
      jd1=0.0d0
      jd2=0.0d0
      goto,bailout
   endif

   ; scan backwards from info.jd, 1 s until star is outside object.
   dt = -1.0d0/86400.0d0
   jdb=info.jd
   repeat begin
      jda = jdb
      jdb = jda+dt
      ephem,jdb,obscode,72,objcode,eph,pipe=pipe
      ssgeom,eph,sun,earth,phang,elong,kscale
      ra=eph[0]
      dec=eph[1]
      astrd2sn,rastar,decstar,ra,dec,xi,eta,/arcsec
      xi = xi*kscale
      eta = eta*kscale
      in = interior(xpoly,ypoly,xi,eta)
   endrep until in eq 0
   repeat begin
      jdc = (jda+jdb)/2.0d0
      ephem,jdc,obscode,72,objcode,eph,pipe=pipe
      ssgeom,eph,sun,earth,phang,elong,kscale
      ra=eph[0]
      dec=eph[1]
      astrd2sn,rastar,decstar,ra,dec,xi,eta,/arcsec
      xi = xi*kscale
      eta = eta*kscale
      if interior(xpoly,ypoly,xi,eta) then begin
        jda=jdc
      endif else begin
        jdb=jdc
      endelse
      dt = jdb-jda
   endrep until abs(dt) lt 0.001d0/86400.0d0
   jd1=(jda+jdb)/2.0d0
;jdstr,jd1,3,jd1s
;print,'D ',jd1s

   ; scan forward from info.jd, 1 s until star is outside object.
   dt = 1.0d0/86400.0d0
   jdb=info.jd
   repeat begin
      jda = jdb
      jdb = jda+dt
      ephem,jdb,obscode,72,objcode,eph,pipe=pipe
      ssgeom,eph,sun,earth,phang,elong,kscale
      ra=eph[0]
      dec=eph[1]
      astrd2sn,rastar,decstar,ra,dec,xi,eta,/arcsec
      xi = xi*kscale
      eta = eta*kscale
      in = interior(xpoly,ypoly,xi,eta)
   endrep until in eq 0
   repeat begin
      jdc = (jda+jdb)/2.0d0
      ephem,jdc,obscode,72,objcode,eph,pipe=pipe
      ssgeom,eph,sun,earth,phang,elong,kscale
      ra=eph[0]
      dec=eph[1]
      astrd2sn,rastar,decstar,ra,dec,xi,eta,/arcsec
      xi = xi*kscale
      eta = eta*kscale
      if interior(xpoly,ypoly,xi,eta) then begin
        jda=jdc
      endif else begin
        jdb=jdc
      endelse
      dt = jdb-jda
   endrep until abs(dt) lt 0.001d0/86400.0d0
   jd2=(jda+jdb)/2.0d0
;jdstr,jd2,3,jd2s
;print,'R ',jd2s

bailout:
   free_lun,pipe

end
