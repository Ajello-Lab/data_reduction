;+
; NAME:
;  patlatlon
; PURPOSE:   (one line only)
;  Compute viewing and illumination geometry for Patroclus-Menoetius system
; DESCRIPTION:
; CATEGORY:
;  Astronomy
; CALLING SEQUENCE:
;  patlatlon,jd,elat,elon,slat,slon,polang,phang,earth,sun,elong
; INPUTS:
;  jd - Julian date for the geometry calculation
;               (double precision, scalar or vector)
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;  ORBIT - String with name of orbit solution to use, the default
;             is to use the most recent orbit.  This default can change
;             as new orbits are added.
;            Currently supported:
;              'owen1' - From Bill Owen, JPL - not much data
;              'grundy1' - From Will Grundy
; OUTPUTS:
;  elat - Latitude of the sub-earth point [radians]
;  elon - East longitude of the sub-earth point [radians]
;  slat - Latitude of the sub-solar point [radians]
;  slon - East longitude of the sub-solar point [radians]
;  polang - Position angle of Pluto's north pole, measured eastward from
;              north [radians]
;  phang - Solar phase angle (Sun-Pluto-Earth angle) [radians]
;  earth - Geocentric distance [AU]
;  sun   - Heliocentric distance [AU]
;  elong - Solar elongation [radians]
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
;  This is a slow routine for vector calls.  Some of the supporting routines
;    cannot handle vectorized input so each output value is computed in an
;    explicit loop.
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  2011/12/30 - Written by Marc W. Buie, Southwest Research Institute
;                 code based on fortran routines written by David Tholen
;  2012/06/04 - MWB, fixed typo on 2012 orbit.
;  2013/06/06 - MWB, added elong output.
;  2018/02/12 - MWB, added Grundy et al., 2018 orbit
;-
pro patlatlon,jd,elat,elon,slat,slon,polang,phang,earth,sun,elong, $
       ORBIT=in_orbit,XM=xm,YM=ym,ZM=zm

   self='patlatlon: '
   if badpar(jd,5,[0,1],caller=self+'(jd) ',npts=npts) then return
   if badpar(in_orbit,[0,7],0,caller=self+'(ORBIT) ', $
                              default='latest') then return

   if in_orbit eq 'latest' then orbit='grundy1' else orbit=in_orbit

   ; The zero-point of the longitude calculation for each orbit solution
   ;    is taken from mutual event calculations where JD=2447352.95988 has
   ;    a sub-earth longitude of 0.039396 degrees.  The first number in the
   ;    lonzpt line comes from running this program with lonzpt set to 0,
   ;    compute a longitude, and insert that into the lonzpt equation.

   case orbit of

;a = 596.1
;e =   0.0
;i =  18.121
;RA node = 65.625 (longitude of the ascending node?)
;arg periapse = 23.401
;M = 304.243
;Epoch = 
;
;longitude of periapsis = node + arg peri
;
;L = M + longitude of periapsis

      ; preferred solution from Bill Owen based on small amount of data
      'owen1': begin
         node   =  65.625d0 / 180.0d0 * !dpi ; longitude of ascending node
         inc    =  18.121d0 / 180.0d0 * !dpi ; inclination
         argperi = 23.401d0 / 180.0d0 * !dpi ; argument of perihelion
         omega  = prival(node+argperi)       ; longitude of periapsis
         epoch  = jdparse('2001-10-01 12:00') ; Epoch of elements
         epoch  = epoch - etut(epoch)/86400.0d0 ; convert from TDB(ET) to UTC
         period = 294248.0d0 / 86400.0d0     ; Orbital period in days
         M0     = 304.243d0 / 180.0d0 * !dpi ; mean anomaly at epoch
         L      = prival(M0+omega)           ; mean longitude
;         lonzpt = (359.698560 - 0.039396d0) / 180.0d0 * !dpi
         lonzpt=0.0d0
      end

      'grundy1': begin
         node   = 269.300d0 / 180.0d0 * !dpi ; longitude of ascending node
         inc    = 164.110d0 / 180.0d0 * !dpi ; inclination
;         argperi = 23.401d0 / 180.0d0 * !dpi ; argument of perihelion
         omega  = 0.0d0                      ; longitude of periapsis
         epoch  = jdparse('2017-05-26 12:00') ; Epoch of elements
;         epoch  = epoch - etut(epoch)/86400.0d0 ; convert from TDB(ET) to UTC
         period = 4.282680d0                 ; Orbital period in days
;         M0     = 304.243d0 / 180.0d0 * !dpi ; mean anomaly at epoch
         L      = 110.1d0                    ; mean longitude
         lonzpt = (359.698560 + 282.28429) / 180.0d0 * !dpi
;         lonzpt=0.0d0
      end

      else: begin
         print,'[',orbit,'] is an invalid orbit choice'
         elat=!NULL
         elon=!NULL
         return
      end

   endcase

   ; Mean motion, radians/day
   n = 2.0d0*!dpi/period

   ; Compute Charon's orbit pole direction
   polera  = node - !dpi/2.0d0 
   poledec = !dpi/2.0d0 - inc

   ; Convert direction of pole into a unit vector
   sphrec,1.0d0,poledec,polera,xpole,ypole,zpole

   ; Mean anomaly at epoch
   M0 = prival(L-node-omega)

   ; Compute the position of Patroclus (geocentric)
   ephem,jd,500,22,'A617',eph
   ra = trimrank(eph[0,*])
   dec = trimrank(eph[1,*])
   xobj = trimrank(eph[2,*])
   yobj = trimrank(eph[3,*])
   zobj = trimrank(eph[4,*])
   xsun = trimrank(eph[5,*])
   ysun = trimrank(eph[6,*])
   zsun = trimrank(eph[7,*])
   xos  = xobj - xsun
   yos  = yobj - ysun
   zos  = zobj - zsun

   au    = sqrt( xsun^2 + ysun^2 + zsun^2 )
   earth = sqrt( xobj^2 + yobj^2 + zobj^2 )
   sun   = sqrt( xos^2  + yos^2  + zos^2  )

   phang = acos((xos*xobj + yos*yobj + zos*zobj)/(earth*sun))

   elong = acos((xsun*xobj + ysun*yobj + zsun*zobj)/(earth*au))

   ; compute the Patroclus-centric time
   ic = 0.0057755184970626873D ; day / AU (inverse speed of light)
   jdsys = jd - earth*ic

   ; compute mean anomaly
   manom = (n*(jdsys-epoch)+M0) mod (2.0d0*!dpi)
   manom = prival(manom+lonzpt)

   elat=dblarr(npts)
   elon=dblarr(npts)
   slat=dblarr(npts)
   slon=dblarr(npts)
   polang=dblarr(npts)
   xm=fltarr(npts)
   ym=fltarr(npts)
   zm=fltarr(npts)

   for i=0,npts-1 do begin

      orbvec,inc,node,manom[i],p,q,r
      rotmat=[[p],[q],[r]]
      rotvec,-xobj[i],-yobj[i],-zobj[i],rotmat,xp,yp,zp,/reverse
      recsph,xp,yp,zp,dummy,elat0,elon0
      rotvec,-xos[i],-yos[i],-zos[i],rotmat,xp,yp,zp,/reverse
      recsph,xp,yp,zp,dummy,slat0,slon0
      xm[i]=xp-xobj[i]
      ym[i]=yp-yobj[i]
      zm[i]=zp-zobj[i]

      elat[i] = elat0
      elon[i] = prival(elon0)
      slat[i] = slat0
      slon[i] = prival(slon0)

      ; rotate onto plane of sky
      skymat,dec[i],ra[i],rotmat
      rotvec,xpole,ypole,zpole,rotmat,xp,yp,zp

      ; compute position angle using the x and y components of the sky
      ;   plane vector
      polang[i] = prival(atan(yp,xp))

   endfor

   elat=trimrank(elat)
   elon=trimrank(elon)
   slat=trimrank(slat)
   slon=trimrank(slon)
   polang=trimrank(polang)

;print,elon*!radeg

end
