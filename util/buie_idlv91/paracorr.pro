;+
; NAME:
;  paracorr
; PURPOSE:   (one line only)
;  Take raw Gaia catalog data information and apply correction to a time
; DESCRIPTION:
; CATEGORY:
;  Astrometry
; CALLING SEQUENCE:
;  paracorr,info
; INPUTS:
;  jd - UT Julian date to correct the catalog positions to
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;  FNCAT - Optional file name to read, default is star.cat.gcat
; OUTPUTS:
;  info - anonymous structure with the information from the input file
;           and the correction position(s)
; KEYWORD OUTPUT PARAMETERS:
; CONFIGURATION:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  2021/01/27, Written by Marc W. Buie, Southwest Research Institute
;  2022/06/21, MWB, added DR3 pm bias correction.
;  2022/06/22, MWB, changed to earth center, not barycenter, and J2000 coords
;  2022/08/29, MWB, changed to heliocenter, not SS barycenter
;-
pro paracorr,jd,info,FNCAT=fncat

   self='paracorr: '
   if badpar(jd,[4,5],0,caller=self+'(epoch) ') then return
   if badpar(fncat,[0,7],0,caller=self+'(FNCAT) ', $
                           default='star.cat.gcat') then return

   if not exists(fncat) then begin
      print,self,'Unable to locate catalog file, ',fncat
      info=!null
      return
   endif

   jd2year,jd,oepoch

   rdstarc,fncat,/silent,/noconvert,info=cinfo

   if cinfo.nstars eq 0 then begin
      print,'No stars found in ',fncat
      info={nstars: 0}
      return
   endif

   ; correct for DR3 bright star proper motion bias
   rapm = cinfo.rapm*1000.0d0*3600.0d0*180.0d0/!dpi*cos(cinfo.dec)
   decpm = cinfo.decpm*1000.0d0*3600.0d0*180.0d0/!dpi
   pmcorrdr3,rapm,decpm,cinfo.ra*180.0d0/!dpi,cinfo.dec*180.0d0/!dpi, $
              cinfo.gmag,newrapm,newdecpm

   cinfo.rapm = newrapm/1000.0d0/3600.0d0/180.0d0*!dpi/cos(cinfo.dec)
   cinfo.decpm = newdecpm/1000.0d0/3600.0d0/180.0d0*!dpi

   ; first correct for proper motion to the output epoch
   dt = (oepoch-cinfo.epoch)
   ra  = cinfo.ra  + cinfo.rapm *dt
   dec = cinfo.dec + cinfo.decpm*dt

;   ; barycentric equatorial position of Earth, J2000
;   ephem,jd,500,39,'P399',eph
   ; heliocentric equatorial position of Earth, J2000
   ;   (Larry says this is correct)
   ephem,jd,500,42,'P399',eph
   eph=eph[0:2]
   x=eph[0]
   y=eph[1]
   z=eph[2]

   ; sense is geo-ssbary, units here are [radians]
   ; Equation 8.15 from Green's Spherical Astronomy, p. 189 (1985 version)
   dra = cinfo.par*(x*sin(cinfo.ra)-y*cos(cinfo.ra))/cos(cinfo.dec)
   ddec = cinfo.par*(x*cos(cinfo.ra)*sin(cinfo.dec) + $
                    y*sin(cinfo.ra)*sin(cinfo.dec)-z*cos(cinfo.dec))

   ra1  = ra  + dra
   dec1 = dec + ddec

   ra1sig = sqrt(cinfo.raerr^2+(cinfo.rapmerr*dt)^2+ $
                               (dra/cinfo.par*cinfo.parerr)^2)
   dec1sig= sqrt(cinfo.decerr^2+(cinfo.decpmerr*dt)^2+ $
                                (ddec/cinfo.par*cinfo.parerr)^2)
   info = { $
      nstars:   cinfo.nstars, $
      cra:      cinfo.ra, $
      cdec:     cinfo.dec, $
      cepoch:   cinfo.epoch, $
      par:      cinfo.par*1000.0d0*3600.0d0*180.0d0/!dpi, $
      rapm:     cinfo.rapm*1000.0d0*3600.0d0*180.0d0/!dpi*cos(cinfo.dec), $
      decpm:    cinfo.decpm*1000.0d0*3600.0d0*180.0d0/!dpi, $
      raerr:    cinfo.raerr*1000.0d0*3600.0d0*180.0d0/!dpi*cos(cinfo.dec), $
      decerr:   cinfo.decerr*1000.0d0*3600.0d0*180.0d0/!dpi, $
      parerr:   cinfo.parerr*1000.0d0*3600.0d0*180.0d0/!dpi, $
      rapmerr:  cinfo.rapmerr*1000.0d0*3600.0d0*180.0d0/!dpi*cos(cinfo.dec), $
      decpmerr: cinfo.decpmerr*1000.0d0*3600.0d0*180.0d0/!dpi, $
      gmag:     cinfo.gmag, $
      gmagerr:  cinfo.gmagerr, $
      bmag:     cinfo.bmag, $
      bmagerr:  cinfo.bmagerr, $
      rmag:     cinfo.rmag, $
      rmagerr:  cinfo.rmagerr, $
      starid:   cinfo.starid, $
      ora:      ra1, $
      odec:     dec1, $
      oepoch:   oepoch, $
      orasig:   ra1sig*1000.0d0*3600.0d0*180.0d0/!dpi, $
      odecsig:  dec1sig*1000.0d0*3600.0d0*180.0d0/!dpi $
      }


end
