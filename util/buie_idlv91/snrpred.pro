;+
; NAME:
;  snrpred
; PURPOSE:   (one line only)
;  Compute an estimated signal-to-noise ratio for a point source
; DESCRIPTION:
; CATEGORY:
;  Photometry
; CALLING SEQUENCE:
;  snrpred,system,exptime,smag,omag,snr
; INPUTS:
;  system - anonymous structure with the following tags:
;             jy0mag:   Janskys for zero Vega magnitude at lambda
;             lambda:   center wavelength [nanometers]
;             topt:     optical surface temperature of telescope [K]
;             emis:     net emissivity to sky
;             rdnoise:  read noise [e-]
;             aper:     telescope aperture [cm]
;             fnum:     f/# at focal plane
;             bandpass: bandpass of instrument [nanometers]
;             pixel:    size of pixel [microns]
;             sky:      sky brightness [mag/sq arcsec]
;             dark:     dark signal [e-/sec/pixel]
;             tput:     throughput, central obs, reflectsion and QE
;             seeing:   FWHM arcseconds
;  exptime - scalar or vector if exposure time values [seconds]
;  smag - Magnitude of the occultation star source (at lambda)
;  omag - Magnitude of the occultating body (at lambda)
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;  VERBOSE - if set will generate lots of printed output.  This is rarely
;              useful if "smag" is a vector.
; OUTPUTS:
;  snr - Estimated signal-to-noise estimation per point
; KEYWORD OUTPUT PARAMETERS:
; CONFIGURATION:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
;  The input values exptime, smag, and omag can be either scalars or vectors.
;    If any are vectors, any that are must be the same length.  This is
;    NOT enforced.  Note that the value of /VERBOSE may be limited with
;    vector inputs as the output printed will be poorly formatted (and long).
; PROCEDURE:
; MODIFICATION HISTORY:
;  Written by Marc W. Buie, Southwest Research Institute, 2022/02/04 and
;     based on a spreadsheet provided by Mike Skrutskie, UVa.
;  2022/02/22, MWB, added object magnitude and scintillation estimates
;  2024/11/14, MWB, added exposure time as an argument instead of including
;                 it in the input structure.
;-
pro snrpred,s,exptime,smag,omag,snr,VERBOSE=verbose,OUTINFO=outinfo

   self='snrpred: '
   snr=!null
   if badpar(s,8,1,caller=self+'(system) ') then return
   if badpar(exptime,[2,3,4,5],[0,1],caller=self+'(smag) ') then return
   if badpar(smag,[2,3,4,5],[0,1],caller=self+'(smag) ') then return
   if badpar(omag,[2,3,4,5],[0,1],caller=self+'(omag) ') then return
   if badpar(verbose,[0,1,2,3],0,caller=self+'(VERBOSE) ',default=0) then return

   k = 1.38e-16 ; boltzman constant
   c = 2.00e+10 ; speed of light
   h = 6.63e-27 ; planck constant

   ; convert from nm to cm
   lambda = s.lambda*1.0e-7
   bandpass = s.bandpass*1.0e-7

   strinfo='Simulation for '+s.name
   if verbose then print,strinfo

   ; diffraction limit in arcseconds
   diflimit = 206265.0*1.22*lambda/s.aper
   if verbose then print,'Diffraction limit           ',diflimit,' arcsec'

   ; telescope platescale (arcsec/mm)
   plate = 1.0/(10*s.fnum*s.aper)*206265.0
   if verbose then print,'Plate scale                 ',plate,' arcsec/mm'

   ; angular size of a pixel in arcseconds
   pixang = s.pixel*plate/1000.0
   if verbose then print,'Size of pixel               ',s.pixel,' microns'
   if verbose then print,'Angular size of pixel       ',pixang,' arcsec'
   pscale = plate*(s.pixel/1000.0)
   if verbose then print,'Image scale                 ',pscale,' arcsec/pixel'

   ; approx number of pixels involved in a measurement
   ;    don't like FWHM/2 in this formula
   if verbose then print,'Seeing                      ',s.seeing,' arcsec FWHM'
   if verbose then print,'Seeing                      ',s.seeing/pscale,' pixels FWHM'
   fp = max([12,5.25*!pi*(2*diflimit/pixang)^2,!pi*((s.seeing/pixang)*s.objrad)^2])
   if verbose then print,'Number of pixels in aperture',fp
   rapereff = sqrt(fp/!pi)
   if verbose then print,'Radius of estimated aperture',rapereff,' pixels'

   ; solid angle subtended by f/#
   omega = !pi/(2*s.fnum)^2
   if verbose then print,'solid angle subtended by f/#',omega,' steradians'

   ; pixel area in square centimeters
   area = (s.pixel*1.0e-4)^2
   if verbose then print,'pixel area                  ',area,' cm^2'

   ; Planck function cgs at t_optics
   blambda = (2*h*c^2/lambda^5)/(exp(h*c/(lambda*k*s.topt)-1))

   ; total source photons detected in the exposure
   srcphot = s.tput*!pi*(0.25*s.aper^2)*s.jy0mag*1.0e-23* $
              10.0^(smag/(-2.5))*(lambda/(h*c))*exptime*(c/lambda^2)*bandpass
   if verbose then print,'Signal from source          ',srcphot,' photons'
   if verbose then print,'Signal from source          ',srcphot/s.gain,' DN'

   objphot = s.tput*!pi*(0.25*s.aper^2)*s.jy0mag*1.0e-23* $
              10.0^(omag/(-2.5))*(lambda/(h*c))*exptime*(c/lambda^2)*bandpass
   if verbose then print,'Signal from occ body        ',objphot,' photons'
   if verbose then print,'Signal from occ body        ',objphot/s.gain,' DN'

   ; sky photons collected in a footprint in exposure
   skyphot = fp*s.tput*exptime*s.jy0mag*10.0^(s.sky/(-2.5))*pixang^2* $
                1.0e-23*0.25*!pi*s.aper^2/(h*c/lambda)*(c/lambda^2)*bandpass
   if verbose then print,'Signal from sky             ',skyphot,' photons'
   if verbose then print,'Signal from sky             ',skyphot/s.gain,' DN'
   if verbose then print,'Source/sky ratio            ',srcphot/skyphot

   ; thermal photons from optics seen in a foorprint in exposure
   thermp = fp*s.tput*blambda*omega*area/(h*c/lambda)* $
               exptime*s.emis*bandpass
   if verbose then print,'Signal from thermal backgrnd',thermp,' photons'

   ; read noise equivalent photons
   rnep = fp*s.rdnoise^2
   if verbose then print,'Read noise equivalent signal',rnep,' photons'

   ; dark counts
   dark = s.dark*exptime*fp
   if verbose then print,'Dark signal                 ',dark,' photons'

   ; scintillation index
   scin = 1.0e-6 * s.cy^2 * (s.aper/100.0)^(-4./3.) $
            / exptime * cos(s.zenith/!radeg)^(-3) * exp(-2*s.alt/s.hscale)
   if verbose then print,'scintillation index         ',scin,' photons'
   scinvar = scin*srcphot^2
   if verbose then print,'scintillation variance      ',scinvar,' photons'

   ; total noise
   noise_noscin=sqrt(srcphot+skyphot+dark+thermp+rnep+objphot)
   noise=sqrt(srcphot+skyphot+dark+thermp+rnep+objphot+scinvar)
   if verbose then print,'Total noise, no scint       ',noise_noscin,' photons'
   if verbose then print,'Total noise all sources     ',noise,' photons'
   if verbose then print,'Total noise all sources     ',noise/s.gain,' DN'

   ; final answer
   snr = srcphot/noise
   if verbose then print,'Final SNR, no scint         ',srcphot/noise_noscin
   if verbose then print,'Final SNR                   ',snr

   outinfo={ $
      label:     s.name, $
      diflimit:  diflimit, $ ; arcsec
      plate:     plate, $    ; arcsec/mm
      pscale:    pscale, $   ; arcsec/pixel
      fp:        fp, $       ; pixels in aperture
      rapereff:  rapereff, $ ; radius of extimated aperture, pixels
      srcphot:   srcphot, $  ; source photons detected
      srcphotdn: srcphot/s.gain, $  ; source photons detected
      srcobj:    objphot, $  ; object photons detected
      srcobjdn:  objphot/s.gain, $  ; object photons detected
      skyphot:   skyphot, $
      skyphotdn: skyphot/s.gain, $
      thermp:    thermp, $
      rnep:      rnep, $
      dark:      dark, $
      scinsig:   sqrt(scinvar), $
      noise_noscin: noise_noscin, $
      noise:     noise, $
      snr:       snr $
      }

end
