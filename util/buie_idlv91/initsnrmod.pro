;+
; NAME:
;  initsnrmod
; PURPOSE:   (one line only)
;  Initialize the support information for a SNR model for a given system
; DESCRIPTION:
;  Information from snr.ini is read from your IDL path (usually in
;   support_files from this library).  The information from that file that
;   matches the input camera name is parsed and returned as an anonymous
;   structure.  This is used by snrpred.pro.
; CATEGORY:
;  Miscellaneous
; CALLING SEQUENCE:
;  initsnrmod,system,info
; INPUTS:
;  system - string with the name of a camera/system to retrieve
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
; OUTPUTS:
;  info - anonymous structure with the information about the camera/system
; KEYWORD OUTPUT PARAMETERS:
; CONFIGURATION:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  Written by Marc W. Buie, Southwest Research Institute, 2024/11/14
;-
pro initsnrmod,system,info

   self='initsnrmod: '
   if badpar(system,7,0,caller=self+'(initsnrmod) ') then return

   rdoccsnr,snrinfo
   if snrinfo.error eq 1 then begin
      info=snrinfo
      return
   endif

   zsys=where(snrinfo.systems eq system,count)
   if count ne 1 then begin
      print,self,'System id [',system,'] is not recognized.'
      info={error:1}
      return
   endif

   zsys=trimrank(zsys)
   info = { $
      name: snrinfo.systems[zsys], $ ; name of system modeled
      jy0mag:     snrinfo.jy0mag, $  ; constant of Vega mag system for G
      lambda:     snrinfo.lambda, $  ; pivot wavenegth [nm]
      topt:         snrinfo.topt, $  ; temperature of telescope [K]
      emis:         snrinfo.emis, $  ; net emissivity to sky
      rdnoise:   snrinfo.rdnoise, $  ; read noise [e-]
      gain:         snrinfo.gain, $  ; gain of camera system (e-/DN)
      aper:   snrinfo.aper[zsys], $  ; telescope aperture [cm]
      fnum:   snrinfo.fnum[zsys], $  ; f/# at focal plane
      bandpass: snrinfo.bandpass, $  ; bandpass of instrument [nm]
      pixel:       snrinfo.pixel, $  ; size of pixel [microns]
      sky:           snrinfo.sky, $  ; sky brightness [mag/sq arcsec]
      dark:         snrinfo.dark, $  ; dark signal [e-/sec/pixel]
      tput:   snrinfo.tput[zsys], $  ; total throughput of system
      cy:             snrinfo.cy, $  ; scintillation correction factor
      zenith:     snrinfo.zenith, $  ; zenith distance (degrees)
      alt:           snrinfo.alt, $  ; observatory altitude [meters]
      hscale:     snrinfo.hscale, $  ; atmospheric turbulence scale ht [m]
      objrad:     snrinfo.objrad, $  ; photometric aperture size/fwhm
      seeing:     snrinfo.seeing}    ; FWHM of image in arcsec

end
