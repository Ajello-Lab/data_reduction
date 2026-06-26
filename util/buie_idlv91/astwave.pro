;+
; NAME:
;  astwave
; PURPOSE:   (one line only)
;  Compute the effective wavelength of an observation for astrometry
; DESCRIPTION:
;  This routine computes the effective wavelength of observation.  This
;    wavelength is actually a pivot wavelength, or, where half of the
;    detected photons (not energy) are blueward of the wavelength and half are
;    redward of the wavelength.  This wavelength depends on the target
;    color and the filter being used.  This wavelength is used to support
;    making a differential refraction correction to an astrometric measurement
;    from ground-based imaging of the sky.
; CATEGORY:
;  Astrometry
; CALLING SEQUENCE:
;  astwave,filter,colname,color,wave
; INPUTS:
;  filter - String, name of the filter used for the observation (case-sensitive)
;  colname - String, name of the color (eg., 'V-R')
;  color   - Value of the color for colname
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;  REFFILE - flux reference file to use (see fluxref.pro for more details)
;              default is provided by fluxref.pro
;  SOLARSYS - Flag, if set treats the object as a reflecting object with
;                the Sun's SED and a linear slope for the spectrum of
;                the object.  Otherwise, the color defines the SED of the
;                source.
;  DEBUG - Flag, if set turns on additional debugging output.
;  FNQE  - Name of a file that provides the QE of your detector.  The default
;          is a flat QE across the filter profile.  If the file is contained
;          somewhere in your IDL_PATH it will be located without the need
;          to provide an explicit path.
; OUTPUTS:
;  wave    - pivot wavelength for the observations in nanometers (nm)
; KEYWORD OUTPUT PARAMETERS:
;  INFO    - anonymous structure that contains intermediate information that
;               supported the computation.  This also happens to be what is
;               stored in the private common block.  Tags defined are:
;                 filter - your input filter name
;                 fnfilter - name of the filter transmission file
;                 wave   - wavelength grid from filter curve.
;                 trans  - filter transmission
;                 fsun   - solar flux spectrum interpolated to match wave
;                 wsun   - wavelength for fsun
;                 sun    - solar flux spectrum, matches wsun
;                 fnqe   - name of the file with the detector QE
;                 qe     - QE of detector, interpolated to match wave
; CONFIGURATION:
; COMMON BLOCKS:
;  astwave_com - used to cache the data used for the calculation.  The
;                  filter curve is saved to prevent reading the filter
;                  curve with each call.  This is effective only if the
;                  filter rarely changes between successive calls.
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
;  The filter transmission curve is loaded (or re-used from the prior call).
;    The color is on the Vegamag system and defines a linear correction
;    to the reference flux spectrum.  This color corrected spectrum is then
;    combined with the filter curve to derive the pivot wavlength in photon
;    units.
; MODIFICATION HISTORY:
;  Written by Marc W. Buie, Southwest Research Institute, 2021/09/02 based
;    partially on prototype code written by Brian Keeney.
;-
pro astwave,filter,colname,color,wave, $
            REFFILE=reffile,INFO=outinfo,SOLARSYS=solarsys,DEBUG=debug, $
            fnqe=in_fnqe

   common astwave_com, info

   self='astwave: '
   if badpar(filter,7,0,caller=self+'(filter) ') then return
   if badpar(colname,7,0,caller=self+'(colname) ') then return
   if badpar(color,[4,5],0,caller=self+'(color) ') then return
   if badpar(solarsys,[0,1,2,3],0,caller=self+'(SOLARSYS) ', $
                                 default=0) then return
   if badpar(debug,[0,1,2,3],0,caller=self+'(DEBUG) ', $
                                 default=0) then return
   if badpar(in_fnqe,[0,7],0,caller=self+'(DEBUG) ', $
                                 default='') then return

   sz_info=size(info)

   ; If no structure yet, the data must be loaded fresh
   if sz_info[n_elements(sz_info)-2] ne 8 then begin
      reload=1
      print,'common block needs first init.'
   endif else begin
      if filter ne info.filter or $
         in_fnqe ne info.fnqe then reload=1 else reload=0
      if reload then begin
         print,'old filter [',info.filter,'], new filter [',filter,']'
         print,'old qe [',info.fnqe,'], new qe [',in_fnqe,']'
      endif
   endelse
   
   if reload then begin

      fnfilter = 'support_files/'+filter+'.dat'
      fnfilter=find_with_def(fnfilter,!path)
      if fnfilter eq '' then begin
         print,self,'Filter file '+fnfilter+' does not exist, cannot continue.'
         stop
      endif

      ; filter files must be in microns,transmission
      print,'Load ',fnfilter
      readcol,fnfilter,fwave,ftrans,format='f,f'

      fnsun = 'support_files/sun.dat'
      fnsun=find_with_def(fnsun,!path)
      if fnsun eq '' then begin
         print,self,'Sun file '+fnsun+' does not exist, cannot continue.'
         stop
      endif

      print,'Load ',fnsun
      readcol,fnsun,wsun,fsun,format='f,f'

      interp,wsun,fsun,fwave,fisun

      ; QE curve
      if in_fnqe ne '' then begin
         fnqe = 'support_files/'+in_fnqe
         fnqe=find_with_def(fnqe,!path)
         if fnqe eq '' then begin
            print,self,'QE file '+fnqe+' does not exist, cannot continue.'
            stop
         endif

         print,'Load ',fnqe
         readcol,fnqe,qwave,rawqe,format='f,f'
         interp,qwave,rawqe,fwave,qe

      endif else begin
         qe=replicate(1.0,n_elements(fwave))
         fnqe=in_fnqe
      endelse

      info={ $
         filter: filter, $
         fnfilter: fnfilter, $
         wave: fwave, $
         trans: ftrans, $
         fsun: fisun, $
         wsun: wsun, $
         sun: fsun, $
         fnqe: in_fnqe, $
         qe: qe}

   endif

   fillist=strsplit(colname,'-',/extract)
   fluxref,fillist[0],wave0,flux0,sun0
   fluxref,fillist[1],wave1,flux1,sun1

   if solarsys then begin

      wv=[wave0,wave1]
      interp,info.wsun,info.sun,wv,sfl
      scolor= -2.5*alog10(sfl[0]/sfl[1]*flux1/flux0)
      cdiff = color-scolor
      fr=10.0^(cdiff/(-2.5))
      wavep = total(info.trans*info.wave)/total(info.trans)
      slope = (1.0-fr)/(wave1-wave0)
      b = 1.0-slope*wavep
;print,fr,wave1-wave0,wavep,slope,b
      spec = slope*info.wave+b

      fsun = info.fsun/max(info.fsun)

      fsun1 = fsun*spec
      fsun1 = fsun1/max(fsun1)

      if debug then begin
         print,wave0,flux0,wave1,flux1,color
         setwin,0
         plot,info.wave,fsun,yr=[0,1], $
            xtitle='Wavelength (microns)', $
            ytitle='Relative energy flux'
         oplot,info.wave,info.trans,color=cpalette(1)
         oplot,info.wave,fsun1,color=cpalette(2)
         oplot,info.wave,info.qe,color=cpalette(3)
      endif


      flsun = info.wave*fsun1
      flsun = flsun/max(flsun)
      wave = total(flsun*info.qe*info.trans*info.wave)/ $
             total(flsun*info.qe*info.trans)

      if debug then begin
         print,'inferred solar color',scolor,' color relative to sun',cdiff
         print,'fr=',fr,', slope=',slope,' per micron'
         print,'pivot wavelength',wave
         print,'pivot wavelength',wavep,' filter only'

         setwin,1
         plot,info.wave,flsun,yr=[0,1], $
            xtitle='Wavelength (microns)', $
            ytitle='Relative energy flux'
         oplot,info.wave,info.trans,color=cpalette(1)
         oplot,info.wave,info.trans*flsun*info.qe,color=cpalette(2)
         oplot,[1,1]*wave,[0,1],color=cpalette(2)
         oplot,[1,1]*wavep,[0,1],color=cpalette(8)
         oplot,info.wave,spec,color=cpalette(8)
         oplot,info.wave,info.qe,color=cpalette(3)
      endif

   endif else begin

      col2teff,[wave0,wave1],[flux0,flux1],color,teff ; ,/debug
      bbflux=planck(info.wave*10000.,teff)
      bbflux=bbflux/max(bbflux)


      if debug then begin
         print,wave0,flux0,wave1,flux1,color,teff
         setwin,0
         plot,info.wave,bbflux,xtitle='Wavelength (microns)', $
            ytitle='Relative energy flux',yr=[0,1]
         oplot,info.wave,info.trans,color=cpalette(1)
         oplot,info.wave,info.qe,color=cpalette(3)
      endif

      bbflux=bbflux*info.wave
      bbflux=bbflux/max(bbflux)
      wave = total(bbflux*info.qe*info.trans*info.wave)/ $
             total(bbflux*info.qe*info.trans)

      if debug then begin
         print,'pivot wavelength',wave

         setwin,1
         plot,info.wave,bbflux,xtitle='Wavelength (microns)', $
            ytitle='Relative photon flux',yr=[0,1]
         oplot,info.wave,info.trans,color=cpalette(1)
         oplot,info.wave,info.trans*bbflux*info.qe,color=cpalette(2)
         oplot,[1,1]*wave,[0,1],color=cpalette(2)
         oplot,info.wave,info.qe,color=cpalette(3)
         oplot,info.wave,info.qe,color=cpalette(3)
      endif

   endelse

   outinfo=info

end
