;+
; NAME:
;  rdsource
; PURPOSE:   (one line only)
;  Read in a source list file created by findsrc or srcast
; DESCRIPTION:
; CATEGORY:
;  CCD data processing
; CALLING SEQUENCE:
;  rdsource,fn,sinfo
; INPUTS:
;  fn - Name of file to read (with path if needed)
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
; OUTPUTS:
;  sinfo - Anonymous structure with everything from the file.
;          The tags in the structure depend on which type of file is
;          being read (src or srd).  The srd structure is a superset of src.
;          The scalar tags provided for src files are:
;            object, airmass, xsize, ysize, sigthresh, gap, objrad, sigwsize,
;            gain, binfac, exptime, maxsig, meanfwhm, skylevel, skysigma,
;            obscura1, obscura2, nsrcs
;          The extra scalar tags provided for srd files are:
;            photzp, maglimit, satlimit
;          The vector tags provided for src files are:
;            xpos, ypos, fwhm, mag, err, snr
;          The additional vector tags provided for srd files are:
;            ra, dec, smag
;          See findsrc.pro for details on the src file information and see
;            srcast.pro for details on the srd file information.
;          If there is an error reading the file then sinfo will be returned
;            as a scalar zero.
; KEYWORD OUTPUT PARAMETERS:
;  HDR - Optional return of the FITS header of the file in case there are
;           extra keywords not parsed by this routine.
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  2016/04/13, Written by Marc W. Buie, Southwest Research Institute
;  2016/08/18, MWB, added support for reading srg files (same format as src).
;  2021/05/17, MWB, fixed so that it properly handles empty source files.
;-
pro rdsource,fn,sinfo,HDR=hdr

   self='rdsource: '
   if badpar(fn,7,0,caller=self+'(fn) ') then return

   sinfo=0
   if not exists(fn) then begin
      print,self,fn
      print,'Unable to find file.  Aborting.'
      return
   endif

   words=strsplit(fn,'.',/extract)
   suffix=words[-1]

   if suffix ne 'src' and suffix ne 'srg' and suffix ne 'srd' then begin
      print,self,fn
      print,'File name must end in either src or srd.  Aborting.'
      return
   endif

   data=readfits(fn,hdr,/silent)

   nsrcs=sxpar(hdr,'NAXIS1')
   object=strtrim(sxpar(hdr,'OBJECT'),2)
   airmass=sxpar(hdr,'AIRMASS')
   xsize=sxpar(hdr,'XSIZE')
   ysize=sxpar(hdr,'YSIZE')
   sigthresh=sxpar(hdr,'SIGTHRSH')
   gap=sxpar(hdr,'GAP')
   objrad=sxpar(hdr,'OBJRAD')
   sigwsize=sxpar(hdr,'SIGWSIZE')
   gain=sxpar(hdr,'GAIN')
   binfac=sxpar(hdr,'BINFAC')
   exptime=sxpar(hdr,'EXPTIME')
   maxsig=sxpar(hdr,'MAXSIG')
   meanfwhm=sxpar(hdr,'MEANFWHM')
   skylevel=sxpar(hdr,'SKYLEVEL')
   skysigma=sxpar(hdr,'SKYSIGMA')
   obscura1=sxpar(hdr,'OBSCURA1')
   obscura2=sxpar(hdr,'OBSCURA2')

   if suffix eq 'src' or suffix eq 'srg' then begin

      if nsrcs gt 0 then begin
         xpos=trimrank(data[*,0])
         ypos=trimrank(data[*,1])
         fwhm=trimrank(data[*,2])
         mag =trimrank(data[*,3])
         err =trimrank(data[*,4])
         snr =trimrank(data[*,5])
      endif else begin
         xpos=0
         ypos=0
         fwhm=0
         mag =0
         err =0
         snr =0
      endelse

      sinfo={ $
         nsrcs:     nsrcs, $
         object:    object, $
         airmass:   airmass, $
         xsize:     xsize, $
         ysize:     ysize, $
         sigthresh: sigthresh, $
         gap:       gap, $
         objrad:    objrad, $
         sigwsize:  sigwsize, $
         gain:      gain, $
         binfac:    binfac, $
         exptime:   exptime, $
         maxsig:    maxsig, $
         meanfwhm:  meanfwhm, $
         skylevel:  skylevel, $
         skysigma:  skysigma, $
         obscura1:  obscura1, $
         obscura2:  obscura2, $
         xpos:      xpos, $
         ypos:      ypos, $
         fwhm:      fwhm, $
         mag:       mag, $
         err:       err, $
         snr:       snr $
         }
         
   endif else begin
      photzp=sxpar(hdr,'PHOTZP')
      maglimit=sxpar(hdr,'MAGLIMIT')
      satlimit=sxpar(hdr,'SATLIMIT')

      if nsrcs gt 0 then begin
         xpos=trimrank(data[*,0])
         ypos=trimrank(data[*,1])
         fwhm=trimrank(data[*,2])
         mag =trimrank(data[*,3])
         err =trimrank(data[*,4])
         ra  =trimrank(data[*,5])
         dec =trimrank(data[*,6])
         snr =trimrank(data[*,7])
         smag=trimrank(data[*,8])
      endif else begin
         xpos=0
         ypos=0
         fwhm=0
         mag =0
         err =0
         ra  =0
         dec =0
         snr =0
         smag=0
      endelse

      sinfo={ $
         nsrcs:     nsrcs, $
         object:    object, $
         airmass:   airmass, $
         xsize:     xsize, $
         ysize:     ysize, $
         sigthresh: sigthresh, $
         gap:       gap, $
         objrad:    objrad, $
         sigwsize:  sigwsize, $
         gain:      gain, $
         binfac:    binfac, $
         exptime:   exptime, $
         maxsig:    maxsig, $
         meanfwhm:  meanfwhm, $
         skylevel:  skylevel, $
         skysigma:  skysigma, $
         obscura1:  obscura1, $
         obscura2:  obscura2, $
         photzp:    photzp, $
         maglimit:  maglimit, $
         satlimit:  satlimit, $
         xpos:      trimrank(data[*,0]), $
         ypos:      trimrank(data[*,1]), $
         fwhm:      trimrank(data[*,2]), $
         mag:       trimrank(data[*,3]), $
         err:       trimrank(data[*,4]), $
         ra:        trimrank(data[*,5]), $
         dec:       trimrank(data[*,6]), $
         snr:       trimrank(data[*,7]), $
         smag:      trimrank(data[*,8]) $
         }
   endelse

end
