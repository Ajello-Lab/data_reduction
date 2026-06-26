;+
; NAME:
;  astromerr
; PURPOSE:   (one line only)
;  Compute an estimate of the astrometric error given FWHM and SNR.
; DESCRIPTION:
; CATEGORY:
;  Astrometry
; CALLING SEQUENCE:
;  astromerr,fwhm,snr,err
; INPUTS:
;  fwhm - scalar or vector, full-width at half-max of the source in
;           (default unit is [pixels])
;  snr  - signal-to-noise ratio.  Size and rank needs to match fwhm
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;  PSCALE - image scale, (default=1), typically arcsec/pixel but you can
;             use you own units.  If supplied, the output result will be
;             scaled by this scalar value.
;  ARCSEC - Flag, if set, the input fwhm will be in arcsec.  In this case
;             pscale will be used to scale to pixels for the computation
;             and then scaled again for return.
; OUTPUTS:
;  err  - positional uncertainty of a centroid measurement for an object
;           aperture equal to the FWHM
; KEYWORD OUTPUT PARAMETERS:
; CONFIGURATION:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
;  This tool works from a basis of a pixelated image and the scaling to
;     angular units is a later step.
;  The valid range for the computation is:
;      fwhm  {1.6-10}.  values less then 1.6 are treated like 1.6, values
;        greater than 10 are treated as 10.
;      snr   {>3}.  You can provide snr <3 and you will get an answer but
;        its vailidity is very limited.
;  The result will never be less than 0.001 pixel nor will it ever be larger
;        than the input fwhm.
; PROCEDURE:
; MODIFICATION HISTORY:
;  Written by Marc W. Buie, Southwest Research Institute, 2021/03/24
;-
pro astromerr,infwhm,insnr,err,PSCALE=pscale,ARCSEC=arcsec

   self='astromerr:'
   if badpar(infwhm,[2,3,4,5],[0,1,2],caller=self+'(fwhm)', $
                                    npts=fwhmnpts,rank=fwhmrank) then return
   if badpar(insnr,[2,3,4,5],[0,1,2],caller=self+'(snr)', $
                                   npts=snrnpts,rank=snrrank) then return
   if badpar(pscale,[0,2,3,4,5],0,caller=self+'(PSCALE)', $
                                  default=1.0) then return
   if badpar(arcsec,[0,1,2,3],0,caller=self+'(ARCSEC)', $
                                  default=0) then return

   if snrnpts ne fwhmnpts or snrrank ne fwhmrank then begin
      print,self,'fwhm and snr must have save size and rank'
      return
   endif

   ifwhm=[2,3,4,5,6,7,8,9,10]
   nf=n_elements(ifwhm)
   carr=[[0.00716248, 0.266836,   0.782474], $
         [-0.0175552, 0.651075,   0.227830], $
         [-0.0119068, 0.796352,  0.0909751], $
         [0.00325244, 0.841546,  0.0855027], $
         [0.00988761, 0.911914,  0.0557227], $
         [0.00869966, 1.01042,   0.0187644], $
         [0.00665395, 1.09512,  0.00165804], $
         [0.0173096,  1.14015,  -0.0125205], $
         [0.0107489,  1.18950,  -0.0193207]]    ; [3 x 9] array

   if arcsec then begin
      fwhm = (round(infwhm/pscale) > 2) < 10
      sfwhm = ((infwhm/pscale) > 2) < 10
   endif else begin
      fwhm = (round(infwhm) > 2) < 10
      sfwhm = (infwhm > 2) < 10
   endelse

   snr = insnr>0.001

   sz=size(fwhm,/dimen)
   if n_elements(fwhm) eq 1 and sz[0] eq 0 then begin
      err=0.
   endif else begin
      err=make_array(dimension=sz,/float)
   endelse

   metric = sfwhm/snr

   for i=0,nf-1 do begin
      z=where(fwhm eq ifwhm[i],count)
      if count gt 0 then begin
         err[z]=poly(metric[z],trimrank(carr[*,i]))
      endif
   endfor

end
