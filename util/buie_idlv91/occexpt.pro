;+
; NAME:
;  occexpt
; PURPOSE:   (one line only)
;  Compute suggested exposure time for occultation conditions
; DESCRIPTION:
; CATEGORY:
;  Occultations
; CALLING SEQUENCE:
;  occexpt,system,gmag,gtarg,speed,diam,exptime,snr
; INPUTS:
;  system - scalar string with name of supported camera/telescope system
;  These four inputs can be scalar or vector but if more than one is a vector
;   all must agree in length.  This is not gracefully enforced.  The normal
;   cases are all scalar, all equal length vectors, and just one quantity
;   is a vector and the rest are scalar.
;  gmag   - G magnitude of the star to be occulted
;  gtarg  - Apparent G magnitude of the occulting body
;  speed  - shadow velocity speed (km/sec)
;  diam   - Expected diameter of the occulting body (km)
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
; OUTPUTS:
;  exptime - resulting suggestion for the input conditions
;  snr     - predicted SNR for the suggested exposure time
; KEYWORD OUTPUT PARAMETERS:
; CONFIGURATION:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  Written by Marc W. Buie, Southwest Research Institute, 2024/11/15
;-
pro occexpt,system,in_gmag,in_gtarg,in_speed,in_diam,out_exptime,out_snr

   common mwb_occexpt,s1

   self='occexpt: '
   if badpar(system,7,0,caller=self+'(system) ') then return
   if badpar(in_gmag,[2,3,4,5],[0,1],caller=self+'(gmag) ', $
                                     npts=n1) then return
   if badpar(in_gtarg,[2,3,4,5],[0,1],caller=self+'(gtarg) ', $
                                      npts=n2) then return
   if badpar(in_speed,[2,3,4,5],[0,1],caller=self+'(speed) ', $
                                      npts=n3) then return
   if badpar(in_diam,[2,3,4,5],[0,1],caller=self+'(diam) ', $
                                     npts=n4) then return

   npts=max([n1,n2,n3,n4])
   if npts gt 1 then begin
      if n1 ne npts then gmag=replicate(in_gmag,npts)   else gmag=in_gmag
      if n2 ne npts then gtarg=replicate(in_gtarg,npts) else gtarg=in_gtarg
      if n3 ne npts then speed=replicate(in_speed,npts) else speed=in_speed
      if n4 ne npts then diam=replicate(in_diam,npts)   else diam=in_diam
   endif else begin
      gmag=in_gmag
      gtarg=in_gtarg
      speed=in_speed
      diam=in_diam
   endelse

   exptimerange=[0.12,2.0]

   info_type=size(cache,/type)
   if info_type eq 0 then begin
      initsnrmod,system,s1
   endif else begin
      if s1.name ne system then initsnrmod,system,s1
   endelse
   exptime1=seqgen(exptimerange[0],0.20,0.01)
   exptime2=seqgen(0.25,exptimerange[1],0.05)
   exptime=[exptime1,exptime2]
   out_exptime=fltarr(npts)
   out_snr=fltarr(npts)
   for i=0,npts-1 do begin
      snrpred,s1,exptime,gmag[i],gtarg[i],snr
      scale=speed[i]*exptime
      nsamp = diam[i]/scale

      zg=where(snr gt 5 and snr lt 10 and nsamp gt 4,countg)
      select=1 ; use max(snr[zg]) if count>1

      if countg eq 0 then begin
;         print,'Relax max SNR constraint'
         zg=where(snr gt 5 and nsamp gt 4,countg)
         select=2 ; use min(snr[zg]) if count>1
      endif

      if countg eq 0 then begin
;         print,'Relax SRN constraint'
         zg=where(snr ge 3 and nsamp gt 4,countg)
         select=1
      endif

      if countg eq 0 then begin
;         print,'Relax nsamp constraint'
         zg=where(snr ge 3 and nsamp gt 2,countg)
         select=3 ; use max(nsamp[zg])
      endif

      if countg eq 0 then begin
         out_exptime[i]=-1
         out_snr[i]=0.
      endif else if countg ne 0 then begin
         if select eq 1 then begin
            z=where(snr[zg] eq max(snr[zg]))
         endif else if select eq 2 then begin
            z=where(snr[zg] eq min(snr[zg]))
         endif else if select eq 3 then begin
            z=where(nsamp[zg] eq max(nsamp[zg]))
         endif
         out_exptime[i] = exptime[zg[z]]
         out_snr[i] = snr[zg[z]]
      endif

   endfor

   out_exptime=trimrank(out_exptime)
   out_snr=trimrank(out_snr)

end
