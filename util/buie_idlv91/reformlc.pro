;+
; NAME:
;  reformlc
; PURPOSE:   (one line only)
;  Reformat an old-style MallinCAM RECON lightcurve to the new occ format
; DESCRIPTION:
; CATEGORY:
;  File I/O
; CALLING SEQUENCE:
;  reformlc,fnin,fnout
; INPUTS:
;  fnin - Name of the file to read.  This is in the format of the video
;           photometry processing tools.  The biggest difference is that
;           the time is encoded as seconds from 0h UT on the date of the event.
;  fnout - Name of the output file.  These are compatible with the new
;             occ*.pro tools and use JD for the time.
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;  JD0 - Julian date of 0h UT on the date of the observations.  By default
;          this is taken from the first 8 characters of the file name which
;          is normally a compact form of the date.  If the file has a
;          non-standard name you need to provide this value.  It can be
;          provided as a double precision JD or a date string 'YYYY-MM-DD'
; OUTPUTS:
; KEYWORD OUTPUT PARAMETERS:
; CONFIGURATION:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
; Written by Marc W. Buie, Southwest Research Institute, 2020/03/26
;-
pro reformlc,fnin,fnout,JD0=jd0

   self='reformlc: '
   if badpar(fnin,7,0,caller=self+'(fnin) ') then return
   if badpar(fnout,7,0,caller=self+'(fnout) ') then return

   if not exists(fnin) then begin
      print,'File ',fnin,' not found.'
      return
   endif

   jds=strmid(fnin,0,4)+'-'+strmid(fnin,4,2)+'-'+strmid(fnin,6,2)

   if badpar(jd0,[0,5,7],0,caller=self+'(JD0) ', $
                           type=jdtype,default=jds) then return

   if jdtype eq 7 then jd0=jdparse(jd0)

   readcol,fnin,idx,dt,flux,fluxerr,format='l,d,f,f',count=nobs

   jd = jd0+dt/86400.0d0

   openw,lun,fnout,/get_lun
   for i=0,nobs-1 do begin
      printf,lun,idx[i],jd[i],flux[i],fluxerr[i], $
         format='(i5,1x,f17.9,1x,f8.5,1x,f7.5)'
   endfor
   free_lun,lun

end
