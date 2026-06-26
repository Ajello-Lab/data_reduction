;+
; NAME:
;  rdtedast
; PURPOSE:   (one line only)
;  Read a Ted Bowell format astrometry file
; DESCRIPTION:
;  This reads a Ted Bowell format astrometry file.  As much precision is read
;    from the file as possible.  Note that this is a fixed format file.
; CATEGORY:
;  Astrometry
; CALLING SEQUENCE:
;  rdtedast,fnted,nobs,jd,ra,dec,mag,fil,obid,obscode,ref
; INPUTS:
;  fnted - String with the name of the file to read.
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
; OUTPUTS:
;  nobs    - Number of observations.
;  jd      - Julian date of the observations
;  ra      - Right ascension (radians)
;  dec     - Declination (radians)
;  mag     - Magnitude of object (99.9 if no data)
;  fil     - Filter code (one character)
;  obid    - Object ID
;  obscode - Observatory code
;  ref     - MPC observation publishing code
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  Written by Marc W. Buie, Southwest Research Institute, 2015/05/05
;-
pro rdtedast,fnted,nobs,jd,ra,dec,mag,fil,obid,obscode,ref

   self='rdtedast: '
   if badpar(fnted,7,0,caller=self+'(fnted) ') then return

   if not exists(fnted) then begin
      nobs=0
      print,self,'File ',fnted,' does not exist.'
      return
   endif

   nobs=long(file_lines(fnted))
   if nobs eq 0 then return

   jd=dblarr(nobs)
   ra=dblarr(nobs)
   dec=dblarr(nobs)
   mag=fltarr(nobs)
   fil=strarr(nobs)
   obid=strarr(nobs)
   obscode=strarr(nobs)
   ref=strarr(nobs)

   openr,lun,fnted,/get_lun

   for i=0,nobs-1 do begin
      line=''
      readf,lun,line,format='(a)'
      jds=strmid(line,0,4)+'/'+strmid(line,5,2)+'/'+strmid(line,9,2)
      fdays=double(strmid(line,11,7))
      jd[i]=jdparse(jds)+fdays
      ras=strmid(line,19,2)+':'+strmid(line,22,2)+':'+strmid(line,25,8)
      ras=repchar(ras,' ','0')
      ra[i]=raparse(ras)
      decs=strmid(line,33,3)+':'+strmid(line,37,2)+':'+strmid(line,40,7)
      decs=repchar(decs,' ','0')
      dec[i]=decparse(decs)
      mags=strcompress(strmid(line,47,4),/remove_all)
      if mags eq '' then mag[i]=99.9 else mag[i]=float(mags)
      fil[i]=strmid(line,51,1)
      obid[i]=strcompress(strmid(line,53,10),/remove_all)
      obscode[i]=strmid(line,63,3)
      ref[i]=strcompress(strmid(line,68,6),/remove_all)
   endfor

   free_lun,lun

end
