;+
; NAME:
;  ephcheck
; PURPOSE:
;  Compare a set of astrometry observations against an ephemeris.
; DESCRIPTION:
;
; CATEGORY:
;  Astrometry
; CALLING SEQUENCE:
;  ephcheck,objcode,obs,astfile
; INPUTS:
;  objcode - Standard object code for ephemeris to check against.  See EPHEM.
;  obs     - Standard observatory code (see RdoBSCOD)
; OPTIONAL INPUT PARAMETERS:
;  astfile - Raw astrometry file to read and compare to ephemeris, default
;              is "objcode.ast"  where the input objcode is first stripped of
;              the leading prefix character.
;     If the file name ends in .ted, it is assumed to be a Ted Bowell format
;        file and is read with rdtedast instead.
; KEYWORD INPUT PARAMETERS:
;  PLOTIT - Flag, if set will cause the information to be plotted as well.
;  LOWESS - If provided and greater than zero, will cause a lowess plot
;             to be superimposed on the plots and the smoothing width will
;             be set to the value given (in days).  This keyword is ignored if
;             PLOTIT is not set.
;
; OUTPUTS:
;  All information is printed to screen.
; KEYWORD OUTPUT PARAMETERS:
;
; COMMON BLOCKS:
;
; SIDE EFFECTS:
;
; RESTRICTIONS:
;
; PROCEDURE:
;
; MODifICATION HISTORY:
;  1998/01/14, Written by Marc W. Buie, Lowell Observatory
;  2007/10/31, MWB, added PLOTIT keyword and added some printed output
;  2007/11/20, MWB, added LOWESS keyword
;  2011/02/02, MWB, minor formatting tweak to protect against short file names
;  2013/09/03, MWB, added ephemeris error information to the printout
;  2016/06/01, MWB, allow string input for observatory code
;  2017/04/25, MWB, added support for reading Ted astrometry files
;-
pro ephcheck,objcode,obs,astfile,PLOTIT=plotit,LOWESS=lowess_val

   if n_params() eq 0 then begin
      print,'ephcheck,objcode,obs,astfile'
      return
   endif

   self='EPHCHECK: '
   if badpar(objcode,7,0,CALLER=self+'(objcode) ') then return
   if badpar(obs,[2,3,7],0,CALLER=self+'(obs) ') then return
   if badpar(astfile,[0,7],0,CALLER=self+'(astfile) ', $
                             default=strmid(objcode,1,99)+'.ast') then return
   if badpar(plotit,[0,1,2,3],0,CALLER=self+'(PLOTIT) ',default=0) then return
   if badpar(lowess_val,[0,2,3,4,5],0,CALLER=self+'(LOWESS) ',default=0.0) then return

   fnsuff=strmid(astfile,strlen(astfile)-3,3)
   if fnsuff eq 'ted' then begin
      rdtedast,astfile,nlines,jd,raobs,decobs,mag ; ,fil,obid,obscode,ref
      fn='obsin'+string(indgen(nlines),format='(i4.4)')
   endif else begin
      rdrawast,astfile,fn,jd,raobs,decobs,mag,nlines
   endelse
   if nlines eq 0 then return

   if nlines eq 1 then begin
      print,'The astrometry file has only one observation.'
   endif

   ; Generate the ephemeris
   ephem,jd,obs,2+50,objcode,eph
   err=angsep(raobs,decobs,eph[0,*],eph[1,*])*!radeg*3600.0
   dra=(raobs-eph[0,*])*cos(eph[1,*])*!radeg*3600.0
   ddec=(decobs-eph[1,*])*!radeg*3600.0
   if n_elements(err) gt 3 then begin
      robomean,err,3.0,0.5,merr,d1,merrsig
      robomean,dra,3.0,0.5,mraerr,d1,mraerrsig
      robomean,ddec,3.0,0.5,mdecerr,d1,mdecerrsig
   endif else begin
      merrsig=stdev(err,merr)
      mraerrsig=stdev(dra,mraerr)
      mdecerrsig=stdev(ddec,mdecerr)
   endelse
   ephem,jd,obs,11,objcode,ephex
   epherr=trimrank(sqrt(ephex[12,*]^2+ephex[13,*]^2))

   ; Compute total motion from first to last point.
   hmotobj=angsep(raobs[0],decobs[0],raobs[nlines-1],decobs[nlines-1]) $
        / ((jd[nlines-1]-jd[0]) * 24.0 ) * !radeg * 3600.0
   hmoteph=angsep(eph[0,0],eph[1,0],eph[0,nlines-1],eph[1,nlines-1]) $
        / ((jd[nlines-1]-jd[0]) * 24.0 ) * !radeg * 3600.0

   ; Compute direction of motion using first and last points.
   objdir = atan(decobs[nlines-1]-decobs[0],raobs[0]-raobs[nlines-1])*!radeg
   ephdir = atan(eph[1,nlines-1] -eph[1,0], eph[0,0]-eph[0,nlines-1])*!radeg

   len=max(strlen(fn))
   lenstr=strn(len)
   print,'Comparing object ',objcode,' to astrometry from ',astfile
   print,'file','midtime','Terr','Derr','Dis','dRA','dDec', $
      format='(4x,a,'+strn((len-8)>6)+'x,7x,a,12x,a,6x,a,5x,a,3x,a,5x,a)'
   fmt1='(a'+lenstr+',1x,a,1x,f10.3,2x,f7.3,2x,f6.1,1x,f8.3,1x,f7.3)'
   for i=0,nlines-1 do begin
      if i eq 0 then $
         dis=0. $
      else $
         dis=angsep(raobs[0],decobs[0],raobs[i],decobs[i])*!radeg*3600.0
      jdstr,jd[i],0,str1
      print,fn[i],str1,err[i],err[i]-merr,dis,dra[i],ddec[i],format=fmt1
   endfor

   print,strn(nlines),' total measurements.'
   print,'Mean ephemeris error           ... ',mean(epherr),' arcsec', $
         format='(a,f8.2,a,1x,f5.2,a)'
   print,'Mean offset from ephemeris     ... ',merr,' arcsec,', $
         mean(err/epherr),' sigma', $
         format='(a,f8.2,a,1x,f5.2,a)'
   print,'Mean RA offset from ephemeris  ... ',mraerr,' +/-',mraerrsig, $
         ' arcsec',format='(a,f8.2,a,f8.2,a)'
   print,'Mean Dec offset from ephemeris ... ',mdecerr,' +/-',mdecerrsig, $
         ' arcsec',format='(a,f8.2,a,f8.2,a)'
   print,'Angular motion rate Eph= ',hmoteph,' Obj= ',hmotobj,'  arcsec/hr', $
         format='(a,f6.1,a,f6.1,a)'
   print,'Motion direction    Eph= ',fix(ephdir+0.5), ' Obj= ',fix(objdir+0.5), '  degrees EofN', $
         format='(a,i4,2x,a,i4,2x,a)'

   if plotit then begin
      setwin,4
      plot,dra,ddec,/iso,psym=8, $
         title='Object code '+objcode+', observatory code ='+strn(obs)+ $
               ', astfile '+astfile, $
         xtitle='RA obs - RA eph (arcsec)', $
         ytitle='Dec obs - Dec eph (arcsec)'
      setwin,5
      jd2year,jd,yr
      plot,yr,dra,psym=8,xtitle='Year',ytitle='RA obs - RA eph (arcsec)'
      if lowess_val gt 0.0 then begin
         lowess,yr*365.25,dra,lowess_val,dra_s
         oplot,yr,dra_s
      endif
      setwin,6
      plot,yr,ddec,psym=8,xtitle='Year',ytitle='Dec obs - Dec eph (arcsec)'
      if lowess_val gt 0.0 then begin
         lowess,yr*365.25,ddec,lowess_val,ddec_s
         oplot,yr,ddec_s
      endif
   endif

end
