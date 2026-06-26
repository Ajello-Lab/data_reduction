;+
; NAME:
;  objappul
; PURPOSE:
;  Generate a list of nightly appulses for a solar system object.
; DESCRIPTION:
;
; CATEGORY:
;  Astronomy
; CALLING SEQUENCE:
;  objappul,starttime,ndays,objcode,outfile
; INPUTS:
;  starttime - Starting time for object ephemeris.  This can be provided as
;                a julian date (scalar), or a 3,4,5, or 6 element time vector
;                [y,m,d,h,m,s].  h, m, s, are optional in order of decreasing
;                significance.
;  ndays     - Number of days to run ephemeris over.
;  objcode   - Object code for solar system object (see EPHEM.PRO)
; OPTIONAL INPUT PARAMETERS:
;  outfile   - Output file name, default is to the screen
;  MINSEP    - Minimum separation of appulse to flag star (arcsec), default=6.
; KEYWORD INPUT PARAMETERS:
;  OBS   - Standard M.P.C. observatory code, default is geocentric or structure
; OUTPUTS:
;
;  A file is created that is in the format specified.
;
; KEYWORD OUTPUT PARAMETERS:
;
; COMMON BLOCKS:
;
; SIDE EFFECTS:
;
; RESTRICTIONS:
;  It is not practical to extract too large of a list.  Also, this program
;    does not have appropriate logic to handle the case where the object's
;    path crosses 0h RA.  There is an arbitrary limit of 2 degrees placed
;    on the extraction widths to prevent extracting the entire star catalog.
;
; PROCEDURE:
;
; MODIFICATION HISTORY:
;  Written by Marc W. Buie, Lowell Observatory, 2001/05/01
;  2001/05/31, MWB, added MINSEP keyword
;  2002/09/09, MWB, added support for string obscode values
;  2003/10/01, MWB, converted my Delfile call to system file_delete routine
;  2024/07/06, MWB, rework for new obs tools
;-
pro objappul,starttime,ndays,objcode,outfile, $
       OBS=in_obs,AMLIMIT=amlimit,MINSEP=minsep

   ;Verify correct number of parameters.
   if n_params() lt 3 then begin
      print,'objappul,starttime,ndays,objcode,[outfile],[OBSCODE=val]'
      return
   endif

   self='objappul: '
   if badpar(starttime,[2,3,4,5],[0,1],CALLER=self+'(starttime) ') then return
   if badpar(ndays,[2,3,4,5],0,CALLER=self+'(ndays) ') then return
   if badpar(objcode,7,0,CALLER=self+'(objcode) ') then return
   if badpar(in_obs,[0,2,3,7,8],0,CALLER=self+'(OBSCODE) ', $
                               default='688',type=codetype) then return

   if badpar(outfile,[0,7],0,CALLER=self+'(outfile) ', $
                         default='[[screen]]') then return
   if badpar(amlimit,[0,2,3,4,5],0,CALLER=self+'(AMLIMIT) ', $
                         default=2.5) then return
   if badpar(minsep,[0,2,3,4,5],0,CALLER=self+'(MINSEP) ', $
                         default=6.0) then return

   if codetype ne 8 then begin
      obs = in_obs
   endif else begin
      obsinfo,in_obs,obs
   endelse

   crital = 0.5*!pi - acos(1.0/amlimit)

   print,'Observatory ',obs.name,' selected.'

   case n_elements(starttime) OF
      1: begin
         jd0=starttime
      end
      3: begin
         jdcnv,starttime[0],starttime[1],starttime[2],0.,jd0
      end
      4: begin
         jdcnv,starttime[0],starttime[1],starttime[2],starttime[3],jd0
      end
      5: begin
         hr = float(starttime[3]) + float(starttime[4])/60.0
         jdcnv,starttime[0],starttime[1],starttime[2],hr,jd0
      end
      6: begin
         hr = float(starttime[3]) + float(starttime[4])/60.0 + $
              float(starttime[5])/3600.0
         jdcnv,starttime[0],starttime[1],starttime[2],hr,jd0
      end
      else: begin
         print,'OBJAPPUL: Error!  starttime must be one of the following:'
         print,'   scalar julian date,'
         print,'   3 element time vector, [y,m,d]'
         print,'   4 element time vecotr, [y,m,d,h]'
         print,'   5 element time vecotr, [y,m,d,h,m]'
         print,'   6 element time vecotr, [y,m,d,h,m,s]'
         print,''
         return
      end
   endcase

   ; Vector of days to calculate, pad by one day to get last one.
   jd = jd0 + dindgen(ndays+1)

   ; Generate a daily ephemeris
   ephem,jd,obs,2,objcode,eph

   ; Get the min and max ra,dec
   rarange = minmax(eph[0,*])
   decrange = minmax(eph[1,*])

   ; The mean of the range is the center for extraction
   ra = total(rarange)/2.0
   dec= total(decrange)/2.0

   ; compute the extraction size
   pad = 20.0
   dra = 2.0 * (rarange[1]-ra)/cos(dec) * !radeg * 3600.0 + 2.0*pad
   ddec= 2.0 * (decrange[1]-dec) * !radeg * 3600.0 + 2.0*pad

   rastr,ra,0,str1
   decstr,dec,0,str2
   print,'Extract stars centered at ',str1,' ',str2
   print,'   field width    ra=',dra/60.0,'  dec=',ddec/60.0,' arcmin', $
      format='(a,f5.1,a,f5.1,a)'

   if dra gt 7200.0 or ddec gt 7200.0 then begin
      print,'OBJAPPUL:  Error!  The field extraction widths are too large.'
      print,'           Operation aborted.'
      return
   endif

   ; Extract the stars
   starfile='tmp.refout'
   refnet,ra,dec,dra,ddec,30.0,30.0,starfile

   ; read the file that was just generated
   readcol,starfile,hr,m1,s1,dgas,m2,s2,smag, $
      format='d,d,d,a,d,d,f',/silent
   nstars=n_elements(smag)

   print,strn(nstars),' total stars found.'

   ; Convert stuff just read to sra,sdec (catalog star positions)
   signas = strmid(dgas,0,1)
   dg = fix(strmid(dgas,1,2))
   hmstorad,hr,m1,s1,sra
   sign = replicate(1.0,n_elements(dg))
   z=where(signas eq '-',count)
   if count ne 0 then sign[z] = -1.0
   dmstorad,sign,abs(dg),m2,s2,sdec

   ; don't need file anymore
   file_delete,starfile,/quiet,/noexpand_path

   ; Step over each day and look at the object's motion wrt the stars
   for i=0,ndays-1 do begin

      ; compute a time in the middle of the present span
      jdc = (jd[i]+jd[i+1])/2.0

      ; Sun position at input JD
      sunpos,jdc,sunra,sundec,/radian

      ; Define night, Sun set to sun rise.
      am  = airmass(jdc,sunra,sundec,obs.lat,obs.wlon,alt=alt,lha=lha,lst=lst)
      hatojd,!dpi,sunra,lst,jdc,jdlclmid ; jd of nearest local midnight
      lsidtim,jdlclmid,obs.wlon,midlst       ; LST at local midnight
      jdofmid = float(long(jdlclmid+0.5d0))-0.5d0
      jdstr,jdofmid,100,thisdate

      ; Hour angle of Sun at sunset, AT, NT, CT
      altoha,-18.0/!radeg,sundec,obs.lat,sunatha,sunattype
      altoha,-12.0/!radeg,sundec,obs.lat,sunntha,sunnttype
      altoha,-6.0/!radeg,sundec,obs.lat,sunctha,suncttype
      altoha,-0.5/!radeg,sundec,obs.lat,sunhorzha,sunhorztype

      ; JD of sunset/sunrise, AT, NT, CT
      jdatset  = jdlclmid - (!dpi-sunatha)/2.0d0/!dpi
      jdatrise = jdlclmid + (!dpi-sunatha)/2.0d0/!dpi
      jdntset  = jdlclmid - (!dpi-sunntha)/2.0d0/!dpi
      jdntrise = jdlclmid + (!dpi-sunntha)/2.0d0/!dpi
      jdctset  = jdlclmid - (!dpi-sunctha)/2.0d0/!dpi
      jdctrise = jdlclmid + (!dpi-sunctha)/2.0d0/!dpi
      jdsset   = jdlclmid - (!dpi-sunhorzha)/2.0d0/!dpi
      jdsrise  = jdlclmid + (!dpi-sunhorzha)/2.0d0/!dpi

      jdstr,jdsset,-12,jdssetstr
      jdstr,jdsrise,-12,jdsrisestr
      jdstr,jdatset,-12,jdatsetstr
      jdstr,jdatrise,-12,jdatrisestr
      jdstr,jdntset,-12,jdntsetstr
      jdstr,jdntrise,-12,jdntrisestr

      am  = airmass(jdc,sunra,sundec,obs.lat,obs.wlon,alt=alt,lha=lha,lst=lst)
      hatojd,!dpi,sunra,lst,jdc,jdlclmid
      lsidtim,jdlclmid,obs.wlon,midlst
      hatojd,0.0d0,eph[0,i],midlst,jdlclmid,jdtrans
      jdstr,jdtrans,-12,objtransstr
      transam = airmass(jdtrans,eph[0,i],eph[1,i],obs.lat,obs.wlon,alt=transalt)
      altoha,crital,eph[1,i],obs.lat,horzha,type
      if type eq 0 then begin
         jdrise  = jdtrans - horzha/2.0d0/!dpi
         jdset   = jdtrans + horzha/2.0d0/!dpi
      endif
      obswind,midlst,obs.lat,eph[0,i],eph[1,i],jdntrise,jdntset,rtime,rkind, $
         stime,skind,objtype

      print,' '
      print,'Sun ',jdntsetstr,' ',jdntrisestr

      if objtype ne -1 and objtype ne 2 then begin

         jdstr,rtime,-12,risestr
         jdstr,stime,-12,setstr
         prior = (rtime-jdntset)*24.0d0
         post  = (jdntrise-stime)*24.0d0
         durat = (stime-rtime)*24.0d0

         ; rtime and stime are JD that bound objects window in night.
         npts=30000.0
         jdpath = rtime+dindgen(npts)/double(npts-1)*(stime-rtime)
         interp,jd,reform(eph[0,*]),jdpath,rapath
         interp,jd,reform(eph[1,*]),jdpath,decpath

         ; convert to tangent plane coords
         astrd2sn,sra,sdec,rapath[0],decpath[0],sxi,seta
         astrd2sn,rapath,decpath,rapath[0],decpath[0],xi,eta

         ; convert xi,eta from radians to arcsec
         sxi  = sxi*180.0d0/!dpi*3600.0d0
         seta = seta*180.0d0/!dpi*3600.0d0
         xi   = xi*180.0d0/!dpi*3600.0d0
         eta  = eta*180.0d0/!dpi*3600.0d0

         lsidtim,rtime,obs.wlon,rlst       ; LST at window start
         lsidtim,stime,obs.wlon,slst       ; LST at window stop
         rastr,rlst,-2,rlststr
         rastr,slst,-2,slststr
         print,objcode,' ',risestr,'[',rlststr,'] ',setstr,'[',slststr,']'

         ; Loop over each star and compute separation at each time
         for j=0,nstars-1 do begin
            angsep = sqrt((xi-sxi[j])^2 + (eta-seta[j])^2)
            z=where(min(angsep) eq angsep)
            z=z[0]
            if angsep[z] lt minsep then begin
               interp,jdpath,angsep,rtime,rsep
               interp,jdpath,angsep,stime,ssep
               jdstr,jdpath[z],-12,mintimestr
               print,mintimestr,rsep,min(angsep),ssep,smag[j], $
                  format='(4x,a,"  sep=",3f5.1," mag=",f4.1)'
            endif
         endfor

      endif else begin
         jdstr,jdlclmid,-12,datestr
         print,strmid(datestr,0,11),' object never rises.'
      endelse

   endfor


end
