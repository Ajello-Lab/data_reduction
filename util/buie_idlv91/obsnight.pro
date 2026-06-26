;+
; NAME:
;  obsnight
; PURPOSE:   (one line only)
;  Determine general details of a given night determined by Sun and Moon
; DESCRIPTION:
; CATEGORY:
;  Astronomy
; CALLING SEQUENCE:
;  obsnight,jd,obs,night
; INPUTS:
;  jd - Julian date for a time near midnight, string or double
;  obs - Observatory information, either an integer, string, or structure
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;  PRINT - Flag, if set causes the information in the structure to be
;            printed to the console.
; OUTPUTS:
;  night - anonymous structure with information about the night
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  2016/02/05, Written by Marc W. Buie, Southwest Research Institute
;  2023/07/16, MWB, added PRINT keyword
;  2024/07/05, MWB, tweaks needed for new
;-
pro obsnight,in_jd,in_obs,night,PRINT=print,LCLMUT=lclmut

   self='obsnight: '
   if badpar(in_jd,[5,7],0,caller=self+'(jd) ',type=jdtype) then return
   if badpar(in_obs,[2,3,7,8],[0,1],caller=self+'(obs) ',type=obstype) then return
   if badpar(print,[0,1,2,3],0,caller=self+'(PRINT) ',default=0) then return
   if badpar(lclmut,[0,2,3,4,5],0,caller=self+'(LCLMUT) ', $
                                 default=0.0d0) then return

   if jdtype eq 7 then begin
      jd=jdparse(in_jd)
   endif else begin
      jd=in_jd
   endelse

   if obstype eq 8 then begin
      obs=in_obs
   endif else begin
      obsinfo,in_obs,obs
   endelse

   ; Nearest midnight
   sunpos,jd,sunra,sundec,/radian
   am  = airmass(jd,sunra,sundec,obs.lat,obs.wlon,alt=alt,lha=lha,lst=lst)
   hatojd,!dpi,sunra,lst,jd,jdlclmid ; jd of nearest local midnight
   lsidtim,jdlclmid,obs.wlon,midlst       ; LST at local midnight
   jdofmid = float(long(jdlclmid+0.5d0))-0.5d0
   jdstr,jdofmid,100,thisdate
   jdstr,jdlclmid,-3,jdlclmids
   jdstr,jdlclmid+lclmut/24.0d0,-3,jdlclmids_lcl

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

   jdstr,jdsset,-2,jdssets
   jdstr,jdsrise,-2,jdsrises
   jdstr,jdatset,-2,jdatsets
   jdstr,jdatrise,-2,jdatrises
   jdstr,jdntset,-2,jdntsets
   jdstr,jdntrise,-2,jdntrises
   jdstr,jdctset,-2,jdctsets
   jdstr,jdctrise,-2,jdctrises

   jdstr,jdsset+lclmut/24.0d0,-2,jdssets_lcl
   jdstr,jdsrise+lclmut/24.0d0,-2,jdsrises_lcl
   jdstr,jdatset+lclmut/24.0d0,-2,jdatsets_lcl
   jdstr,jdatrise+lclmut/24.0d0,-2,jdatrises_lcl
   jdstr,jdntset+lclmut/24.0d0,-2,jdntsets_lcl
   jdstr,jdntrise+lclmut/24.0d0,-2,jdntrises_lcl
   jdstr,jdctset+lclmut/24.0d0,-2,jdctsets_lcl
   jdstr,jdctrise+lclmut/24.0d0,-2,jdctrises_lcl

   ; Moon information, at midnight
   moonpos,jdlclmid,moonra,moondec,/radian
   mphase,jdlclmid,moonphase

   hatojd,!dpi,moonra,lst,jd,jdmoontran ; jd of nearest lunar transit
   altoha,-0.5/!radeg,moondec,obs.lat,moonhorzha,moonhorztype
   jdmset   = jdmoontran - (!dpi-sunhorzha)/2.0d0/!dpi
   jdmrise  = jdmoontran + (!dpi-sunhorzha)/2.0d0/!dpi

   jdstr,jdmset,-2,jdmsets
   jdstr,jdmrise,-2,jdmrises

   events=['CT','CT','NT','NT','AT','AT','Mid','Mrise','Mset','Srise','Sset']
   times=[jdctset,jdctrise,jdntset,jdntrise,jdatset,jdatrise, $
          jdlclmid,jdmrise,jdmset,jdsrise,jdsset]

   idx=sort(times)
   events=events[idx]
   times=times[idx]

   z=where(times ge jdsset and times le jdsrise,/null)
   events=events[z]
   times=times[z]
   nevents=n_elements(events)
   jdstr,times,-2,times_str
   jdstr,times+lclmut/24.0d0,-2,times_str_lcl

   night={ $
      utdates:   thisdate, $
      jdlclmid:  jdlclmid, $
      jdlclmids: jdlclmids, $
      jdatset:   jdatset, $
      jdatrise:  jdatrise, $
      jdntset:   jdntset, $
      jdntrise:  jdntrise, $
      jdctset:   jdctset, $
      jdctrise:  jdctrise, $
      jdsset:    jdsset, $
      jdsrise:   jdsrise, $
      jdatsets:  jdatsets, $
      jdatrises: jdatrises, $
      jdntsets:  jdntsets, $
      jdntrises: jdntrises, $
      jdctsets:  jdctsets, $
      jdctrises: jdctrises, $
      jdssets:   jdssets, $
      jdsrises:  jdsrises, $
      jdmsets:   jdmsets, $
      jdmrises:  jdmrises, $
      events:    events, $
      times:     times_str, $
      nevents:   nevents, $
      sunra:     sunra, $
      sundec:    sundec, $
      moonra:    moonra, $
      moondec:   moondec, $
      moonphase: moonphase $
      }

   if print then begin
      info=[]
      str='Night summary for '+night.utdates+' at '+obs.name
      info=[info,str]
      cvtsixty,obs.lat,-0.5d0*!dpi,0.5d0*!dpi,0,['N','S'],lats,places=0
      cvtsixty,obs.lon,0.0d0,2.0d0*!dpi,0,['E','W'],lons,places=0
      str='     Lat = '+lats+' Lon = '+lons+ $
          ' Alt = '+strn(obs.alt,format='(f10.1)')+'m'
      info=[info,str]
      str='     Lunar phase = '+strn(moonphase,format='(f10.2)')
      info=[info,str]
      rastr,midlst,0,midlsts
      str='     LST at local midnight '+midlsts
      info=[info,str]
      for i=0,nevents-1 do begin
         str=string(events[i],format='(a-5)')+' '+times_str[i]+' UT'
         if lclmut ne 0 then str=str+'    '+times_str_lcl[i]+' local'
         info=[info,str]
      endfor
      for i=0,n_elements(info)-1 do $
         print,info[i]
   endif


end
