;+
; NAME:
;  reconplan
; PURPOSE:   (one line only)
;  Generate a summary plan for future RECON occultation campaigns
; DESCRIPTION:
;  An hmtl file with a summary of future occultations is written based
;   on the events in the recon database
; CATEGORY:
;  Occultations
; CALLING SEQUENCE:
;  reconplan
; INPUTS:
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
; OUTPUTS:
; KEYWORD OUTPUT PARAMETERS:
; CONFIGURATION:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  Written by Marc W. Buie, Southwest Research Institute, 2024/11/09
;-
pro reconplan

   openmysql,dbpipe,'recon'

   jdnow=systime(/ut,/julian)
   jdend=jdnow+365
   jdstr,jdnow,300,jdnows
   jdstr,jdend,300,jdends

   cmd=['select idx,eventid,geotime,objname,', $
        'gstar,gtarg,nstations,stlist,', $
        'fprob1,fprob2,snr,exptime,orbit,etrack,regions', $
        'from targeted', $
        'where geotime>'+jdnows, $
        'and geotime<'+jdends, $
        'and exptime>0', $
        'order by jd;']
;print,cmd
   mysqlquery,dbpipe,cmd,idx,eventid,geotime,objname, $
      gstar,gtarg,nsta,stlist, $
      fprob1,fprob2,snr,exptime,orbit,etrack,regions, $
      format='l,a,a,a, f,f,i,a, f,f,f,f,a,f,a',ngood=nevents

   free_lun,dbpipe

   exptime = round(exptime*100)/100.0

   if not exists('www') then file_mkdir,'www'

   openw,lun,'www/reconplan.html',/get_lun

   printf,lun,'<html>'
   printf,lun,'<head>'
   printf,lun,'<title>RECON observing schedule</title>'
   printf,lun,'</head>'
   printf,lun,'<body>'
   printf,lun,'<h1>RECON observing schedule</h1>'

   printf,lun,'<table border=1>'
   printf,lun,'<tr>'
   printf,lun,'<th>Index#</th>'
   printf,lun,'<th>GeoTime</th>'
   printf,lun,'<th>Object</th>'
   printf,lun,'<th>Gstar</th>'
   printf,lun,'<th>Gobj</th>'
   printf,lun,'<th>Exptime</th>'
   printf,lun,'<th>Nsta</th>'
   printf,lun,'<th style="width:3in">RECON Stations</th>'
   printf,lun,'<th>P1</th>'
   printf,lun,'<th>P2+</th>'
   printf,lun,'<th>snr</th>'
   printf,lun,'<th>Orbit</th>'
   printf,lun,'<th>etrack</th>'
   printf,lun,'<th>Regions</th>'
   printf,lun,'</tr>'

   for i=0,nevents-1 do begin
      pos=strpos(regions[i],'RECON')
      if pos ge 0 then recon=1 else recon=0

      fndetail=eventid[i]+'.html'
      list = repchar(stlist[i],',','zZz')
      list = repchar(list,'zZz',', ')
      printf,lun,'<tr>'
      printf,lun,htmltcell(strn(idx[i]),/center,url=fndetail)
      printf,lun,htmltcell(geotime[i])
      printf,lun,htmltcell(objname[i])
      printf,lun,htmltcell(strn(gstar[i],format='(f10.2)'))
      printf,lun,htmltcell(strn(gtarg[i],format='(f10.2)'))
      printf,lun,htmltcell(strn(exptime[i],format='(f10.3)'),/center)
      printf,lun,htmltcell(strn(nsta[i]),/center)
      printf,lun,htmltcell(list)
      if recon then begin
         printf,lun,htmltcell(strn(fprob1[i],format='(f10.3)'))
         printf,lun,htmltcell(strn(fprob2[i],format='(f10.3)'))
      endif else begin
         printf,lun,htmltcell('&nbsp')
         printf,lun,htmltcell('&nbsp')
      endelse
      printf,lun,htmltcell(strn(snr[i],format='(f10.1)'))
      printf,lun,htmltcell(orbit[i])
      printf,lun,htmltcell(strn(etrack[i],format='(f10.1)'),/center)
      printf,lun,htmltcell(regions[i])
      printf,lun,'</tr>'
   endfor

   printf,lun,'</table>'

   jd=systime(/julian)
   jdstr,jd,0,str
   printf,lun,'<hr>'
   printf,lun,'<address>'
   printf,lun,'Created by reconplan.pro, ',str,' MT'
   printf,lun,'</address>'
   printf,lun,'</body>'
   printf,lun,'</html>'

   free_lun,lun

end
