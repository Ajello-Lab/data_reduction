;+
; NAME:
;  recondetail
; PURPOSE:   (one line only)
;  Generate html and email detail files for a single campaign
; DESCRIPTION:
; CATEGORY:
;  Occultations
; CALLING SEQUENCE:
;  recondetail,idx
; INPUTS:
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;  FORCEUPDATE - Flag, if set forces the update to be done even if the
;                  indicators say otherwise.
; OUTPUTS:
; KEYWORD OUTPUT PARAMETERS:
; CONFIGURATION:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  Written by Marc W. Buie, Southwest Research Institute, 2024/11/10
;  2024/12/02, MWB, major upgrades to handle regions
;-
pro recondetail,idx,FORCEUPDATE=forceupdate,DBPIPE=dbpipe, $
                    EPHPIPE=ephpipe,ERROR=error

   self='recondetail: '
   if badpar(idx,[2,3],0,caller=self+'(idx) ') then return
   if badpar(forceupdate,[0,1,2,3],0,caller=self+'(FORCEUPDATE) ', $
                                     default=0) then return

   if badpar(dbpipe,[0,2,3],0,caller=self+'(DBPIPE) ',default=-99) then return
   if badpar(ephpipe,[0,2,3],0,caller=self+'(EPHPIPE) ',default=-99) then return

   error=1

   ephwasopen=0;
   if ephpipe gt 0 then begin
      st=fstat(ephpipe)
      ephwasopen = st.open eq 1B
   endif

   if ephwasopen eq 0 then begin
      spawn,'geteph',unit=ephpipe
   endif

   dbwasopen=0;
   if dbpipe gt 0 then begin
      st=fstat(dbpipe)
      dbwasopen = st.open eq 1B
   endif

   if dbwasopen eq 0 then begin
      openmysql,dbpipe,'recon'
   endif
   c=','

   logstr=[]
   wdir='www/'
   if not exists(wdir) then file_mkdir,wdir

   jdnow=systime(/ut,/julian)
   jdstr,jdnow,300,jdnows

   cmd=['select eventid,jd,objname,objectid,sra,sdec,', $
        'gstar,gtarg,earth,selong,melong,mphase,kmscale,speed,rate,', $
        'hv,xtdiam,diam1,diam2,etime,etrack,mobile,', $
        'snr,exptime,tscale,pred,orbit,regions', $
        'from targeted', $
        'where idx='+strn(idx), $
        'and geotime>'+jdnows, $
        'and active=1;']
;print,cmd
   mysqlquery,dbpipe,cmd,eventid,jd,objname,objectid,sra,sdec, $
      gstar,gtarg,earth,selong,melong,mphase,kmscale,speed,rate, $
      hv,xtdiam,diam1,diam2,etime,etrack,mobile, $
      snr,exptime,tscale,pred,orbit,regions, $
      format='a,d,a,a,d,d, f,f,d,f,f,f,f,d,d, f,f,f,f,d,d,a, f,f,f,a,a,a', $
      ngood=ncheck

   if ncheck ne 1 then begin
      str=['event index '+strn(idx)+ $
          ' is blocked from updating', $
          'either it does not exist, is flagged as inactive,'+ $
          ' or is in the past', $
          'Aborting recondetail operation.']
      logstr=[logstr,str]
      goto,bailout
   endif

   region=strsplit(regions,',',/extract)
   nregions=n_elements(region)
   if nregions le 0 then begin
      str='number of regions is zero, this should not happen, aborting'
      logstr=[logstr,str]
      goto,bailout
   endif

   safejd=jd
   fnlist1=eventid+'_ofdate.txt'
   fnlist2=eventid+'_j2000.txt'
   if not exists(wdir+fnlist1) then begin
      starset,jd,sra,sdec,gtarg,objname,listing
      wrstrarr,listing,wdir+fnlist1
   endif else begin
      rdstrarr,wdir+fnlist1,listing
   endelse
   if not exists(wdir+fnlist2) then begin
      starset,jd,sra,sdec,gtarg,objname,listing_j2000,/j2000
      wrstrarr,listing_j2000,wdir+fnlist2
   endif else begin
      rdstrarr,wdir+fnlist2,listing_j2000
   endelse

   fnchart=eventid+'_chart.png'
   if not exists(wdir+fnchart) then begin
      ; Orientation for a site in the middle of the network
      obslat=41.0*!dpi/180
      obslon=120.0*!dpi/180
      mkastinfo,sra,sdec,0,0,1.0,syninfo
      astcvt,'xy',0,0,syninfo,'rd',ra,dec
      dray= 60.0d0/3600.0d0*!dpi/180.0d0
      hangle,jd,ra,dec,obslat,obslon,ha0,lst0
      altaz,ha0,obslat,dec,alt0,az0
      lcltoeq,alt0+dray,az0,obslat,ha1,dec1
      ra1=lst0-ha1
      astcvt,'rd',ra1,dec1,syninfo,'xy',xray,yray
      lpa = atan(yray,xray) * !radeg
      rotang=lpa+90
      starchart,sra,sdec,1600,1600,1.0,wdir+fnchart,maglim=17,jd=jd, $
         title='RECON   '+eventid,orient=rotang
   endif

   if xtdiam le 0 then xtdiam=diam1

   einfo={objectid:objectid,ra:sra,dec:sdec,jd:safejd, $
                diam:xtdiam,speed:speed,etrack:etrack}

   ff='(f10.1)'
   ff3='(f10.3)'

   fnhtml = eventid+'.html'
;   fnemail = eventid+'.mail'

   print,'Writing to ',wdir+fnhtml
   openw,hlun,wdir+fnhtml,/get_lun
;   openw,elun,wdir+fnemail,/get_lun

   printf,hlun,'<html>'
   printf,hlun,'<head>'
   printf,hlun,'<title>RECON event prediction for '+eventid+'</title>'
   printf,hlun,'</head>'
   printf,hlun,'<body>'
   printf,hlun,'<h1>RECON observing guide for '+eventid+'</h1>'

   words=strsplit(objname,'_',/extract)
   str='Occultation event with ('+words[0]+') '+words[1]+ $
       ', event index number '+strn(idx)
   printf,hlun,'<p>'
   printf,hlun,str
   printf,hlun,'<br>'
   printf,hlun,'Regions able to see the event: ',regions
   printf,hlun,'</p>'

   jdstr,jd,0,jds
   str='Geocentric closest approach at '+jds+' UTC'
   printf,hlun,'<p>'
   printf,hlun,str
   printf,hlun,'</p>'

   printf,hlun,'<p>'
   rastr,sra,1,sras
   decstr,sdec,0,sdecs
   str='J2000 position of star is '+sras+' '+sdecs
   printf,hlun,str,'<br>'
;   printf,elun,str
   jd2year,safejd,eqofdate
   sra_ofd=sra
   sdec_ofd=sdec
   precess,sra_ofd,sdec_ofd,2000.0,eqofdate,/radian
   rastr,sra_ofd,1,sra_ofds
   decstr,sdec_ofd,0,sdec_ofds
   str='Equinox of date position of star is '+sra_ofds+' '+sdec_ofds
   printf,hlun,str,'<br>'
;   printf,elun,str
   str='Star is '+strn(round(melong))+' degrees from the moon. Moon is '+ $
       strn(round(mphase*100))+'% illuminated.'
   printf,hlun,str,'<br>'
;   printf,elun,str
   str='Stellar brightness G='+strn(gstar,format='(f10.1)')+ $
       ', apparent brightess of occulting body is G='+ $
       strn(gtarg,format='(f10.1)')
   printf,hlun,str,'<br>'
;   printf,elun,str
   mag2flx,gstar,0.01,fluxstar,zeropt=24.0
   mag2flx,gtarg,0.01,fluxtarg,zeropt=24.0
   fluxdrop = fluxtarg/(fluxstar+fluxtarg)
   str='Use an <b>exposure time of '+strn(exptime,format='(f10.2)')+ $
       ' seconds</b> with the standard RECON-QHY system.'
   printf,hlun,str,'<br>'
;   printf,elun,str
   str='Expected flux drop is '+strn(round(fluxdrop*100))+ $
       '% with SNR of '+strn(snr,format='(f10.1)')+' per integration'
   printf,hlun,str,'<br>'
;   printf,elun,str
   printf,hlun,'</p>'

   printf,hlun,'<p>'
   str='Apparent velocity is '+strn(speed,format='(f10.1)')+ $
       ' km/sec on the sky relative to the star, or, '+ $
       strn(rate,format='(f10.1)')+' arcsec/hr.'
   printf,hlun,str,'<br>'
;   printf,elun,str
   str='The recommended exposure time corresponds to '+ $
       strn(speed*exptime,format='(f10.1)')+' km per image.'
   printf,hlun,str,'<br>'
;   printf,elun,str
   str='The 1-sigma error in the time of the event is '+ $
       strn(etime,format='(f10.1)')+' seconds.'
   printf,hlun,str,'<br>'
;   printf,elun,str
   str='The 1-sigma cross-track error in the shadow position is '+ $
       strn(etrack,format='(f10.1)')+' km.'
   printf,hlun,str,'<br>'
;   printf,elun,str
   str='The sky-plane scale is '+strn(kmscale,format='(f10.1)')+' km/arcsec.'
   printf,hlun,str,'<br>'
;   printf,elun,str
   printf,hlun,'</p>'

   printf,hlun,'<p>'
   str='Diameter estimates:'
   printf,hlun,str,'<br>'
;   printf,elun,str
   str=strn(diam1,format='(f10.1)')+' km assuming a 5% albedo, aximum of '+ $
       strn(diam1/speed,format='(f10.1)')+' sec for a central chord'
   printf,hlun,str,'<br>'
;   printf,elun,str
   str=strn(diam2,format='(f10.1)')+' km assuming a 30% albedo, maximum of '+ $
       strn(diam2/speed,format='(f10.1)')+' sec for a central chord'
   printf,hlun,str,'<br>'
;   printf,elun,str
   if xtdiam gt 0 then begin
      str='Cross-track diameter of '+strn(xtdiam,format='(f10.1)')+ $
          ' km used for deployment plan.'
   endif else begin
      str='Cross-track diameter of '+strn(diam1,format='(f10.1)')+ $
          ' km used for deployment plan.'
      xtdiam = diam1
   endelse
   printf,hlun,str,'<br>'
;   printf,elun,str
   printf,hlun,'</p>'

   printf,hlun,'<p><pre>'
   for i=0,n_elements(listing)-1 do printf,hlun,listing[i]
   printf,hlun,'</pre></p>'

   if exists(wdir+fnchart) then begin
      printf,hlun,'<p><img src="',fnchart,'"></a></p>'
   endif

print,regions,nregions
   for j=0,nregions-1 do begin
print,j,' ',region[j]
      reconscore,dbpipe,einfo,full_prob,obs_score,minsig,maxsig, $
               netinfo=netinfo,pipe=ephpipe,region=region[j] ; ,/debug
      if netinfo.nsites gt 0 then begin
         zo=where(netinfo.salt lt -15.0 and netinfo.oalt gt 20,counto)
         if counto eq 0 then begin
            logstr=[logstr,'no fixed sites in '+region[j]+' can see this event']
            continue
         endif
         obsinfo={nsites:counto, $
                  name:netinfo.obsname[zo], $
                  xtrack:netinfo.xtrack[zo]}
         eventprob,obsinfo,xtdiam,speed,etrack,results, $
            /nodisplay,/silent,ntrials=30000
         prob=fltarr(netinfo.nsites)
         listcor,results.name,netinfo.obsname,ind1,ind2
         prob[ind2]=results.prob[ind1]
         sidx=reverse(sort(netinfo.xtrack))
         zpick=where(netinfo.salt[sidx] lt -15.0 and $
                     netinfo.oalt[sidx] ge  20.0 and $
                     prob[sidx] gt 0.0004, countpick)
         nsta=countpick

         sinfo=strarr(netinfo.nsites)
         oinfo=strarr(netinfo.nsites)
         minfo=strarr(netinfo.nsites)
         z=where(netinfo.salt lt -18,count)
         if count ne 0 then sinfo[z]='Dark'
         z=where(netinfo.salt lt -12 and netinfo.salt ge -18,count)
         if count ne 0 then sinfo[z]='AT'
         z=where(netinfo.salt lt -6 and netinfo.salt ge -12,count)
         if count ne 0 then sinfo[z]='NT'
         z=where(netinfo.salt lt -0.8 and netinfo.salt ge -6,count)
         if count ne 0 then sinfo[z]='CT'
         z=where(netinfo.salt ge -0.8,count)
         if count ne 0 then sinfo[z]='Sun up'
         z=where(netinfo.oalt gt 30,count)
         if count ne 0 then oinfo[z]='Up'
         z=where(netinfo.oalt le 30 and netinfo.oalt gt 20,count)
         if count ne 0 then oinfo[z]='Low'
         z=where(netinfo.oalt le 20 and netinfo.oalt gt 0,count)
         if count ne 0 then oinfo[z]='Really Low'
         z=where(netinfo.oalt le 0,count)
         if count ne 0 then oinfo[z]='Not Up'
         z=where(netinfo.malt lt 0,count)
         if count ne 0 then minfo[z]='Down'
         z=where(netinfo.malt ge 0,count)
         if count ne 0 then minfo[z]='Up'
         djd = 3.*60./86400.0d0 ; 3 minutes
         jd1=netinfo.evjd-djd
         jd2=netinfo.evjd+djd
         jdstr,jd1,0,jd1s
         jdstr,jd2,0,jd2s
         jd1s=strmid(jd1s,11)
         jd2s=strmid(jd2s,11)

         printf,hlun,'<p>'
         printf,hlun,'This table contains the target star visibility for registered'
         printf,hlun,region[j],' sites that are relevant for this occultation.'
         printf,hlun,'This list has been filtered to only those sites that can'
         printf,hlun,'see the field and are within range of the shadow given the'
         printf,hlun,'uncertainties.  The observing time range has been calculated'
         printf,hlun,'to cover the time of the event at each site with allowances'
         printf,hlun,'for the event timing uncertainty plus time for obtaining'
         printf,hlun,'a pre and post event baseline lightcurve.  This set of'
         printf,hlun,'stations has a ',strn(results.prob1*100,format='(f10.1)')
         printf,hlun,'% chance'
         printf,hlun,'of getting at least one chord.  There is a'
         printf,hlun,strn(results.prob2*100,format='(f10.1)'),'% chance of 2 or more'
         printf,hlun,'chords assuming clear weather for all.'

         printf,hlun,'<table border=1>'
         printf,hlun,'<tr>'
         printf,hlun,'<th>Site Name</th>'
         printf,hlun,'<th>Site mid-time<br>(UT)</th>'
         printf,hlun,'<th>Sun alt<br>(deg)</th>'
         printf,hlun,'<th>Sky</th>'
         printf,hlun,'<th>Moon</th>'
         printf,hlun,'<th>Star alt<br>(deg)</th>'
         printf,hlun,'<th>Star az<br>(deg)</th>'
         printf,hlun,'<th>Target</th>'
         printf,hlun,'<th>Observing Time</th>'
         printf,hlun,'<th>Xtrack<br>(km)</th>'
         printf,hlun,'<th>Prob</th>'
         printf,hlun,'</tr>'
         printf,hlun,'<tr>'

         for i=0,nsta-1 do begin
            printf,hlun,htmltcell(strn(netinfo.obsname[sidx[zpick[i]]]))
            jdstr,netinfo.evjd[sidx[zpick[i]]],0,jdmids
            printf,hlun,htmltcell(strmid(jdmids,11),/center)
            printf,hlun,htmltcell(strn(netinfo.salt[sidx[zpick[i]]],format=ff),/center)
            printf,hlun,htmltcell(sinfo[sidx[zpick[i]]],/center)
            printf,hlun,htmltcell(minfo[sidx[zpick[i]]],/center)
            printf,hlun,htmltcell(strn(netinfo.oalt[sidx[zpick[i]]],format=ff),/center)
            printf,hlun,htmltcell(strn(netinfo.oaz[sidx[zpick[i]]],format=ff),/center)
            printf,hlun,htmltcell(oinfo[zpick[i]],/center)
            printf,hlun,htmltcell(jd1s[zpick[i]]+'-'+jd2s[zpick[i]])
            printf,hlun,htmltcell(strn(netinfo.xtrack[sidx[zpick[i]]],format=ff),/center)
            printf,hlun,htmltcell(strn(prob[sidx[zpick[i]]],format=ff3),/center)
            printf,hlun,'</tr>'
         endfor
         printf,hlun,'</table>'

      endif
   endfor

   printf,hlun,'Azimuth is measured in degrees eastward from north. North'
   printf,hlun,'is at an azimuth of 0, due East is at an azimuth of 90'
   printf,hlun,'degrees, due South is 180, and due West is 270.'
   printf,hlun,'</p>'

   printf,hlun,'<p><pre>'
   for i=0,n_elements(listing_j2000)-1 do printf,hlun,listing_j2000[i]
   printf,hlun,'</pre></p>'

   jdnow=systime(/julian)
   jdstr,jdnow,0,str
   printf,hlun,'<hr>'
   printf,hlun,'<address>'
   printf,hlun,'Created by recondetail.pro, ',str,' MT'
   printf,hlun,'</address>'
   printf,hlun,'</body>'
   printf,hlun,'</html>'

   jdnow=systime(/julian,/ut)
   jdstr,jdnow,300,jdnows
   cmd=['update targeted', $
        'set plot='+jdnows, $
        'where idx='+strn(idx)+';']
   mysqlcmd,dbpipe,cmd

   error=0

bailout:
   if ephwasopen eq 0 then begin
      free_lun,ephpipe
   endif

   if dbwasopen eq 0 then begin
      free_lun,dbpipe
   endif

   for i=0,n_elements(logstr)-1 do $
      print,logstr[i]

   test=size(hlun,/type)
   if test ne 0 then free_lun,hlun ; ,elun

end
