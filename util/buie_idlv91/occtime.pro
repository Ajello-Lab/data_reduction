;+
; NAME:
;  occtime
; PURPOSE:   (one line only)
;  Extract simple occultation timing from an occultation lightcurve
; DESCRIPTION:
;  This program reads a lightcurve file that containes an occultation and 
;    fits a perfect occultation model to get the start and stop times of
;    the occultation.
;  There are other files expected that provide supporting information for
;    the occultation.
;   xtrack.in   - contains general information about the occultation
;                   leading and trailing blanks on a line do not matter
;                   any lines after the required set are ignored
;     line 1 - Object code (for geteph, see ephem.pro)
;     line 2 - RA of the star (sexigesimal string)
;     line 3 - Dec of the star (sexigesimal string)
;     line 4 - geocentric UT date and time of appulse
;     line 5 - 1-sigma down-track uncertainty (in seconds)
;     line 6 - 1-sigma cross-track uncertainty (in km)
;
;   crosstrack.dat - created by occxtrack
;
;   lcinfo.dat  - definition of region of fitting for the lightcurves for
;                   all stations
;     col 1 - Site code
;     col 2 - Number of points in perfect model
;     col 3 - Start time of model relative to prediction
;     col 4 - End time of model relative to prediction
;     col 5 - point number of ingress (first point showing a drop)
;     col 6 - point number of egress (last point showing a drop)
;
; CATEGORY:
;  Occultations
; CALLING SEQUENCE:
;  occtime,fnlc
; INPUTS:
;  fnlc - Name of lightcurve file.  The data are expected to be in tabular
;           format, space separated.  The first column is often the point
;           number but is not used by this program.  Only these columns are
;           used:
;           col 2 - Julian Date of lightcurve measurement (mid-time, UTC)
;           col 3 - Flux from target star, usually normalized
;
;           Anything after column 3 is ignored but usually contains
;             then non-normalized flux, x,y position, and FWHM in pixels.
;
;           If the filename is of the form <team>.<start time>.lc, the
;             <team> string is used to provide the default value for TEAM
;
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;  NOSAVE - Flag, if set suppresses saving the output grapics
;  TEAM   - String, name of the team/data [default from fnlc if the right form]
;  XRANGE - time range to plot [seconds], default is to plot it all
;  YRANGE - flux range to plot, default is to plot full range
;  NOTSIG - Flag, if set suppresses showing the time sigma hacks on the plot
;  MCMC   - Flag, if set runs a MCMC fit and error analysis run on the
;              event.  This should not be run until you have a good simple
;              fit for the event and lcinfo.dat is set properly.  Note that
;              that the event times found in events.dat are not used by
;              this program and are not updated automatically.
;  FNCONFIG - Config file to read, default='config.ini', overrides TAG
;  FNSITES - name of the file with site location information (default=sites.dat)
;  FNEVENTS - name of occultation events file (default=events.dat)
;  FNXTRACK - crosstrack file to read, default='crosstrack.dat', overrides TAG
;  TAG      - Extra tag for certain file names.  Default is ''.  This is
;              used when there are multiple objects in an occultation
;              dataset.  The files that are modified are:
;               fnconfig, fnxtrack, and saved lightcurve plots
;              crosstrack.dat becomes crosstrack_<tag>.dat, you don't
;              provide the underscore, just the tag.  Note that the sites.dat
;              and events. dat files work for all bodies.
; 
; OUTPUTS:
;   plots mostly but window 0 is saved to Results/<team>_<body>_lc.png,
;      unless you supply a TAG then it would be <team>_<body>_<tag>_lc.png
; KEYWORD OUTPUT PARAMETERS:
; CONFIGURATION:
;   File: config.ini
;   [global]
;     objectid - string with the object id (see ephem.pro)
;     ora      - Apparent position of star at time of occultation
;     odec     - Apparent position of star at time of occultation
;     geomid   - UT date and time of geocentric closest approach
;     tsig     - 1-sigma uncertainty in event time (down-track) in seconds
;     xsig     - 1-sigma cross-track uncertainty in km
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  Written by Marc W. Buie, Southwest Research Institute, 2020/01/31
;  2020/03/29, MWB, code simplifications
;  2020/10/06, MWB, added support for config.ini file
;  2022/03/27, MWB, now reads model information from config.ini if lcinfo.dat
;                      is not found
;  2022/06/07, MWB, added FNCONFIG,FNCHORDS,FNXTRACK keywords
;  2022/12/14, MWB, convert to using fnsites.dat and fnevents.dat
;  2023/03/16, MWB, fixed D&R time to account for non-zero baseline
;  2023/03/28, MWB, force renormalization of lightcurve
;-
compile_opt strictarrsubs
function occtime_chisq,vals
   common occtime_com,data

;   meanval = randomn(data.seed,1)*data.stdmean + data.meanval
   meanval = data.meanval

   if data.type eq 0 then begin
      t1=vals[0]
      t2=vals[1]
   endif else begin
      t1=vals[0]
      t2=t1+vals[1]
   endelse
   data.t1=t1
   data.t2=t2

   occmodel,data.time[data.zf],data.t1,data.t2,model,error
   data.model=model*meanval
   if error then begin
      chisq=!values.d_infinity
   endif else begin
      chisq = total(((data.flux[data.zf]-data.model)/ $
                      data.fluxerr[data.zf])^2)/double(data.nf-2)
   endelse
   data.chisq=chisq
   return,chisq
end

pro occtime,fnlc,NOSAVE=nosave,TEAM=team,XRANGE=xrange,YRANGE=yrange, $
                 NOTSIG=notsig,MCMC=mcmc,FNCONFIG=fnconfig,FNSITES=fnsites, $
                 FNEVENTS=fnevents,FNXTRACK=fnxtrack,TAG=tag

   common occtime_com,data

   self='fnlc: '
   if badpar(fnlc,7,0,caller=self+'(fnlc) ') then return
   if badpar(nosave,[0,1,2,3],0,caller=self+'(NOSAVE) ',default=0) then return
   if badpar(notsig,[0,1,2,3],0,caller=self+'(NOTSIG) ',default=0) then return
   if badpar(mcmc,[0,1,2,3],0,caller=self+'(MCMC) ',default=0) then return
   if badpar(tag,[0,7],0,caller=self+'(TAG) ',default='') then return
   def_fnsites = 'sites.dat'
   def_fnevents = 'events.dat'
   if tag eq '' then begin
      def_fnconfig = 'config.ini'
      def_fnxtrack = 'crosstrack.dat'
   endif else begin
      def_fnconfig = 'config_'+tag+'.ini'
      def_fnxtrack = 'crosstrack_'+tag+'.dat'
   endelse
   if badpar(fnconfig,[0,7],0,caller=self+'(FNCONFIG) ', $
                              default=def_fnconfig) then return
   if badpar(fnsites,[0,7],0,caller=self+'(FNSITES) ', $
                              default=def_fnsites) then return
   if badpar(fnevents,[0,7],0,caller=self+'(FNEVENTS) ', $
                              default=def_fnevents) then return

   words=strsplit(fnlc,'.',/extract)
   if n_elements(words) ge 2 or n_elements(words) le 4 then begin
      def_team=words[0]
      if badpar(team,[0,7],0,caller=self+'(TEAM) ',default=def_team) then return
   endif else begin
      if badpar(team,7,0,caller=self+'(TEAM) ') then return
   endelse

   ; all positions from fnsites are to be East Longitude
   loadocc,oinfo,fnsites=fnsites,fnxtrack=fnxtrack,fnevents=fnevents

   if oinfo.nsites eq 0 then begin
      print,'no valid site data found in ',fnsites
      return
   endif

   if nofile(fnlc,'Input lightcurve') then return

   if exists(fnconfig) then begin
      loadini,cinfo,file=fnconfig
      getvalue,cinfo,'global','objectid',objectid
      getvalue,cinfo,'global','ora',ras
      getvalue,cinfo,'global','odec',decs
      getvalue,cinfo,'global','geomid',geomid
      getvalue,cinfo,'global','tsig',tsig,type=5
      getvalue,cinfo,'global','xsig',xsig,type=5
      ra=raparse(ras)
      dec=decparse(decs)
      jdgeomid=jdparse(geomid)
      getvalue,cinfo,team,'xrange',cfg_xrange,type=4,/array,default=[-5,5]
      getvalue,cinfo,team,'yrange',cfg_yrange,type=4,/array,default=[0,1.2]
      if badpar(xrange,[0,1,2,3,4,5],1,caller=self+'(XRANGE) ', $
                                       default=cfg_xrange) then return
      if badpar(yrange,[0,1,2,3,4,5],1,caller=self+'(YRANGE) ', $
                                       default=cfg_yrange) then return
   endif else begin
      if nofile('xtrack.in','Occultation description') then return
      rdxtrack,event,objectid,ra,dec,jdgeomid,tsig,xsig
      if badpar(xrange,[0,1,2,3,4,5],1,caller=self+'(XRANGE) ', $
                                       default=[-5,5]) then return
      if badpar(yrange,[0,1,2,3,4,5],1,caller=self+'(YRANGE) ', $
                                       default=[0,1.2]) then return
   endelse

   getvalue,cinfo,team,'baseline',baseline,type=4,default=0.0
   getvalue,cinfo,team,'toffset',toffset,type=5,default=0.0d0

   if exists('lcinfo.dat') then begin
      readcol,'lcinfo.dat',lteam,lmnpts,lmt0,lmt1,lzm1,lzm2, $
         format='a,l,f,f,l,l',count=ninfo
      zl=where(team eq lteam,count)
      if count eq 1 then begin
         zl=trimrank(zl)
         model=1
         mnpts=lmnpts[zl]
         mt0=lmt0[zl]
         mt1=lmt1[zl]
         zmark=[lzm1[zl],lzm2[zl]]
         print,'indexes ',zmark
      endif else begin
         model=0
      endelse
   endif else begin
      getvalue,cinfo,team,'mnpts',mnpts,type=3,default=4000
      getvalue,cinfo,team,'modelt0',mt0,type=4,default=0.
      getvalue,cinfo,team,'modelt1',mt1,type=4,default=0.
      getvalue,cinfo,team,'mark',in_zmark,default='-1 -1'
print,'*** ',team,' ',in_zmark
      words=strsplit(in_zmark,' ',/extract)
      zmark=long(words)
      if min(zmark) ge 0 then model=1 else model=0
   endelse

   zs=trimrank(where(team eq oinfo.team,countc))
   if countc ne 1 then begin
      print,'Unable to find team ',team,' in sites.dat'
      return
   endif

   jdref=oinfo.jdref[zs]
   jdstr,jdref,3,jdrefs

   obsinfo,'G**',obs,oinfo.lat_r[zs],oinfo.lon_r[zs],oinfo.alt[zs],oinfo.team[zs]
;   obsinfo={obscode: 'G**', name:'mobile', $
;            lat: oinfo.lat_r[zs], lon: -oinfo.lon_r[zs], alt: oinfo.alt[zs]}
   appuldis,objectid,obs,jdref,ra,dec,info=ginfo

   jd0=(long(jdgeomid)+0.5d0)

   if not exists('Results') then file_mkdir,'Results'

   if tag eq '' then begin
      fn0='Results/'+team+'_lc.png'
   endif else begin
      fn0='Results/'+team+'_'+tag+'_lc.png'
   endelse
;   fn1='Results/'+team+'_sky.png'
;   fn2='Results/'+team+'_trans.png'
;   fn3='Results/'+team+'_lccor.png'
;   fn4='Results/'+team+'_nstars.png'

   readcol,fnlc,jd_raw,flux,format='x,d,f',count=ndata
   jd = jd_raw + toffset/86400.0d0
   time=(jd-jdref)*86400.0d0

   bad=bytarr(ndata)
   z=where(flux lt median(flux)/2.0,count)
   if count ne 0 then bad[z]=1B
   robomean,flux,3.0,0.5,meanval,dummy,noise,bad=bad
   flux=flux/meanval
   fluxerr=replicate(noise,n_elements(flux))/meanval

   if model then begin
      jdstr,jd[zmark],3,jds
      print,'First marked point ',jds[0]
      print,'Last  marked point ',jds[1]
      mdt=double(mt1-mt0)/double(mnpts-1)
      mdtime=findgen(mnpts)*mdt + mt0
      mflux=replicate(1.0,mnpts)
      midval = (1.0+baseline)/2.0
      deltat=(time[zmark[0]+1]-time[zmark[0]-1])/2.0
      print,'D Time spacing',deltat,' seconds'
      t1=time[zmark[0]]+(flux[zmark[0]]/meanval-midval)*deltat
      deltat=(time[zmark[1]+1]-time[zmark[1]-1])/2.0
      print,'R Time spacing',deltat,' seconds'
      t2=time[zmark[1]]+(midval-flux[zmark[1]]/meanval)*deltat
      z=where(mdtime ge t1 and mdtime le t2,count)
      if count ne 0 then mflux[z]=baseline
   endif else begin
      print,'No model event set for this lightcurve.'
   endelse

   title=team+': '+jdrefs+', '+strn(oinfo.xtrack[zs],format='(f10.1)')+' km'

   xr=xrange
   yr=yrange
   if xrange[0] eq 0 and xrange[1] eq 0 then xr=minmax(time)
   if yrange[0] eq 0 and yrange[1] eq 0 then yr=[0,max(flux)]

   setwin,1,xsize=1024,ysize=768
   plot,flux,psym=8,yr=yr

   setwin,0,xsize=1024,ysize=768
   print,'Total time range of data',minmax(time)
   ploterror,time,flux,fluxerr,psym=8,xr=xr,yr=yr,title=title, $
      xtitle='Time from predicted mid-time (sec)',symsize=1.5, $
      ytitle='Target star signal (e-)',charsize=2.0,ytickformat='(f3.1)'
   if not notsig then begin
      oplot,tsig*[-3,-3],yr,color=cpalette(2),thick=2
      oplot,tsig*[-2,-2],yr,color=cpalette(3),thick=2
      oplot,tsig*[-1,-1],yr,color=cpalette(4),thick=2
      oplot,tsig*[+0,+0],yr,color=cpalette(5),thick=2
      oplot,tsig*[+1,+1],yr,color=cpalette(4),thick=2
      oplot,tsig*[+2,+2],yr,color=cpalette(3),thick=2
      oplot,tsig*[+3,+3],yr,color=cpalette(2),thick=2
   endif
   z=where(time ge xr[0] and time le yr[1],count)
   if count ne 0 then begin
      print,'First and last point in windowed plot',z[0],z[-1]
   endif
   if toffset ne 0 then begin
      str='Time offset '+strn(toffset,format='(f10.3)')+' seconds'
      xyouts,0.85,0.88,str,align=1,color=cpalette(35),/normal,charsize=1.5
   endif


   if model then begin
      print,'times of first and last marked points', $
         time[zmark[0]],time[zmark[1]]
      oplot,time[zmark[0]:zmark[1]],flux[zmark[0]:zmark[1]],psym=8, $
         color=cpalette(35),symsize=1.5
      jd_d=jdref+double(t1)/86400.0
      jd_r=jdref+double(t2)/86400.0
      print,'JD D&R',jd_d,jd_r,format='(a,1x,f16.8,1x,f16.8)'
      jdstr,jd_d,3,jd_ds
      jdstr,jd_r,3,jd_rs
      dstr='D: '+jd_ds
      rstr='R: '+jd_rs
      print,dstr,' ',rstr
      tlen=t2-t1
      cstr='Chord: '+strn(tlen,format='(f10.3)')+' sec, ' + $
                     strn(tlen*ginfo.speed,format='(f10.1)')+' km, '
      xyouts,0.15,0.88,dstr,align=0,color=cpalette(35),/normal,charsize=1.5
      xyouts,0.15,0.86,rstr,align=0,color=cpalette(35),/normal,charsize=1.5
      xyouts,0.15,0.84,cstr,align=0,color=cpalette(35),/normal,charsize=1.5
      oplot,mdtime,mflux,color=cpalette(35)
   endif

   if not nosave then begin
      tvgrab,fn0,0,/png
   endif

   if mcmc then begin
      occtsig,fnlc,team=team,info=info,fnconfig=fnconfig
      print,'Length ',info.tlen*ginfo.speed,info.tlen_sig*ginfo.speed,' km'
      occmodel,time,info.t1,info.t2,model,error
      z=where(model ne model[0],count)
      z1=(min(z)-3)>0
      z2=(max(z)+3)<(n_elements(time)-1)
      z=lindgen(z2-z1+1)+z1
      oplot,time[z],model[z],psym=4,color=cpalette(3),symsize=3

;      chisq = total(((flux[z]-model[z])/fluxerr[z])^2)/float(count-3)
;      print,chisq

      val=randomn(seed,1)

      data={time: time, $
            flux: flux, $
            fluxerr: fluxerr, $
            zf:   z, $
            nf:   count, $
            meanval: info.meanval, $
            stdmean: info.stdmean, $
            seed:  seed, $
            type:  0, $ ; 0=t1,t2; 1=t1,len
            t1:    info.t1, $
            t2:    info.t2, $
            model: flux*0., $
            chisq: 0.0d0 $
            }

      if data.type eq 0 then begin
         vals=[info.t1,info.t2]
         scale=[info.t1_sig,info.t2_sig]
;         scale=[info.deltat,info.deltat]*0.1
      endif else begin
         vals=[info.t1,info.t2-info.t1]
         scale=[info.t1_sig,info.t2_sig]
;         scale=[info.deltat,info.deltat]*0.1
      endelse
      chisq=occtime_chisq(vals)
      print,'Initial chisq',chisq
      region=scale*5
      mcmcsamp,vals,scale,region,pdf,function_name='occtime_chisq', $
         verbose=0,display=0,nsteps=10000

      if data.type eq 0 then begin
         print,'t1,t2 fit'
         t1_avg = mean(pdf[0,*])
         t1_sig = stddev(pdf[0,*])
         t2_avg = mean(pdf[1,*])
         t2_sig = stddev(pdf[1,*])
         print,'T1             ',t1_avg,t1_sig
         print,'T2             ',t2_avg,t2_sig
         print,'Length         ',mean(pdf[1,*]-pdf[0,*]),stddev(pdf[1,*]-pdf[0,*]),' sec'
         print,'Length         ',mean(pdf[1,*]-pdf[0,*])*ginfo.speed,stddev(pdf[1,*]-pdf[0,*])*ginfo.speed,' km'
      endif else begin
         print,'t1,len fit'
         t1_avg = mean(pdf[0,*])
         t1_sig = stddev(pdf[0,*])
         t2_avg = mean(pdf[0,*]+pdf[1,*])
         t2_sig = stddev(pdf[0,*]+pdf[1,*])
         print,'T1             ',t1_avg,t1_sig
         print,'T2             ',t2_avg,t2_sig
         print,'Length         ',mean(pdf[1,*]),stddev(pdf[1,*]),' km'
         print,'Length         ',mean(pdf[1,*])*ginfo.speed,stddev(pdf[1,*])*ginfo.speed,' km'
      endelse

   jd_d=jdref+double(t1_avg)/86400.0
   jd_r=jdref+double(t2_avg)/86400.0

   print,'JD D&R',jd_d,jd_r,format='(a,1x,f16.8,1x,f16.8)'

   jdstr,jd_d,3,jd_ds
   jdstr,jd_r,3,jd_rs
   dstr='D: '+jd_ds+' +/- '+string(t1_sig,format='(f5.3)')
   rstr='R: '+jd_rs+' +/- '+string(t2_sig,format='(f5.3)')
   print,dstr,' ',rstr
      
   endif

end
