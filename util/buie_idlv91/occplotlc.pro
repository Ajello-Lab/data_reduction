;+
; NAME:
;  occplotlc
; PURPOSE:   (one line only)
;  Plot a single site lightcurve from an occultation dataset
; DESCRIPTION:
; CATEGORY:
;  Occultations
; CALLING SEQUENCE:
;  occplotlc,team,binfac
; INPUTS:
;  team - name of the team to process (needs to match database or file)
; OPTIONAL INPUT PARAMETERS:
;  binfac - optional when there is data from only one binning factor recorded
;             in the database.  If there are two, this must be specified.
;             Having two is rare but possible.
; KEYWORD INPUT PARAMETERS:
;  FNCONFIG - name of configuration file (default is config.ini)
;  FNXTRACK - Name of the file with the crosstrack position information,
;              default='crosstrack.dat'
;  FNSITES - Name of the file with the site locations.
;                default='sites.dat'
;  NOSAVE - Flag if set suppresses saving the final graphics
;  EXTERNAL - Flag, if set the binfac is forced to 1 and it is assumed that
;               a lightcurve file is already present with the right file
;               name to be read and processed.  This is used to bring in
;               externally generated lightcurves (like from IOTA).
;  REGEN - Flag, if set, will regenerate the lightcurve in case the database
;            has changed since the last run.  This flag and EXTERNAL are
;            mutually exclusive.
; OUTPUTS:
; KEYWORD OUTPUT PARAMETERS:
; CONFIGURATION:
;   File: config.ini (default)
;   [global]
;     event   - string with short ID name of event (ex: OR20200914)
;     dir     - relative directory for this data set added to the root dir
;     date    - YYYY-MM-DD both date and directory name part of path
;     xrange  - val1 val2      (xrange, 0 0 means full range)
;   [TEAM]   - name of the team (input variable)
;     xrange  - override on value in [global]
;   [ddir]   - section to list potential root directories to find data
;     XXX - <value>
;       XXX is a unique string (eg., D01)
;       <value> is a directory path
;       for additional details see getddir.pro
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
;  The real purpose of this routine is to create and save the lightcurve
;    to a file.  It is plotted as well but occtime.pro is a far more
;    important lightcurve analysis tool.  The plotting this tool does is
;    just for reassurances.
;
;  The lightcurve files have the form of <team>_binfac.dat, Example:
;    T81_2.dat would be a lightcurve from team T81 with a binning factor
;    of two.  If the binning factor is one, the name is just <team>.dat
;    If there are lightcurves using multiple binning factors for the same
;    team, the database setting selects which to use unless you override
;    with the BINFAC keyword.
; MODIFICATION HISTORY:
;  Written by Marc W. Buie, Southwest Research Institute, 2020/09/24
;  2021/12/03, MWB, added REGEN, FNCONFIG keywords
;  2023/03/19, MWB, converted to use sites.dat instead of chords.dat
;                   Also changed to write the lightcurve file with JD not
;                   the time of day in seconds
;-
pro occplotlc,team,binfac,NOSAVE=nosave,EXTERNAL=external,REGEN=regen, $
              FNCONFIG=fnconfig,FNXTRACK=fnxtrack,FNSITES=fnsites

   self='occplotlc: '
   if badpar(team,7,0,caller=self+'(team) ') then return
   if badpar(nosave,[0,1,2,3],0,caller=self+'(NOSAVE) ', $
                default=0) then return
   if badpar(external,[0,1,2,3],0,caller=self+'(EXTERNAL) ', $
                default=0) then return
   if badpar(regen,[0,1,2,3],0,caller=self+'(REGEN) ', $
                default=0) then return
   if badpar(fnconfig,[0,7],0,caller=self+'(FNCONFIG) ', $
                             default='config.ini') then return
   if badpar(fnxtrack,[0,7],0,caller=self+'(FNXTRACK) ', $
                                  default='crosstrack.dat') then return
   if badpar(fnsites,[0,7],0,caller=self+'(FNSITES) ', $
                                  default='sites.dat') then return

   if external and regen then begin
      print,self,'Fatal error!  only one of EXTERNAL and REGEN can be set'
      return
   endif

   if nofile(fnconfig,'Configuration file') then return
   if nofile(fnxtrack,'Ephemeris support data (run occxtrack)') then return
   if nofile(fnsites,'Site locations') then return

   getddir,cinfo,root,team=team,/verbose,file=fnconfig
   getvalue,cinfo,'global','objectid',objectid
   getvalue,cinfo,'global','event',event
   getvalue,cinfo,'global','date',date
   getvalue,cinfo,'global','tsig',tsig,type=4
   getvalue,cinfo,'global','xsig',xsig,type=4
   getvalue,cinfo,team,'xrange',in_xrange
   jd0=jdparse(date)

   if in_xrange eq '0 0' then begin
      xr=[0,0]
   endif else begin
      words=strsplit(in_xrange,' ',/extract)
      xr=float(words)
   endelse

   readcol,fnxtrack,fteam,xtrack,indate,inmidtime,format='a,f,a,a'
   jdmid=jdparse(indate+' '+inmidtime)

   ; Load site information (east lon)
   readcol,fnsites,siteid,lat,lon,alt,format='a,x,d,d,d',count=nsites
   lat_r=lat/180.0d0*!dpi
   lon_r=lon/180.0d0*!dpi

   getvalue,cinfo,team,'baseline',baseline,type=4,default=0.0
   getvalue,cinfo,team,'mnpts',mnpts,type=3,default=4000
   getvalue,cinfo,team,'modelt0',mt0,type=4
   getvalue,cinfo,team,'modelt1',mt1,type=4
   getvalue,cinfo,team,'mark',in_zmark,default='-1 -1'
print,'baseline ',baseline
print,'mark     ',in_zmark

   if in_zmark eq '-1 -1' then begin
      model=0
   endif else begin
      words=strsplit(in_zmark,' ',/extract)
      zmark=long(words)
      model=1
   endelse

   openmysql,dblun,'occlc'

   cmd='select geomid,ora,odec from campaign where event='+quote(event)+';'
   mysqlquery,dblun,cmd,geomid,ra,dec,format='a,d,d',ngood=ncheck
   if ncheck ne 1 then begin
      print,cmd
      print,'campaign db entry not found.'
      goto,bailout
   endif

   zm=trimrank(where(team eq fteam,countm))
   if countm ne 1 then begin
      print,'There appear to be multiple entries in ',fnxtrack,' for ',team
      goto,bailout
   endif
   jdref=jdmid[zm]
   jdstr,jdmid[zm],3,jdrefs

   zs=trimrank(where(team eq siteid,counts))
   if counts ne 1 then begin
      print,'There appear to be multiple entries in ',fnsites,' for ',team
      goto,bailout
   endif
   obscode='G**'
   obspos=[lat_r[zs],lon_r[zs],alt[zs]]

   cmd=['select binfac,count(*)', $
        'from info', $
        'where event='+quote(event), $
        'and team='+quote(team), $
        'group by binfac;']
   mysqlquery,dblun,cmd,dbinfac,dnpts,format='i,l',ngood=nfound

   if not external then begin
      if nfound eq 1 then begin
         if badpar(binfac,[0,2,3],0,caller=self+'(binfac) ', $
                                    default=dbinfac[0]) then goto,bailout
      endif else begin
         if badpar(binfac,[2,3],0,caller=self+'(binfac) ') then begin
            print,'multiple binfac datasets found, must choose one'
            print,'found: ',dbinfac
            goto,bailout
         endif
         z=where(binfac eq dbinfac,count)
         if count ne 1 then begin
            print,'Requested binfac not in processed data'
            print,'found: ',dbinfac
            goto,bailout
         endif
      endelse
   endif else begin
      binfac=1
   endelse

   if binfac eq 1 then begin
      btag=''
   endif else begin
      btag='_'+strn(binfac)
   endelse

   fn0='Results/'+team+'_'+event+btag+'_lc.png'
   fn1='Results/'+team+'_'+event+btag+'_lcfull.png'

   fnlc=team+btag+'.dat'

   if not exists(fnlc) or regen then begin
      print,'Generating ',fnlc
      cmd=['select jd,mag,magerr', $
           'from target,info', $
           'where info.idx=target.imidx', $
           'and target.event='+quote(event), $
           'and binfac='+strn(binfac), $
           'and team='+quote(team), $
           'order by jd;']
      print,cmd
      mysqlquery,dblun,cmd,jd,mag,err,format='d,f,f',ngood=nvals
      if nvals eq 0 then begin
         print,'file ',fnlc,' not found.'
         print,'No data in a file or in the data base, quitting.'
         goto,bailout
      endif
      djd=(jd-jd0)*86400.0d0
      bad=bytarr(nvals)
      z=where(mag gt 30,count)
      if count ne 0 then bad[z]=1B
      mag2flx,mag,0.01,flux,fluxerr,zeropt=24.0
      robomean,mag,3.0,0.5,meanmag,dummy,stdev,bad=bad
      magerr=replicate(stdev,nvals)
      mag2flx,mag,magerr,flux,fluxerr,zeropt=24.0
      robomean,flux,3.0,0.5,meanflux,bad=bad
      flux = flux/meanflux
      fluxerr = fluxerr/meanflux
      print,'Writing ',fnlc
      openw,lun,fnlc,/get_lun
      for i=0,nvals-1 do begin
         printf,lun,i,jd[i],flux[i],fluxerr[i], $
            format='(i5,1x,f16.8,2(1x,f9.4))'
      endfor
      free_lun,lun
   endif

   print,'Load ',fnlc
   readcol,fnlc,seconds,flux,fluxerr,format='x,d,f,f'
   jd=jd0+seconds/86400.0d0
   time=(jd-jdmid[zm])*86400.0d0

   robomean,flux,3.0,0.5,meanval

;   flux = flux>baseline

   if model then begin
      jdstr,jd[zmark],3,jds
      print,'First marked point ',jds[0]
      print,'First marked point ',jds[1]
      mdt=double(mt1-mt0)/double(mnpts-1)
      mdtime=findgen(mnpts)*mdt + mt0
      mflux=replicate(meanval,mnpts)
      fluxp = (flux-baseline)/(1.0-baseline)
      deltat=(time[zmark[0]+1]-time[zmark[0]-1])/2.0
      print,'D Time spacing',deltat,' seconds'
      t1=time[zmark[0]]+(fluxp[zmark[0]]/meanval-0.5)*deltat
      deltat=(time[zmark[1]+1]-time[zmark[1]-1])/2.0
      print,'R Time spacing',deltat,' seconds'
      t2=time[zmark[1]]+(0.5-fluxp[zmark[1]]/meanval)*deltat
      z=where(mdtime ge t1 and mdtime le t2,count)
      if count ne 0 then mflux[z]=baseline
   endif

   title=team+'['+strn(binfac)+']: '+jdrefs+', '+ $
            strn(xtrack[zm],format='(f10.1)')+' km'

   setwin,0,xsize=1024,ysize=768
   if xr[0] eq 0 and xr[1] eq 0 then xr=minmax(time)
   yr=[0,max(flux)]
   print,'Total time range of data',minmax(time)
   ploterror,time,flux,fluxerr,psym=8,xr=xr,yr=yr,title=title, $
      xtitle='Time from predicted mid-time (sec)',symsize=1.5, $
      ytitle='Target star signal (e-)',charsize=2.0,ytickformat='(f3.1)'
   oplot,tsig*[-3,-3],yr,color=cpalette(2),thick=2
   oplot,tsig*[-2,-2],yr,color=cpalette(3),thick=2
   oplot,tsig*[-1,-1],yr,color=cpalette(4),thick=2
   oplot,tsig*[+0,+0],yr,color=cpalette(5),thick=2
   oplot,tsig*[+1,+1],yr,color=cpalette(4),thick=2
   oplot,tsig*[+2,+2],yr,color=cpalette(3),thick=2
   oplot,tsig*[+3,+3],yr,color=cpalette(2),thick=2

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

      tlen=t2-t1 ; seconds

      ephem,[jd_d,jd_r],obscode,72,objectid,eph,obspos=obspos
      ssgeom,eph,sun,earth,phang,elong
      ra1=trimrank(eph[0,0])
      dec1=trimrank(eph[1,0])
      ra2=trimrank(eph[0,1])
      dec2=trimrank(eph[1,1])
      astrd2sn,ra2,dec2,ra1,dec1,xid,etad,/arcsec
      rate=sqrt(xid^2 + etad^2)/tlen ; arcsec/sec
      kmscale = 1.0/(1.0/(earth[0]*1.49598e8)*!radeg*3600.0) ; km/arcsec
      speed = rate*kmscale ; km/sec
      print,'plane-of-sky speed',speed,' km/sec'

      cstr='Chord: '+strn(tlen,format='(f10.3)')+' sec, ' + $
                     strn(tlen*speed,format='(f10.1)')+' km, '
      xyouts,0.15,0.88,dstr,align=0,color=cpalette(35),/normal,charsize=1.5
      xyouts,0.15,0.86,rstr,align=0,color=cpalette(35),/normal,charsize=1.5
      xyouts,0.15,0.84,cstr,align=0,color=cpalette(35),/normal,charsize=1.5
      oplot,mdtime,mflux,color=cpalette(35)
   endif

   setwin,1,xsize=1024,ysize=768
   xr=minmax(time)
   yr=[0,max(flux)]
   print,'Total time range of data',minmax(time)
   ploterror,time,flux,fluxerr,psym=8,xr=xr,yr=yr,title=title, $
      xtitle='Time from predicted mid-time (sec)',symsize=1.5, $
      ytitle='Target star signal (e-)',charsize=2.0,ytickformat='(f3.1)'
   oplot,tsig*[-3,-3],yr,color=cpalette(2),thick=2
   oplot,tsig*[-2,-2],yr,color=cpalette(3),thick=2
   oplot,tsig*[-1,-1],yr,color=cpalette(4),thick=2
   oplot,tsig*[+0,+0],yr,color=cpalette(5),thick=2
   oplot,tsig*[+1,+1],yr,color=cpalette(4),thick=2
   oplot,tsig*[+2,+2],yr,color=cpalette(3),thick=2
   oplot,tsig*[+3,+3],yr,color=cpalette(2),thick=2

   if not nosave then begin
      if not exists('Results') then file_mkdir,'Results'
      tvgrab,fn0,0,/png
      tvgrab,fn1,1,/png
   endif

bailout:
   free_lun,dblun

end
