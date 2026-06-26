;+
; NAME:
;  occtsig
; PURPOSE:   (one line only)
;  Estimate occultation event timing uncertainty
; DESCRIPTION:
;
;  Must first run occtime to get the nominal timing values.
; CATEGORY:
;  Occultations
; CALLING SEQUENCE:
;  occtsig,fnlc
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
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
; OUTPUTS:
; KEYWORD OUTPUT PARAMETERS:
;  INFO - anonymous structure with various results from the calculation
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  Written by Marc W. Buie, Southwest Research Institute, 2020/03/24
;-
compile_opt strictarrsubs
pro occtsig,fnlc,TEAM=team,INFO=info,FNCONFIG=fnconfig

   self='fnlc: '
   if badpar(fnlc,7,0,caller=self+'(fnlc) ') then return
   if badpar(fnconfig,7,0,caller=self+'(FNCONFIG) ', $
                          default='config.ini') then return

   words=strsplit(fnlc,'.',/extract)
   if n_elements(words) eq 3 then begin
      def_team=words[0]
      if badpar(team,[0,7],0,caller=self+'(TEAM) ',default=def_team) then return
   endif else begin
      if badpar(team,7,0,caller=self+'(TEAM) ') then return
   endelse

   if nofile(fnlc,'Input lightcurve') then return
   if nofile('crosstrack.dat','Ephemeris support data') then return
   if nofile('sites.dat','Site location') then return
   if nofile('events.dat','Event times') then return

   if exists('lcinfo.dat') then begin
      readcol,'lcinfo.dat',lteam,lmnpts,lmt0,lmt1,lzm1,lzm2, $
         format='a,l,f,f,l,l',count=ninfo
      zl=where(team eq lteam,count)
      if count ne 1 then begin
         print,'Team ',team,' not found in lcinfo.dat, unable to proceed.'
         return
      endif
      zl=trimrank(zl)
      mnpts=lmnpts[zl]
      mt0=lmt0[zl]
      mt1=lmt1[zl]
      zmark=[lzm1[zl],lzm2[zl]]
      toffset=0
      print,'indexes ',zmark
   endif else begin
      if nofile(fnconfig,'Configuration file') then return
      loadini,cinfo,file=fnconfig
      getvalue,cinfo,team,'mnpts',mnpts,type=3,default=4000
      getvalue,cinfo,team,'modelt0',mt0,type=4,default=0.
      getvalue,cinfo,team,'modelt1',mt1,type=4,default=0.
      getvalue,cinfo,team,'mark',in_zmark,default='-1 -1'
      words=strsplit(in_zmark,' ',/extract)
      zmark=long(words)
      if min(zmark) ge 0 then model=1 else model=0
      getvalue,cinfo,team,'toffset',toffset,type=5,default=0.0d0
   endelse

   ; all positions from fnsites are to be East Longitude
   loadocc,oinfo,fnsites=fnsites,fnxtrack=fnxtrack,fnevents=fnevents

;   readcol,'chords.dat',site,success,lat,lon,alt,date,dtime,rtime, $
;      format='a,a,d,d,d,a,a,a',count=nsites

   zsel=trimrank(where(oinfo.team eq team,count))
   if count ne 1 then begin
      print,'Team ',team,' not found, unable to proceed.'
      return
   endif

;   readcol,'crosstrack.dat',fteam,xtrack,middate,midtime,format='a,f,a,a'
   jdref=oinfo.jdref[zsel]
   jdstr,jdref,3,jdrefs

   readcol,fnlc,jd_raw,flux,format='x,d,f'
   jd = jd_raw + toffset/86400.0d0
   time=(jd-jdref)*86400.0d0

;   jd0=(long(jdgeomid)+0.5d0)

   jdstr,jd[zmark],3,jds
   print,'First marked point ',jds[0]
   print,'Last  marked point ',jds[1]

   robomean,flux,3.0,0.5,meanval,dummy,noise,stdmean=stdmean
   fluxerr=replicate(noise,n_elements(flux))
   print,'Unocculted flux',meanval,' +/- ',stdmean

   deltatd=(time[zmark[0]+1]-time[zmark[0]-1])/2.0
   print,'D Time spacing',deltatd,' seconds'
   deltatr=(time[zmark[1]+1]-time[zmark[1]-1])/2.0
   print,'R Time spacing',deltatr,' seconds'

   deltat=(deltatd+deltatr)/2.0d0
   print,'average time spacing',deltat,' seconds'

   ; replicate nominal value
   t1=time[zmark[0]]+(flux[zmark[0]]/meanval-0.5)*deltat
   t2=time[zmark[1]]+(0.5-flux[zmark[1]]/meanval)*deltat

   npts=5000

   m_samp = meanval+randomn(seed,npts)*stdmean
   fd_samp = flux[zmark[0]]+randomn(seed,npts)*noise
   fr_samp = flux[zmark[1]]+randomn(seed,npts)*noise

   fdr_samp = ((fd_samp/m_samp) > 0) < 1
   frr_samp = ((fr_samp/m_samp) > 0) < 1

   t1_samp=time[zmark[0]]+(fdr_samp-0.5)*deltat
   t2_samp=time[zmark[1]]+(0.5-frr_samp)*deltat

   tlen_samp=t2_samp - t1_samp

   robomean,t1_samp,5.0,0.5,t1_avg,dummy,t1_sig
   robomean,t2_samp,5.0,0.5,t2_avg,dummy,t2_sig
   robomean,tlen_samp,5.0,0.5,tlen_avg,dummy,tlen_sig

   print,'D',t1,t1_avg,t1_sig
   print,'R',t2,t2_avg,t2_sig
   print,'length',t2-t1,tlen_avg,tlen_sig

   jd_d=jdref+double(t1_avg)/86400.0
   jd_r=jdref+double(t2_avg)/86400.0

   print,'JD D&R',jd_d,jd_r,format='(a,1x,f16.8,1x,f16.8)'

   jdstr,jd_d,3,jd_ds
   jdstr,jd_r,3,jd_rs
   dstr='D: '+jd_ds+' +/- '+string(t1_sig,format='(f5.3)')
   rstr='R: '+jd_rs+' +/- '+string(t2_sig,format='(f5.3)')
   print,dstr,' ',rstr

   info={team: team, $
         jd_d:     jd_d, $
         jd_r:     jd_r, $
         t1:       t1, $
         t2:       t2, $
         t1_sig:   t1_sig, $
         t2_sig:   t2_sig, $
         tlen:     tlen_avg, $
         tlen_sig: tlen_sig, $
         deltat:   deltat, $
         meanval:  meanval, $
         stdmean:  stdmean, $
         noise:    noise $
         }

end
