;+
; NAME:
;  reconscore
; PURPOSE:   (one line only)
;  Calculate occultation circumstances for the RECON telescope stations
; DESCRIPTION:
; CATEGORY:
;  Occultations
; CALLING SEQUENCE:
;  reconscore,dblun,idx,full_prob,obs_score,minsig,maxsig
; INPUTS:
;  dblun - Previously opened LUN for a mysql db connection to the recon database
;  idx   - Event index number that should be found in recon.event
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;  RAOFFSET - Prediction offset (seconds of time), default=0
;  DECOFFSET - Prediction offset (seconds of arc), default=0
;  OFFSETERR - Error on prediction (arcsec), this should only be used
;                with the offsets (default=normal orbit error)
; OUTPUTS:
;  full_prob - Probability of the shadow falling somewhere within the
;                 region covered by RECON.   This is a number between 0 and 1
;  obs_score - Fraction of the sites that can see the target star under
;                 observable conditions (sun below -15 deg and target above 20)
;  minsig - minimum site distance from centerline in units of km
;  maxsig - maximum site distance from centerline in units of km
; KEYWORD OUTPUT PARAMETERS:
;  NETINFO - anonymous structure that contains more detailed information about
;               the event as viewed from each site.  The tags provided by
;               this structure are:
;      nsites:  Number of sites in the network
;      obsname: String array with the names for all the sites
;      evjd:    Array of topocentric JD-UT of minimum distance for each site
;      salt:    Array of Sun elevation at evjd for each site
;      malt:    Array of Moon elevation at evjd for each site
;      oalt:    Array of target elevation at evjd for each site
;      oaz:     Array of target azimuth at evjd for each site (W from S)
;      obs_score: RECON observability score (1=all sites can see it)
;      full_prob: RECON success probability
;      minsig:  Minimum cross-track distance in sigma
;      maxsig:  Maximum cross-track distance in sigma
;      meandtrack: mean value of cross-track site spacing in km
;      meddtrack:  median value of cross-track site spacing in km
;      mindtrack:  smallest cross-track separation in km
;      maxdtrack:  largest cross-track separation in km
;  VERBOSE-  Flag, if set enables a lot of information to be printed out
;  PIPE   -  Optional LUN of an open pipe for geteph.  If not provided, this
;               connection is automatically opened for the duration of this
;               call but closed at the end.
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  2015/04/15 - Written by Marc W. Buie, Southwest Research Institute
;  2015/10/04, MWB, numerous enhancements for returned information plus
;                 a correct calculation of event probability
;  2016/05/23, MWB, changed to use database for site position
;  2016/06/08, MWB, added RAOFFSET, DECOFFSET, and OFFSETERR keywords
;  2019/06/13, MWB, added support for active/non-active sites
;  2022/12/12, MWB, added PREDERR keyword
;  2024/11/04, MWB, chages for new non-MPC observatory coordinates
;-
pro reconscore,dblun,idx,full_prob,obs_score,minsig,maxsig, $
       NETINFO=netinfo,VERBOSE=verbose,PIPE=pipe, $
       RAOFFSET=raoffset,DECOFFSET=decoffset,OFFSETERR=offseterr,DEBUG=debug, $
       PREDERR=prederr,REGION=region

   self='reconscore: '
   if badpar(dblun,3,0,caller=self+'(dblun) ') then return
   if badpar(idx,[2,3,8],[0,1],caller=self+'(idx) ',type=idxtype) then return
   if badpar(pipe,[0,3,7],0,caller=self+'(PIPE) ',default='') then return
   if pipe eq '' then localpipe=1 else localpipe=0
   if badpar(raoffset,[0,2,3,4,5],0,caller=self+'(RAOFFSET) ', $
                                    default=0.0) then return
   if badpar(decoffset,[0,2,3,4,5],0,caller=self+'(DECOFFSET) ', $
                                     default=0.0) then return
   if badpar(offseterr,[0,2,3,4,5],0,caller=self+'(OFFSETERR) ', $
                                     default=-1.0) then return
   if badpar(prederr,[0,4,5],1,caller=self+'(PREDERR) ', $
                               default=[-1.,-1.]) then return
   if badpar(region,[0,7],0,caller=self+'(REGION) ', $
                               default='RECON') then return

   ; convert the offsets to radians, this offset can either be added to
   ; the object ephemeris or subtracted from the star position
   raoff=raoffset*15.0d0/3600.0d0*!dpi/180.0d0
   decoff=decoffset/3600.0d0*!dpi/180.0d0

   if idxtype eq 8 then begin
      objectid=idx.objectid
      ra=idx.ra
      dec=idx.dec
      jd=idx.jd
   endif else begin
      cmd=['select objectid,ra,decl,etjd from appulse', $
           'where idx='+strn(idx)+';']
      mysqlquery,dblun,cmd,objectid,ra,dec,jd,format='a,d,d,d',ngood=nfound

      if nfound eq 0 then begin
         print,'Warning!  idx=',strn(idx),' not found.  Nothing done.'
         return
      endif
   endelse

   ; tweak the star
   ra=ra-raoff
   dec=dec-decoff

   if localpipe then spawn,'geteph',unit=pipe

   cmd=['select lon,lat,alt,name,pred', $
        'from sites', $
        'where lon is not NULL', $
        'and active=1', $
        'and region like '+quote('%'+region+'%')+';']
   mysqlquery,dblun,cmd,lon,lat,alt,siteid,pred,format='d,d,f,a,i',ngood=nsites
   if nsites eq 0 then begin
      netinfo={nsites:0}
      goto,bailout
   endif

   objecttype = strmid(objectid,0,1)
   if objecttype ne 'A' and objecttype ne 'C' and objecttype ne 'P' then begin
      objcode='A'+objectid
   endif else begin
      objcode=objectid
   endelse
   appuldis,objcode,'500',jd,ra,dec,jdmin,sep,geopa,info=ginfo,starerr=0.001, $
      verbose=keyword_set(verbose),pipe=pipe,debug=debug,prederr=prederr

   if offseterr gt 0 then begin
      xtracksig=offseterr*ginfo.kmscale
   endif else begin
      xtracksig=ginfo.etrack ; provided number is the error 1-sigma (half-width)
   endelse

   obs_score=0.
   xtrack=fltarr(nsites)
   prob=fltarr(nsites)
   salt=fltarr(nsites)
   malt=fltarr(nsites)
   oalt=fltarr(nsites)
   oaz=fltarr(nsites)
   evjd=dblarr(nsites)
   if keyword_set(verbose) then begin
      fmt='(a,18x,a,4x,a,3x,a,3x,a,1x,a,1x,a,3x,a,3x,a,3x,a)'
      print,'Site','sep','PA ','Sun',' Obj','Y',' sig', $
            'P(x)','Xtrack',' dt  ',format=fmt
      print,'    ','(")','deg','alt',' alt',' ','    ', $
            '    ',' (km) ','(sec)',format=fmt
   endif
   for i=0,nsites-1 do begin
      if debug then begin
         print,siteid[i]
         print,lat[i],lon[i],alt[i]
      endif
      obsinfo,'G**',obs,lat[i],lon[i],alt[i],siteid[i],/west
      appuldis,objcode,obs,jd,ra,dec,jdmin,sep,pa, $
         info=info,pipe=pipe,debug=debug
      if debug then begin
         help,obs,/st
         help,info,/st
      endif

      if sep gt 0 then begin
         subscore= (info.salt lt -15.0 and info.oalt gt 20)
         salt[i]=info.salt
         malt[i]=info.malt
         oalt[i]=info.oalt
         oaz[i]=info.oaz
         evjd[i]=info.jd

         if pa gt 0 then sign=-1.0 else sign=1.0

         obs_score += subscore

         prob[i] = 1.0/(2.0*xtracksig)/sqrt(!pi)* $
            exp(-1.0*(info.minsep*info.kmscale/(2.0*xtracksig))^2)*subscore

         xtrack[i]=sign*info.minsep*info.kmscale

         if keyword_set(verbose) then $
            print,siteid[i],info.minsep,info.pa,info.salt,info.oalt, $
               subscore,xtrack[i]/xtracksig, $
               prob[i]*(2.0*xtracksig*sqrt(!pi)),xtrack[i], $
               (info.jd-jd)*24.0*60.0*60.0, $
               format='(a-20,1x,f5.3,1x,f6.1,2(1x,f5.1),'+ $
                      '1x,i1,1x,f5.2,1x,f6.4,2(1x,f7.1))'
      endif else begin
         subscore=0
         obs_score += subscore
         prob[i]=0.0
         xtrack[i]=1.0e20
      endelse

   endfor

   ; new probability calculation
;   z=where(xtrack lt 1.0e10 and pred eq 1,count)
;   if count ne 0 then begin
;      obsinfo={ nsites:count, siteid:siteid[z], xtrack:xtrack[z]}
;      eventprob,obsinfo,idx.diam,idx.speed,idx.etrack,results,/nodisplay ; ,/silent,/nodisplay
;help,results
;   endif

   full_xtrack=xtrack

   z=where(prob gt 0,count)

   if count lt 4 then begin
      full_prob=0.0
      minsig=0.0
      maxsig=0.0
      meandtrack=0.0
      meddtrack=0.0
      mindtrack=0.0
      maxdtrack=0.0
   endif else begin
      xtrack=xtrack[z]
      iprob=prob[z]

      tidx=uniq(xtrack,sort(xtrack))
      full_prob=int_tabulated(xtrack[tidx],iprob[tidx])
      minsig=min(xtrack)/xtracksig
      maxsig=max(xtrack)/xtracksig

      dtrack=abs(xtrack[tidx[0:-2]]-xtrack[tidx[1:-1]])
      meandtrack=mean(dtrack)
      meddtrack=median(dtrack)
      mindtrack=min(dtrack)
      maxdtrack=max(dtrack)

   endelse
   obs_score=obs_score/float(nsites)

   if keyword_set(verbose) then begin
      print,'Integrated probability of success ',full_prob
      print,'Network observability score       ',obs_score
      print,'Track range covered ',minsig,maxsig,' sig'
      print,'Track range covered ',min(xtrack),max(xtrack),' km'
      print,'Mean network spacing ',meandtrack,' km'
      print,'Median network spacing ',meddtrack,' km'
      print,'Min/max network spacing ',mindtrack,maxdtrack,' km'
   endif

   netinfo={ $
      nsites:     nsites, $
      kmscale:    ginfo.kmscale, $
      obsname:    siteid, $
      evjd:       evjd, $
      salt:       salt, $
      malt:       malt, $
      oalt:       oalt, $
      oaz:        oaz, $
      obs_score:  obs_score, $
      prob:       prob, $
      full_prob:  full_prob, $
      xtrack:     full_xtrack, $
      minsig:     minsig, $
      maxsig:     maxsig, $
      meandtrack: meandtrack, $
      meddtrack:  meddtrack, $
      mindtrack:  mindtrack, $
      maxdtrack:  maxdtrack $
      }

bailout:
   if localpipe then begin
      free_lun,pipe
      pipe=''
   endif

end
