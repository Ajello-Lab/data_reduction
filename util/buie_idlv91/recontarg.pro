;+
; NAME:
;  recontarg
; PURPOSE:   (one line only)
;  Add or update a specific RECON targeted occultation campaign
; DESCRIPTION:
; CATEGORY:
;  Occultations
; CALLING SEQUENCE:
;  recontarg,idx=<index> or
;  recontarg,jd=<jd>,objectid=<objectid>
; INPUTS:
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;
;  These two keywords are required for adding new events:
;
;  JD - Julian Date (UT) of geocentric mid-time of appulse
;  OBJECTID - Object code used by geteph (includes prefix), this must resolved
;           to a valid ephemeris.
;
;  This is the required keyword to use for events already in the database:
;
;  IDX - index value in the database.  JD and OBJECTID are ignored if this is
;           provided.  This is NOT used for a new event to be added.  For that
;           use the JD and OBTECTID keywords.
;
;  The following keywords are relevant only when adding a new event.
;
;  The next three keywords control the star picked up as the
;    occultation target star.  This works best if the position is the catalog
;    star position, not the position at epoch.  This is used to guide the
;    Gaia catalog search and is not directly stored in the database.  The
;    Gmag is not required but can help automatically find the star.  RA and
;    DEC is used only if both are present.  If none of these are present,
;    the geocentric position of the object at the input time is used to search
;    the catalog.  Some times this leads to multiple hits and providing the
;    magnitude can be the easiest way to latch onto the correct star.
;  RA - catalog position of star (radians or string).
;  DEC - catalog position of star (radians or string).
;
; The following keywords are always relevant.
;  DBPIPE - LUN of an open pipe to a database connection.  If negative
;              (default), this tool will open a connection for you at the
;              start and then close just before returning.  Providing this
;              can save time if making many calls to this routine in a row.
;  EPHPIPE - LUN of an open pipe to a connection to geteph for ephemeris
;              calculations.  If negative (default), this tool will open a
;              connection for you at the start and then close it just before
;              returning.  Useful for many calls to this tool.
;  GTARG - Use this to overide the automatic system for estimating the
;              apparent brightness of the target.
;  GMAG  - This is a check magnitude to make sure the Gaia catalog search
;              picked up the right star.  If the one star found doesn't match
;              it will generate and error
;
; The following keywords are relevant only for pre-existing events.
;  REDOGAIA - Flag, if set will re-run the Gaia catalog search and update
;                the star position.  This flag is irrelevant if a new record
;                is needed.  This is provided to help support refreshing
;                the prediction in the case of a new Gaia catalog release.
;  FORCEUPDATE - Flag, if set will bypass any logic that decides if an
;                  existing record should be updated.  Without this, an
;                  update will be run only if it seems likely there's been
;                  a change.
;  ACTIVATE - Flag, if set, will re-activate an event.  This is ignored if
;                there is no matching record that is marked in-active.
;                If the re-activation is successsful, an update is considered
;                and done if needed.
;  DEACTIVATE - Flag, if set, will mark an active event as inactive.  This
;                is ignored if there is no matching record.
;
; OUTPUTS:
;   All output information goes either to the screen for positive feedback
;     or to the "recon" database and the "targeted" table.
; KEYWORD OUTPUT PARAMETERS:
;  ERROR - flag set if something went wrong
;  LOGSTR - String array of log information about run of program.
; CONFIGURATION:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  Written by Marc W. Buie, Southwest Research Institute, 2024/03/24
;  2024/11/09, MWB, futher development work
;  2024/11/15, MWB, integrated new exposure time calculation and improved
;                      auto-location of target star
;-
pro recontarg,IDX=idx,JD=in_jd,OBJECTID=objectid, $
              RA=RA,DEC=DEC, $
              GTARG=gtarg,GMAG=gmag_check, $
              REDOGAIA=redogaia,FORCEUPDATE=forceupdate, $
              DBPIPE=dbpipe,EPHPIPE=ephpipe, $
              ACTIVATE=activate,DEACTIVATE=deactivate,ERROR=error,$
              NOCATDEL=nocatdel,CATWIDTH=catwidth,LOGSTR=logstr, $
              SILENT=silent,REGIONS=regions

   error=1
   self='recontarg: '
   if badpar(idx,[0,2,3],0,caller=self+'(IDX) ',default=-1L) then return

   if idx le 0 then begin
      if badpar(in_jd,[4,5,7],0,caller=self+'(jd) ', $
                                type=jd_type) then return
      if badpar(objectid,7,0,caller=self+'(objectid) ') then return
   endif

   if badpar(dbpipe,[0,2,3],0,caller=self+'(DBPIPE) ',default=-99) then return
   if badpar(ephpipe,[0,2,3],0,caller=self+'(EPHPIPE) ',default=-99) then return
   if badpar(gtarg,[0,2,3,4,5],0,caller=self+'(GTARG) ',default=-1) then return

   if badpar(gmag_check,[0,2,3,4,5],0,caller=self+'(GMAG) ', $
                                      default=-100.) then return
   if gmag_check lt 0 then def_catwidth=3 else def_catwidth=30
   if badpar(catwidth,[0,2,3,4,5],0,caller=self+'(CATWIDTH) ', $
                                    default=def_catwidth) then return

   if badpar(in_ra,[0,4,5,7],0,caller=self+'(RA) ', $
                               default='',type=ra_type) then return
   if badpar(in_dec,[0,4,5,7],0,caller=self+'(DEC) ', $
                               default='',type=dec_type) then return

   if badpar(nocatdel,[0,1,2,3],0,caller=self+'(NOCATDEL) ', $
                                  default=0) then return
   if badpar(redogaia,[0,1,2,3],0,caller=self+'(REDOGAIA) ', $
                                  default=0) then return
   if badpar(forceupdate,[0,1,2,3],0,caller=self+'(FORCEUPDATE) ', $
                                  default=0) then return
   if badpar(activate,[0,1,2,3],0,caller=self+'(ACTIVATE) ', $
                                  default=0) then return
   if badpar(deactivate,[0,1,2,3],0,caller=self+'(DEACTIVATE) ', $
                                  default=0) then return
   if badpar(silent,[0,1,2,3],0,caller=self+'(SILENT) ', $
                                  default=0) then return
   if badpar(regions,[0,7],0,caller=self+'(REGIONS) ', $
                                  default='RECON') then return

   if exists('star.cat.gcat') and not nocatdel then file_delete,'star.cat.gcat'

   if idx lt 0 then begin
      if jd_type eq 7 then begin
         jd = jdparse(in_jd)
         jds = in_jd
      endif else begin
         jd = in_jd
         jdstr,jd,1,jds
      endelse
   endif

   if ra_type eq 7 then begin
      if in_ra eq '' then begin
         ra  = -1.0
         ras = ''
         havecatpos = 0
      endif else begin
         ras=in_ra
         ra=raparse(ras)
         havecatpos = 0
      endelse
   endif else begin
      ra=in_ra
      rastr,in_ra,5,ras
      havecatpos = 1
   endelse

   if dec_type eq 7 then begin
      if in_dec eq '' then begin
         dec = -!pi
         decs = ''
      endif else begin
         decs=in_dec
         dec=decparse(decs)
         havecatpos = havecatpos+1
      endelse
   endif else begin
      dec=in_dec
      decstr,in_dec,4,decs
      havecatpos = havecatpos+1
   endelse
   if havecatpos eq 2 then havecatpos=1 else havecatpos=0

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

   ; Decide if processing new or old record
   if idx ge 1 then begin
      cmd='select count(*) from targeted where idx='+strn(idx)+';'
      mysqlquery,dbpipe,cmd,check,format='a'
      if check eq 0 then begin
         str='Index '+strn(idx)+' was not found in the database.  Aborting.'
         logstr=[logstr,str]
         goto,bailout
      endif
      newrecord=0
      cmd='select jd from targeted where idx='+strn(idx)+';'
      mysqlquery,dbpipe,cmd,jd,format='d'
      jdstr,jd,1,jds
   endif else begin
      cmd=['select idx,jd from targeted', $
           'where abs(jd-'+strn(jd,format='(f20.5)')+') < 0.0015;']
      mysqlquery,dbpipe,cmd,newidx,newjd,format='l,d',ngood=nfound
      if nfound eq 0 then begin
         newrecord=1
      endif else if nfound eq 1 then begin
         newrecord=0
         idx = newidx
         str='Previous record ('+strn(idx)+') found for UT '+ $
              jds+', object '+objectid
         logstr=[logstr,str]
         logstr=[logstr,'Processing of this request halted.']
         goto,bailout
      endif else begin
         str,'Multiple hits found for UT '+jds+', object '+objectid
         logstr=[logstr,str]
         logstr=[logstr,'Processing of this request halted.']
         goto,bailout
      endelse
   endelse

   jdnow=systime(/julian,/ut)
   jdstr,jdnow,300,jdnows

   if jd lt jdnow then begin
      str='Event time is in the past, not adding.'
      logstr=[logstr,str]
      error=0
      goto,bailout
   endif

   if newrecord then begin
      str='Adding new record'
      logstr=[logstr,str]
      str='UT '+jds+', object '+objectid
      logstr=[logstr,str]
   endif else begin
      str='Processing existing record '+strn(idx)
      logstr=[logstr,str]
   endelse

   gaiasearch=0
   if redogaia  then gaiasearch=1
   if newrecord then gaiasearch=1

   if gaiasearch then begin
      str='Accessing Gaia catalog to find target star information'
      logstr=[logstr,str]
      if not havecatpos then begin
         str='Using target position at reference time to initiate search'
         logstr=[logstr,str]
         ephem,jd,'500',2,objectid,eph,pipe=ephpipe
         ra = trimrank(eph[0])
         dec = trimrank(eph[1])
         if ra lt 0 then begin
            str='bad objectid ('+objectid+'), no ephemeris returned'
            logstr=[logstr,str]
            goto,bailout
         endif
         rastr,ra,5,ras
         decstr,dec,4,decs
         str='object '+objectid+' is at '+ras+' '+decs
         logstr=[logstr,str]
      endif else begin
         str='Using input position: '+ras+' '+decs
         logstr=[logstr,str]
      endelse
      jd2year,jd,epoch
      if not exists('star.cat.gcat') then $
         refnet,ra,dec,catwidth,catwidth,30.,30.,'star.cat', $
            gaia=epoch,/noconvert
      paracorr,jd,info
      if info.nstars eq 0 then begin
         str='ERROR!  There are no stars in star.cat.gcat'
         logstr=[logstr,str]
         goto,bailout
      endif
      if info.nstars gt 1 and gmag_check lt 0 then begin
         str='ERROR!  There are '+strn(info.nstars)+' stars in star.cat.gcat'
         logstr=[logstr,str]
         str='either edit star.cat.gcat or provide a check magnitude'
         logstr=[logstr,str]
         goto,bailout
      endif
      if info.nstars gt 1 then begin
         sep=angsep(info.ora,info.odec,ra,dec)*180.0d0/!dpi*3600.0
         z=where(abs(info.gmag-gmag_check) le 0.02,count)
         if count eq 0 then begin
            str='ERROR!  no stars pass the magnitude check, how odd.'
            logstr=[logstr,str]
            str='sep='+strjoin(string(sep),',')
            logstr=[logstr,str]
            str='info.gmag='+strjoin(string(info.gmag),',')
            logstr=[logstr,str]
            str='gmag_check='+strn(info.gmag)
            logstr=[logstr,str]
            str='info.gmag-gmag_check='+ $
               strjoin(string(info.gmag-gmag_check),',')
            logstr=[logstr,str]
            goto,bailout
         endif
         zz=where(sep[z] eq min(sep[z]))
         z=z[zz]
      endif else begin
         z=0
      endelse
      gmag = trimrank(info.gmag[z])
      if abs(gmag-gmag_check) gt 0.02 then begin
         str='ERROR!  mismatch on Gaia star magnitude'
         logstr=[logstr,str]
         str='Gmag='+strn(gmag)+'  checkmag='+strn(gmag_check)
         logstr=[logstr,str]
         goto,bailout
      endif

      file_delete,'star.cat.gcat'
      cra = trimrank(info.cra[z])
      cdec = trimrank(info.cdec[z])
      sra = trimrank(info.ora[z])
      sdec = trimrank(info.odec[z])
      sraerr = trimrank(info.orasig[z])
      sdecerr = trimrank(info.odecsig[z])
      rastr,cra,5,cras
      decstr,cdec,4,cdecs
      rastr,sra,5,sras
      decstr,sdec,4,sdecs
      str='Target star found, catalog position '+cras+' '+cdecs
      logstr=[logstr,str]
      str='Apparent position: '+sras+' '+sdecs+ $
          '  Gmag='+strn(gmag,format='(f10.2)')
      logstr=[logstr,str]
   endif ; end of gaiasearch block

;help,jd,sra,sdec,jdmin,sep

   if forceupdate or newrecord then update=1 else update=0

   if newrecord then begin
;help,objectid,jd,sra,sdec
      appuldis,objectid,'500',jd,sra,sdec,jdmin,sep,info=ainfo,starerr=0.0001
      if ainfo.error eq 1 then begin
         str='ERROR! failed to find an appulse'
         logstr=[logstr,str]
         goto,bailout
      endif
      if gtarg lt 0 then gtarg = ainfo.gtarg
;help,ainfo
;goto,bailout
      jd=jdmin
      jdstr,jd,300,jds
      jdstr,jd,1,jdmins
      str='Geocentric mid-time is '+jdmins
      logstr=[logstr,str]

      if strmid(objectid,0,1) eq 'A' then begin
         eventid = nobname(ainfo.objname)+'_'+jds
      endif else begin
         cmd=['select name,bspfile from kernel', $
              'where naifid='+quote(objectid), $
              'and active=1;']
         mysqlquery,dbpipe,cmd,name,bspfile,format='a,a',ngood=nfound
         if nfound ne 1 then begin
            str='objectid '+objectid+' found '+strn(nfound)+ $
                ' times in kernel table, should be 1'
            logstr=[logstr,str]
            goto,bailout
         endif
         eventid = nobname(name)+'_'+jds
      endelse

      cmd=['insert into targeted set', $
           'eventid='+quote(eventid)+c, $
           'jd='+strn(jd,format='(f20.8)')+c, $
           'geotime='+quote(jds)+c, $
           'objectid='+quote(objectid)+c, $
           'objname='+quote(nobname(ainfo.objname))+c, $
           'cra='+strn(cra,format='(f20.10)')+c, $
           'cdec='+strn(cdec,format='(f20.10)')+c, $
           'catalog='+quote('edr3')+c, $
           'sra='+strn(sra,format='(f20.10)')+c, $
           'sdec='+strn(sdec,format='(f20.10)')+c, $
           'sraerr='+strn(sraerr,format='(f20.4)')+c, $
           'sdecerr='+strn(sdecerr,format='(f20.4)')+c, $
           'gtarg='+strn(gtarg,format='(f20.3)')+c, $
           'gstar='+strn(gmag,format='(f20.4)')+c, $
           'hv='+strn(ainfo.hv,format='(f20.4)')+c, $
           'diam1='+strn(ainfo.diam1,format='(f20.2)')+c, $
           'diam2='+strn(ainfo.diam2,format='(f20.2)')+c, $
           'orbit='+quote('astorb')+c, $
           'regions='+quote(regions)+c, $
           'active=1;' $
           ]
      logstr=[logstr,cmd]
      mysqlcmd,dbpipe,cmd
      cmd=['select idx from targeted', $
           'where eventid='+quote(eventid)+';']
      mysqlquery,dbpipe,cmd,idx,format='l'
   endif

   cmd='select active from targeted where idx='+strn(idx)+';'
   mysqlquery,dbpipe,cmd,active,format='d,i'
   if active eq 0 then begin
      if active eq 0 then logstr=[logstr,'Event is marked inactive']
      error=0
      goto,bailout
   endif

   ; Check for extra reasons to update
   if not update then begin
      cmd='select objectid,pred from targeted where idx='+strn(idx)+';'
      mysqlquery,dbpipe,cmd,objid,lastpred,format='a'
      jdpred=jdparse(lastpred)
      logstr=[logstr,'Event index number = '+strn(idx)+ $
                     ', last updated '+lastpred]
      logstr=[logstr,'Object id in database '+objid]
      if strmid(objid,0,1) eq 'A' then begin
         ephem,jdnow,'500',11,objid,edata,pipe=ephpipe
         jdlast=edata[14]
         if jdlast le jdpred then begin
            logstr=[logstr,'Prediction is up to date already']
            error=0
            goto,bailout
         endif
         logstr=[logstr,'There is new data in astorb, update prediction']
         update=1
      endif else begin
         cmd='select source from targeted where idx='+strn(idx)+';'
         mysqlquery,dbpipe,source,format='a';
         if source ne bspfile then begin
            logstr=[logstr,'update kernel to '+source]
            bspfile=source
            update=1
         endif else begin
            logstr=[logstr,'ephemeris kernel file already up to date']
            error=0
            goto,bailout
         endelse
      endelse
   endif

   if update then begin

      cmd=['select objectid,xtdiam,diam1,sra,sdec,gstar,gtarg', $
           'from targeted where idx='+strn(idx)+';']
      mysqlquery,dbpipe,cmd,objectid,xtdiam,diam1,sra,sdec,gmag,gtarg, $
         format='a,f,f,d,d,f,f'
      if xtdiam lt 0 then xtdiam=diam1

      appuldis,objectid,'500',jd,sra,sdec,jdmin,sep,info=ainfo,starerr=0.0001

      einfo={objectid:objectid,ra:sra,dec:sdec,jd:jd, $
             diam:xtdiam,speed:ainfo.speed,etrack:ainfo.etrack}
;print,'xoxoxoxo'
;help,einfo
;print,'xoxoxoxo'

;print,'einfo'
;help,einfo,/st
      pos=strpos('RECON',regions)
      if pos ge 0 then begin
         reconscore,dbpipe,einfo,full_prob,obs_score,minsig,maxsig, $
               netinfo=netinfo,pipe=ephpipe
         zo=where(netinfo.salt lt -15.0 and netinfo.oalt gt 20,counto)
         if counto eq 0 then begin
            logstr=[logstr,'no fixed sites can see this event']
            goto,bailout
         endif
         obsinfo={nsites:counto, $
                  name:netinfo.obsname[zo], $
                  xtrack:netinfo.xtrack[zo]}
         eventprob,obsinfo,xtdiam,ainfo.speed,ainfo.etrack, $
            results,/nodisplay,/silent,ntrials=30000
         prob=fltarr(netinfo.nsites)
         listcor,results.name,netinfo.obsname,ind1,ind2
         prob[ind2]=results.prob[ind1]
         sidx=reverse(sort(netinfo.xtrack))
         zpick=where(netinfo.salt[sidx] lt -15.0 and $
                     netinfo.oalt[sidx] ge  20.0 and $
                     prob[sidx] gt 0.0004, countpick)
         recon=1
      endif else begin
         recon=0
         countpick=0
      endelse

      xtlim = (3.0*ainfo.etrack + xtdiam/2.0)
      idealspa = xtdiam*0.40
      idealsta = ceil(2*xtlim/idealspa)

      if strmid(objectid,0,1) eq 'A' then begin
         orbitsource='astorb'
      endif else begin
         orbitsource=bspfile
      endelse

      occexpt,'C11',gmag,gtarg,ainfo.speed,xtdiam,exptime,snr
      tscale = ainfo.speed*exptime

      cmd=['update targeted set', $
         'jd='+strn(jd,format='(f20.8)')+c, $
         'geotime='+quote(jds)+c, $
         'gtarg='+strn(gtarg,format='(f20.3)')+c, $
         'earth='+strn(ainfo.earth,format='(f20.10)')+c, $
         'selong='+strn(ainfo.selong,format='(f20.2)')+c, $
         'melong='+strn(ainfo.melong,format='(f20.2)')+c, $
         'mphase='+strn(ainfo.mphase,format='(f20.2)')+c, $
         'kmscale='+strn(ainfo.kmscale,format='(f20.3)')+c, $
         'speed='+strn(ainfo.speed,format='(f20.6)')+c, $
         'rate='+strn(ainfo.rate,format='(f20.6)')+c, $
         'angsha='+strn(ainfo.angsha,format='(f20.6)')+c, $
         'etime='+strn(ainfo.etime,format='(f20.6)')+c, $
         'etrack='+strn(ainfo.etrack,format='(f20.6)')+c, $
         'idealsta='+strn(idealsta)+c, $
         'idealspa='+strn(idealspa,format='(f10.1)')+c, $
         'nstations='+strn(countpick)+c]
      if recon then begin
         cmd=[cmd, $
              'stlist='+quote(strjoin(netinfo.obsname[sidx[zpick]],c))+c, $
              'fprob1='+strn(results.prob1,format='(f20.3)')+c, $
              'fprob2='+strn(results.prob2,format='(f20.3)')+c]
      endif else begin
         cmd=[cmd, $
              'stlist='+quote('')+c, $
              'fprob1=0'+c, $
              'fprob2=0'+c]
      endelse
      cmd=[cmd, $
         'snr='+strn(snr,format='(f10.1)')+c, $
         'exptime='+strn(exptime,format='(f10.3)')+c, $
         'tscale='+strn(tscale,format='(f10.1)')+c, $
         'pred='+quote(jdnows)+c, $
         'orbit='+quote(orbitsource), $
         'where idx='+strn(idx)+';']
      logstr=[logstr,cmd]
      mysqlcmd,dbpipe,cmd
   endif

   error=0

bailout:
   if ephwasopen eq 0 then begin
      free_lun,ephpipe
   endif

   if dbwasopen eq 0 then begin
      free_lun,dbpipe
   endif

   if not silent then begin
      for i=0,n_elements(logstr)-1 do print,logstr[i]
   endif

end
