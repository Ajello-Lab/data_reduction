;+
; NAME:
;  mkoccconfig
; PURPOSE:   (one line only)
;  Build an initial config.ini for PSF reduction of occultation data
; DESCRIPTION:
; CATEGORY:
;  Occultations
; CALLING SEQUENCE:
;  mkoccconfig,evidx
; INPUTS:
;  evidx - Set this to the RECON event index.  If this isn't a RECON
;             event, set this to zero.  In this case, there must already be a
;             config file that has geomid and objectid
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;  HSIZE - half-size of search region to find the star, default=10 [arcsec]
; OUTPUTS:
;  for RECON events, a config.ini file is written, nor other events, a newconfig.ini
;    file with information to be (manually) added to config.ini is generated
; KEYWORD OUTPUT PARAMETERS:
; CONFIGURATION:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  2021/01/27, Written by Marc W. Buie, Southwest Research Institute
;  2021/03/18, MWB, added non-RECON event support
;  2022/10/31, MWB, added interactive creation of initial config.ini file
;                     for non-RECON events
;-
pro mkoccconfig,evidx,HSIZE=hsize

   self='mkoccconfig: '
   if badpar(evidx,[2,3],0,caller=self+'(evidx) ') then return
   if badpar(hsize,[0,2,3],0,caller=self+'(HSIZE) ',default=10) then return

   if evidx ne 0 then begin
      openmysql,dblun,'recon'
      cmd=['select pidx,utdate,utgeomid,objid', $
           'from campaign', $
           'where evidx='+strn(evidx)+';']
      mysqlquery,dblun,cmd,idx,dates,times,objid,format='l,a,a,a',ngood=ncheck
      if ncheck ne 1 then begin
         print,cmd
         print,'event index ',strn(evidx),' not found'
         free_lun,dblun
         return
      endif
      cmd=['select starid,ra,decl', $
           'from appulse', $
           'where idx='+strn(idx[0])+';']
      mysqlquery,dblun,cmd,starid,ra,dec,format='a,d,d',ngood=ncheck
      if ncheck ne 1 then begin
         print,cmd
         print,'appulse index ',strn(idx[0]),' not found'
         free_lun,dblun
         return
      endif
      cmd=['select jd,etime,etrack', $
           'from event', $
           'where idx='+strn(idx[0])+';']
      mysqlquery,dblun,cmd,jd,etime,etrack,format='d,d,d',ngood=ncheck
      if ncheck ne 1 then begin
         print,cmd
         print,'event index ',strn(idx[0]),' not found'
         free_lun,dblun
         return
      endif
      free_lun,dblun
      objid='A'+objid

      jd2year,jd,epoch

      if not exists('star.cat.gcat') then $
         refnet,ra,dec,hsize,hsize,20,20,'star.cat',gaia=epoch,/noconvert,/debug
      
      paracorr,jd,info

      if info.nstars ne 1 then begin
         print,'ERROR!  There are ',strn(info.nstars),' in star.cat.gcat'
         print,' There should only be one.'
         return
      endif

      event = 'RECON'+strn(evidx)

   endif else begin
      if nofile('config.ini','Configuration file') then begin
         ans=''
         read,prompt='Event name (def=quit) ',ans
         event=strtrim(ans,2)
         if event eq '' then return
         read,prompt='Geocentric UTC mid-time ',ans
         geomid=strtrim(ans,2)
         if geomid eq '' then return
         read,prompt='Object id (geteph code) ',ans
         objid=strtrim(ans,2)
         if objid eq '' then return
         openw,lun,'config.ini',/get_lun
         printf,lun,'[id]'
         printf,lun,'type = occ'
         printf,lun,'version = v1.0'
         printf,lun,''
         printf,lun,'[global]'
         printf,lun,'event = ',event
         printf,lun,'geomid = ',geomid
         printf,lun,'objectid = ',objid
         free_lun,lun
      endif
      loadini,cinfo
      getvalue,cinfo,'global','event',event
      getvalue,cinfo,'global','geomid',geomid
      getvalue,cinfo,'global','objectid',objid
      jd=jdparse(geomid)
      ephem,jd,'500',2,objid,eph,/debug
      objra=trimrank(eph[0])
      objdec=trimrank(eph[1])
      jd2year,jd,epoch
      if not exists('star.cat.gcat') then $
         refnet,objra,objdec,hsize,hsize,20,20,'star.cat',gaia=epoch,/noconvert,/debug
      paracorr,jd,info
      if info.nstars ne 1 then begin
         print,'ERROR!  There are ',strn(info.nstars),' in star.cat.gcat'
         print,' There can be only one.'
         return
      endif
      appuldis,objid,'500',jd,info.ora[0],info.odec[0],jdmin,sep,info=ainfo,/verbose
      jd=jdmin
      starid=trimrank(info.starid)
      etime = 0.1  ; dummy value
      etrack = 100 ; dummy value
   endelse

   jdstr,jd,1,jds,timesep='-'
   dates=strmid(jds,0,10)
;   dates=repchar(dates,'/','-')

   rastr,info.ora,6,oras
   decstr,info.odec,5,odecs
   rastr,info.cra,6,cras
   decstr,info.cdec,5,cdecs

   if strmid(info.starid,0,2) eq 'GA' then $
      catalog='DR2' $
   else $
      catalog='XXX'

   openw,lun,'newconfig.ini',/get_lun
   printf,lun,'[id]'
   printf,lun,'type = occ'
   printf,lun,'version = v1.0'
   printf,lun,''
   printf,lun,'[global]'
   printf,lun,'dir = ','RECON_QHY'
   printf,lun,'date = ',dates
   printf,lun,'event = '+event
   printf,lun,'geomid = ',jds
   printf,lun,'objectid = ',objid
   printf,lun,'ora = ',oras
   printf,lun,'odec = ',odecs
   printf,lun,'oepoch = ',strn(info.oepoch)
   printf,lun,'catalog = ',catalog
   printf,lun,'orsig = ',strn(info.orasig)
   printf,lun,'odsig = ',strn(info.odecsig)
   printf,lun,'cra = ',cras
   printf,lun,'cdec = ',cdecs
   printf,lun,'cepoch = ',strn(info.cepoch)
   printf,lun,'parallax = ',strn(info.par,format='(f10.3)')
   printf,lun,'pmra = ',strn(info.rapm,format='(f10.3)')
   printf,lun,'pmdec = ',strn(info.decpm,format='(f10.3)')
   printf,lun,'crsig = ',strn(info.raerr,format='(f10.3)')
   printf,lun,'cdsig = ',strn(info.decerr,format='(f10.3)')
   printf,lun,'parasig = ',strn(info.parerr,format='(f10.3)')
   printf,lun,'pmrsig = ',strn(info.rapmerr,format='(f10.3)')
   printf,lun,'pmdsig = ',strn(info.decpmerr,format='(f10.3)')
   printf,lun,'gmag = ',strn(info.gmag,format='(f10.4)')
   printf,lun,'gmagerr = ',strn(info.gmagerr,format='(f10.4)')
   printf,lun,'bmag = ',strn(info.bmag,format='(f10.4)')
   printf,lun,'bmagerr = ',strn(info.bmagerr,format='(f10.4)')
   printf,lun,'rmag = ',strn(info.rmag,format='(f10.4)')
   printf,lun,'rmagerr = ',strn(info.rmagerr,format='(f10.4)')
   printf,lun,'catname = ',starid
   printf,lun,'maxphotsig = 60000'
   printf,lun,'snrlimit = 5.0'
   printf,lun,'minfwhm = 2.0'
   printf,lun,'fwhmguess = 10'
   printf,lun,'hwfov = 1800.0'
   printf,lun,'maglim = 17.0'
   printf,lun,'tsig = '+strn(etime,format='(f12.1)')
   printf,lun,'xsig = '+strn(etrack,format='(f12.1)')
   printf,lun,'xrange = -120 120'
   printf,lun,'baseline = 0.0'
   printf,lun,'mnpts = 4000'
   printf,lun,'modelt0 = -20.0'
   printf,lun,'modelt1 =  20.0'
   printf,lun,'mark = -1 -1'
   printf,lun,''
   printf,lun,'[teamname]'
   printf,lun,'dirtime = '
   printf,lun,''
   printf,lun,'[ddir]'
   printf,lun,'d00 = /net/mandor/raid/buie/'
   free_lun,lun

end
