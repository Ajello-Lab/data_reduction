;+
; NAME:
;  occsumtab
; PURPOSE:   (one line only)
;  Create table of occultation attributes
; DESCRIPTION:
; CATEGORY:
;  Occultations
; CALLING SEQUENCE:
;  occsumtab,set
; INPUTS:
;  set - Occultation set(s)
; KEYWORD INPUT PARAMETERS:
; OUTPUTS:
;  For each set in the input list, a file called [set].tab is created.
; KEYWORD OUTPUT PARAMETERS:
; CONFIGURATION:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  Written by Brian Keeney, Southwest Research Institute, 2022-02-24
;  2023/04/04, MWB, generalization and some restructuring
;  2024/11/13, MWB, removed geominsep option from .ini file, this is now
;                     dynamically calculated for each appulse
;-
pro occsumtab,set

   self='occsumtab: '
   if badpar(set,7,[0,1],caller=self+'(set) ') then return

   ; Header for table files
   header='event                 jdgeo           ra              dec        gmag  speed   g*  ' + $
          'moonelon moonphase etrack etime  regions'
   fmt='(a-20,1x,f13.5,2(1x,a),1x,f5.2,1x,f5.1,1x,f5.2,1x,f6.1,1x,f8.2,1x,f7.1,1x,f5.1,4x,a)'

   for i=0,n_elements(set)-1 do begin

      fnini = set[i]+'.ini'

      if nofile(fnini,'Configuration') then return
      loadini,info,file=fnini
     
      ; Look up other config files
      getvalue,info,'global','roiconfig',roiconfig

      rdoccroi,fnconfig=roiconfig,roi
      if roi.error then begin
         print,'Unable to read the ROI configuration'
         return
      endif

      getvalue,info,'global','fnlist',fnlist
      if nofile(fnlist,'Object list') then return
;      getvalue,info,'global','geominsep',seplimit,type=4
      getvalue,info,'global','minsel',minsel,type=4

      ; master list with the full name
      print,'read ',fnlist
      rdmatch,fnlist,objids,objnames
      print,objids
      print,objnames
      nobjs = n_elements(objids)
      mobjid=strarr(nobjs)
      for j=0,nobjs-1 do begin
         if strmid(objids[j],0,1) eq 'P' then begin
            mobjid[j]=objids[j]
         endif else begin
            mobjid[j]=strmid(objids[j],1) ; strip off leading 'A'
         endelse
      endfor
      print,mobjid
      nobjs = n_elements(mobjid)
      print,strn(nobjs),' objects to process'

      ; Load all of the appulse data
      objid=[]
      starid=[]
      jd=[]
      ras=[]
      decs=[]
      gmag=[]
      geominsep=[]
      melong=[]
      rearth=[]
      for j=0,nobjs-1 do begin
         fnout = set[i]+'/'+mobjid[j]+'.out'
         readcol,fnout,starid0,geominsep0,et0,ras0,decs0,gmag0,melong0, $
            format='a,f,d,x,x,a,a,f,x,f',count=nread
         print,'File ',fnout,' has ',strn(nread),' entries'
         jd0=et0-etut(et0)/86400.0d0
         objid=[objid,replicate(mobjid[j],nread)]
         starid=[starid,starid0]
         geominsep=[geominsep,geominsep0]
         jd=[jd,jd0]
         ras=[ras,ras0]
         decs=[decs,decs0]
         gmag=[gmag,gmag0]
         melong=[melong,melong0]
         ephem,jd0,'500',22,objids[i],eph
         ssgeom,eph,sun,earth,phang,elong,kscale
         rearth0 = 6378./kscale
         rearth=[rearth,rearth0]
      endfor

      ra=raparse(ras)
      dec=raparse(decs)
      sunpos,jd,sunra,sundec,/radian
      selong=angsep(ra,dec,sunra,sundec)*!radeg

      z=where(melong gt 30 and selong gt minsel and geominsep lt rearth,count)
      if count eq 0 then begin
         print,'no valid appulses found'
         return
      endif
      objid=objid[z]
      starid=starid[z]
      geominsep=geominsep[z]
      jd=jd[z]
      ra=ra[z]
      dec=dec[z]
      ras=ras[z]
      decs=decs[z]
      gmag=gmag[z]
      melong=melong[z]
      selong=selong[z]
      napp=count

      jdstr,jd,300,datetime
;      ra=raparse(ras)
;      dec=decparse(decs)
;      napp=n_elements(jd)
      tag=strarr(napp)
      speed=fltarr(napp)
      gstar=fltarr(napp)
;      moonelon=fltarr(napp)
      moonphase=fltarr(napp)
      etrack=fltarr(napp)
      etime=fltarr(napp)
      regions=strarr(napp)
      fntab=set[i]+'.tab'

      nevents=0
      lastobjid = ''

      for j=0,napp-1 do begin
print,j,' start ',objid[j]
         if objid[j] ne lastobjid and strmid(objid[j],0,1) eq 'P' then begin
;print,j,' ',objid[j],' ',lastobjid
            getvalue,info,objid[j],'name',name,type=7,default=''
;print,name
;print,'Check for supplemental information on ',objid[j],' ',name
            if name ne '' then begin
               getvalue,info,objid[j],'diameter',diameter,type=4,default=10.0
               getvalue,info,objid[j],'albedo',albedo,type=4,default=0.05
               getvalue,info,objid[j],'etrack',setrack,type=4,default=10.0
               getvalue,info,objid[j],'etime',setime,type=4,default=1.0
               hv=diamptoh(diameter,albedo)
               supp={name:name, diameter:diameter, hv:hv, albedo:albedo, $
                     etrack: setrack, etime: setime}
               etrack[j] = setrack
               etime[j]  = setime
            endif else begin
               supp=0
            endelse
         endif
         lastobjid=objid[j]
;print,'xxx ',objid[j],' ',lastobjid


         app={objid:objid[j],datetime:datetime[j],starid:starid[j], $
              rastr:ras[j],decstr:decs[j],gmag:gmag[j],jdgeo:jd[j], $
              objname:objnames[i]}
         occattr,app,roi,attr,dir=set[i],supp=supp ; ,/verbose
         if attr.error then continue
         if attr.tag ne 'empty' then begin
            tag[j] = attr.tag
            speed[j] = attr.speed
            gstar[j] = attr.gstar
            moonphase[j] = attr.moonphase
            regions[j] = attr.regions
            etrack[j] = attr.etrack
            etime[j] = attr.etime

            print,j,napp-j,' ',tag[j],' ',regions[j]
            if regions[j] ne '' then nevents++
;if nevents eq 15 then break
         endif
;help,attr,/st
;return
      endfor
      print,strn(nevents),' viable events in all regions'

      idx=sort(jd)
      openw,lun,fntab,/get_lun
      printf,lun,header
      for j=0,napp-1 do begin
         if regions[idx[j]] eq '' then continue
         line = string(f=fmt,$
                       tag[idx[j]],jd[idx[j]],$
                       ras[idx[j]],decs[idx[j]],gmag[idx[j]], $
                       speed[idx[j]],gstar[idx[j]],$
                       melong[idx[j]],moonphase[idx[j]], $
                       etrack[idx[j]],etime[idx[j]], $
                       regions[idx[j]])
         printf,lun,line
      endfor
      free_lun,lun

   endfor

end
