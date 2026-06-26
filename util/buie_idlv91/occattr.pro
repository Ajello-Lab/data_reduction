;+
; NAME:
;  occattr
; PURPOSE:   (one line only)
;  Determine occulation attributes
; DESCRIPTION:
; CATEGORY:
;  Occultations
; CALLING SEQUENCE:
;  occattr,app,roi,attr
; xxx    ,dir,roiconfig,sunobs,starobs,/force,/verbose
; INPUTS:
;  app - anonymous structure with the following tags:
;         objid:     Occulting body id (e.g., '617')
;         datetime:  Occultation date/time in format 'YYYYMMDDHHMMSS'
;  roi      - Structure returned by rdoccroi.pro
; OPTIONAL INPUT PARAMETERS:
;  DIR      - Directory for writing output files (default='')
;  roiconfig - ROI configuration file (default='')
;  sunobs  - Maximum Sun altitude in observable zone (degrees; default=-12)
;  starobs - Minimum star altitude in observable zone (degrees; default=20)
; KEYWORD INPUT PARAMETERS:
;  /FORCE   - Force creation of centerline file if it already exists
;  /VERBOSE - Print extra output to the screen
;  SUPP - optional anonymous structure that contains extra information about
;           the object.  This can be used either to override the standard
;           information, or to provide missing information in cases where
;           a spice kernel is being used.  Tags required are:
;             name: descriptive name the way you want to see it
;             diameter: size of object in km
;             hv: absolute magnitude of object
;             albedo: albedo for surface
;             etrack: 1-sigma uncertainty in cross-track position [km]
;             etime: 1-sigma uncertainty in the down-track position [seconds]
; OUTPUTS:
;  attr - Anonymous structure containing occultation attributes
; KEYWORD OUTPUT PARAMETERS:
; CONFIGURATION:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  Written by Brian Keeney, Southwest Research Institute, 2022-02-24
;  2023/04/04, MWB, structural reworking and simplifications
;  2023/07/25, MWB, added SUPP keyword
;-
pro occattr,app,roi,attr,DIR=in_dir, $
             FORCE=force,VERBOSE=verbose,SUNOBS=sunobs,STAROBS=starobs,SUPP=supp

   attr={error:1}

   self='occattr: '
   if badpar(app,8,1,caller=self+'(app) ') then return
   if badpar(roi,8,1,caller=self+'(roi) ') then return
   if badpar(in_dir,[0,7],0,caller=self+'(DIR) ',default='') then return

   if badpar(sunobs,[0,2,3,4,5],0,caller=self+'(SUNOBS) ', $
                                  default=-12.) then return
   if badpar(starobs,[0,2,3,4,5],0,caller=self+'(STAROBS) ', $
                                   default=20.) then return
   if badpar(force,[0,1,2,3],0,caller=self+'(force) ',default=0) then return
   if badpar(verbose,[0,1,2,3],0,caller=self+'(verbose) ',default=0) then return
   if badpar(supp,[0,2,3,8],[0,1],caller=self+'(SUPP) ', $
                                  type=supptype) then return

   if verbose then begin
      print,'Object id ',app.objid,'  DateTime ',app.datetime
      print,strn(roi.nregions),' regions to evaluate'
      if supptype eq 8 then print,'Supplemental object information supplied'
   endif

   ; Check for trailing slash in input directory
   if in_dir ne '' then dir=addslash(in_dir) else dir=in_dir
   tag = app.objid+'_'+app.datetime
  
   if verbose then begin
      print, 'dir=',dir
      print, 'tag=',tag
   endif

   starra  = raparse(app.rastr)/!dtor    ; degrees
   stardec = decparse(app.decstr)/!dtor  ; degrees

   if verbose then print,'calling appuldis'
   if strmid(app.objid,0,1) eq 'P' then begin
      thisobjid=app.objid
   endif else begin
      thisobjid='A'+app.objid
   endelse

   if supptype eq 8 then begin
       thisobjname=supp.name
   endif else begin
       thisobjname=app.objname
   endelse

   appuldis,thisobjid,'500',app.jdgeo,starra*!dtor,stardec*!dtor,$
              jdmin,sep,starerr=0.001,info=ginfo,verbose=verbose, $
              objname=thisobjname,supp=supp
   if verbose then print,'returned from appuldis'
   if ginfo.error then begin
      if verbose then begin
         print,'appuldis quit early'
      endif
      return
   endif

   diam = ginfo.diam1
   speed = ginfo.speed
   kmscale = ginfo.kmscale
   gstar = app.gmag - 2.5d * alog10((diam/40d)/(speed/20d))

   ; Calculate moon circumstances
   moonpos,app.jdgeo,moonra,moondec
   mphase,app.jdgeo,moonphase
   gcirc,2,starra,stardec,moonra,moondec,moonelon
   moonelon /= 3600d   ; degrees
  
   if verbose then begin
      print, f="('jdgeo=',f13.5)",app.jdgeo
      print, f="('diam=',f5.1)",diam
      print, f="('speed=',f5.2)",speed
      print, f="('kmscale=',f6.1)",kmscale
      print, f="('gmag=',f5.2)",app.gmag
      print, f="('gstar=',f5.2)",gstar     
      print, f="('moonelon=',f5.1)",moonelon
      print, f="('moonphase=',f5.3)",moonphase
   endif
  
   ; Create centerline file, if necessary
   cline = dir+tag+'_center.dat'

   if not exists(dir+tag+'.out') or force then begin
      if verbose then begin
         print, 'Creating '+cline+'...'
      endif

      jdstr,app.jdgeo,0,dategeo
      mkocceph,thisobjname,thisobjid,dategeo, $
         app.starid,app.rastr,app.decstr,version,verbose=verbose,supp=supp

      if verbose then print,'Writing mkcenter script'
      openw, lun, 'mkcenter',/get_lun
      printf, lun, '#!/bin/csh'
      printf, lun
      printf, lun, 'mv '+version+'/'+version+'.in .'
      printf, lun, 'mv '+version+'/'+version+'.info '+dir+tag+'.info'
      printf, lun, 'rmdir '+version
      printf, lun
      printf, lun, 'ocmapex << EOI'
      printf, lun, 'o'
      printf, lun, thisobjid
      printf, lun, version+'.in'
      printf, lun, 'EOI'
      printf, lun
      printf, lun, 'rm tempor.tmp'
      printf, lun, 'mv '+version+'.in '+dir+tag+'.in'
      printf, lun, 'mv ocmap.out '+dir+tag+'.out'
      printf, lun, 'if (-f center.out) then'
      printf, lun, '  rm north.out'
      printf, lun, '  rm south.out'
      printf, lun, '  mv center.out '+dir+tag+'_center.dat'
      printf, lun, 'endif'
      free_lun, lun

      file_chmod, 'mkcenter', /a_execute
      if verbose then begin
         print,'mkcenter script file'
         spawn,'cat mkcenter'
         print,'====='
      endif
      spawn, 'mkcenter', result
      file_delete, 'mkcenter'
   endif

   if verbose then print,'Looking for output file ',cline
  
   ; Read in centerline file
   if not exists(cline) then begin
      if verbose then print,cline,' not found, no prediction'
      ; Larry's ocmapex code won't create centerline files if the Sun
      ; is up everywhere along the path...
      attr = {error:1,tag:'empty'}
      return
   endif

   readcol, cline, f='d,d,d,f', cjd, clat, clon, calt

   ; NB: Larry's centerline file uses W longitude, but ROIs use E longitude!!
   clon *= -1

   ; Determine whether centerline crosses any ROIs
   rinfo=roi.info
   regions = ''

   cobs = intarr(n_elements(cjd))
  
   for i=0,roi.nregions-1 do begin  
      region=roi.regions[i]
      getvalue,rinfo,region,'lat',latstr
      getvalue,rinfo,region,'lon',lonstr

      plat = double(strsplit(latstr,',',/extract))
      plon = double(strsplit(lonstr,',',/extract))

      inside = interior(plon,plat,clon,clat)
      in = where(inside eq 1)

      if (in[0] gt -1) then begin
         if verbose then $
            print, 'Centerline crosses ',region
        
         ; Calculate Sun and star circumstances
         sunpos,cjd[in],sunra,sundec
         ct2lst,lst,clon[in],dummy,cjd[in]

         hadec2altaz,15*lst-sunra,sundec,clat[in],sunalt,sunaz
         hadec2altaz,15*lst-starra,stardec,clat[in],staralt,staraz

         ; Determine observability
         obs = where((sunalt le sunobs) and (staralt ge starobs))

         if (obs[0] ne -1) then begin
            if verbose then $
               print, 'Target is observable from ',region

            cobs[in[obs]] = 1
            regions = (regions[0] eq '') ? region : [regions,region]
         endif
      endif
   endfor

   regions=strjoin(regions,',')
   attr = {error:0, tag:tag,objid:app.objid,objname:thisobjname, $
           jdgeo:app.jdgeo,diam:diam,speed:speed,kmscale:kmscale, $
           starid:app.starid,starra:starra,stardec:stardec,gmag:app.gmag, $
           gstar:gstar,moonelon:moonelon,moonphase:moonphase,regions:regions, $
           etrack:ginfo.etrack,etime:ginfo.etime}

end
