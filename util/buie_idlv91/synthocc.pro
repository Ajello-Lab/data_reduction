;+
; NAME:
;  synthocc
; PURPOSE:   (one line only)
;  Generate simulated occultation data
; DESCRIPTION:
; CATEGORY:
;  Occultations
; CALLING SEQUENCE:
;  synthocc,shape
; INPUTS:
;  shape - input shape to use for the object.  There are two options supported,
;             either an ellipse or a numerical hull.
;          ellipse - five element vector (float or double)
;             0 - semi-major axis of ellipse
;             1 - semi-minor axis of ellipse
;             2 - x position of center
;             3 - y position of center
;             4 - position angle of ellipse (radians)
;          hull - string with the file name to read, this file should have
;                  a list of xi,eta points that define the hull
;          all spatial values should be in kilometers
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;  FNCONFIG - name of configuration file (overrides xtrack.in),
;              the default is config.ini but if the configuration file
;              is not found then operation reverts to using xtrack.in
;              The minimal content of the config file must have:
;  FNSITES - name of the file with site location information (default=sites.dat)
;  FNEVENTS - name of occultation events file (default=events.dat)
;  FNXTRACK - Name of the file with the crosstrack position information,
;              default='crosstrack.dat'
;  XYFLAG   - Flag, if set reverses the first axis of points in a hull shape
;               file.  The values plotted are always sky normal xi/eta
;               coordinates which are in a left-handed coordinate system.
;               This flag lets you use a right-handed set of x/y coordinates
;               in the file and they will be inverted left-to-right.
; OUTPUTS:
; KEYWORD OUTPUT PARAMETERS:
; CONFIGURATION:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  Written by Marc W. Buie, Southwest Research Institute, 2023/09/14
;-
pro synthocc,in_shape,FNCONFIG=fnconfig,FNXTRACK=fnxtrack, $
       FNSITES=fnsites,FNEVENTS=fnevents,XYFLAG=xyflag

   self='synthocc: '
   if badpar(in_shape,[4,5,7],[0,1],caller=self+'(shape) ', $
                                 type=shapetype) then return
   if badpar(fnconfig,[0,7],0,caller=self+'(FNCONFIG) ', $
                             default='config.ini') then return
   if badpar(fnsites,[0,7],0,caller=self+'(FNSITES) ', $
                                  default='sites.dat') then return
   if badpar(fnevents,[0,7],0,caller=self+'(FNEVENTS) ', $
                                  default='events.dat') then return
   if badpar(fnxtrack,[0,7],0,caller=self+'(FNXTRACK) ', $
                                  default='crosstrack.dat') then return
   if badpar(xyflag,[0,1,2,3],0,caller=self+'(XYFLAG) ', $
                                  default=0) then return

   if shapetype eq 7 then begin
      if nofile(in_shape,'shape file') then return
      readcol,in_shape,xi,eta,format='f,f'
   endif else begin
      ellipse2hull,in_shape,xi,eta,npts=301
   endelse
   if xyflag then xi = -1.0*xi

   shape = [[xi],[eta]]

   if nofile(fnconfig,'Configuration file') then return
   if nofile(fnsites,'Site position file') then return
   if nofile(fnxtrack,'Cross-track information file') then return

   if exists(fnevents) then file_delete,fnevents

   print,'Load configuration from ',fnconfig
   loadini,info,file=fnconfig
   getvalue,info,'[global]','geomid',geomid
   jdgeomid=jdparse(geomid)
   getvalue,info,'[global]','ora',ora
   rastar=raparse(ora)
   getvalue,info,'[global]','odec',odec
   decstar=decparse(odec)
   getvalue,info,'[global]','objectid',obj
   getvalue,info,'global','nephpts',ncp,default=382,type=3
   getvalue,info,'global','ephdt',dte,default=0.1d0,type=5

   print,'Load cross track information from ',fnxtrack
   loadocc,oinfo,fnxtrack=fnxtrack
   appuldis,obj,'500',jdgeomid,rastar,decstar,jdmin,sep,pa,info=info
   kmscale=info.kmscale

   jdc=dblarr(ncp,oinfo.nsites)
   akscale=dblarr(ncp,oinfo.nsites)
   ara=dblarr(ncp,oinfo.nsites)
   adec=dblarr(ncp,oinfo.nsites)
   xic=dblarr(ncp,oinfo.nsites)
   etac=dblarr(ncp,oinfo.nsites)

   spawn,'geteph',unit=pipe

   for i=0,oinfo.nsites-1 do begin
      ; geometry of ephemeris
      jdc[*,i]=oinfo.jdref[i]+dte*(dindgen(ncp)-ncp/2)/86400.0d0
      obsinfo,'G**',obsinfo,oinfo.lat[i], $
                            oinfo.lon[i], $
                            oinfo.alt[i], $
                            oinfo.team[i],/degrees
      ephem,trimrank(jdc[*,i]),obsinfo,72,obj,eph,pipe=pipe
      ssgeom,eph,sun,earth,phang,elong
      kscale = earth * 1.496e8 * !pi / 180.0 / 3600.0
      akscale[*,i] = kscale
      ara[*,i] = trimrank(eph[0,*])
      adec[*,i] = trimrank(eph[1,*])
      astrd2sn,trimrank(eph[0,*]),trimrank(eph[1,*]), $
         rastar,decstar,xi0,eta0,/arcsec
      xic[*,i]=xi0*kscale
      etac[*,i]=eta0*kscale
   endfor

   nchord=0
   openw,lun,fnevents,/get_lun
   for i=0,oinfo.nsites-1 do begin

      obsinfo,'G**',obsinfo,oinfo.lat[i], $
                            oinfo.lon[i], $
                            oinfo.alt[i], $
                            oinfo.team[i],/degrees

;      if oinfo.team[i] eq 'PM6' then debug=1 else debug=0
      drtimes,oinfo.jdref[i],rastar,decstar,shape,obsinfo,obj,jd_d,jd_r, $
         debug=debug

      if jd_d ne 0 then jdstr,jd_d,3,jd_ds
      if jd_r ne 0 then jdstr,jd_r,3,jd_rs

      if jd_d ne 0 then nchord++

      if jd_d eq 0 then begin
         print,oinfo.team[i], $
            obsinfo.lat_d,obsinfo.lon_d,obsinfo.alt, $
            "      x        x            x        0.    0.", $
            format='(a-10,1x,"y",1x,f+10.6,1x,f+11.6,1x,f6.1,1x,a)'
      endif else begin
         fmt='(a-10,1x,"y",1x,f+10.6,1x,f+11.6,1x,f6.1,1x,a,1x,a,2(1x,f5.3))'
         print,oinfo.team[i], $
            obsinfo.lat_d,obsinfo.lon_d,obsinfo.alt, $
            jd_ds,strmid(jd_rs,11),0.001,0.001,format=fmt
      endelse
      if jd_d ne 0 then begin
         printf,lun,oinfo.team[i],' 0 D 1 1 ',jd_ds,' 0.010'
         printf,lun,oinfo.team[i],' 0 R 1 1 ',jd_rs,' 0.010'
      endif

   endfor
   free_lun,lun,pipe

;   print,'test plot'
;   plot,xic,etac,/iso,psym=3
;   oplot,xi,eta,color=cpalette(1)

end
