;+
; NAME:
;  occprof
; PURPOSE:   (one line only)
;  Plot an occultation profile based on a set of chords
; DESCRIPTION:
; CATEGORY:
;  Occultation
; CALLING SEQUENCE:
;  occprof
; INPUTS:
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;  FNCONFIG - name of configuration file (overrides xtrack.in),
;              the default is config.ini but if the configuration file
;              is not found then operation reverts to using xtrack.in
;              The minimal content of the config file must have:
;   [global]
;     event    - Name of the event
;     objectid - string with the object id (see ephem.pro)
;     ora      - Apparent position of star at time of occultation
;     odec     - Apparent position of star at time of occultation
;     geomid   - UT date and time of geocentric closest approach
;     tsig     - 1-sigma uncertainty in event time (down-track) in seconds
;     xsig     - 1-sigma cross-track uncertainty in km
;   optional values:
;     nephpts  - Number of ephemeris points to calculate for tracks,
;                    default is 382.
;     ephdt    - Time spacing in ephemeris points [seconds], default 0.1
;
;  FNSITES - name of the file with site location information (default=sites.dat)
;  FNEVENTS - name of occultation events file (default=events.dat)
;  FNXTRACK - Name of the file with the crosstrack position information,
;              default='crosstrack.dat'
;  FNXE     - Name of the file to save the sky-plane points to.
;              default=event+'_xe.dat', the direct graphics will be saved
;              to the same root name with a suffix of .png.
;  FNRESULTS - Name of the file to save the fitting results to.
;               default=event+'.results'
;  EVENT   - Name of the event.  This is required if using xtrack.in
;             but ignored if using config.ini
;  HSIZE   - half-size of the physical dimension of plot in km.  Provide
;              a two element vector [x_hwidth,y_hwidth].  Default=[30,20]
;
;  SHOWHULL- Flag, if set attempts to draw a smooth outline passing through
;              all of the data (fitted or not).
;  SHOWEXTRA - Flag, if set, shows the extra hull points
;  HULLPTS - extra points to add to hull (Nx2 array).
;  SAVEHULL - File to save the final hull to (default = hull.dat)
;  ARES - angular resolution smoothing width for the hull (degrees),
;            default=20 (mkhull2)
; 
;  OFFSET  - amount, in km, to shift the center of the occultation profile
;               from its natural center.  Provide a two element vector
;               [xoffset,yoffset].  Default is [0,0].  
;  REFNAME - Name of site that is to serve as the reference chord for
;              extracting astrometry.  If not provided the astrometry
;              step will be skipped.
;  PUBINFO - anonymous structure with information needed for building a
;              publiation quality graphic.  If this structure is provided,
;              a mode is enabled that will use the function graphics calls
;              and the output is intended to be used for publication graphics.
;              Without this structure, direct graphics are used.  The output
;              should look the same regardless of the mode used.  Some control
;              options on the output products are provided by this structure.
;              The tags that will be used are:
;               fnlist - string or string array of names to save the
;                          graphic to at the end.  Using the array option
;                          here let's you save a version of the graphic
;                          in multiple formats from the same run.  The file
;                          suffix is used to dictate the output file type.
;               loc    - This is passed to the plot() POSITION keyword
;                         if not provided in the structure a default value of
;                           loc=[0.1,0.9,0.1,0.9] is used.
;               dim    - This is passed to the plot() DIMENSION keyword
;                         if not provided in the structure a default value of
;                           dim=[500,500] is used.
;  POSTFUNC - This is the name of a procedure to call at the end of the
;               built in plotting.  It allows you to further modify or add
;               to the plot for non-standard cases.  This procedure must
;               support one argument and a keyword, PUB.  The PUB keyword
;               is a flag that when set dictates the use of the plot functions
;               for use in publications.  This keyword is set in regard to
;               the use of the PUBINFO keyword.  If /PUB is set, the argument
;               to the external procudure will be the id of the root level
;               graphic in the plot.  You don't have to use it but it's there
;               if you need it.  If PUB=0, then the argument will be set to
;               0 and would generally be ignored by your own routine.
;               to the plot for non-standard cases.  This procedure must
;               support one argument and a keyword, PUB.
;  WEIGHTS - weighting factors for each constraint.  The default is equal
;               weights for all points.  This optional input should be
;               a 2 x N array, where N is equal to the number of stations
;               (not chords).  Elements [0,*] are for the disappearance, and
;               [1,*] are for the disapperance and should appear in the
;               same order as found in the events.dat file.  You can choose
;               automatic weighting as well, see AUTO_WEIGHT.
;  AUTO_WEIGHT - Flag, if set will compute and use weights based on the
;                  timing uncertainties found in the events.dat file.
;                  If this is set, any values provided via the WEIGHTS
;                  input keyword are ignored.  The auto weights are set
;                  to 1.0/(err > 0.001)
;  START_PARAMS - optional starting parameters for the ellipse fitting.
;                This is a five element vector:
;                  0 = semi-major axis of ellipse
;                  1 = semi-minor axis of ellipse
;                  2 = ellipse center - x value
;                  3 = ellipse center - y value
;                  4 = PA of ellipse (radians)
;  PARINFO - for full documentation consult the Markwardt routine, MPFIT.PRO
;               there are a lot of detailed and potentially complex interactions
;               enabled by this optional routine.  The most common use for this
;               is to control which parameters are fitted.  The default is to
;               fit them all.  This is an array of anonymous structures, one
;               for each parameter, in the case of this routine this structure
;               should have 5 elements in the array.  Each array can have
;               tags from the set of supported tags, none are required.  These
;               tags are the ones most likely to be of use to this routine.
;                 .value - starting parameter value (semi-redundant with
;                            START_PARMS, see MPFIT.PRO for more details).
;                 .fixed - flag, if set, parameter is used, not fitted.
;                 .limited - two element boolean array to control limits
;                               on the allowed range for a variable and must
;                               be paired with .limits
;                 .limits  - two element float or double array for the limits
;  CIRCULAR - Flag (passed to mpfitellipse) that if, set constrains the fit
;               to be circular.  This keyword simplifies this common option
;               but can be replicated without this flag but more complex
;               options in PARINFO.
;  SHOWSTART - Flag, used only in direct graphics mode.  If set will plot
;               START_PARAMS if provided.
;  LABEL_ANGLE - Angle to set labels to, default=0
;  WINDOW - if using direct graphcis, this controls which window is used,
;            (default=0)
;  NOLOG - Flag, if set will suppress writing a log file of output from the
;            fitting process.
;  SILENT - Flag, if set will suppress printing any results information to
;             the console.  This does not apply to error messages.
;  NOSAVE  - Flag, if set suppresses saving the images (direct graphics only)
;  EXTRATITLE - Extra string to append to title of plot.  Used only for
;                 the direct graphics version
;  TAGSTYLE - Optional control on what tags to show for site tracks
;              0 - default, show all of them that fit in the display window
;              1 - suppress labels for sites with no data
;  BODY - Single character string of the object to fit, default = '0'
; OUTPUTS:
; KEYWORD OUTPUT PARAMETERS:
; CONFIGURATION:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
;  The weighting factors are not rigorously handled.  The true uncertainties
;     are 1-D and aligned with the track of each site.  The ellipse fitting
;     routine applies this to both xi and eta in equal measure.
;
;  Requires the Markwardt ellipse fitting program be in your IDL_PATH
;    (mpfitellipse.pro).  This is from a separate library of routines.
;
; PROCEDURE:
; MODIFICATION HISTORY:
;  Written by Marc W. Buie, Southwest Research Institute, 2020/01/31
;  2020/03/31, MWB, many things cleaned up, new documentation, new keywords
;  2020/05/14, MWB, added START_PARAMS keyword
;  2020/05/21, MWB, added weighting keywords
;  2020/06/08, MWB, added saving the xi,eta D&R coordinates to a file
;  2020/10/07, MWB, added support for config.ini file
;  2021/01/25, MWB, added POSTFUNC keyword
;  2021/02/04, MWB, added FNCHORDS keyword
;  2021/03/10, MWB, added FNXTRACK and FNXE keywords
;  2021/05/13, MWB, added WINDOW keyword
;  2021/12/17, MWB, added FNRESULTS, NOLOG, and SILENT keywords
;  2022/01/18, MWB, fixed problem with SHOWSTART keyword
;  2022/01/25, MWB, added NOSAVE keyword
;  2022/02/01, MWB, corrected an error in the appearance of the data in
;                       the plot.  Added GNDVIEW keyword.  Also fixed a
;                       serious error due to a missing coordinate
;                       transformation.
;  2022/02/02, MWB, added compass and direction of motion arrows and
;                       EXTRATITLE keyword
;  2022/02/27, MWB, added nephpts and ephdt to config.ini options as well
;                      as cleaning up some output text formatting.
;  2022/08/22, MWB, added support for just D or R points from a site rather
;                      than restricting it to just site with both
;  2022/12/07, MWB, removed FNCHORDS keyword, this is now split into FNSITES
;                      and FNEVENTS
;  2022/12/13, MWB, removed GNDVIEW option and keyword, added BODY keyword
;  2023/03/09, MWB, fixed a problem with xe files
;  2023/03/28, MWB, added SHOWHULL, HULLPTS keyword
;  2023/04/03, MWB, added SHOWEXTRA keyword
;  2023/05/02, MWB, reactivated the pub graphics component that was broken
;                     after the upgrades last August.
;  2023/08/17, MWB, changed to use new loadocc/rdevents to control what
;                     gets used for the hull.  Added ARES keyword
;  2023/09/05, MWB, added calculation for the area of the hull.
;  2023/09/14, MWB, added SAVEHULL keyword
;  2023/10/19, MWB, added support for using config file to control label
;                     offsets.  This new feature eliminates the use of
;                     TAGTWEAK.  Also added TAGSTYLE keyword.
;  2024/03/19, MWB, minor change to print the ephemeris position for the
;                     reference site when doing astrometry
;  2024/06/04, MWB, new parameters to control direction arrow labels
;  2024/07/31, MWB, modified to use new ephem/obscode tools
;-
compile_opt strictarrsubs
pro occprof,REFNAME=refname,OFFSET=offset,HSIZE=hsize, $
       PUBINFO=pubinfo,START_PARAMS=start_params,PARINFO=parinfo, $
       SHOWSTART=showstart,WEIGHTS=weights,AUTO_WEIGHT=auto_weight, $
       EVENT=event,FORCE=force,CIRCULAR=circular,POSTFUNC=postfunc, $
       FNSITES=fnsites,LABEL_ANGLE=label_angle,FNXTRACK=fnxtrack, $
       FNXE=fnxe,WINDOW=window,FNCONFIG=fnconfig,FNRESULTS=fnresults, $
       NOLOG=nolog,SILENT=silent,NOSAVE=nosave,HULLPTS=hullpts, $
       EXTRATITLE=extratitle,FNEVENTS=fnevents,BODY=body,SHOWHULL=showhull, $
       SHOWEXTRA=showextra,ARES=ares,SAVEHULL=savehull,TAGSTYLE=tagstyle

   self='occprof: '
   if badpar(refname,[0,7],0,caller=self+'(REFNAME) ',default='') then return
   if badpar(offset,[0,2,3,4,5],1,caller=self+'(OFFSET) ', $
                                  default=[0,0]) then return
   if badpar(hsize,[0,2,3,4,5],1,caller=self+'(HSIZE) ', $
                                  default=[30,23]) then return
   if badpar(ares,[0,2,3,4,5],0,caller=self+'(ARES) ', $
                                  default=20) then return
   if badpar(pubinfo,[0,8],[1],caller=self+'(PUBINFO) ', $
                               type=type_pubinfo) then return
   if badpar(start_params,[0,2,3,4,5],1,caller=self+'(START_PARAMS) ', $
                               type=type_start_params) then return
   if badpar(parinfo,[0,8],1,caller=self+'(PARINFO) ', $
                               type=type_parinfo) then return
   if badpar(showhull,[0,1,2,3],0,caller=self+'(SHOWHULL) ', $
                                  default=0) then return
   if badpar(showextra,[0,1,2,3],0,caller=self+'(SHOWEXTRA) ', $
                                  default=0) then return
   if badpar(showstart,[0,1,2,3],0,caller=self+'(SHOWSTART) ', $
                                  default=0) then return
   if badpar(auto_weight,[0,1,2,3],0,caller=self+'(AUTO_WEIGHT) ', $
                                  default=0) then return
   if badpar(circular,[0,1,2,3],0,caller=self+'(CIRCULAR) ', $
                                  default=0) then return
   if badpar(force,[0,1,2,3],0,caller=self+'(FORCE) ', $
                                  default=0) then return
   if badpar(nosave,[0,1,2,3],0,caller=self+'(NOSAVE) ', $
                                  default=0) then return
   if badpar(silent,[0,1,2,3],0,caller=self+'(SILENT) ', $
                                  default=0) then return
   if badpar(nolog,[0,1,2,3],0,caller=self+'(NOLOG) ', $
                                  default=0) then return
   if badpar(postfunc,[0,7],0,caller=self+'(POSTFUNC) ', $
                                  default='') then return
   if badpar(fnsites,[0,7],0,caller=self+'(FNSITES) ', $
                                  default='sites.dat') then return
   if badpar(fnevents,[0,7],0,caller=self+'(FNEVENTS) ', $
                                  default='events.dat') then return
   if badpar(fnxtrack,[0,7],0,caller=self+'(FNXTRACK) ', $
                                  default='crosstrack.dat') then return
   if badpar(label_angle,[0,2,3,4,5],0,caller=self+'(LABEL_ANGLE) ', $
                                  default=0.0) then return
   if badpar(window,[0,2,3],0,caller=self+'(WINDOW) ', $
                                  default=0) then return
   if badpar(fnconfig,[0,7],0,caller=self+'(FNCONFIG) ', $
                             default='config.ini') then return
   if badpar(extratitle,[0,7],0,caller=self+'(EXTRATITLE) ', $
                             default='') then return
   if badpar(body,[0,7],0,caller=self+'(BODY) ', $
                                  default='0') then return
   if badpar(hullpts,[0,4,5],2,caller=self+'(HULLPTS) ', $
                                  type=hullpts_type) then return
   if badpar(savehull,[0,7],0,caller=self+'(SAVEHULL) ', $
                                  default='hull.dat') then return
   if badpar(tagstyle,[0,2,3],0,caller=self+'(TAGSTYLE) ', $
                                  default=0) then return

   aa=0 ; workaround for graphics problem as of 2024/06/05

   ; all positions from fnsites are to be East Longitude
   loadocc,oinfo,fnsites=fnsites,fnxtrack=fnxtrack,fnevents=fnevents

;   readcol,fnchords,site,success,lat,lon,alt,date,dtime,rtime,derr,rerr, $
;      format='a,a,d,d,d,a,a,a,f,f',count=nsites
   if oinfo.nsites eq 0 then begin
      print,'no valid site data found in ',fnsites
      return
   endif

   sref=!null
   if refname ne '' then begin
      z=where(oinfo.team eq refname,count)
      if count eq 1 then sref=z[0]
   endif

   if auto_weight then begin
      weights=1.0/(oinfo.err>0.001)
   endif else begin
      if badpar(weights,[0,2,3,4,5],2,caller=self+'(WEIGHTS) ', $
                                      default=replicate(1.0,oinfo.nevents), $
                                      dimen=wdimen) then return
      if oinfo.nsites ne 1 then begin
         if wdimen ne oinfo.nevents then begin
            print,'dimensions on weight do not match the data'
            help,weights,oinfo.nevents
            return
         endif
      endif
   endelse

   if nofile(fnconfig,'Configuration file') then return
   loadini,cinfo,file=fnconfig
   getvalue,cinfo,'global','event',event
   getvalue,cinfo,'global','objectid',objectid
   getvalue,cinfo,'global','ora',ras
   getvalue,cinfo,'global','odec',decs
   getvalue,cinfo,'global','geomid',geomid
   getvalue,cinfo,'global','tsig',tsig,type=5
   getvalue,cinfo,'global','xsig',xsig,type=5
   getvalue,cinfo,'global','nephpts',ncp,default=382,type=3
   getvalue,cinfo,'global','ephdt',dte,default=0.1d0,type=5
   getvalue,cinfo,'profile','tagbase',tagbase,default=[0,0],type=4,/array
   getvalue,cinfo,'profile','dimension',dim,default=[1000,750],type=3,/array
   getvalue,cinfo,'profile','elabelgap',elabelgap,default=0,type=4
   getvalue,cinfo,'profile','nlabelgap',nlabelgap,default=0,type=4
   getvalue,cinfo,'profile','radialgap',radialgap,default=0.95,type=4
   getvalue,cinfo,'profile','bitmapscale',bitmapscale,default=1,type=4
   sra=raparse(ras)
   sdec=decparse(decs)
   jdgeomid=jdparse(geomid)

   if badpar(fnxe,[0,7],0,caller=self+'(FNXE) ', $
                                  default=event+'_'+body+'_xe.dat') then return
   if badpar(fnresults,[0,7],0,caller=self+'(FNRESULTS) ', $
                                  default=event+'_'+body+'.results') then return

   if not nolog then openw,lunlog,fnresults,/get_lun

   str=strn(oinfo.nsites)+' sites found in '+fnsites+' for event '+event
   if not silent then print,str
   if not nolog then printf,lunlog,str

   spawn,'geteph',unit=pipe

   jd=replicate(-1.0d0,3,oinfo.nevents)
   xi=dblarr(3,oinfo.nevents)
   eta=dblarr(3,oinfo.nevents)

   jdc=dblarr(ncp,oinfo.nsites)
   akscale=dblarr(ncp,oinfo.nsites)
   ara=dblarr(ncp,oinfo.nsites)
   adec=dblarr(ncp,oinfo.nsites)
   xic=dblarr(ncp,oinfo.nsites)
   etac=dblarr(ncp,oinfo.nsites)

   ; geometry of occultation points
   for i=0,oinfo.nevents-1 do begin
      jd[*,i] = oinfo.jdev[i]+[0,-1,1]*oinfo.err[i]/86400.0d0
      obsinfo,'G**',obsinfo,oinfo.lat[oinfo.sidx[i]], $
                            oinfo.lon[oinfo.sidx[i]], $
                            oinfo.alt[oinfo.sidx[i]], $
                            oinfo.team[oinfo.sidx[i]],/degrees
      ephem,jd[*,i],obsinfo,72,objectid,eph,pipe=pipe,/debug
      ssgeom,eph,sun,earth,phang,elong
      kscale = earth * 1.496e8 * !pi / 180.0 / 3600.0
      astrd2sn,trimrank(eph[0,*]),trimrank(eph[1,*]), $
         sra,sdec,xi0,eta0,/arcsec
      xi[*,i]=xi0*kscale
      eta[*,i]=eta0*kscale
   endfor

   ; geometry of site tracks
   for i=0,oinfo.nsites-1 do begin
      ; geometry of ephemeris
      jdc[*,i]=oinfo.jdref[i]+dte*(dindgen(ncp)-ncp/2)/86400.0d0
      obsinfo,'G**',obsinfo,oinfo.lat[i], $
                            oinfo.lon[i], $
                            oinfo.alt[i], $
                            oinfo.team[i],/degrees
      ephem,trimrank(jdc[*,i]),obsinfo,72,objectid,eph,pipe=pipe
      ssgeom,eph,sun,earth,phang,elong
      kscale = earth * 1.496e8 * !pi / 180.0 / 3600.0
      akscale[*,i] = kscale
      ara[*,i] = trimrank(eph[0,*])
      adec[*,i] = trimrank(eph[1,*])
      astrd2sn,trimrank(eph[0,*]),trimrank(eph[1,*]),sra,sdec,xi0,eta0,/arcsec
      xic[*,i]=xi0*kscale
      etac[*,i]=eta0*kscale
   endfor

   free_lun,pipe

   color=['7f7f7f'xl,'00ffff'xl,'ff00ff'xl]

   ; this block converts from a coordinate system that is centered and
   ;   referenced to the star to a coordinate system that is referenced
   ;   to the object.  After the conversion, the origin is the center of
   ;   the body as defined by the _ephemeris_ not the real body.
   xi  = -1.0*xi
   eta = -1.0*eta
   xic  = -1.0*xic
   etac = -1.0*etac

   ; select the points to be fitted
   zf=where(oinfo.body eq body and oinfo.fit eq 1,nfit)
   ; select the points to be shown
   zs=where(oinfo.body eq body,nshow)
   ; select the points to use for a hull
   zh=where(oinfo.body eq body and oinfo.hfit eq 1,nfit)

   nell=101
   phi = dindgen(nell)*2D*!dpi/100

   pfit=fltarr(5,3)
   xcirc=fltarr(nell,3)
   ycirc=fltarr(nell,3)
   
   if nfit eq 0 then begin
      print,'No points to fit'
      goto,bailout
   endif

   for i=0,2 do begin

      ; 0: semi-major axis, 1: semi-minor axis, 2: x pos, 3: y pos, 4: pang[rad]
      xifit=trimrank(xi[i,zf])
      etafit=trimrank(eta[i,zf])
      w=trimrank(weights[zf])
      if not silent then begin
         print,'Weights'
         print,w
      endif
      if not nolog then begin
         printf,lunlog,'Weights'
         printf,lunlog,w
      endif
      if force and type_start_params ne 0 then begin
         str='manual override, no fit.  Using start_params'
         if not silent then print,str
         if not nolog then printf,lunlog,str
         p1=start_params
      endif else begin
         if type_start_params ne 0 then begin
            str='Providing starting point for ellipse fit.'
            if not silent then print,str
            if not nolog then printf,lunlog,str
            p1=mpfitellipse(xifit,etafit,start_params,/tilt,status=status, $
                            errmsg=errmsg,parinfo=parinfo,/quiet,weights=w, $
                            circular=circular)
         endif else begin
            p1=mpfitellipse(xifit,etafit,/tilt,/quiet,status=status, $
                            errmsg=errmsg,parinfo=parinfo,weights=w, $
                            circular=circular)
         endelse
         str='nominal fit return status='+strn(status)
         if status eq 0 then print,errmsg
         if not nolog then printf,lunlog,errmsg
      endelse
      xcirc1=p1[2]+p1[0]*cos(phi)*cos(p1[4])+p1[1]*sin(phi)*sin(p1[4])
      ycirc1=p1[3]-p1[0]*cos(phi)*sin(p1[4])+p1[1]*sin(phi)*cos(p1[4])

      pfit[*,i]=p1
      xcirc[*,i]=xcirc1
      ycirc[*,i]=ycirc1

   endfor

   ; starting parameters
   if type_start_params ne 0 then begin
      p0=start_params
      xcirc0=p0[2]+p0[0]*cos(phi)*cos(p0[4])+p0[1]*sin(phi)*sin(p0[4])
      ycirc0=p0[3]-p0[0]*cos(phi)*sin(p0[4])+p0[1]*sin(phi)*cos(p0[4])
   endif

   ; center of plot
   xcen = ( min(xi[*,zf])  + max(xi[*,zf])  )/2.0
   ycen = ( min(eta[*,zf]) + max(eta[*,zf]) )/2.0
   x0=xcen+offset[0]
   y0=ycen+offset[1]

   xr=hsize[0]*[1,-1]+x0 ; xi,eta are left-handed
   yr=hsize[1]*[-1,1]+y0

   panorth =  90.0
   paeast  = 180.0
   
   str=['','Information on all sites with associated track labeling info', $
        '   idx Site          xi_l   eta_l    xtag    ytag  angle tag/val']
   if not silent then for ii=0,n_elements(str)-1 do print,str[ii]
   if not nolog then for ii=0,n_elements(str)-1 do printf,lunlog,str[ii]

   ; figure out where the track labels go
   tagged=bytarr(oinfo.nsites)
   xtag=fltarr(oinfo.nsites)
   ytag=fltarr(oinfo.nsites)
   tangle=fltarr(oinfo.nsites)
;print,'xr',xr[*]
   for i=0,oinfo.nsites-1 do begin
      tagval=' N'

      ; East to the left, maxmimum xi is on the left
      xloc=max(xic[*,i])<xr[0]
      interp,trimrank(xic[*,i]),trimrank(etac[*,i]),xloc,yloc
      if yloc ge yr[0] and yloc le yr[1] then begin
         tagged[i]=1
         tagval=' a'
         xtag[i]=xloc
         ytag[i]=yloc
      endif else if yloc lt yr[0] then begin
         interp,trimrank(etac[*,i]),trimrank(xic[*,i]),yr[0],xloc
         if signum(xr[0]-xloc) eq signum(xloc-xr[1]) then begin
            tagged[i]=1
            tagval=' b'
            xtag[i]=xloc
            ytag[i]=yr[0]
         endif
      endif else begin
         interp,trimrank(etac[*,i]),trimrank(xic[*,i]),yr[1],xloc
         if signum(xr[0]-xloc) eq signum(xloc-xr[1]) then begin
            tagged[i]=1
            tagval=' c'
            xtag[i]=xloc
            ytag[i]=yr[1]
         endif
      endelse

      dx=xic[-1,i]-xic[0,i]
      dy=etac[-1,i]-etac[0,i]
      tangle[i] = atan(dy,dx)

      str=string(i,oinfo.team[i],xic[0,i],etac[0,i],xtag[i],ytag[i], $
                 tangle[i]*!radeg,tagged[i],tagval, $
                 format='(i6,1x,a-10,4(1x,f7.1),1x,f6.1,1x,i1,a)')
      if not silent then print,str
      if not nolog then printf,lunlog,str
   endfor

   if refname eq '' then begin
     angidx=0
   endif else begin
     angidx=sref
   endelse

   openw,lunxe,fnxe,/get_lun
   fmtxe='(a-8,2(1x,a1),1x,i1,1x,a,1x,f5.3,2(1x,f12.3))'
   for i=0,oinfo.nevents-1 do begin
      if oinfo.body[i] ne body then continue
      jdstr,oinfo.jdev[i],3,jdevs
      printf,lunxe,oinfo.team[oinfo.sidx[i]],oinfo.body[i],oinfo.type[i], $
                   oinfo.fit[i],jdevs,oinfo.err[i], $
                   xi[0,i],eta[0,i],format=fmtxe
   endfor
   free_lun,lunxe

   str=['','Information on all sites with a positive detection', $
        'Site     F T  E-tref    E_xi   E_eta']
   if not silent then for ii=0,n_elements(str)-1 do print,str[ii]
   if not nolog then for ii=0,n_elements(str)-1 do printf,lunlog,str[ii]

   for i=0,nshow-1 do begin
      jdstr,oinfo.jdev[zs[i]],3,jds
      str=string(oinfo.team[oinfo.sidx[zs[i]]], $
                 oinfo.fit[zs[i]], $
                 oinfo.type[zs[i]], $
                 (jd[0,zs[i]]-oinfo.jdref[oinfo.sidx[zs[i]]])*86400.0d0, $
                 xi[0,zs[i]],eta[0,zs[i]], $
                 jds, $
                 format='(a-8,1x,i1,1x,a,2x,f7.3,2(1x,f7.1),2x,a)')
      if not silent then print,str
      if not nolog then printf,lunlog,str
   endfor

   str=['','Fitting results', $
        ' Nominal  +1-sig  -1-sig parameter']
   if not silent then for ii=0,n_elements(str)-1 do print,str[ii]
   if not nolog then for ii=0,n_elements(str)-1 do printf,lunlog,str[ii]

   if nfit gt 0 then begin
      fmt1='(3(1x,f8.2),2x,a)'
      fmt2='(3(1x,f8.3),2x,a)'
      fmt3='(3(2x,i7),3x,a)'
      str=[]
      str=[str,string(pfit[0,*],'semi-major axis [km]',format=fmt1)]
      str=[str,string(pfit[1,*],'semi-minor axis [km]',format=fmt1)]
      str=[str,string(pfit[2,*], $
                  'x (xi)  position relative to ephemeris [km]', $
                  format=fmt1)]
      str=[str,string(pfit[3,*], $
                  'y (eta) position relative to ephemeris [km]', $
                  format=fmt1)]
      str=[str,string(pfit[4,*]*!radeg-90, $
                  'position angle [degrees]',format=fmt1)]

      str=[str,string(round(!pi*pfit[0,*]*pfit[1,*]), $
                      'area of fitted ellipse [km^2]',format=fmt3)]
      str=[str,string(pfit[1,*]/pfit[0,*], $
                  'aspect ratio of ellipse',format=fmt2)]
      if not silent then for ii=0,n_elements(str)-1 do print,str[ii]
      if not nolog then for ii=0,n_elements(str)-1 do printf,lunlog,str[ii]
   endif

   if type_pubinfo eq 0 then begin

      setwin,window,xsize=dim[0],ysize=dim[1]
      plot,[0],[1],/iso,xr=xr,yr=yr,/nodata,charsize=2.0, $
         xtitle='xi (km)',ytitle='eta (km)',background='ffffff'xl,color=0, $
         title=event+extratitle

      ;plot the site tracks
      ;  n - no data, gray
      ;  x - not processed yet, light yellow
      ;  y - with event seen, red
      ;      with no event seen, blue
      for i=0,oinfo.nsites-1 do begin

         lcolor=-1

         if oinfo.flag[i] eq 'n' then lcolor='a0a0a0'xl
         if oinfo.flag[i] eq 'x' then lcolor=cpalette(25)

         if oinfo.flag[i] eq 'y' then begin
            z=where(oinfo.team[i] eq oinfo.eteam[zs],count)
            if count eq 0 then lcolor=cpalette(8) else lcolor=cpalette(4)
         endif

         ; don't plot if it doesn't match the prior rules
         if lcolor lt 0 then continue

         oplot,xic[*,i],etac[*,i],color=lcolor,thick=2
         thistag = tagged[i]
         if tagstyle eq 1 and oinfo.flag[i] eq 'n' then thistag=0
         if thistag then begin
            if oinfo.team[i] eq refname then rtag=' (ref)' else rtag=''
            getvalue,cinfo,'profile',oinfo.team[i],teamoffset, $
               default=[0,0],type=4,/array
            xtagdel = tagbase[0]+teamoffset[0]
            ytagdel = tagbase[1]+teamoffset[1]
            xyouts,xtag[i]+xtagdel,ytag[i]+ytagdel, $
                   oinfo.team[i]+rtag,/data,color=lcolor,align=0.0, $
                   charsize=1.5,orientation=label_angle
         endif

      endfor

      ; plot star apparent motion arrow
      daro=[(xr[1]-xr[0])*0.90+xr[0],(yr[1]-yr[0])*0.78+yr[0]]
      arcen=daro
      dart=daro+0.15*abs(xr[1]-xr[0])*[cos(tangle[angidx]),sin(tangle[angidx])]
      middar=(daro+dart)/2.0
      daro=daro-middar+arcen
      dart=dart-middar+arcen
      arrow,daro[0],daro[1],dart[0],dart[1],color=cpalette(48),/data,thick=3

      if nshow gt 0 then begin
         if showhull then begin
            oxi  = trimrank(xi[0,zh]-pfit[2,0])
            oeta = trimrank(eta[0,zh]-pfit[3,0])
            if hullpts_type ne 0 then begin
               oxi = [oxi,trimrank(hullpts[*,0])]
               ;  HULLPTS - extra points to add to hull.
               oeta = [oeta,trimrank(hullpts[*,1])]
            endif
            ;mkhull,oxi,oeta,hxi,heta,npts=360
            nhullpts = 500
            mkhull2,oxi,oeta,hxi,heta,npts=nhullpts,ares=ares,sortidx=hidx
            hr=sqrt(hxi^2+heta^2)
            xhc = total(hxi*sqrt(0.5)*hr^2)/total(hr^2)+pfit[2,0]
            yhc = total(heta*sqrt(0.5)*hr^2)/total(hr^2)+pfit[3,0]
            harea = total(abs(0.5*(hxi[0:-2]*heta[1:-1] - heta[0:-2]*hxi[1:-1])))
            polyfill,hxi+pfit[2,0],heta+pfit[3,0],color='c0c0c8'xl
            oplot,hxi+pfit[2,0],heta+pfit[3,0],color=cpalette(32),thick=2
            oplot,[xhc],[yhc],psym=6,color=cpalette(32)
            if showextra then begin
               oplot,oxi+pfit[2,0],oeta+pfit[3,0],psym=8,symsize=1.0,color=0
               xiextra = trimrank(hullpts[*,0])
               etaextra = trimrank(hullpts[*,1])
               xyouts,xiextra+pfit[2,0],etaextra+pfit[3,0], $
                 strcompress(string(indgen(n_elements(xiextra))),/remove_all), $
                       /data,alignment=0.,color=0
            endif else begin
               oplot,oxi+pfit[2,0],oeta+pfit[3,0],psym=8,symsize=1.0,color=0
               idx=lindgen(n_elements(oxi))
               xy = transpose([[oxi[hidx]+pfit[2,0]],[oeta[hidx]+pfit[3,0]]])
               rt=cv_coord(from_rect=xy,/to_polar)
               rt[1,*] = radialgap*rt[1,*]
               xy=cv_coord(from_polar=rt,/to_rect)
               loxi  = trimrank(xy[0,*])
               loeta = trimrank(xy[1,*])

               xyouts,loxi,loeta, $
                 ' '+strcompress(string(idx),/remove_all), $
                       /data,alignment=0.5,color=0
            endelse
            if savehull ne '' then begin
               openw,lun,savehull,/get_lun
               for i=0,nhullpts-1 do begin
                  printf,lun,hxi[i],heta[i]
               endfor
               free_lun,lun
            endif
         endif
         for i=0,nshow-1 do begin
            if oinfo.body[zs[i]] ne body then continue
            if oinfo.fit[zs[i]] then setusym,1 else setusym,-1
            oplot,[xi[0,zs[i]]],[eta[0,zs[i]]], $
                     psym=8,color=cpalette(1),symsize=2
            oplot,[xi[1,zs[i]]],[eta[1,zs[i]]], $
                     psym=8,color=cpalette(1),symsize=1
            oplot,[xi[2,zs[i]]],[eta[2,zs[i]]], $
                     psym=8,color=cpalette(1),symsize=1
         endfor
         setusym,1


      endif

      if nfit gt 0 then begin
         linestyle=[0,2,2]
         for i=0,2 do $
            oplot,xcirc[*,i],ycirc[*,i], $
            color=cpalette(57),linestyle=linestyle[i]
         oplot,[pfit[2,0]],[pfit[3,0]],psym=5,color=cpalette(3)
      endif

      if type_start_params ne 0 and showstart then begin
         oplot,xcirc0,ycirc0,color=cpalette(59)
      endif

      oplot,[0.],[0.],psym=7,symsize=2.0,color=0

      cx = !d.x_size - 120
      cy = !d.y_size - 120
      one_arrow,cx,cy,panorth,'N',color=cpalette(48)
      one_arrow,cx,cy,paeast,'E',color=cpalette(48)

      if postfunc ne '' then void=execute(postfunc+',0')

      words=strsplit(fnxe,'.',/extract)
      if not nosave then tvgrab,words[0]+'.png',window,/png

   endif else begin

      options=tag_names(pubinfo)
      z=where(options eq 'LOC')
      if z[0] ge 0 then begin
         loc=pubinfo.loc
      endif else begin
         loc=[0.1,0.1,0.9,0.9]
      endelse

      pl=plot([0],[1],aspect_ratio=1.0,xr=axextend(xr),yr=axextend(yr), $
         /nodata,xtitle='$\xi$ [km]',ytitle='$\eta$ [km]', $ ;position=loc, $
         dimension=dim,font_size=12,anti=0)

      ar1=[(xr[1]-xr[0])*0.9+xr[0],(yr[1]-yr[0])*0.86+yr[0]]
      arn=ar1+[0,1]*0.045*abs(xr[1]-xr[0])
      are=ar1+[1,0]*0.045*abs(xr[1]-xr[0])
      a1 = arrow([ar1[0],arn[0]],[ar1[1],arn[1]],color=cpalette(48,/array), $
                 /data,/current,head_indent=0.8,head_size=0.6,line_thick=2)
      a1 = arrow([ar1[0],are[0]],[ar1[1],are[1]],color=cpalette(48,/array), $
                 /data,/current,head_indent=0.8,head_size=0.6,line_thick=2)
      tx=text(arn[0],arn[1]+nlabelgap,'N',/data,align=0.5, $
              color=cpalette(48,/array),font_size=14)
      tx=text(are[0]+elabelgap,are[1],'E',/data,align=1, $
              color=cpalette(48,/array),vertical_alignment=0.5,font_size=14)

      ; plot star apparent motion arrow
      daro=[(xr[1]-xr[0])*0.90+xr[0],(yr[1]-yr[0])*0.78+yr[0]]
      arcen=daro
      dart=daro+0.15*abs(xr[1]-xr[0])*[cos(tangle[angidx]),sin(tangle[angidx])]
      middar=(daro+dart)/2.0
      daro=daro-middar+arcen
      dart=dart-middar+arcen
      a1 = arrow([daro[0],dart[0]],[daro[1],dart[1]], $
                 color=cpalette(48,/array),/data,/current,line_thick=2)
                  

      for i=0,oinfo.nsites-1 do begin

         lcolor=-1

         if oinfo.flag[i] eq 'n' then lcolor=[160,160,160]
         if oinfo.flag[i] eq 'x' then lcolor=cpalette(4,/array)

         if oinfo.flag[i] eq 'y' then begin
            z=where(oinfo.team[i] eq oinfo.eteam[zs],count)
            if count eq 0 then lcolor=cpalette(8,/array) $
            else lcolor=cpalette(4,/array)
         endif

         ; don't plot if it doesn't match the prior rules
         if lcolor[0] lt 0 then continue

         pl1=plot(/overplot,/current,xic[*,i],etac[*,i],color=lcolor, $
                 thick=1.0,anti=0)
         if oinfo.flag[i] eq 'n' then pl1.order,/send_to_back
         thistag = tagged[i]
         if tagstyle eq 1 and oinfo.flag[i] eq 'n' then thistag=0
         if thistag then begin
            if oinfo.team[i] eq refname then rtag=' (ref)' else rtag=''
            getvalue,cinfo,'profile',oinfo.team[i],teamoffset, $
               default=[0,0],type=4,/array
            xtagdel = tagbase[0]+teamoffset[0]
            ytagdel = tagbase[1]+teamoffset[1]
print,'*** ',xtagdel,ytagdel
            tx=text(xtag[i]+xtagdel,ytag[i]+ytagdel, $
                    oinfo.team[i]+rtag, $
                    /data,align=0,orientation=label_angle,color=lcolor)
         endif

      endfor

      if nshow gt 0 then begin
         if showhull then begin
            oxi  = trimrank(xi[0,zh]-pfit[2,0])
            oeta = trimrank(eta[0,zh]-pfit[3,0])
            if hullpts_type ne 0 then begin
               oxi = [oxi,trimrank(hullpts[*,0])]
               ;  HULLPTS - extra points to add to hull.
               oeta = [oeta,trimrank(hullpts[*,1])]
            endif
            mkhull2,oxi,oeta,hxi,heta,npts=360,ares=ares,sortidx=hidx
            hr=sqrt(hxi^2+heta^2)
            xhc = total(hxi*sqrt(0.5)*hr^2)/total(hr^2)+pfit[2,0]
            yhc = total(heta*sqrt(0.5)*hr^2)/total(hr^2)+pfit[3,0]
            harea = total(abs(0.5*(hxi[0:-2]*heta[1:-1] - heta[0:-2]*hxi[1:-1])))

            po=polygon(hxi+pfit[2,0],heta+pfit[3,0],/data,/fill_background, $
               fill_color=[225,217,217],target=pl,fill_transparency=10,anti=0)

            pl2=plot(/overplot,/current,hxi+pfit[2,0],heta+pfit[3,0], $
                     color=cpalette(32,/array),thick=2,anti=0)
            pl3=plot(/overplot,/current,[xhc],[yhc], $
                     symbol='square',linestyle='none', $
                     color=cpalette(32,/array),thick=2,anti=0)
            if showextra then begin
;               oplot,oxi+pfit[2,0],oeta+pfit[3,0],psym=8,symsize=1.0,color=0
               pl3=plot(/overplot,/current,oxi+pfit[2,0],oeta+pfit[3,0], $
                     linestyle='none', $
                     symbol='circle',/sym_filled,sym_size=0.5,color='black',anti=0)
               xiextra = trimrank(hullpts[*,0])
               etaextra = trimrank(hullpts[*,1])
               tt=text(/current,xiextra+pfit[2,0],etaextra+pfit[3,0], $
                 strcompress(string(indgen(n_elements(xiextra))),/remove_all), $
                       /data,alignment=0.,color='black')
            endif else begin
               pl3=plot(/overplot,/current,oxi+pfit[2,0],oeta+pfit[3,0], $
                     linestyle='none', $
                     symbol='triangle',/sym_filled,sym_size=0.5,color='black',anti=0)
               idx=lindgen(n_elements(oxi))
               xy = transpose([[oxi[hidx]],[oeta[hidx]]])
               rt=cv_coord(from_rect=xy,/to_polar)
               rt[1,*] = radialgap*rt[1,*]
               xy=cv_coord(from_polar=rt,/to_rect)
               loxi  = trimrank(xy[0,*])
               loeta = trimrank(xy[1,*])
               tl=text(loxi+pfit[2,0],loeta+pfit[3,0],strcompress(string(idx),/remove_all), $
                       /data,alignment=0.5,vertical_alignment=0.5,color='black')
            endelse
         endif
         for i=0,nshow-1 do begin
            if oinfo.body[zs[i]] ne body then continue
            if oinfo.fit[zs[i]] then filled=1 else filled=0
            plp=plot(/overplot,/current,[xi[0,zs[i]]],[eta[0,zs[i]]], $
                     color=cpalette(1,/array),linestyle='none', $
                     symbol='circle',sym_filled=filled,sym_size=1.0,anti=0)
            plp.order,/bring_to_front
            plp=plot(/overplot,/current,[xi[1,zs[i]]],[eta[1,zs[i]]], $
                     color=cpalette(1,/array),linestyle='none', $
                     symbol='circle',sym_filled=filled,sym_size=0.5,anti=0)
            plp.order,/bring_to_front
            plp=plot(/overplot,/current,[xi[2,zs[i]]],[eta[2,zs[i]]], $
                     color=cpalette(1,/array),linestyle='none', $
                     symbol='circle',sym_filled=filled,sym_size=0.5,anti=0)
            plp.order,/bring_to_front
         endfor

      endif

      if nfit gt 0 then begin
         linestyle=[0,2,2]
         for i=0,2 do $
            pl1=plot(/overplot,/current,xcirc[*,i],ycirc[*,i], $
                     color=cpalette(57,/array),linestyle=linestyle[i],anti=0)
         pl1=plot(/overplot,/current,[pfit[2,0]],[pfit[3,0]], $
                  symbol='diamond',color=cpalette(3,/array),anti=0)
      endif

      if type_start_params ne 0 and showstart then begin
         pl1=plot(/overplot,/current,xcirc0,ycirc0,color=cpalette(59,/array),anti=0)
      endif

      pl1=plot(/overplot,/current,[0.],[0.],symbol='X', $
               color='black',anti=0)

      if postfunc ne '' then void=execute(postfunc+',pl,/pub')

      z=where(options eq 'FNLIST')
      if z[0] ge 0 then begin
         fnlist=pubinfo.fnlist
         for i=0,n_elements(pubinfo.fnlist)-1 do begin
            print,'save to ',pubinfo.fnlist[i]
            words=strsplit(pubinfo.fnlist[i],'.',/extract)
            fntype=words[-1]
            if fntype eq 'png' or fntype eq 'jpg' then begin
               pl.save,fnlist[i],width=dim[0]*bitmapscale
            endif else begin
               pl.save,fnlist[i]
            endelse
         endfor
      endif

   endelse

   if refname eq '' then begin
      str='No reference site supplied, skipping astrometry section.'
      print,str
      if not nolog then printf,lunlog,str
      goto,bailout
   endif
   if sref eq !null then begin
      str='Reference site name '+refname+' was not found.  Quitting.'
      print,str
      if not nolog then printf,lunlog,str
      goto,bailout
   endif

   ; begin astrometry section

   z=where(oinfo.eteam eq oinfo.team[sref] and oinfo.body eq body and $
           oinfo.fit,count)
   if count ne 2 then begin
      str=['', $
          'Reference team '+oinfo.team[sref]+' with index '+strn(sref), $
          'does not have two fitted events, skipping astrometry']
      if not silent then for ii=0,n_elements(str)-1 do print,str[ii]
      if not nolog then for ii=0,n_elements(str)-1 do printf,lunlog,str[ii]
      goto,bailout
   endif

   ; Time of the middle of the reference chord
   jdmid = mean(oinfo.jdev[z])
   jdstr,jdmid,3,jdmids

   ; xi,eta of ephemeris at the mid-time
   interp,trimrank(jdc[*,sref]),trimrank(xic[*,sref]),jdmid,ephxi
   interp,trimrank(jdc[*,sref]),trimrank(etac[*,sref]),jdmid,epheta
   ; ra,dec of ephemeris at the mid-time
   interp,trimrank(jdc[*,sref]),trimrank(ara[*,sref]),jdmid,era
   interp,trimrank(jdc[*,sref]),trimrank(adec[*,sref]),jdmid,edec
   rastr,era,6,eras
   decstr,edec,5,edecs
   print,'Ephemeris position at reference time ',eras,' ',edecs

   if showhull then begin
      str='Using center of hull for astrometry'
      dxi =(xhc-ephxi )/kscale[0] ; arcsec
      deta=(yhc-epheta)/kscale[0] ; arcsec
      str=[str, $
           string('Hull center',xhc,yhc,'km',format='(a-30,2(1x,f7.2),1x,a)')]
      str=[str, $
           string('Hull center is offset from ellipse by', $
                  xhc-pfit[2,0],yhc-pfit[3,0],'km', $
                  format='(a-30,2(1x,f7.2),1x,a)')]
      str=[str, $
           string('Area of the hull is '+strn(harea)+' km^2')]
   endif else begin
      str='Using center of ellipse for astrometry'
      dxi =(pfit[2,0]-ephxi )/kscale[0] ; arcsec
      deta=(pfit[3,0]-epheta)/kscale[0] ; arcsec
   endelse
   for i=0,n_elements(str)-1 do print,str[i]
   if not nolog then $
      for i=0,n_elements(str)-1 do printf,lunlog,str[i]

   astsn2rd,dxi,deta,sra,sdec,ora,odec,/arcsec
   rastr,ora,6,oras
   decstr,odec,5,odecs

   str=''
   str=[str,'astrometry reference chord is '+oinfo.team[sref]]
   str=[str,string('offset of ephemeris from star at predicted mid-time', $
                   ephxi,epheta,format='(a,2(1x,f6.1)," km")')]
   str=[str,string('offset of ellipse from chord center', $
                   dxi*kscale[sref],deta*kscale[sref], $
                   format='(a,16x,2(1x,f6.1)," km")')]
   str=[str,string('sky-plane scale',kscale[sref],'km/arcsec', $
                   format='(a,39x,f10.4,2x,a)')]
   str=[str,'Time at middle of reference chord is '+jdmids]
   str=[str,string('Observer location lat,lon,alt', $
                   oinfo.lat[sref],oinfo.lon[sref],oinfo.alt[sref], $
                   format='(a,1x,f10.6,1x,f11.6,1x,f6.1)')]
   str=[str,oras+' '+odecs+' final topocentric position']
   str=[str,'assumes that the star and ellipse center are coincident at jdref']
   str=[str,string(jdmids,oinfo.lat[sref],oinfo.lon[sref],oinfo.alt[sref], $
             oras,odecs,format='(a,1x,f10.6,1x,f11.6,1x,f6.1,1x,a,1x,a)')]
   if not silent then for ii=0,n_elements(str)-1 do print,str[ii]
   if not nolog then for ii=0,n_elements(str)-1 do printf,lunlog,str[ii]

bailout:
   if not nolog then free_lun,lunlog

end
