;+
; NAME:
;  mkocceph
; PURPOSE:   (one line only)
;  Compute an occultation ephemeris for Wasserman prediction tools
; DESCRIPTION:
;  Calculates ephemeris position for an object at a specified date and
;    time, and creates a .in file suitable for ingestion in OCMAP. At
;    the time of execution a unique string identifier is
;    created for version control and a new subdirectory is created. The
;    .in file is saved to this sudirectory along with a .info file that
;    captures the screen output. These files can be set to be read-only.
;
;  These two files form the basis for a given prediction that is based on
;    the orbit of the object known as of the time this program is run.
;    The .in file captures that ephemeris to insulate against future changes.
; CATEGORY:
;  Occultations
; CALLING SEQUENCE:
;  mkocceph,objname,objcode,jdgeos,starid,sras,sdecs
; INPUTS:
;  objname     :String, occulting object name (e.g., '(11351) Leucus')
;  objcode     :String, geteph code for ephemeris lookup (e.g., 'A11351')
;  jdgeos      :String, date and time of geocentric appulse
;                         (YYYY/MM/DD HH:MM:SS)
;  starid      :String, star identifier
;  sras        :String, star RA (J2000 sexagesimal)
;  sdecs       :String, star Dec (J2000 sexagesimal)
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;  PROTECT - Flag, if set will write-protect output files
;  VERBOSE - Flag, if set gives verbose output
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
;  A subdirectory is created with a unique string based on the time of
;    execution to the nearest second. In the directory are a .in file for
;    OCMAP and a .info file that captures the screen output
;  version - The name of the output subdirectory
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  2018/10/17, Written by Marc Buie, Southwest Research Institute
;  2018/11/17, Brian Keeney, added version control and saving info output
;  2022/01/11, MWB, added appuldis summary to info output file
;  2022/02/08, BK, Fixed problem with output radius, made write-protecting
;              output optional (off by default), and enabled passing the
;              subdirectory name to the caller
;  2022/05/26, MWB, fixed problem with a truncated ephemeris table
;  2023/04/06, MWB, added VERBOSE flag
;  2023/07/28, MWB, added SUPP keyword
;-
pro mkocceph,objname,objcode,jdgeos,starid,sras,sdecs,version, $
       PROTECT=protect,VERBOSE=verbose,SUPP=supp

   compile_opt strictarrsubs

   self='mkocceph: '
   if badpar(objname,7,0,caller=self+'(objname) ') then return
   if badpar(objcode,7,0,caller=self+'(objcode) ') then return
   if badpar(jdgeos,7,0,caller=self+'(jdgeo) ') then return
   if badpar(starid,7,0,caller=self+'(starid) ') then return
   if badpar(sras,7,0,caller=self+'(ra) ') then return
   if badpar(sdecs,7,0,caller=self+'(dec) ') then return
   if badpar(protect,[0,1,2,3],0,caller=self+'(PROTECT) ',default=0) then return
   if badpar(verbose,[0,1,2,3],0,caller=self+'(VERBOSE) ',default=0) then return
   if badpar(supp,[0,2,3,8],[0,1],caller=self+'(SUPP) ', $
                                  type=supptype) then return

   jdgeo=jdparse(jdgeos)
   sra=raparse(sras)
   sdec=decparse(sdecs)

   if verbose then begin
      print,'Starting mkocceph'
      if supptype eq 8 then print,'Supplemental object information supplied'
   endif

   ; generate the range of times needed for the ephemeris to compute
   jdstr,jdgeo,0,jds
   if verbose then print,'Geocentric time of appulse ',jds,' UT'
   jdstr,jdgeo,-7,jds
   if verbose then print,jds
   jdmid=jdparse(jds)

   ; Time range for Larry's capture ephemeris 
   njd=13
   djd=(dindgen(njd)-njd/2)/24.0d0
   jd=jdmid+djd

   jdstr,jd,0,jds

   jd0s=jds[0]
   if verbose then print,jd0s

   ephem,jd,'500',72,objcode,eph
   ssgeom,eph,sun,earth,phang,elong

   ; this is for a check
   appuldis,objcode,'500',jdgeo,sra,sdec,jdmin,sep, $
      starerr=0.001,info=ginfo,verbose=verbose,supp=supp
   
   jdstr,jdmin,0,jdmins
   if verbose then print,'check time ',jdmins

   jdcur=systime(/julian,/ut)
   jdstr,jdcur,300,jdcurs

   version = 'v'+jdcurs
   file_mkdir, version
   
   openw, lun, version+'/'+version+'.info', /get_lun

   jdstr,jdgeo,0,jds
   printf,lun,'Geocentric time of appulse ',jds,' UT'   
   printf,lun,ginfo.etrack,' km 1-sigma cross-track uncertainty'
   printf,lun,ginfo.etime,' sec 1-sigma down-track uncertainty'
   printf,lun,ginfo.diam1,' km diameter (5% albedo)'
   printf,lun,ginfo.kmscale,' km/arcsec'
   printf,lun,ginfo.speed,' km/sec'
   printf,lun,ginfo.err,' arcsec, 1-sigma ephemeris uncertainty'
   for i=0,n_elements(ginfo.summary)-1 do $
      printf,lun,ginfo.summary[i]

   close, lun
   free_lun,lun

   if verbose then begin
      print,'Geocentric time of appulse ',jds,' UT'   
      print,ginfo.etrack,' km 1-sigma cross-track uncertainty'
      print,ginfo.etime,' sec 1-sigma down-track uncertainty'
      print,ginfo.diam1,' km diameter (5% albedo)'
      print,ginfo.kmscale,' km/arcsec'
      print,ginfo.speed,' km/sec'
      print,ginfo.err,' arcsec, 1-sigma ephemeris uncertainty' 
   endif
   rastr,sra,6,srasf,sepchar=' '
   decstr,sdec,5,sdecsf,sepchar=' '

   jdstf=jd0s
   jdstf=strmid(jdstf,0,13)
   jdstf=repchar(jdstf,'/',' ')

   rastr,trimrank(eph[0,*]),5,ras,sepchar=' '
   decstr,trimrank(eph[1,*]),4,decs,sepchar=' '

   if verbose then print,'Writing file ',version+'/'+version+'.in'
   openw,lun,version+'/'+version+'.in',/get_lun
   printf,lun,'Occultation of ',starid,' by ',objname,' [',version,']'
   
   ; For reference, Jupiter's diameter is ~140,000 km
   if (ginfo.diam1 gt 200000.) then begin
      radius=99999.9
   endif else begin
      radius=ginfo.diam1/2.
   endelse

   printf,lun,radius,radius,1.0,'U',srasf,sdecsf,jdstf,'C',2000., $
      format='(2f10.1,1x,f4.1,a1,3x,a,1x,a,1x,a,1x,a,f6.1)'
   printf,lun,'       0.0    0.0000     0.000       0.0'
   for i=0,njd-1 do begin
      printf,lun,ras[i],decs[i],earth[i], $
         format='(a,6x,a,1x,f14.10)'
   endfor
   printf,lun,1,format='(a80,i1)'
   printf,lun,'U'

   close,lun
   free_lun,lun

   if protect then begin
      file_chmod, version+'/'+version+'.in',   /a_read, a_write=0, a_execute=0
      file_chmod, version+'/'+version+'.info', /a_read, a_write=0, a_execute=0
   endif

   if verbose then print,'End of mkocceph'
end
