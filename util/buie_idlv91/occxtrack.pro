;+
; NAME:
;  occxtrack
; PURPOSE:   (one line only)
;  Compute cross-track and other geometric information for an occultation
; DESCRIPTION:
; CATEGORY:
;  Occultations
; CALLING SEQUENCE:
;  occxtrack
; INPUTS:
;  All inputs are fixed name files.
;   xtrack.in   - contains general information about the occultation
;                   leading and trailing blanks on a line do not matter
;                   any lines after the required set are ignored
;     line 1 - Name of event
;     line 2 - Object code (for geteph, see ephem.pro)
;     line 3 - RA of the star (sexigesimal string)
;     line 4 - Dec of the star (sexigesimal string)
;     line 5 - geocentric UT date and time of appulse
;     line 6 - 1-sigma down-track uncertainty (in seconds)
;     line 7 - 1-sigma cross-track uncertainty (in km)
;
;   chords.dat - list of all stations, same file used for occprof
;     col 1 - siteid (short string)
;     col 2 - flag, 'y' means good data, 'n' means no good data
;     col 3 - latitude (degrees)
;     col 4 - east longitude (degrees)
;     col 5 - altitude (m)
;     col 6 - date of occultation (x if no occultation seen)
;     col 7 - time of disappearance (x if no occultation seen)
;     col 8 - time or reappearance (x if no occultation seen)
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;  FILE - name of file to read with sites.  default = sites.dat
;                This can still read a chords.dat file but this feature
;                   is deprecated and all usage needs to shift to the
;                   new file setup.
;  OUTFILE - name of file to write to, default = crosstrack.dat
;  FNCONFIG - name of configuration file (overrides xtrack.in),
;              the default is config.ini but if the configuration file
;              is not found then operation reverts to using xtrack.in
;              The minimal content of the config file must have:
;   [global]
;     event   - string with short ID oame of event (ex: OR20200914)
;     objectid - object code (see ephem.pro)
;     ora      - HH:MM:SS.sss position of occultation star at epoch of event
;     odec     - +DD:MM:SS.sss position of occultation star at epoch of event
;     geomid   - YYYY-MM-DD HH:MM:SS.s UT time of geocentric mid-time of event
;     tsig     - 1-sigma down-track error in seconds
;     xsig     - 1-sigma cross-track error in km
;
; OUTPUTS:
;  All outputs are fixed name files.
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  Written by Marc W. Buie, Southwest Research Institute, 2020/01/31
;  2020/09/28, MWB, added FILE keyword
;  2021/03/10, MWB, added OUTFILE keyword
;  2021/05/07, MWB, added FNCONFIG keyword
;  2022/12/20, MWB, changed to use sites.dat instead of chords.dat
;-
compile_opt strictarrsubs
pro occxtrack,VERBOSE=verbose,FILE=in_file,OUTFILE=outfile,FNCONFIG=fnconfig

   self='occxtrack: '
   if badpar(verbose,[0,1,2,3],0,caller=self+'(VERBOSE) ',default=0) then return
   if badpar(in_file,[0,7],0,caller=self+'(FILE) ', $
                             default='sites.dat') then return
   if badpar(outfile,[0,7],0,caller=self+'(OUTFILE) ', $
                             default='crosstrack.dat') then return
   if badpar(fnconfig,[0,7],0,caller=self+'(FNCONFIG) ', $
                             default='config.ini') then return

   if nofile(in_file,'Site location and times') then return

   if exists(fnconfig) then begin
      loadini,cinfo,file=fnconfig
      getvalue,cinfo,'global','event',event
      getvalue,cinfo,'global','objectid',objectid
      getvalue,cinfo,'global','ora',ras
      getvalue,cinfo,'global','odec',decs
      ra=raparse(ras)
      dec=decparse(decs)
      getvalue,cinfo,'global','geomid',geomid
      jdgeomid=jdparse(geomid)
      getvalue,cinfo,'global','tsig',tsig,type=5
      getvalue,cinfo,'global','xsig',xsig,type=5
   endif else begin
      if nofile('xtrack.in','Occultation description') then return
      rdxtrack,event,objectid,ra,dec,jdgeomid,tsig,xsig
   endelse

   ; Load site information
   readcol,in_file,siteid,lat,lon,alt,format='a,x,d,d,d',count=nsites
   lat_r=lat/180.0d0*!dpi
   lon_r=lon/180.0d0*!dpi

   openw,lun,outfile,/get_lun
   for i=0,nsites-1 do begin
      obsinfo,'G**',obs,lat_r[i],lon_r[i],alt[i],siteid[i]; ,/west
;      obs={obscode: 'G**', name:'mobile', $
;           lat: lat_r[i], lon: -lon_r[i], alt: alt[i]}
      appuldis,objectid,obs,jdgeomid,ra,dec,jdmin,sep, $
         info=info,prederr=[xsig,tsig]
      if sep lt 0 then begin
         print,'No valid appulse for ',siteid[i]
         continue
      endif
      jdstr,jdmin,3,jdmins
      offset=sep*info.kmscale
      if info.pa gt 0 then offset = -1*offset
      printf,lun,siteid[i],offset,jdmins,sep*info.kmscale,info.pa, $
         format='(a-10,1x,f9.2,1x,a,1x,f6.1,1x,f6.1)'
      if verbose then $
         print,siteid[i],offset,' km, ',info.kmscale,' km/arcsec, ', $
               offset/info.kmscale*1000.0,' mas'
   endfor
bailout:
   free_lun,lun

end
