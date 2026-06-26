;+
; NAME:
;  rdxtrack
; PURPOSE:   (one line only)
;  Read an occultation definition file
; DESCRIPTION:
; CATEGORY:
;  File I/O
; CALLING SEQUENCE:
;  rdxtrack,event,objectid,ra,dec,jdgeo,tsig,xsig
; INPUTS:
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;  FILE - File to read, default='xtrack.in'
; OUTPUTS:
;  event    - Name of the event
;  objectid - Object code (see ephem.pro)
;  ra       - RA of star [radians]
;  dec      - Dec of star [radians]
;  jdgeo    - JD (UTC) of time of geocentric appulse [days]
;  tsig     - 1-sigma uncertainty of event, downtrack [sec]
;  xsig     - 1-sigma cross-track uncertainty of centerline [km]
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  Written by Marc W. Buie, Southwest Research Institute, 2020/03/29
;-
compile_opt strictarrsubs
pro rdxtrack,event,objectid,ra,dec,jdgeo,tsig,xsig,FILE=file

   self='rdxtrack: '
   if badpar(file,[0,7],0,caller=self+'(FILE) ',default='xtrack.in') then return

   openr,lun,'xtrack.in',/get_lun
   line=''
   readf,lun,line,format='(a)'
   event=strtrim(line,2)
   readf,lun,line,format='(a)'
   objectid=strtrim(line,2)
   readf,lun,line,format='(a)'
   ras=strtrim(line,2)
   readf,lun,line,format='(a)'
   decs=strtrim(line,2)
   readf,lun,line,format='(a)'
   jdgeomids=strtrim(line,2)
   readf,lun,line,format='(a)'
   tsig=float(strtrim(line,2))
   readf,lun,line,format='(a)'
   xsig=float(strtrim(line,2))
   free_lun,lun
   ra=raparse(ras)
   dec=decparse(decs)
   jdgeo=jdparse(jdgeomids)

end
