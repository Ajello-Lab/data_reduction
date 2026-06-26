;+
; NAME:
;  rdevents
; PURPOSE:   (one line only)
;  Read an occultation event file
; DESCRIPTION:
; CATEGORY:
;  Occultations
; CALLING SEQUENCE:
;  rdevents,fn,team,body,type,fit,jd,err
; INPUTS:
;  fn - Name of file to read (usually just events.dat)
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
; OUTPUTS:
;  team - Name of team (short string, no blanks)  if there is nothing in the
;           file or no file exists, then team=!null and nvals=0
;  body - string, one character, meaningful values are:
;          x - ignore this line
;          0 - Primary object (this is the normal case)
;          1 - Secondary (or more) object (1-9 is transparently supported)
;          Any other single character is something to carry along but may
;            not be fittable but may need to be plotted
;  type - Single character that indicatest the type of event: D - disappearance
;            of the star, R = reappearance of the star
;  fit  - flag, 0 = don't fit point, 1 = fit point, this applies to the
;            fitting of an ellipse to the points
;  hfit - flag, 1 - use point for hull, 0 - don't use point for hull
;  jd   - UT Julian date of the event
;  err  - Uncertainty on the time in seconds
; KEYWORD OUTPUT PARAMETERS:
;  NVALS - number of valid entries read
; CONFIGURATION:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
;  This file is a companion to sites.dat (see rdsites.pro).  The team name
;   used in this file must match exactly with a line in the sites.dat file.
; 
; PROCEDURE:
; MODIFICATION HISTORY:
;  Written by Marc W. Buie, Southwest Research Institute, 2022/12/07
;  2023/08/17, MWB, changed to include input "hfit"
;-
pro rdevents,fn,team,body,type,fit,hfit,jd,err,NVALS=nvals

   team=!null
   nvals=0

   self='rdevents: '
   if badpar(fn,7,0,caller='(fn) ') then return

   if nofile(fn,'input event file') then return

   readcol,fn,team,body,type,fit,hfit,date,time,err, $
      format='a,a,a,i,i,a,a,f',count=nread
   if nread eq 0 then return

   jd=jdparse(date+' '+time)

   z=where(body ne 'x',nvals)

   if nvals gt 0 and nvals ne nread then begin
      team = team[z]
      body = body[z]
      type = type[z]
      fit  = fit[z]
      hfit = hfit[z]
      jd   = jd[z]
      err  = err[z]
   endif

end
