;+
; NAME:
;  logusage
; PURPOSE:   (one line only)
;  Record a usage line to a log file
; DESCRIPTION:
; CATEGORY:
;  File I/O
; CALLING SEQUENCE:
;  logusage,name
; INPUTS:
;  name - this is the name of the calling program.  Designed to use the
;           self variable commonly used with badpar calls.   Looks like
;           'name: '.  This rountine will using non-blanks characters
;           up to but not including the colon when printing to the log file.
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;  LOGFILE - name of the log file to append to.  Default is 'usage.log'
; OUTPUTS:
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
;  Each time this routine is called, a line is written to the log file.
;     hostname   name   Time (UT) of call
;  The host name is the simple (non-qualified) name of the host.
;  name is the input variable
;  The time is YYYY-MM-DDThh:mm:ss.  This can be parsed back to JD with jdparse
; MODIFICATION HISTORY:
;  Written by Marc W. Buie, Southwest Research Institute, 2018/10/13
;-
pro logusage,name,LOGFILE=logfile

   self='logusage: '
   if badpar(name,7,0,caller=self+'(name) ') then return
   if badpar(logfile,[0,7],0,caller=self+'(LOGFILE) ', $
                             default='usage.log') then return

   spawn,'hostname -s',host
   host=trimrank(host)

   tname=strtrim(name,2)
   pos=strpos(name,':')
   if pos gt 0 then tname=strmid(tname,0,pos)

   jd=systime(/julian,/ut)
   jdstr,jd,0,jds,timesep='-'
   jds=repchar(jds,' ','T')

   openw,lun,logfile,/get_lun,/append
   printf,lun,host,' ',tname,' ',jds
   free_lun,lun

end
