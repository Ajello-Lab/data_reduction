;+
; NAME:
;  logerror
; PURPOSE:
;  Simplified error logging program.
; DESCRIPTION:
;
; CATEGORY:
;  Miscellaneous
; CALLING SEQUENCE:
;  logerror,file,msg
; INPUTS:
;  file  - string containing file name of error log file
;  msg   - string (scalar or array) to be written to log file
; OPTIONAL INPUT PARAMETERS:
;
; KEYWORD INPUT PARAMETERS:
;  CLEARLOG - Flag, if set, causes log file to be cleared before writing.
;               The default is that all messages are appended to the file.
;               If the file doesn't already exist this flag has no effect.
;  QUIET    - 0 - default, message printed to screen also, with CALLER tag.
;             1 - nothing printed to screen
;  CALLER   - Optional id string that identifies the calling program.
;
;  NOTAG    - Flag, if set will suppress the leading time+caller tag.  This
;               is meant for things that have syntactical structure that
;               is broken by introducing the tags, such as a multi-line mysql
;               query or command.
; OUTPUTS:
;
; KEYWORD OUTPUT PARAMETERS:
;
; COMMON BLOCKS:
;
; SIDE EFFECTS:
;
; RESTRICTIONS:
;
; PROCEDURE:
;
; MODIFICATION HISTORY:
;  Written by Marc W. Buie, Lowell Observatory, 2000/09/12
;  2001/04/18, MWB, changed systime call.
;  2021/03/20, MWB, added NOTAG option
;  2021/03/25, MWB, changed NOTAG operation
;-
pro logerror,file,msg,CLEARLOG=clearlog,QUIET=quiet,CALLER=caller,NOTAG=notag

   if badpar(file,7,0,caller='LOGERROR: (file) ') then return
   if badpar(msg,7,[0,1],caller='LOGERROR: (file) ',npts=nlines) then return
   if badpar(clearlog,[0,1,2,3],0,caller='LOGERROR: (CLEARLOG) ', $
                                  default=0) then return
   if badpar(quiet,[0,1,2,3],0,caller='LOGERROR: (QUIET) ', $
                               default=0) then return
   if badpar(notag,[0,1,2,3],0,caller='LOGERROR: (NOTAG) ', $
                               default=0) then return
   if badpar(caller,[0,7],0,caller='LOGERROR: (CALLER) ', $
                            default='') then return

   if clearlog then $
      openw,lun,file,/get_lun,width=132 $
   else $
      openw,lun,file,/get_lun,width=132,/append

   jdstr,systime(/julian,/utc),0,time

   if caller ne '' then time = time + ' ' + caller

   if notag then begin
      printf,lun,msg
      if not quiet then print,msg
   endif else begin
      for i=0,nlines-1 do begin
         printf,lun,time,' ',msg[i]
         if not quiet then print,msg[i]
      endfor
   endelse

   free_lun,lun

end
