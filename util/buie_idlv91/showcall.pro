;+
; NAME:
;  showcall
; PURPOSE:   (one line only)
;  Print a copy of the command line
; DESCRIPTION:
;  This attempts to show the calling sequence for a procedure or function.
;    For some forms of this call, the printed command can be executed
;    directly.
; CATEGORY:
;  Utility
; CALLING SEQUENCE:
;  showcall,name,arg1,arg2,...,arg25
; INPUTS:
;  arg# - information about an argument
;          non-structure input:
;             value to be shown on command line
;             scalar: shown as the number
;             vector: shows as [a,b,c,...]
;             array: not supported (see structure input)
;          anonymous structure input, tags:
;             name - string
;             value - handled the same as a value input
;            This is intended to mostly be used to show keyword values
;              and the name tag is the name of the keyword.   If the name
;              is blank it will suppress the quotes around the value if
;              value is a string.  This lets you provide an argument by
;              a name rather than a value.
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
; OUTPUTS:
; KEYWORD OUTPUT PARAMETERS:
;  RESULT - str with the resulting information as printed, useful for log files
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  Written by Marc W. Buie, Southwest Research Institute, 2017/11/16
;  2021/03/16, MWB, added RESULT output keyword
;-
function showcall_val2str,arg,NOQUOTE=noquote,FORMAT=format

   if not keyword_set(format) then format=''

   type=size(arg,/type)
   rank=size(arg,/n_dimensions)
   str='XXXX'

   if rank eq 0 then begin
      if type eq 0 then begin
         str=''
      endif else if type eq 1 then begin
         str=strcompress(string(fix(arg),format=format),/remove_all)+'B'
      endif else if type ge 2 and type le 3 then begin
         str=strcompress(string(arg,format=format),/remove_all)
         if abs(arg) gt 32767L then str=str+'L'
      endif else if type ge 4 and type le 5 then begin
         str=strcompress(string(arg,format=format),/remove_all)
      endif else if type eq 6 then begin
         str='complex'+strcompress(string(arg,format=format),/remove_all)
      endif else if type eq 7 then begin
         if keyword_set(noquote) then $
            str=arg $
         else $
            str=quote(arg)
      endif
   endif else if rank eq 1 then begin
      if type eq 1 then begin
         str=strcompress(string(fix(arg),format=format),/remove_all)+'B'
         str='['+strjoin(str,',')+']'
      endif else if type ge 2 and type le 3 then begin
         str=strcompress(string(fix(arg),format=format),/remove_all)
         str='['+strjoin(str,',')+']'
      endif
   endif

   return,str

end

function showcall_string,arg

   type=size(arg,/type)
   rank=size(arg,/n_dimensions)

   if type eq 8 then begin
      tags=tag_names(arg)
      z=where(tags eq 'FORMAT',count)
      if count ne 0 then format=arg.format else format=''
      if arg.name eq '' then begin
         str=showcall_val2str(arg.value,/noquote,format=format)
      endif else begin
         str=arg.name+'='+showcall_val2str(arg.value,format=format)
      endelse
   endif else begin
      str=showcall_val2str(arg)
   endelse

   return,str
end

pro showcall,name,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16, $
                  a17,a18,a19,a20,a21,a22,a23,a24,a25,RESULT=str

   str=name

   newstr=showcall_string(a1)
   if newstr ne '' then str=str+','+newstr
   newstr=showcall_string(a2)
   if newstr ne '' then str=str+','+newstr
   newstr=showcall_string(a3)
   if newstr ne '' then str=str+','+newstr
   newstr=showcall_string(a4)
   if newstr ne '' then str=str+','+newstr
   newstr=showcall_string(a5)
   if newstr ne '' then str=str+','+newstr
   newstr=showcall_string(a6)
   if newstr ne '' then str=str+','+newstr
   newstr=showcall_string(a7)
   if newstr ne '' then str=str+','+newstr
   newstr=showcall_string(a8)
   if newstr ne '' then str=str+','+newstr
   newstr=showcall_string(a9)
   if newstr ne '' then str=str+','+newstr
   newstr=showcall_string(a10)
   if newstr ne '' then str=str+','+newstr
   newstr=showcall_string(a11)
   if newstr ne '' then str=str+','+newstr
   newstr=showcall_string(a12)
   if newstr ne '' then str=str+','+newstr
   newstr=showcall_string(a13)
   if newstr ne '' then str=str+','+newstr
   newstr=showcall_string(a14)
   if newstr ne '' then str=str+','+newstr
   newstr=showcall_string(a15)
   if newstr ne '' then str=str+','+newstr
   newstr=showcall_string(a16)
   if newstr ne '' then str=str+','+newstr
   newstr=showcall_string(a17)
   if newstr ne '' then str=str+','+newstr
   newstr=showcall_string(a18)
   if newstr ne '' then str=str+','+newstr
   newstr=showcall_string(a19)
   if newstr ne '' then str=str+','+newstr
   newstr=showcall_string(a20)
   if newstr ne '' then str=str+','+newstr
   newstr=showcall_string(a21)
   if newstr ne '' then str=str+','+newstr
   newstr=showcall_string(a22)
   if newstr ne '' then str=str+','+newstr
   newstr=showcall_string(a23)
   if newstr ne '' then str=str+','+newstr
   newstr=showcall_string(a24)
   if newstr ne '' then str=str+','+newstr
   newstr=showcall_string(a25)
   if newstr ne '' then str=str+','+newstr

   print,str

end
