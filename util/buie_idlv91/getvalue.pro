;+
; NAME:
;  getvalue
; PURPOSE:   (one line only)
;  Retrieve a value from a configuration information structure
; DESCRIPTION:
; CATEGORY:
;  Miscellaneous
; CALLING SEQUENCE:
;  getvalue,info,section,name,value
; INPUTS:
;  info - anonymous structure returned from loadini.pro
;  section - value of section to extract value from.  If set to an empty
;              string then the value is taken regardless of section
;  name - value of name to extract value from.  If set to an empty string
;              then the value is taken regardless of name
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;  TYPE - Integer code for variable type to return.  Default=7.  0 is ignored.
;  DEFAULT - Value to return if the key isn't found.  In this case, TYPE is
;              ignored and what you provide is returned as is.  If this is
;              not provided and the section is not 'global', then a value
;              from the [global] section is used for the default.
;  ARRAY - Flag, if set will lead to parsing the value, breaking the string
;              by spaces, and returning a vector of values that matches the
;              output type.  This keyword only works if both section and
;              name are provided and is intended for a single match.
; OUTPUTS:
;  value - string scalar or vector for values matching section and name
;            if nothing matches the output will be a scalar empty string
;            Note that type conversion is not applied if nothing is found.
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  Written by Marc W. Buie, Southwest Research Institute, 2020/06/22
;  2020/07/01, MWB, added DEFAULT keyword
;  2020/09/24, MWB, added inheritance from global section if default not given
;  2023/10/04, MWB, added ARRAY keyword
;-
pro getvalue,info,section,name,value,TYPE=type,DEFAULT=default,ARRAY=array

   self='getvalue: '
   if badpar(info,8,1,caller=self+'(info) ') then return
   if badpar(section,7,0,caller=self+'(section) ') then return
   if badpar(name,7,0,caller=self+'(name) ') then return
   if badpar(type,[0,2,3],0,caller=self+'(TYPE) ',default=7) then return
   if badpar(default,[0,1,2,3,4,5,7],[0,1],caller=self+'(DEFAULT) ', $
                                       type=type_default) then return
   if badpar(array,[0,1,2,3],0,caller=self+'(ARRAY) ',default=0) then return

   if section eq '' and name eq '' then begin
      value=info.value
   endif else if section eq '' then begin
      z=where(info.name eq name,count)
      if count eq 0 then begin
         value=''
      endif else begin
         value=trimrank(info.value[z])
      endelse
   endif else if name eq '' then begin
      z=where(info.section eq section,count)
      if count eq 0 then begin
         value=''
      endif else begin
         value=trimrank(info.value[z])
      endelse
   endif else begin
      z=where(info.section eq section and info.name eq name,count)
      if count eq 0 then begin
         value=''
      endif else if count eq 1 then begin
         tmpvalue=trimrank(info.value[z])
         if array then begin
            tmpvalue=strcompress(strtrim(tmpvalue,2))
            value=strsplit(tmpvalue,' ',/extract)
         endif else begin
            value = trimrank(tmpvalue)
         endelse
      endif else begin
         value=trimrank(info.value[z])
      endelse
   endelse

;   if value[0] eq '' and type_default ne 0 then begin
;      value=default
;      return
;   endif
   if value[0] eq '' then begin
      if section ne 'global' then begin
         getvalue,info,'global',name,gvalue,array=array
         value=gvalue 
      endif
      if value[0] eq '' and type_default ne 0 then begin
         value=default
         return
      endif
   endif

   if value[0] ne '' and type ne 0 and type ne 7 then value=fix(value,type=type)

end
