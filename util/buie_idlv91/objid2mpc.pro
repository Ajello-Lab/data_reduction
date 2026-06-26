;+
; NAME:
;  objid2mpc
; PURPOSE:   (one line only)
;  Convert a geteph object id to a Minor Planet Center name
; DESCRIPTION:
; CATEGORY:
;  Astrometry
; CALLING SEQUENCE:
;  objid2mpc,objid,type,name
; INPUTS:
;  objid - string with the geteph object id
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
; OUTPUTS:
; KEYWORD OUTPUT PARAMETERS:
; CONFIGURATION:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
;  Only some name can be converted.  if type comes back 'unknown' then the
;    input could not be converted.
; PROCEDURE:
; MODIFICATION HISTORY:
; Written by Marc W. Buie, Southwest Research Institute, 2021/04/08
;-
pro objid2mpc,objid,type,name

   self='objid2mpc: '
   if badpar(objid,7,0,caller=self+'(objid) ') then return

   category = strmid(objid,0,1)
   data = strmid(objid,1)

   if category eq 'A' then begin
      number=long(data)
      teststr=strn(number,length=strlen(data),padchar='0')
      if teststr eq data then begin
         type='permid'
         name=strn(number)
      endif else if strn(number) eq strmid(data,0,4) then begin
         type='provid'
         name=strn(number)+' '+strmid(data,4)
      endif else begin
         type='unknown'
         name=''
      endelse
   endif else begin
      type='unknown'
      name=''
   endelse

end
