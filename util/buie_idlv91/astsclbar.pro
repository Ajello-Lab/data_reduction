;+
; NAME:
;  astsclbar
; PURPOSE:   (one line only)
;  Draw a scale bar on an astronomical image.
; DESCRIPTION:
; CATEGORY:
;  Astronomy
; CALLING SEQUENCE:
;  astsclbar,pscale
; INPUTS:
;  pscale - Image scale in arcsec/pixel
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;  THICK = changes thickness of line, default=2
;
;  Recognizes all standard plot graphics keywords.
;  
; OUTPUTS:
; KEYWORD OUTPUT PARAMETERS:
; CONFIGURATION:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  Written by Marc W. Buie, Southwest Research Institute, 2022/03/11
;  2024/03/10, MWB, added case for very large FOV scale bars
;-
pro astsclbar,pscale,_EXTRA=_extra,THICK=thick

   self='astsclbar: '
   if badpar(pscale,[2,3,4,5],0,caller=self+'(pscale) ') then return
   if badpar(thick,[0,2,3,4,5],0,caller=self+'(THICK) ',default=2) then return

;   print,!d.x_size

   if !d.x_size lt 100 then return

   x1 = round(0.1*!d.x_size)<50
   y1 = round(0.1*!d.y_size)<50

;   print,x1,y1

   angwidth = pscale*float(!d.x_size)
;   print,angwidth

   tenthwidth = 0.1*angwidth
;   print,tenthwidth

   arcsec=ceil(tenthwidth/10.0)*10
   arcmin=ceil(tenthwidth/60.0/10.0)*10
   arcdeg=ceil(tenthwidth/60.0/60.0/10.0)*10

   print,arcsec,arcmin,arcdeg

   if arcsec lt 60 then begin
      x2 = arcsec/pscale
      y2 = y1
      label=strn(arcsec)+'"'
   endif else if arcmin lt 60 then begin
      x2 = arcmin*60.0/pscale
      y2 = y1
      label=strn(arcmin)+"'"
   endif else if arcmin ge 60 then begin
      x2 = arcdeg*3600.0/pscale
      y2 = y1
      label=strn(arcdeg)+"d"
   endif

;   help,x2,y2

   cgplots,[x1,x2],[y1,y2],/device,_STRICT_Extra=_extra,thick=thick
   cgplots,x1*[1,1],y1+[-10,10],/device,_STRICT_Extra=_extra,thick=thick
   cgplots,x2*[1,1],y1+[-10,10],/device,_STRICT_Extra=_extra,thick=thick

   cgtext,(x1+x2)/2.0,y1+4,label,align=0.5,/device,_STRICT_Extra=_extra

end
