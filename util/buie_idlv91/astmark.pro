;+
; NAME:
;  astmark
; PURPOSE:   (one line only)
;  Annotate a graphic by marking an astronomical source in an image
; DESCRIPTION:
; CATEGORY:
;  Astronomy
; CALLING SEQUENCE:
;  astmark,x,y
; INPUTS:
;  x - position of object to mark
;  y - position of object to mark
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;  DATA - Flag, if set, positions are in data units (default if none specified)
;  NORMAL - Flag, if set, positions are in normal coordinates
;  DEVICE - Flaf, if set, positions are in device coordinates
;  GAP - distance to skip from position to start of mark (default=10)
;  LEN - length of mark (default=15)
;  DANG - angle between the two marks (default=120), in degrees
;  ROTANG - rotation angle of two-mark pattern, in degrees
;
;  Recognizes all standard plot graphics keywords.
;
; OUTPUTS:
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  Written by Marc W. Buie, Southwest Research Institute, 2015/03/17
;-
pro astmark,x,y,_EXTRA=_extra, $
      DATA=data,NORMAL=normal,DEVICE=device, $
      GAP=gap,LEN=len,DANG=dang,ROTANG=rotang

   self='astmark: '
   if badpar(x,[2,3,4,5],0,caller=self+'(x) ') then return
   if badpar(y,[2,3,4,5],0,caller=self+'(y) ') then return
   if badpar(data,[0,1,2,3],0,caller=self+'(DATA) ',default=0) then return
   if badpar(normal,[0,1,2,3],0,caller=self+'(NORMAL) ',default=0) then return
   if badpar(device,[0,1,2,3],0,caller=self+'(DEVICE) ',default=0) then return
   if badpar(gap,[0,2,3,4,5],0,caller=self+'(GAP) ',default=10) then return
   if badpar(len,[0,2,3,4,5],0,caller=self+'(LEN) ',default=15) then return
   if badpar(dang,[0,2,3,4,5],0,caller=self+'(DANG) ',default=90) then return
   if badpar(rotang,[0,2,3,4,5],0,caller=self+'(ROTANG) ',default=0) then return

   if data eq 0 and normal eq 0 and device eq 0 then data=1

   l1x=[gap,gap+len]
   l1y=[0.,0.]
   l1z=[0.,0.]
   rotpoint,l1x,l1y,l1z,'z',rotang,l1xp,l1yp,l1zp,/deg

   l2x=l1x
   l2y=l1y
   l2z=l1z
   rotpoint,l2x,l2y,l2z,'z',dang+rotang,l2xp,l2yp,l2zp,/deg

   cgplots,x+l1xp,y+l1yp,device=device,normal=normal,_STRICT_Extra=_extra
   cgplots,x+l2xp,y+l2yp,device=device,normal=normal,_STRICT_Extra=_extra

end
