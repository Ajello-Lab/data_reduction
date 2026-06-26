;+
; NAME:
;  ellipse2hull
; PURPOSE:   (one line only)
;  Given an ellipse, sample it to a discrete curve
; DESCRIPTION:
; CATEGORY:
;  Miscellaneous
; CALLING SEQUENCE:
;  ellipse2hull,ellipse,x,y
; INPUTS:
;  ellipse - five element vector (used for fitting)
;            [0] = semi-major axis
;            [1] = semi-minor axis
;            [2] = x center of ellipse
;            [3] = y center of ellipse
;            [4] = position angle of ellipse [radians]
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;  NPTS - number of points in curve (default=101), first and last will be
;            the same so that it is a closed curve
; OUTPUTS:
;  x - discrete x points on curve
;  y - discrete y points on curve
; KEYWORD OUTPUT PARAMETERS:
; CONFIGURATION:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  Written by Marc W. Buie, Southwest Research Institute, 2022/04/06
;-
pro ellipse2hull,el,x,y,NPTS=npts

   self='ellipse2hull: '
   if badpar(el,[4,5],1,caller=self+'(ellipse) ',npts=nellip) then return
   if nellip ne 5 then begin
      x=!null
      y=!null
      print,self,'Ellipse array must have 5 values'
      return
   endif
   if badpar(npts,[0,2,3],0,caller=self+'(NPTS) ',default=101) then return

   phi = dindgen(npts)*2D*!dpi/(npts-1)

   x=el[2]+el[0]*cos(phi)*cos(el[4])+el[1]*sin(phi)*sin(el[4])
   y=el[3]-el[0]*cos(phi)*sin(el[4])+el[1]*sin(phi)*cos(el[4])

end
