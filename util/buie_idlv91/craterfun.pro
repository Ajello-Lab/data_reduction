;+
; NAME:
;  craterfun
; PURPOSE:   (one line only)
;  Compute depth as a function of distance from center for a crater
; DESCRIPTION:
;  Based on equation 12 from Statler, Icarus, 202, 502-513 (2009).
; CATEGORY:
;  Miscellaneous
; CALLING SEQUENCE:
;  res=craterfun(r,d)
; INPUTS:
;  r - distance from crater center (same units as d)
;  d - crater diameter (arbitrary units)
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;  delta - width of the raised rim (default = d/20)
;  q     - depth to diameter ratio (default = 0.2)
; OUTPUTS:
;  crater depth (same units as d)
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  Written by Marc W. Buie, Southwest Research Institute, 2018/03/15 
;-
function craterfun,r,d,DELTA=delta,Q=q

   self='craterfun: '
   if badpar(r,[2,3,4,5],[0,1,2],caller=self+'(r) ') then return,!null
   if badpar(d,[2,3,4,5],0,caller=self+'(d) ') then return,!null
   if badpar(delta,[0,2,3,4,5],0,caller=self+'(delta) ', $
                                 default=d/20.0) then return,!null
   if badpar(q,[0,2,3,4,5],0,caller=self+'(q) ', $
                             default=0.2) then return,!null

   depth=q*d*(1.0-4.0*(r/d)^2)

   z=where(r gt d/2.0+3.0*delta,count)
   if count ne 0 then depth[z]=0.0

   z=where(r gt d/2.0 and r le d/2.0+3.0*delta,count)
   if count ne 0 then begin
      fac=exp(-0.5*((r[z]-d/2.0)/delta)^2)
      depth[z]=fac*depth[z]
   endif

   return,depth

end
