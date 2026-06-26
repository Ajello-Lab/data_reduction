;+
; NAME:
;  wfc3_smear
; PURPOSE:   (one line only)
;  Compute a smear kernel for a star while tracking a solar system object
; DESCRIPTION:
; CATEGORY:
;  Astrometry
; CALLING SEQUENCE:
;  wfc3_smear,data,ocode,x,y,kernel
; INPUTS:
;  data   - anonymous structure produced by rdwfc3.pro
;  ocode  - NAIF object code
;  x      - x position of source
;  y      - y position of source
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
; OUTPUTS:
;  kernel - smear kernel
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
;  This only works with spice kernel orbits.
; PROCEDURE:
; MODIFICATION HISTORY:
;  2020/06/24, Written by Marc W. Buie, Southwest Research Institute
;-
pro wfc3_smear,data,ocode,x,y,kernel

   self='wfc3_smear: '
   if badpar(data,8,1,caller=self+'(data) ') then return
   if badpar(ocode,7,0,caller=self+'(ocode) ') then return
   if badpar(x,[2,3,4,5],0,caller=self+'(x) ') then return
   if badpar(y,[2,3,4,5],0,caller=self+'(y) ') then return

   objcode = 'R-48-'+ocode

   npts=401
   dt=(dindgen(npts)-(npts-1)/2)/double(npts-1)*data.exptime
   jd = data.jdmid+dt/86400.0d0

   ephem,jd,'500',2,objcode,eph
   ra=trimrank(eph[0,*])
   dec=trimrank(eph[1,*])
   dra=ra-ra[0]
   ddec=dec-dec[0]

   xy2ad,x,y,data.astinfo,tra,tdec
   tra=tra-dra*180.0d0/!dpi
   tdec=tdec-ddec*180.0d0/!dpi
   ad2xy,tra,tdec,data.astinfo,xsm,ysm
   dt=(jd-jd[0])*86400.0d0
   dx=xsm-xsm[0]
   dy=ysm-ysm[0]
   jitterk,dt,dx,dy,data.exptime,kernel,/expcenter

end
