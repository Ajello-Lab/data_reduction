;+
; NAME:
;  rhosc
; PURPOSE:   (one line only)
;  Compute rhosinp,rhocosp from observatory lat,alt
; DESCRIPTION:
;  This is from a formalism used by Ted Bowell, the Minor Planet Center,
;    Larry Wasserman and probably others to compute topcentric X, Y, Z.
; CATEGORY:
;  Astronomy
; CALLING SEQUENCE:
;  rhosc,lat,alt,rhosinp,rhocosp
; INPUTS:
;  lat - observatory latitude in WGS84 datum [radians]        (scalar or vector)
;  alt - observatory altitude above reference datum [meters]  (match lat)
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
; OUTPUTS:
;  rhosinp - units and values as tabulated by MPC for observatories
;  rhocosp - units and values as tabulated by MPC for observatories
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
;  No protections on input for speed of computation
; PROCEDURE:
; MODIFICATION HISTORY:
;  Written by Marc W. Buie, Southwest Research Institute, 2019/10/21
;-
pro rhosc,lat,alt,rhosinp,rhocosp,REVERSE=reverse

   if keyword_set(reverse) then begin

print,'input rsp,rcp',rhosinp,rhocosp

      lat = atan(rhosinp,rhocosp)
      ctphi=cos(2.0d0*lat)
      cfphi=cos(4.0d0*lat)
      scon=0.99497430d0-0.00167078d0*ctphi+0.00000210d0*cfphi
      ccon=1.00167993d0-0.00168204d0*ctphi+0.00000212d0*cfphi
      htcor=rhosinp/sin(lat)-scon
      const=0.156785d-06
      alt=htcor/const
print,'first lat,alt',lat,alt

      for i=0,4 do begin
;print,i
      rhosc,lat,alt,newrhosinp,newrhocosp
;print,'new rsp,rcp  ',newrhosinp,newrhocosp

      drcp = newrhocosp - rhocosp
;print,'drcp         ',drcp

      ;compute partial of rsp or rcp wrt lat
;d rcp / d lat = (ccon+ht*const)*sin(lat)
      ctphi=cos(2.0d0*lat)
      cfphi=cos(4.0d0*lat)
      ccon=1.00167993d0-0.00168204d0*ctphi+0.00000212d0*cfphi
      dlat = drcp/((ccon+alt*const)*sin(lat))
;print,'dlat         ',dlat

      newlat = lat+dlat
;print,'new lat      ',newlat

;      rhocosp=newrhocosp
;      rhosc,newlat,alt,newrhosinp,newrhocosp
;print,'new rsp,rcp  ',newrhosinp,newrhocosp

;      drcp = newrhocosp - rhocosp
;print,'drcp         ',drcp

      ;compute partial of rsp or rcp wrt alt
;d rcp / d ht = const*cos(lat)
      dalt = drcp/(const*cos(newlat))
;print,'dalt         ',dalt
      newalt = alt + dalt
;print,'new alt      ',newalt

      lat = newlat
      alt = newalt
      rhosinp=newrhosinp
      rhocosp=newrhocosp
print,i,'      lat,alt',lat,alt,drcp

      endfor


   endif else begin

      ctphi=cos(2.0d0*lat)
      cfphi=cos(4.0d0*lat)

      ; these are for the J2000 flattening of 1/298.257
      scon=0.99497430d0-0.00167078d0*ctphi+0.00000210d0*cfphi
      ccon=1.00167993d0-0.00168204d0*ctphi+0.00000212d0*cfphi
      htcor=0.156785d-06*alt

      rhosinp=(scon+htcor)*sin(lat)
      rhocosp=(ccon+htcor)*cos(lat) 
   endelse

end
