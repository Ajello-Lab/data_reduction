;+
; NAME:
;  mkhull
; PURPOSE:   (one line only)
;  Create a smoothed outline based on an occultation limb profile
; DESCRIPTION:
;  This routine attempts to create a smooth outline that passes through a set
;    of occultation limb points.  It is not meant to be a fit but can still
;    be a useful tool rather than fitting strict mathematical shapes.
; CATEGORY:
;  Occultations
; CALLING SEQUENCE:
;  mkhull,xi,eta,hxi,heta
; INPUTS:
;  xi - Tangent plane coordinate of measured point on the limb
;  eta - Tangent plane coordinate of measured point on the limb
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;  NPTS - Number of points in the outline (default=100)
;  SIGMA - spline control factor (see spline.pro documentation), default=0.5
; OUTPUTS:
;  hxi - coordinates on the smoothed outline
;  heta - coordinates on the smoothed outline
; KEYWORD OUTPUT PARAMETERS:
; CONFIGURATION:
; COMMON BLOCKS:
; SIDE EFFECTS:
; PROCEDURE:
;  The Cartesian coordinates are converted to polar coordinates, sorted by
;    angle, padded by 2pi before and after the data with a copy of the data,
;    spline fitted, then the principal value is converted back to Cartesian
;    coordinates.
; RESTRICTIONS:
;  This won't work if there aren't enough points but where, when, and how
;    it will fail isn't easy to predict.  The coordinate origin should
;    be interior to the hull and works best if the origin is actually in
;    center (though this isn't strictly required).
; MODIFICATION HISTORY:
;  Written by Marc W. Buie, Southwest Research Institute, 2023/03/25
;-
pro mkhull,xi,eta,hxi,heta,NPTS=npts,SIGMA=sigma

   self='mkhull: '
   if badpar(xi,[4,5],1,caller=self+'(xi) ') then return
   if badpar(eta,[4,5],1,caller=self+'(eta) ') then return
   if badpar(npts,[0,2,3],0,caller=self+'(NPTS) ',default=100) then return
   if badpar(sigma,[0,4,5],0,caller=self+'(SIGMA) ',default=0.5) then return

   xy = transpose([[xi],[eta]])

   rt=cv_coord(from_rect=xy,/to_polar)

   r = trimrank(rt[1,*])
   t = trimrank(rt[0,*])
   r = [r,r,r]
   t = [t-2*!pi,t,t+2*!pi]

   idx=sort(t)
   r=r[idx]
   t=t[idx]

   tsyn=findgen(npts)/(npts-2)*2.0*!pi-!pi

   outline=spline(t,r,tsyn,sigma)
;   lowess,t,r,60./!radeg,newx=tsyn,outline,order=2

;plot,t,r,psym=-8,yr=[0,max(r)]
;oplot,tsyn,outline

   rt = transpose([[tsyn],[outline]])
   xy = cv_coord(from_polar=rt,/to_rect)
   
   hxi = trimrank(xy[0,*])
   heta = trimrank(xy[1,*])

end
