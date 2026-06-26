;+
; NAME:
;  mkhull2
; PURPOSE:   (one line only)
;  Create a smoothed outline based on an occultation limb profile
; DESCRIPTION:
;  This routine attempts to create a smooth outline that passes near a set
;    of occultation limb points.  The points are converted to polar and
;    straight lines between points are used in this coordinate system.  The
;    line segments are smoothed by the use of a lowess call with the
;    smoothing width set to ARES.  It is not meant to be a fit but can still
;    be a useful tool rather than fitting strict mathematical shapes.
; CATEGORY:
;  Occultations
; CALLING SEQUENCE:
;  mkhull2,xi,eta,hxi,heta
; INPUTS:
;  xi - Tangent plane coordinate of measured point on the limb
;  eta - Tangent plane coordinate of measured point on the limb
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;  NPTS - Number of points in the outline (default=100)
;  ARES - angular resolution smoothing width (degrees), default=20
; OUTPUTS:
;  hxi - coordinates on the smoothed outline
;  heta - coordinates on the smoothed outline
; KEYWORD OUTPUT PARAMETERS:
;  SORTIDX - vector of indicies into xi,eta that are sorted by angle
; CONFIGURATION:
; COMMON BLOCKS:
; SIDE EFFECTS:
; PROCEDURE:
;  The Cartesian coordinates are converted to polar coordinates, sorted by
;    angle, padded by 2pi before and after the data with a copy of the data,
;    lowess fitted, then the principal value is converted back to Cartesian
;    coordinates.
; RESTRICTIONS:
;  This won't work if there aren't enough points but where, when, and how
;    it will fail isn't easy to predict.
; MODIFICATION HISTORY:
;  Written by Marc W. Buie, Southwest Research Institute, 2023/03/25
;  2023/05/17, MWB, cloned from mkhull
;  2024/06/05, MWB, added SORTIDX
;-
pro mkhull2,xi,eta,hxi,heta,NPTS=npts,ARES=ares,DEBUG=debug,SORTIDX=sortidx

   self='mkhull2: '
   if badpar(xi,[4,5],1,caller=self+'(xi) ',npts=nxi) then return
   if badpar(eta,[4,5],1,caller=self+'(eta) ') then return
   if badpar(npts,[0,2,3],0,caller=self+'(NPTS) ',default=100) then return
   if badpar(ares,[0,2,3,4,5],0,caller=self+'(SIGMA) ',default=20.0) then return
   if badpar(debug,[0,1,2,3],0,caller=self+'(DEBUG) ',default=0) then return

   ; initial center guess
   xi0 = mean(minmax(xi))
   eta0 = mean(minmax(eta))

   if debug then begin
      setwin,0
      plot,xi-xi0,eta-eta0,psym=8,xr=maxmin(xi-xi0),yr=minmax(eta-eta0),/iso
   endif

   xy = transpose([[xi-xi0],[eta-eta0]])

   rt=cv_coord(from_rect=xy,/to_polar)

   r = trimrank(rt[1,*])
   t = trimrank(rt[0,*])
   r = [r,r,r]
   t = [t-2*!pi,t,t+2*!pi]

   idx=sort(t)

   r=r[idx]
   t=t[idx]

   tsyn=findgen(npts)/(npts-2)*2.0*!pi-!pi
   tsyn = [tsyn-2*!pi,tsyn,tsyn+2*!pi]
   interp,t,r,tsyn,rsyn

   if ares gt 0. then begin
      lowess,tsyn,rsyn,ares/!radeg,rsynf,order=2
   endif else begin
      rsynf=rsyn
   endelse

   if debug then begin
      setwin,1
      plot,t,r,psym=8,yr=[0,max(r)]
      oplot,tsyn,rsyn,color=cpalette(1)
      oplot,tsyn,rsynf,color=cpalette(2)
   endif

   rt = transpose([[tsyn],[rsynf]])
   xy = cv_coord(from_polar=rt,/to_rect)
   
   hxi = trimrank(xy[0,npts:2*npts-1])+xi0
   heta = trimrank(xy[1,npts:2*npts-1])+eta0

   if debug then begin
      setwin,2
      plot,xi-xi0,eta-eta0,psym=8, $
         xr=maxmin([xi-xi0,hxi]),yr=minmax([eta-eta0,heta]),/iso
      oplot,hxi,heta,psym=3,color=cpalette(1)
   endif

   sortidx = idx[nxi:2*nxi-1] - nxi

end
