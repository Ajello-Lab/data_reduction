;+
; NAME:
;  wcsarrows
; PURPOSE:   (one line only)
;  Draw sky-plane orientation arrows on an image based on WCS information
; DESCRIPTION:
; CATEGORY:
;  Astronomy
; CALLING SEQUENCE:
;  wcsarrows,info
; INPUTS:
;  info - anonymous structure that carries the astrometric information.
;            You can provide a structure returned from the Astron library
;            (extast) or from my library (astinfo).
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;  X - x location of vertex of drawn arrows (default=upper right)  DEVICE COORDS
;  Y - y location of vertex of drawn arrows (default=upper right)  DEVICE COORDS
;  PA - position angle of one extra vector to draw, typically this is for
;         denoting the projected north pole vector.  This angle is given in
;         degrees east of north.  The default is to not draw this vector.
;  LABEL - label for the extra vector, ignored if PA not specified.
;            default = 'Np'
;  NOPLOT - Flag, if set will do all the computations but skip the actual
;             display.   This is useful if you want the ainfo structure
;             but aren't plotting.
; OUTPUTS:
;  All output is in the form of plotted information on the current window
; KEYWORD OUTPUT PARAMETERS:
;  AINFO - anonymous structure that contains information computed here
;            x: device coordinate of center of compass
;            y: device coordinate of center of compass
;            north: angle for North vector, in degrees (0 is to the right)
;            east:  angle for East vector, in degrees
;            pscale: Image scale in arcsec/pixel, computed in center of image
;            pang:  angle for extra vector, if not provided this is the
;                      the same as north.
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  2015/10/07, Written by Marc W. Buie, Southwest Research Institute
;  2017/04/20, MWB, added pscale to output structure
;  2017/07/05, MWB, added support for use of !order for image orientation.
;  2021/01/18, MWB, added NOPLOT keyword
;  2021/02/01, MWB, fixed bug when using NOPLOT keyword
;  2021/03/31, MWB, fixed incorrect plate scale calculation for my solutions
;-
pro wcsarrows,info,X=x,Y=y,PANG=pang,LABEL=label,COLOR=color,PACOLOR=pacolor, $
              AINFO=ainfo,NOPLOT=noplot

   self='wcsarrows: '
   if badpar(info,8,1,caller=self+'(info) ') then return
   if badpar(noplot,[0,1,2,3],0,caller=self+'(NOPLOT) ',default=0) then return

   def_x = !d.x_size - 80
   def_y = !d.y_size - 80

   if badpar(x,[0,2,3,4,5],0,caller=self+'(X) ',default=def_x) then return
   if badpar(y,[0,2,3,4,5],0,caller=self+'(Y) ',default=def_y) then return
   if badpar(pang,[0,2,3,4,5],0,caller=self+'(PA) ',type=patype) then return
   if badpar(label,[0,7],0,caller=self+'(LABEL) ',default='P') then return

   tags=tag_names(info)
   z=where(tags eq 'RENORMFAC',count)

   if count eq 1 then begin

      nra=[0.,0.]+info.raref
      ndec=[0.,5/3600.0d0*!dpi/180.0d0]+info.decref
      era=[0.,5/3600.0d0*!dpi/180.0d0]+info.raref
      edec=[0.,0.]+info.decref

      ; compute a pair of points that will point to the north and use that
      ;   to get the angle toward the north
      astcvt,'rd',nra,ndec,info,'xy',xn,yn

      ; do the same for something to the east
      astcvt,'rd',era,edec,info,'xy',xe,ye

   endif else begin
      nra=[0.,0.]+info.crval[0]
      ndec=[0.,5/3600.0d0]+info.crval[1]
      era=[0.,5/3600.0d0]+info.crval[0]
      edec=[0.,0.]+info.crval[1]

      ad2xy,nra,ndec,info,xn,yn
      ad2xy,era,edec,info,xe,ye
   endelse

   if !order eq 1 then begin
      yn=-1*yn
      ye=-1*ye
   endif

   pscale = 5.0/sqrt((xn[1]-xn[0])^2+(yn[1]-yn[0])^2)

   north=atan(yn[1]-yn[0],xn[1]-xn[0])*180.d0/!dpi
   east=atan(ye[1]-ye[0],xe[1]-xe[0])*180.d0/!dpi

   if not noplot then begin
      one_arrow,x,y,north,'N',color=color
      one_arrow,x,y,east,'E',color=color
   endif

   if patype ne 0 then begin
      if not noplot then $
         one_arrow,x,y,pang+north,label,color=pacolor
      outpang=pang+north
   endif else begin
      outpang=north
   endelse

   ainfo={ $
      x:     x, $
      y:     y, $
      north: north, $
      east:  east, $
      pscale: pscale, $
      pang:  outpang $
      }

end
