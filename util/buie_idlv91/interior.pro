;+
; NAME:
;  interior
; PURPOSE:   (one line only)
;  Compute if a point is interior to a polygon
; DESCRIPTION:
; CATEGORY:
;  Miscellaneous
; CALLING SEQUENCE:
;  result = interior(xpoly,ypoly,x,y)
; INPUTS:
;   xpoly - X coordinates of polygon verticies, must be a vector and really
;             only makes sense if it has four or more points.  Note that the
;             first and last points must be identical so that the shape is
;             closed (this is not checked).  No check are performed to ensure
;             a sensible polygon.
;   ypoly - Y coordinates of polygon verticies, length of this vector must
;             match xpoly and the first and last points must also match.
;   x     - X coordinate of a point to test.  May be either a scalar or vector
;   y     - Y coordinate of a point to test.  May be either a scalar or vector
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
; OUTPUTS:
;   return is a byte-type variable with the same rank and size as the input
;      x,y values.  1B indicates point is interior to polygon and 0 indicates
;      point is outside the polygon.  Critical cases are treated as interior.
; KEYWORD OUTPUT PARAMETERS:
; CONFIGURATION:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  Written by Marc W. Buie, Southwest Research Institute, 2022/01/29
;   This program is based on the approach implemented by David Fanning
;           http://www.idlcoyote.com/programs/inside.pro
;   with stylistic changes to conform to my library as well as some feature
;   enhancements for performance improvements, most notably applying a
;   check for trivial cases before doing the full calculation.
;-
function interior,xpoly,ypoly,x,y

   self='interior: '
   if badpar(xpoly,[2,3,4,5],1,caller=self+'(xpoly) ', $
                               npts=nxpoly) then return,!null
   if badpar(ypoly,[2,3,4,5],1,caller=self+'(ypoly) ', $
                               npts=nypoly) then return,!null
   if badpar(x,[2,3,4,5],[0,1],caller=self+'(x) ', $
                               npts=nx) then return,!null
   if badpar(y,[2,3,4,5],[0,1],caller=self+'(y) ', $
                               npts=ny) then return,!null

   if nxpoly ne nypoly then begin
      print,self,'Length of x and y polygon vectors must match.'
      return,!null
   endif

   if nxpoly lt 4 then begin
      print,self,'Length of x and y polygon vectors must be >= 4.'
      return,!null
   endif

   if nx ne ny then begin
      print,'Length of x and y input point(s) must match.'
      return,!null
   endif

   value=bytarr(nx)

   npoly = nxpoly
   xpolymin=min(xpoly)
   ypolymin=min(ypoly)
   xpolymax=max(xpoly)
   ypolymax=max(ypoly)

   zt=where(x ge xpolymin and x le xpolymax and $
           y ge ypolymin and y le ypolymax, nt)

   txpoly=[xpoly,xpoly[0]]
   typoly=[ypoly,ypoly[0]]

   if nt eq 0 then return,trimrank(value)

   x1 = txpoly[0:-2] # replicate(1,nt) - replicate(1,npoly) # x[zt]
   y1 = typoly[0:-2] # replicate(1,nt) - replicate(1,npoly) # y[zt]
   x2 = txpoly[1:-1] # replicate(1,nt) - replicate(1,npoly) # x[zt]
   y2 = typoly[1:-1] # replicate(1,nt) - replicate(1,npoly) # y[zt]

   dp = x1*x2 + y1*y2 ; Dot-product
   cp = x1*y2 - y1*x2 ; Cross-product
   theta = atan(cp,dp)

   z = where(abs(total(theta,1)) gt 0.01, count)
   if (count gt 0) then value[zt[z]] = 1B

   return,trimrank(value)

end
