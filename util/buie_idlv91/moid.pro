;+
; NAME:
;  moid
; PURPOSE:   (one line only)
;  Compute the minimum orbit intersection distance between two orbits
; DESCRIPTION:
; CATEGORY:
;  Celestial Mechanics
; CALLING SEQUENCE:
;  distance = moid(orb1,orb2)
; INPUTS:
;  orb1 - Anonymous structure containing the first orbit.  The following
;           tags must be defined in the structure:
;             semi - semi-major axis in AU
;             ecc  - orbital eccentricity
;             inc  - orbital inclination (radians)
;             node - Longitude of the ascending node (radians)
;             arg  - Argument of perihelion (radians)
;             manom - mean anomoly (radians)
;             jdepoch - Julian Date of the epoch of the orbit
;
;  orb2 - Structure with the second orbit (same tags as the first orbit)
;
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
; OUTPUTS:
;  return value - the minimum orbital intersection distance between the two
;                   orbits, in AU.
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
;  mwb_moid - for internal use only
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  Written by Marc W. Buie, Southwest Research Institute, 2014/01/09
;-

; Input:
; jd0 - this is the time from which the calculations begin, it is not
;         very critical but really ought to be near the epoch of the
;         orbits.
; Output:
; jd1 - time near the MOID for orbit 1
; jd2 - time near the MOID for orbit 2
; mindis - distance between the orbits at these two times (in AU)
;
; Note: moid_setup must have already been run before this.
;
; This function is used to get the starting point for the final minimiation
;   operation.
pro moid_startpoint,jd0,jd1,jd2,mindis
   
   common mwb_moid,info

   npts=50

   dt1=dindgen(npts)/float(npts-1)*info.period1
   dt2=dindgen(npts)/float(npts-1)*info.period2

   jd1=jd0+dt1
   jd2=jd0+dt2

   elem2xyz,info.orb1.jdepoch,info.orb1.manom,info.orb1.arg,info.orb1.node, $
            info.orb1.inc,info.orb1.ecc,info.orb1.semi,jd1,x1,y1,z1,/ecliptic

   elem2xyz,info.orb2.jdepoch,info.orb2.manom,info.orb2.arg,info.orb2.node, $
            info.orb2.inc,info.orb2.ecc,info.orb2.semi,jd2,x2,y2,z2,/ecliptic

   p1=-1
   p2=-1
   mindis=10*(info.orb1.semi+info.orb2.semi)
   for i=0,npts-1 do begin
      dis=sqrt((x1[i]-x2)^2+(y1[i]-y2)^2+(z1[i]-z2)^2)
      z=where(dis eq min(dis))
      z=z[0]
      if dis[z] lt mindis then begin
         p1=i
         p2=z
         mindis=dis[z]
      endif
   endfor
   jd1=jd1[p1]
   jd2=jd2[p2]

;   setwin,0
;   plot,[0],/nodata,xr=[-1.1,1.1],yr=[-1.1,1.1],/iso
;   oplot,x1,y1
;   oplot,x2,y2
;   y1tmp=y1
;   z=where(z1 ge 0,count)
;   if count ne 0 then y1tmp[z]=1.e20
;   oplot,x1,y1tmp,color='707070'xl,max_value=1.e10
;   plots,x1[p1],y1[p1],color='000070'xl,psym=8
;   plots,x2[p2],y2[p2],color='000070'xl,psym=8
;
;   setwin,1
;   plot,[0],/nodata,xr=[-1.1,1.1],yr=[-1.1,1.1],/iso
;   oplot,x1,z1
;   oplot,x2,z2
;   z1tmp=z1
;   if count ne 0 then z1tmp[z]=1.e20
;   oplot,x1,z1tmp,color='707070'xl,max_value=1.e10
;   plots,x1[p1],z1[p1],color='000070'xl,psym=8
;   plots,x2[p2],z2[p2],color='000070'xl,psym=8
;
;   setwin,2
;   plot,[0],/nodata,xr=[-1.1,1.1],yr=[-1.1,1.1],/iso
;   oplot,y1,z1
;   oplot,y2,z2
;   oplot,y1,z1tmp,color='707070'xl,max_value=1.e10
;   plots,y1[p1],z1[p1],color='000070'xl,psym=8
;   plots,y2[p2],z2[p2],color='000070'xl,psym=8

end

; Input: the two orbit structures, this sets up the common block information
pro moid_setup,orb1,orb2

   common mwb_moid,info

   period1 = 365.25*orb1.semi^1.5
   period2 = 365.25*orb2.semi^1.5

   info={ $
      orb1: orb1, $
      period1: period1, $
      orb2: orb2, $
      period2: period2 $
      }

end

; jd1,jd2 are intended to be scalars
; jd=[jd1,jd2]
; This is the function that will be minimized to find the final MOID value
function moid_distance,jd

   common mwb_moid,info

   elem2xyz,info.orb1.jdepoch,info.orb1.manom,info.orb1.arg,info.orb1.node, $
            info.orb1.inc,info.orb1.ecc,info.orb1.semi,jd[0],x1,y1,z1,/ecliptic

   elem2xyz,info.orb2.jdepoch,info.orb2.manom,info.orb2.arg,info.orb2.node, $
            info.orb2.inc,info.orb2.ecc,info.orb2.semi,jd[1],x2,y2,z2,/ecliptic

   dis=sqrt((x1-x2)^2+(y1-y2)^2+(z1-z2)^2)

   return,dis

end

; This is the main program, it doesn't really need access to the common block.
function moid,orb1,orb2

;   common mwb_moid,info

   moid_setup,orb1,orb2
   moid_startpoint,jdparse('2014/01/01'),jd1,jd2,mindis

;   print,'1st pass MOID=',mindis

   start=[jd1,jd2]
   scale=[0.5,0.5]
   ftol=1.0e-6
   fitval=amoeba(ftol,function_name='moid_distance', $
                 p0=start,scale=scale,nmax=1000)

   mindis=moid_distance(fitval)

   return,mindis

end
