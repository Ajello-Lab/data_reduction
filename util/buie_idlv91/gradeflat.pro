;+
; NAME:
;  gradeflat
; PURPOSE:   (one line only)
;  Automatic quality grading of a set of CCD flat images against a superflat
; DESCRIPTION:
; CATEGORY:
;  CCD data processing
; CALLING SEQUENCE:
;  gradeflat,cube,flat,grade,fail
; INPUTS:
;  cube - 3-d cube of original flat images, if you need to subtract overscan
;              and crop, this should be done prior to building the cube
;  flat - 2-d image which is the superflat frame
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;  DISPLAY  - Flag, if set, generates some diagnostic plots
;  MAXSIG   - Maximum sigma excursion allowed for an image (default=3) before
;                it is flagged as bad.
; OUTPUTS:
;  grade - This is the grade of this cube/flat pair.  This is related to
;             a unit of standard deviation.  A bad set of data will have
;             a large value, typically much greater than 3.   A good set of
;             data will be comfortably less than 3.
;  fail  - This is the number of frames that fail the test of being consistent
;             with the ensemble of frames.  This number will be somewhere
;             between 0 and the number of frames in the cube.  Don't expect
;             this number to be zero but it should be small.
; KEYWORD OUTPUT PARAMETERS:
;  SIGRATIO - Standard deviation of the individual good image grades.
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
;  This tool is ok but could be better.
; PROCEDURE:
; MODIFICATION HISTORY:
;  Written by Marc W. Buie, Southwest Research Institute, 2015/04/26
;-
pro gradeflat,cube,flat,grade,fail,DISPLAY=display, $
       SIGRATIO=sigratio,MAXSIG=maxsig

   self='gradeflat: '
   if badpar(cube,[4,5],3,caller=self+'(cube) ') then return
   if badpar(flat,[4,5],2,caller=self+'(flat) ') then return
   if badpar(display,[0,1,2,3],0,caller=self+'(DISPLAY) ',default=0) then return
   if badpar(maxsig,[0,2,3,4,5],0,caller=self+'(MAXSIG) ',default=3.0) then return

   sz=size(cube,/dimen)
   nx=sz[0]
   ny=sz[1]
   nf=sz[2]
   npix=float(nx)*float(ny)

   fno=indgen(nf)

   xsub=17
   ysub=17
   dw=13
   dw=13
   npix=(2*dw+1)^2
   xidx=indgen(nx)
   yidx=indgen(ny)

   xpos=fix(indgen(xsub)*nx/xsub+0.5*nx/xsub)
   ypos=fix(indgen(ysub)*ny/ysub+0.5*ny/ysub)

   nsub=xsub*ysub

   skewerr=sqrt(6.0/nsub)
   kurterr=sqrt(24.0/nsub)

   m1=fltarr(nf) ; mean
   m2=fltarr(nf) ; standard deviation
   m3=fltarr(nf) ; skew
   m4=fltarr(nf) ; kurtosis
   imavg=fltarr(nf) ; mean of each of the flattened flats
   imsig=fltarr(nf) ; standard deviation of each flattened flat
   avg=fltarr(nf,nsub)
   sig=fltarr(nf,nsub)
   icut=0
   for i=0,nf-1 do begin
      ratio=cube[*,*,i]/flat
      skysclim,ratio,d1,d2,mratio,msig,npts=60000
      imavg[i]=mratio
      imsig[i]=msig
      rdev=ratio-mratio
      if display then showsrc,ratio,window=1
      num=0

      ; get local avgerages and standard deviations
      for j=0,xsub-1 do begin
         for k=0,ysub-1 do begin
            i0=xpos[j]-dw
            i1=xpos[j]+dw
            j0=ypos[k]-dw
            j1=ypos[k]+dw
            robomean,rdev[i0:i1,j0:j1],3.0,0.5,meanval,stdmean=sigma
            avg[i,num]=meanval
            sig[i,num]=sigma
            num++
         endfor
         icut++
      endfor

   endfor

   avgratio=fltarr(nf)
   sigma=fltarr(nf)
   for i=0,nf-1 do begin
      moment4,avg[i,*]/sig[i,*],v1,d1,v2,d2,v3,v4
      m1[i]=v1
      m2[i]=v2
      m3[i]=v3
      m4[i]=v4
      sigma[i]=stdev(avg[i,*],mval)
      avgratio[i]=mval
   endfor

   if display then begin

      setwin,3
      plot,avg[*],psym=8,ytitle='Spot average values [DN]'
      setwin,4
      plot,sig[*],psym=8,ytitle='Spot sigma values [DN]'
      setwin,5
      plot,avg[*]/sig[*],psym=8,ytitle='Spot average/sigma'
      setwin,8
      !p.multi=[0,2,2]
      plot,m1,psym=8,ytitle='mean of spots'
;print,mean(m1),mean(m2),mean(m3),mean(m4)
      plot,fno,m2,psym=8,ytitle='sigma of spots'
      bad=bytarr(nf)
      robomean,m2,2.0,0.5,bad=bad
      z=where(bad eq 1,count)
      if count ne 0 then oplot,fno[z],m2[z],psym=8,color='0000ff'xl
      plot,fno,m3,psym=8,ytitle='skew of spots'
      z=where(abs(m3) gt skewerr,count)
      if count ne 0 then oplot,fno[z],m3[z],psym=8,color='0000ff'xl
      plot,fno,m4,psym=8,ytitle='kurtosis of spots'
      z=where(abs(m4) gt kurterr,count)
      if count ne 0 then oplot,fno[z],m4[z],psym=8,color='0000ff'xl
      !p.multi=0

   endif

   z=where(m2 gt maxsig,fail)

   grade=fail
   sigratio=m2
   skew=m3
   kurt=m4

end
