;+
; NAME:
;  eventprob
; PURPOSE:   (one line only)
;  Compute the success probability for an occultation from discrete sites
; DESCRIPTION:
; CATEGORY:
;  Occultations
; CALLING SEQUENCE:
;  eventprob,fn,in_diam,vel,sigma
; INPUTS:
;  fn - Name of input file that has all of the cross track positions
;         of the mobile assets.  This input variable can also be a structure
;         containing the mobile site information.  Two tags are required:
;            nsites: Number of sites, if 0 then the other tags are not needed
;            name: string vector with the name of the mobile site
;            xtrack: float vector with the cross track position in km
;  diam - Diameter of the object in km
;  vel  - Plane-of-the sky velocity of the object in km/sec
;  sigma - Cross track uncertainty of centerline in km
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;  FIXED - File name containing cross-track locations of the fixed sites.
;             Default is no fixed sites.  These are not affected by use
;             of TRACKERR.  This can also be a structure similar to the
;             fn input variable.
;  NTRIALS - Number of random trials, default=10000
;  TRACKERR - Mobile site placement error in km.  This is modeled as a uniform
;               random error from -0.5 to 0.5 of this value relative to
;               the planned location.
;  SILENT - Flag, if set will suppress all non-error printed output
;  NODISPLAY - Flag, if set will suppress all plotted output
;  SYSTEMATIC - Total amount of systematic error.  This is modeled as a
;                 uniform random offset relative to nominal.  The number you
;                 provide is end to end of the range so the maximum it will
;                 be from nominal is half of this number.  Default=0
; OUTPUTS:
;  results - Structure containing all the computed statistics
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  Written by Marc W. Buie, Southwest Research Institute, 2018/10/27
;  2020/05/18, MWB, added prob1 and prob2 fields to results.
;  2022/02/09, MWB, fixed minor issue with reported cross-track sigma range
;  2023/12/15, MWB, added PDF keyword
;-
compile_opt strictarrsubs
pro eventprob,in_fn,in_diam,in_vel,in_sigma,results, $
       NTRIALS=ntrials,FIXED=in_fixed,TRACKERR=trackerr, $
       SILENT=silent,NODISPLAY=nodisplay,SYSTEMATIC=systematic,TITLE=title, $
       PDF=pdf

   self='eventprob: '
   if badpar(in_fn,[7,8],[0,1],caller=self+'(fn) ',type=fntype) then return
   if badpar(in_diam,[2,3,4,5],0,caller=self+'(diam) ') then return
   if badpar(in_vel,[2,3,4,5],0,caller=self+'(vel) ') then return
   if badpar(in_sigma,[2,3,4,5],0,caller=self+'(sigma) ') then return

   if badpar(ntrials,[0,2,3],0,caller=self+'(NTRIALS) ', $
                               default=10000) then return
   if badpar(in_fixed,[0,7,8],[0,1],caller=self+'(FIXED) ', $
                           default='[[none]]',type=fixedtype) then return
   if badpar(trackerr,[0,2,3,4,5],0,caller=self+'(TRACKERR) ', $
                                    default=0.0) then return
   if badpar(silent,[0,1,2,3],0,caller=self+'(SILENT) ', $
                                    default=0) then return
   if badpar(nodisplay,[0,1,2,3],0,caller=self+'(NODISPLAY) ', $
                                    default=0) then return
   if badpar(systematic,[0,2,3,4,5],0,caller=self+'(SYSTEMATIC) ', $
                                    default=0.0) then return
   if badpar(title,[0,7],0,caller=self+'(TITLE) ', $
                                    default='') then return
   if badpar(pdf,[0,4,5],[0,1],caller=self+'(fn) ',npts=pdfnpts) then return

   diam=float(in_diam)
   vel=float(in_vel); km/arcsec
   sigma=float(in_sigma); km/arcsec

   if fntype eq 7 then begin
      if not exists(in_fn) then begin
         print,self,'Input file [,',in_fn,'] does not exist.'
         return
      endif
      readcol,in_fn,mname,mxtrack,format='a,f',count=nmobile
      if not silent then print,strn(nmobile),' sites loaded from file.'
   endif else begin
      nmobile=in_fn.nsites
      if nmobile ne 0 then begin
         mname=in_fn.name
         mxtrack=in_fn.xtrack
      endif
   endelse
   if nmobile ne 0 then begin
      mtype=replicate('M',nmobile)
   endif

   if fixedtype eq 7 then begin
      if in_fixed ne '[[none]]' then begin
         if not exists(in_fixed) then begin
            print,'Warning!  Fixed site file ',in_fixed,' not found.  Ignoring'
            nfixed=0
         endif else begin
            readcol,in_fixed,fname,fxtrack,format='a,f',count=nfixed
         endelse
      endif else begin
         nfixed=0
      endelse
   endif else begin
      nfixed=in_fixed.nsites
      if nfixed ne 0 then begin
         fname=in_fixed.name
         fxtrack=in_fixed.xtrack
      endif
   endelse
   if nfixed ne 0 then begin
      ftype=replicate('F',nfixed)
   endif

   if nfixed ne 0 and nmobile ne 0 then begin
      name=[mname,fname]
      xtrack=[mxtrack,fxtrack]
      type=[mtype,ftype]
   endif else if nfixed eq 0 then begin
      name=mname
      xtrack=mxtrack
      type=mtype
   endif else if nmobile eq 0 then begin
      name=fname
      xtrack=fxtrack
      type=ftype
   endif else begin
      print,self,'No sites!  Not sure this can ever happen. Aborting.'
      return
   endelse

   nsites=nfixed+nmobile

   idx=reverse(sort(xtrack))
   name=name[idx]
   xtrack=xtrack[idx]
   type=type[idx]

   if nsites gt 1 then begin
      delx=xtrack[0:-2]-xtrack[1:-1]
   endif else begin
      delx=[0.0]
   endelse

   sig=xtrack/sigma

   scount=intarr(ntrials)
   sworst=fltarr(ntrials)
   syes=fltarr(nsites)
   zf=where(type eq 'F')

   for i=0,ntrials-1 do begin

      ; position of centerline relative to nominal
      dctl = trimrank(randomn(seed,1))*sigma

      if systematic gt 0 then $
         dctl += (trimrank(randomu(seed,1))-0.5)*systematic/2.0

      if pdfnpts gt 1 then $
         dctl -= trimrank(pdf[randomu(seed,1)*pdfnpts])

      ; random relocation of the mobile sites
      poserr=(randomu(seed,nsites)-0.5)/0.5 * trackerr
      if nfixed gt 0 then poserr[zf]=0.0

      cdepth = (xtrack+poserr+dctl)/(diam/2.0)
      z=where(abs(cdepth) lt 0.86,count)
      scount[i]=count
      sworst[i]=max(abs(cdepth[z]))
      if count gt 0 then begin
         syes[z] = syes[z]+1
      endif
   endfor
   prob=syes/ntrials

   if not silent then begin
      print,'Central chord for D=',diam[0],' is ',diam[0]/vel,' seconds'
      print,'Median site spacing',median(delx),' km'
      if systematic gt 0 then print,'Systematic error component is ',systematic
   endif

   if not nodisplay then begin
      setwin,3
      stats,scount,/silent,title=title
   endif
   countlist=scount[uniq(scount,sort(scount))]

   cumtotal=0L
   cumatotal=0L
   for i=0,n_elements(countlist)-1 do begin
      z=where(scount eq countlist[i],count)
      if not silent then print,countlist[i],count,count/float(ntrials), $
         format='(i2,1x,i7,1x,f4.2)'
      if countlist[i] ge 2 then cumtotal += count
      if countlist[i] ge 1 then cumatotal += count
   endfor
   if not silent then begin
      print,'Fraction with 1 or more chords ',cumatotal/float(ntrials)
      print,'Fraction with 2 or more chords ',cumtotal/float(ntrials)
   endif

   if not nodisplay then begin
      setwin,4
      y=replicate(1,nsites)
      plot,[0],/nodata,psym=8,yr=[0,2], $
         xr=minmax([xtrack,-3*sigma-diam/2.0,3*sigma+diam/2.0]), $
         xtitle='Crosstrack position (km)', $
         color=0,background='ffffff'xl,title=title
      z=where(type eq 'M',count)
      if count ne 0 then begin
         if trackerr gt 0 then begin
            for i=0,count-1 do $
               oplot,xtrack[z[i]]+0.5*trackerr*[-1,1],y[z[i]]*[1,1],color=0
         endif
         oplot,xtrack[z],y[z],psym=8,color=0
      endif
      z=where(type eq 'F',count)
      if count ne 0 then begin
         if trackerr gt 0 then begin
            for i=0,count-1 do $
               oplot,xtrack[z[i]]+0.5*trackerr*[-1,1],y[z[i]]*[1,1]-0.1, $
                  color=cpalette(1)
         endif
         oplot,xtrack[z],y[z]-0.1,psym=8,color=cpalette(1)
      endif
      oplot,(-3*sigma-diam/2)*[1,1],[0,2],color=cpalette(2)
      oplot,(-2*sigma-diam/2)*[1,1],[0,2],color=cpalette(3)
      oplot,(-1*sigma-diam/2)*[1,1],[0,2],color=cpalette(4)
      oplot,( 0*sigma-diam/2)*[1,1],[0,2],color=cpalette(0)
      oplot,( 0*sigma       )*[1,1],[0,2],color=cpalette(5)
      oplot,( 0*sigma+diam/2)*[1,1],[0,2],color=cpalette(0)
      oplot,( 1*sigma+diam/2)*[1,1],[0,2],color=cpalette(4)
      oplot,( 2*sigma+diam/2)*[1,1],[0,2],color=cpalette(3)
      oplot,( 3*sigma+diam/2)*[1,1],[0,2],color=cpalette(2)
      for i=0,nsites-1 do begin
         if type[i] eq 'M' then $
            xyouts,xtrack[i],1.1,name[i],orient=90,/data,charsize=2,color=0
         if type[i] eq 'F' then $
            xyouts,xtrack[i],0.85,name[i],orient=270,/data,charsize=2,color=cpalette(1)
      endfor

      setwin,5
      plot,xtrack,prob,psym=8,yr=[0,max(prob)], $
         color=0,background='ffffff'xl, $
         xtitle='Cross track distance (km)', $
         ytitle='Probability of detection',title=title
      z=where(prob lt 0.01,count)
      if count ne 0 then $
         oplot,xtrack[z],prob[z],psym=8,color=cpalette(1)
   endif

   if not silent then begin
      print,'Range of cross-track sigma    is ', $
         (min(xtrack)+diam/2.0)/sigma,(max(xtrack)-diam/2.0)/sigma
      print,'Range of cross-track distance is ',minmax(xtrack)
   end

   results={ $
      diam:     diam, $
      vel:      vel, $
      sigma:    sigma, $
      trackerr: trackerr, $
      name:     name, $
      type:     type, $
      xtrack:   xtrack, $
      scount:   scount, $
      countlist: countlist, $
      ntrials:  ntrials, $
      prob:     prob, $
      prob2:    cumtotal/float(ntrials), $
      prob1:    cumatotal/float(ntrials), $
      nsites:   nsites $
      }

end
