;+
; NAME:
;  hstspdfp
; PURPOSE:   (one line only)
;  Plot stellar PDF from a fit and save covariance to database
; DESCRIPTION:
; CATEGORY:
;  Astrometry
; CALLING SEQUENCE:
;  starpdfplot
; INPUTS:
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;  MAXTODO - maximum number of PDF's to process, default=all
;  MULTITHREAD - Flag, set if you want to use mutiple core per instance.
;                 Default is to limit to one core so that you can run multiple
;                 copies.
;  VERBOSE - Flag, if set will generate verbose output of the calculations
;  HIDDEN  - Flag, if set let's the program use the Z buffer instead of
;               and active X display.  Most effective for working over a
;               slow network and you don't really need to see what it's doing.
; OUTPUTS:
; KEYWORD OUTPUT PARAMETERS:
; CONFIGURATION: (optional, overrides xtrack.in)
;   File: config.ini
;   [global]
;     subdir   - naem of sub-directory  added to base path to find data
;   [ddir]   - section to list potential root directories to find data
;     XXX - <value>
;       XXX is a unique string (eg., D01)
;       <value> is a directory path
;       for additional details see getddir.pro
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  2020/06/25, Written by Marc W. Buie, Southwest Research Institute
;-
pro hstspdfp,MAXTODO=maxtodo,MULTITHREAD=multithread, $
       HIDDEN=hidden,VERBOSE=verbose

   compile_opt strictarrsubs
   common com_hststarfit,info

   pname='hstspdfp'
   self=pname+': '
   if badpar(maxtodo,[0,1,2,3],0,caller=self+'(MAXTODO) ',default=0) then return

   if badpar(multithread,[0,1,2,3],0,caller=self+'(MULTITHREAD) ', $
                                     default=0) then return
   if badpar(hidden,[0,1,2,3],0,caller=self+'(HIDDEN) ', $
                                     default=0) then return
   if badpar(verbose,[0,1,2,3],0,caller=self+'(VERBOSE) ', $
                                     default=0) then return

   getddir,info,ddir
   getvalue,info,'global','subdir',subdir

   dirty=0
   d_name=!d.name
   if not multithread then cpu,tpool_nthreads=1

   pdfdir=ddir+'pdf/'
   pldir='plots'

   fnlist=file_search(pdfdir+'S*.pdf',count=nfiles)
   fnlist=strmid(fnlist,strlen(pdfdir))

   if not exists(pldir) then file_mkdir,pldir
   pldir=addslash(pldir)

   openmysql,dblun,'hstast'
   c=','

   print,strn(nfiles),' PDF files found.'
   if hidden then cs=1.5 else cs=2
   white='ffffff'xl
   black='000000'xl

   ndone=0
   for i=0,nfiles-1 do begin

      if verbose then print,pdfdir+fnlist[i]
      words=strsplit(fnlist[i],'.',/extract)
      idx=long(strmid(words[0],1))

      cmd='select hidx'+ $
          ' from stars where idx='+strn(idx)+';'
      mysqlquery,dblun,cmd,hidx,format='l',ngood=nhit
      if nhit ne 1 then begin
         print,cmd
         print,self,' star record not found'
         goto,bailout
      endif

      hstinitstar,dblun,idx,hidx,error
      if error then begin
         goto,bailout
      endif

      fnpng=strmid(fnlist[i],0,strlen(fnlist[i])-4)+'.png'
      pdfinfo=file_info(pdfdir+fnlist[i])
      pnginfo=file_info(pldir+fnpng)
      if pnginfo.exists then begin
         if pdfinfo.mtime le pnginfo.mtime then begin
            if verbose then print,fnpng+' newer than PDF, skipping'
            continue
         endif
      endif

      print,fnpng

      data=double(readfits(pdfdir+fnlist[i],pdfhdr))
      sz=size(data,/dimen)
      npts=sz[1]
      xpdf=trimrank(data[0,*])
      ypdf=trimrank(data[1,*])
      magpdf=trimrank(data[2,*])

      moment4,xpdf,avg,avgdev,stddev,var,skew,kurt
      xstat=moment(xpdf)
      ystat=moment(ypdf)
      mstat=moment(magpdf)
      mag2flx,mstat[0],sqrt(mstat[1]),fl,flerr,zeropt=24.0

      vals=[info.xpos,info.ypos,info.flux]
      imag=24.0-2.5*alog10(info.flux)

      info.magval=0
      info.quiet=1
      info.chired=1

      chisq=hstmodstar(vals)

      boxm,info.fullimage,info.xnav,info.ynav,5,5,xmax,ymax
      peaksig=info.fullimage[xmax,ymax]
      subl=((info.fitimage-(info.back-2.0*info.backsig) > 0)+10)^0.1
      maxv=((peaksig>(info.back+2.0*info.backsig))- $
                     (info.back-2.0*info.backsig)+10)^0.1
      minv=1.0>min(subl)
      zf=1
      bsub=bytscl(info.fitimage,min=0,max=info.fullimage[xmax,ymax],top=255)
      bsub=rebin(bsub,(2*info.dw+1)*zf,(2*info.dw+1)*zf,/sample)
      bsub2=bytscl(subl,min=minv,max=maxv,top=255)
      bsub2=rebin(bsub2,(2*info.dw+1)*zf,(2*info.dw+1)*zf,/sample)

      resimage = info.dimage
      bres=bytscl(resimage,min=min(resimage),max=max(resimage),top=255)
      bres=rebin(bres,(2*info.dw+1)*zf,(2*info.dw+1)*zf,/sample)
      sigimage = info.dimage/info.simage
      bsig=bytscl(sigimage,min=-3.0,max=3.0,top=255)
      bsig=rebin(bsig,(2*info.dw+1)*zf,(2*info.dw+1)*zf,/sample)

      if hidden then begin
         set_plot,'Z'
         device,decomposed=1,z_buffering=0, $
            set_resolution=[1024,1024],set_pixel_depth=24
      endif else begin
         setwin,0,xsize=1024,ysize=1024
      endelse
      erase,white

      x0=700
      y0=864
      tv,bsub,x0,y0,/device
;      astmark,x0+info.dw*zf,y0+info.dw*zf,/device,color=cpalette(2)
;      astmark,x0+(info.dw-xnav+info.xpos)*zf,y0+(info.dw-ynav+info.ypos)*zf, $
;         /device,color=cpalette(1) ; ,rotang=180
;      xyouts,x0+info.dw*zf-50,y0+1,'ticks:', $
;         /device,align=0.0,color=white
;      xyouts,x0+info.dw*zf-10,y0+1,'NAV', $
;         /device,align=0.0,color=cpalette(2)
;      xyouts,x0+info.dw*zf+20,y0+1,'FIT', $
;         /device,align=0.0,color=cpalette(1)

      tv,bsub2,x0+(2*info.dw+1)*zf+10,y0,/device

      tv,bres,x0,y0-(2*info.dw+1)*zf-10,/device
      xyouts,x0+info.dw*zf,y0-(2*info.dw+1)*zf-20,'DN resid (min/max)', $
         /device,align=0.5,color=black
      tv,bsig,x0+(2*info.dw+1)*zf+10,y0-(2*info.dw+1)*zf-10,/device
      xyouts,x0+(2*info.dw+1)*zf+info.dw*zf+10,y0-(2*info.dw+1)*zf-20,'-3 to +3 sigma', $
         /device,align=0.5,color=black

      !p.multi=[3,3,3]
      plot,xpdf,ypdf,xtitle='X',ytitle='Y',psym=3,charsize=cs, $
         background=white,color=black,/noerase
      !p.multi=[6,3,3]
      plot,xpdf,magpdf,xtitle='X',ytitle='Mag',psym=3,charsize=cs, $
         background=white,color=black,/noerase
      !p.multi=[2,3,3]
      plot,magpdf,ypdf,xtitle='Mag',ytitle='Y',psym=3,charsize=cs, $
         background=white,color=black,/noerase

      !p.multi=[1,3,3]
      stats,ypdf,window=0,/silent,nbins=50,charsize=cs,xtitle='Y',/gaussian
      !p.multi=[9,3,3]
      stats,xpdf,window=0,/silent,nbins=50,charsize=cs,xtitle='X',/gaussian
      !p.multi=[5,3,3]
      stats,magpdf,window=0,/silent,nbins=50,charsize=cs,xtitle='Mag',/gaussian

      dy = 0.025
      ytop=0.95
      left=0.36
      xyouts,0.50,ytop,info.root,align=0.5,charsize=cs*2,color=black,/normal
      str='UVIS'+strn(info.det)
      xyouts,left,ytop-dy,str,align=0.0,charsize=cs*1.2,color=black,/normal
      str='idx='+strn(idx)+' hidx='+strn(hidx)
      xyouts,left,ytop-2*dy,str,align=0.0,charsize=cs*1.2,color=black,/normal
      str='NAV ('+strn(info.xnav)+','+strn(info.ynav)+ $
             ') ['+strn(info.fullimage[info.xnav,info.ynav],format='(f10.1)')+']'
      xyouts,left,ytop-3*dy,str,align=0.0,charsize=cs*1.2,color=black,/normal
      str='Max ('+strn(xmax)+','+strn(ymax)+ $
             ') ['+strn(info.fullimage[xmax,ymax],format='(f10.1)')+']'
      xyouts,left,ytop-4*dy,str,align=0.0,charsize=cs*1.2,color=black,/normal
      str='sky='+strn(info.back,format='(f10.2)')+'+/-'+strn(info.backsig,format='(f10.2)')
      xyouts,left,ytop-5*dy,str,align=0.0,charsize=cs*1.2,color=black,/normal
      str='chisq='+strn(chisq,format='(f10.2)')
      xyouts,left,ytop-6*dy,str,align=0.0,charsize=cs*1.2,color=black,/normal
      str='Fit ('+strn(info.xpos,format='(f10.2)')+','+strn(info.ypos,format='(f10.2)')+')'
      xyouts,left,ytop-7*dy,str,align=0.0,charsize=cs*1.2,color=black,/normal
      str='Rmag='+strn(info.rmag,format='(f10.2)')
      xyouts,left,ytop-8*dy,str,align=0.0,charsize=cs*1.2,color=black,/normal
      str='Flux='+strn(info.flux,format='(f15.1)')
      xyouts,left,ytop-9*dy,str,align=0.0,charsize=cs*1.2,color=black,/normal
      str='imag='+strn(imag,format='(f10.3)')
      xyouts,left,ytop-10*dy,str,align=0.0,charsize=cs*1.2,color=black,/normal
      str='exptime='+strn(round(info.exptime))+'s'
      xyouts,left,ytop-11*dy,str,align=0.0,charsize=cs*1.2,color=black,/normal

      ytop=0.67
      left=0.725
      dy = 0.020
      str='MCMC results'
      cf=1.0
      xyouts,left,ytop-1*dy,str,align=0.0,charsize=cs*cf,color=black,/normal
      str='X mean  '+string(xstat[0],format='(f7.2)')
      xyouts,left,ytop-2*dy,str,align=0.0,charsize=cs*cf,color=black,/normal
      str='  sigma '+string(sqrt(xstat[1]),format='(f7.2)')
      xyouts,left,ytop-3*dy,str,align=0.0,charsize=cs*cf,color=black,/normal
      str='  skew  '+string(xstat[2],format='(f7.2)')
      xyouts,left,ytop-4*dy,str,align=0.0,charsize=cs*cf,color=black,/normal
      str='  kurt  '+string(xstat[3],format='(f7.2)')
      xyouts,left,ytop-5*dy,str,align=0.0,charsize=cs*cf,color=black,/normal
      str='Y mean  '+string(ystat[0],format='(f7.2)')
      xyouts,left,ytop-6*dy,str,align=0.0,charsize=cs*cf,color=black,/normal
      str='  sigma '+string(sqrt(ystat[1]),format='(f7.2)')
      xyouts,left,ytop-7*dy,str,align=0.0,charsize=cs*cf,color=black,/normal
      str='  skew  '+string(ystat[2],format='(f7.2)')
      xyouts,left,ytop-8*dy,str,align=0.0,charsize=cs*cf,color=black,/normal
      str='  kurt  '+string(ystat[3],format='(f7.2)')
      xyouts,left,ytop-9*dy,str,align=0.0,charsize=cs*cf,color=black,/normal
      str='mag mean  '+string(mstat[0],format='(f7.2)')
      xyouts,left,ytop-10*dy,str,align=0.0,charsize=cs*cf,color=black,/normal
      str='    sigma '+string(sqrt(mstat[1]),format='(f7.2)')
      xyouts,left,ytop-11*dy,str,align=0.0,charsize=cs*cf,color=black,/normal
      str='    skew  '+string(mstat[2],format='(f7.2)')
      xyouts,left,ytop-12*dy,str,align=0.0,charsize=cs*cf,color=black,/normal
      str='    kurt  '+string(mstat[3],format='(f7.2)')
      xyouts,left,ytop-13*dy,str,align=0.0,charsize=cs*cf,color=black,/normal
      str='flux='+strn(fl,format='(f15.1)')+'+/-'+strn(flerr,format='(f15.1)')
      xyouts,left,ytop-14*dy,str,align=0.0,charsize=cs*cf,color=black,/normal
      str='SNR='+strn(fl/flerr,format='(f15.1)')
      xyouts,left,ytop-15*dy,str,align=0.0,charsize=cs*cf,color=black,/normal
      str='Nsamples  '+strn(npts)
      xyouts,left,ytop-16*dy,str,align=0.0,charsize=cs*cf,color=black,/normal

      if hidden then begin
         tvgrab,pldir+fnpng,-1,/png
      endif else begin
         tvgrab,pldir+fnpng,0,/png
      endelse

      cmd=['update stars set', $
           'xerr='+strn(sqrt(xstat[1]))+c, $
           'yerr='+strn(sqrt(ystat[1]))+c, $
           'fluxerr='+strn(flerr)+c, $
           'imax='+strn(info.fullimage[xmax,ymax]), $
           'where idx='+strn(idx)+';']
      print,cmd
      dirty=1
      mysqlcmd,dblun,cmd

      hdrmax=sxpar(pdfhdr,'MAX')
      if hdrmax eq 0 then begin
         sxaddpar,pdfhdr,'MAX',info.fullimage[xmax,ymax],' Peak signal in source'
         sxaddpar,pdfhdr,'CHISQ',chisq,' Chisq from PSF fit'
         sxaddpar,pdfhdr,'SNR',fl/flerr,' Signal-to-noise ratio of source'
         modfits,pdfdir+fnlist[i],0,pdfhdr
      endif

      ndone++
      if maxtodo ne 0 and ndone ge maxtodo then break
   endfor

   if maxtodo gt 0 and ndone eq maxtodo then $
      print,'Early termination, maxtodo=',strn(maxtodo) $
   else $
      print,'Done.  Processed ',strn(ndone),' files.'

   if dirty then begin
      context=subdir
      action='covariance for '+strn(ndone)+' stars'
      jdcur=systime(/julian,/ut)
      jdstr,jdcur,300,jds
      cmd=['insert into history set', $
           'posted='+jds+c, $
           'context='+quote(context)+c, $
           'tool='+quote(pname)+c, $
           'action='+quote(action)+';']
      print,cmd
      mysqlcmd,dblun,cmd
   endif

bailout:
   !p.multi=0
   free_lun,dblun
   cpu,/reset
   set_plot,d_name

end
