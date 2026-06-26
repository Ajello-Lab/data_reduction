;+
; NAME:
;  hsttpdfp
; PURPOSE:   (one line only)
;  Plot target PDF from a fit and save covariance to database
; DESCRIPTION:
; CATEGORY:
;  Astrometry
; CALLING SEQUENCE:
;  starpdfplot
; INPUTS:
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
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
;  2020/06/30, Written by Marc W. Buie, Southwest Research Institute
;-
pro hsttpdfp,MAXTODO=maxtodo,MULTITHREAD=multithread, $
       HIDDEN=hidden,VERBOSE=verbose

   compile_opt strictarrsubs
   common com_hsttargfit,info

   pname='hsttpdfp'
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

   fnlist=file_search(pdfdir+'T*.pdf',count=nfiles)
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
          ' from pos where idx='+strn(idx)+';'
      mysqlquery,dblun,cmd,hidx,format='l',ngood=nhit
      if nhit ne 1 then begin
         print,cmd
         print,self,' star record not found'
         goto,bailout
      endif

      hstinittarg,dblun,idx,hidx,error
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

      chisq=hstmodtarg(vals)
      sinfo = info

      dw=sinfo.dw
      xnav=sinfo.xnav
      ynav=sinfo.ynav
      xpos=sinfo.xpos
      ypos=sinfo.ypos
      flux=sinfo.flux

      boxm,sinfo.fullimage,xnav,ynav,1,1,xmax,ymax
      peaksig=sinfo.fullimage[xmax,ymax]
      subl=((sinfo.fitimage-(sinfo.back-2.0*sinfo.backsig) > 0)+10)^0.1
      maxv=((peaksig>(sinfo.back+2.0*sinfo.backsig))- $
                     (sinfo.back-2.0*sinfo.backsig)+10)^0.1
      minv=1.0>min(subl)
      zf=3
      bsub=bytscl(sinfo.fitimage,min=0,max=info.fullimage[xmax,ymax],top=255)
      bsub=dispmask(bsub,sinfo.mask,zf)
      bsub2=bytscl(subl,min=minv,max=maxv,top=255)
      bsub2=dispmask(bsub2,sinfo.mask,zf)

      resimage=sinfo.dimage
      bres=bytscl(resimage,min=min(resimage),max=max(resimage),top=255)
      bres=dispmask(bres,sinfo.mask[*,*],zf)
      sigimage = resimage/sinfo.simage
      bsig=bytscl(sigimage,min=-3.0,max=3.0,top=255)
      bsig=dispmask(bsig,sinfo.mask[*,*],zf)

      boxm,info.fullimage[*,*],xnav,ynav,5,5,xmax,ymax
      peaksig=info.fullimage[xmax,ymax]
      subl=((info.fitimage[*,*]-(info.back-2.0*info.backsig) > 0)+10)^0.1
      maxv=((peaksig>(info.back+2.0*info.backsig))- $
                     (info.back-2.0*info.backsig)+10)^0.1
      minv=1.0>min(subl)
      zf=3
      bsubn=bytscl(info.fitimage[*,*],min=0,max=info.fullimage[xmax,ymax],top=255)
      bsubn=dispmask(bsubn,info.mask[*,*],zf)
      bsub2n=bytscl(subl,min=minv,max=maxv,top=255)
      bsub2n=dispmask(bsub2n,info.mask[*,*],zf)

      resimage=info.dimage[*,*]
      bresn=bytscl(resimage,min=min(resimage),max=max(resimage),top=255)
      bresn=dispmask(bresn,info.mask[*,*],zf)
      sigimage = resimage/info.simage[*,*]
      bsign=bytscl(sigimage,min=-3.0,max=3.0,top=255)
      bsign=dispmask(bsign,info.mask[*,*],zf)

      if hidden then begin
         set_plot,'Z'
         device,decomposed=1,z_buffering=0, $
            set_resolution=[1024,1024],set_pixel_depth=24
      endif else begin
         setwin,0,xsize=1024,ysize=1024
      endelse
      erase,white

      x0=840
      y0=935
      tv,bsub,x0,y0,/device,true=3
      astmark,x0+dw*zf,y0+dw*zf,/device,color=cpalette(2)
      astmark,x0+(dw-xnav+xpos)*zf,y0+(dw-ynav+ypos)*zf, $
         /device,color=cpalette(1)
      xyouts,x0+4*dw*zf+35,y0+35,'ticks',/device,align=0.5,color=black
      xyouts,x0+4*dw*zf+35,y0+23,'NAV',/device,align=0.5,color=cpalette(2)
      xyouts,x0+4*dw*zf+35,y0+11,'FIT',/device,align=0.5,color=cpalette(1)

      tv,bsub2,x0+(2*dw+1)*zf+10,y0,/device,true=3
      tv,bsig,x0,y0-(2*dw+1)*zf-10,/device,true=3
      tv,bres,x0+(2*dw+1)*zf+10,y0-(2*dw+1)*zf-10,/device,true=3

      xyouts,x0-50,y0,'Original',align=0.5,color=black,/device,charsize=cs
      xyouts,x0-50,y0-20,'Data',align=0.5,color=black,/device,charsize=cs
      plots,x0,y0,psym=4,color=black,/device

      x0=x0-000
      y0=y0-160
      tv,bsubn,x0,y0,/device,true=3
      astmark,x0+dw*zf,y0+dw*zf,/device,color=cpalette(2)
      astmark,x0+(dw-xnav+xpos)*zf,y0+(dw-ynav+ypos)*zf, $
         /device,color=cpalette(1)
      tv,bsub2n,x0+(2*dw+1)*zf+10,y0,/device,true=3
      tv,bsign,x0,y0-(2*dw+1)*zf-10,/device,true=3
      tv,bresn,x0+(2*dw+1)*zf+10,y0-(2*dw+1)*zf-10,/device,true=3
      xyouts,x0-50,y0,'StarSub',align=0.5,color=black,/device,charsize=cs
      xyouts,x0-50,y0-20,'Data',align=0.5,color=black,/device,charsize=cs
      plots,x0,y0,psym=4,color=black,/device

      x0=x0-100
      y0=y0+70
      if hidden then csl=1.0 else csl=1.2
      xyouts,x0+10,y0+30,'Data (e!E-!N)', $
         align=0.5,color=black,/device,charsize=csl
      xyouts,x0-35,y0+12,string(177B)+'3!4r!X', $
         align=0.5,color=black,/device,charsize=csl
      xyouts,x0+35,y0+12,'(-2!4r!X!96!XMax)!E0.1!N', $
         align=0.5,color=black,/device,charsize=csl
      xyouts,x0-35,y0-12,string(177B)+'3', $
         align=0.5,color=black,/device,charsize=csl
      xyouts,x0+35,y0-12,'min!96!Xmax', $
         align=0.5,color=black,/device,charsize=csl
      xyouts,x0+10,y0-30,'Residual (!4r!X)', $
         align=0.5,color=black,/device,charsize=csl
      plots,x0+[-1,1.50,1.50,-1,-1]*51, $
            y0+[-1,-1,1,1,-1]*40+3,color=black,/device
      plots,x0+[-1,1.50]*51,y0+[0,0]+25,color='707070'xl,/device
      plots,x0+[-1,1.50]*51,y0+[0,0]+3,color=black,/device
      plots,x0+[-1,1.50]*51,y0+[0,0]-18,color='707070'xl,/device
      plots,x0+[0,0]-13,y0+[25,-18],color='303030'xl,/device

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
      str='Flux='+strn(info.flux,format='(f15.1)')
      xyouts,left,ytop-8*dy,str,align=0.0,charsize=cs*1.2,color=black,/normal
      str='imag='+strn(imag,format='(f10.3)')
      xyouts,left,ytop-9*dy,str,align=0.0,charsize=cs*1.2,color=black,/normal
      str='exptime='+strn(round(info.exptime))+'s'
      xyouts,left,ytop-10*dy,str,align=0.0,charsize=cs*1.2,color=black,/normal
      jdstr,info.jdmid,0,str
      str='UT mid='+str
      xyouts,left,ytop-11*dy,str,align=0.0,charsize=cs*1.2,color=black,/normal

      ytop=0.67
      left=0.725
      dy = 0.020
      str='MCMC results'
      cf=1.0
      xyouts,left,ytop-1*dy,str,align=0.0,charsize=cs*cf,color=black,/normal
      str='X mean  '+string(xstat[0],format='(f8.3)')
      xyouts,left,ytop-2*dy,str,align=0.0,charsize=cs*cf,color=black,/normal
      str='  sigma '+string(sqrt(xstat[1]),format='(f8.3)')
      xyouts,left,ytop-3*dy,str,align=0.0,charsize=cs*cf,color=black,/normal
      str='  skew  '+string(xstat[2],format='(f7.2)')
      xyouts,left,ytop-4*dy,str,align=0.0,charsize=cs*cf,color=black,/normal
      str='  kurt  '+string(xstat[3],format='(f7.2)')
      xyouts,left,ytop-5*dy,str,align=0.0,charsize=cs*cf,color=black,/normal
      str='Y mean  '+string(ystat[0],format='(f8.3)')
      xyouts,left,ytop-6*dy,str,align=0.0,charsize=cs*cf,color=black,/normal
      str='  sigma '+string(sqrt(ystat[1]),format='(f8.3)')
      xyouts,left,ytop-7*dy,str,align=0.0,charsize=cs*cf,color=black,/normal
      str='  skew  '+string(ystat[2],format='(f7.2)')
      xyouts,left,ytop-8*dy,str,align=0.0,charsize=cs*cf,color=black,/normal
      str='  kurt  '+string(ystat[3],format='(f7.2)')
      xyouts,left,ytop-9*dy,str,align=0.0,charsize=cs*cf,color=black,/normal
      str='mag mean  '+string(mstat[0],format='(f8.3)')
      xyouts,left,ytop-10*dy,str,align=0.0,charsize=cs*cf,color=black,/normal
      str='    sigma '+string(sqrt(mstat[1]),format='(f8.3)')
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

      cmd=['update pos set', $
           'x='+strn(xstat[0],format='(f10.3)')+c, $
           'y='+strn(ystat[0],format='(f10.3)')+c, $
           'xerr='+strn(sqrt(xstat[1]),format='(f10.3)')+c, $
           'yerr='+strn(sqrt(ystat[1]),format='(f10.3)')+c, $
           'flux='+strn(fl,format='(f15.3)')+c, $
           'fluxerr='+strn(flerr,format='(f10.2)'), $
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
      action='covariance for '+strn(ndone)+' targets'
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
