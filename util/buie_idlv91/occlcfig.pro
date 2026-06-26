;+
; NAME:
;  occlcfig
; PURPOSE:   (one line only)
;  Generate a set of publication quality lightcurves for an occultation campaign
; DESCRIPTION:
; CATEGORY:
;  Occultations
; CALLING SEQUENCE:
;  occlcfig,event
; INPUTS:
; OPTIONAL INPUT PARAMETERS:
;  event - Name of occultation campaign.  The default is to use the current
;            directory as a candidate name.  If the reduction area in "occred"
;            is found, that is used.  If this doesn't resolve then you must
;            provide this input.
; KEYWORD INPUT PARAMETERS:
;  ROOT - Directory where the "occred" folder can be found.
;            Default = '/net/frakir/raid/buie/'
;  NOSAVE - Flag, if set, suppresses saving graphics to files.
; OUTPUTS:
;  Generates .png, .eps, and .pdf format versions of the graphics needed.
;    This will generate as many "pages" as needed to show all the data.
; KEYWORD OUTPUT PARAMETERS:
; CONFIGURATION:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
;   Options for controlling the output of this routine are contained in the
;     configuration file, config.ini, found in occred/<event>.  All options
;     are in an [lcfig] section.  If not provided, these default first to
;     the [global] section and then to built in internal values.
; MODIFICATION HISTORY:
;  Written by Marc W. Buie, Southwest Research Institute, 2024/01/28
;  2024/03/16, MWB, heavy modifications to control pagination and plot scaling
;  2024/04/26, MWB, minor bug fixes
;  2024/09/05, MWB, modified to use new ephem/obscode tools
;-
pro occlcfig,event,ROOT=root,NOSAVE=nosave

   self='occlcfig: '
   if badpar(root,[0,7],0,caller=self+'(root) ', $
                          default='/net/frakir/raid/buie/') then return
   if badpar(nosave,[0,1,2,3],0,caller=self+'(NOSAVE) ',default=0) then return

   roccred=addslash(root)+'occred/'
   if not exists(roccred) then begin
      print,'Directory ',roccred,' not found.'
      return
   endif

   cd,current=current
   words=strsplit(current,'/',/extract)
   event_default=words[-1]
   if exists(roccred+event_default) then begin
      print,'Default event from current directory: ',event_default
      if badpar(event,[0,7],0,caller=self+'(event) ', $
                              default=event_default) then return
   endif else begin
      if badpar(event,7,0,caller=self+'(event) ') then return
   endelse

   fnconfig=roccred+addslash(event)+'config.ini'
   if nofile(fnconfig,'reduction configuration file') then return
   print,'Config file: ',fnconfig

   loadini,file=fnconfig,info
   getvalue,info,'lcfig','fnstem',fnstem
   if fnstem eq '' then begin
      print,'[lcfig] fnstem config value missing.'
      return
   endif
;   getvalue,info,'global','event',event
   getvalue,info,'global','objectid',objectid
   getvalue,info,'global','ora',oras
   ra=raparse(oras)
   getvalue,info,'global','odec',odecs
   dec=decparse(odecs)
   getvalue,info,'lcfig','maxperpage',maxperpage,type=3,default=4
   getvalue,info,'lcfig','numperpage',numperpage,type=3,default=maxperpage,/array
   getvalue,info,'lcfig','dim',dim,type=3,default=[850,400],/array
   getvalue,info,'lcfig','xrange',xrange,type=4,default=[-25.,25.],/array
   getvalue,info,'lcfig','yrange',yrange,type=4,default=[0.,2.],/array
   getvalue,info,'lcfig','left',left,type=4,default=0.08
   getvalue,info,'lcfig','right',right,type=4,default=0.12
   getvalue,info,'lcfig','bottom',bottom,type=4,default=0.10
   getvalue,info,'lcfig','top',top,type=4,default=0.10
   getvalue,info,'lcfig','xdel',xdel,type=4,default=2.5
   getvalue,info,'lcfig','space',space,type=4,default=0.015
   getvalue,info,'lcfig','labeloffset',labeloffset,type=4,default=0.07
   getvalue,info,'lcfig','ylabel_xpos',ylabel_xpos,type=4,default=0.04
   getvalue,info,'lcfig','label_size',label_size,type=3,default=12
   getvalue,info,'lcfig','sym_size',sym_size,type=4,default=0.9
   getvalue,info,'lcfig','xmajor',xmajor,type=3,default=-1
   getvalue,info,'lcfig','xminor',xminor,type=3,default=-1
   getvalue,info,'lcfig','ymajor',ymajor,type=3,default=-1
   getvalue,info,'lcfig','yminor',yminor,type=3,default=-1
   getvalue,info,'lcfig','ytickinterval',ytickinterval,type=4,default=0.5

   loadocc,fnsites=roccred+addslash(event)+'sites.dat', $
           fnxtrack=roccred+addslash(event)+'crosstrack.dat', $
           fnevents=roccred+addslash(event)+'events.dat', $
           oinfo
   if oinfo.nsites eq 0 then return

   spawn,'geteph',unit=pipe

   nteams=oinfo.nsites
   team=oinfo.team
   xtrack=oinfo.xtrack
   jdmid=oinfo.jdref
   flag=oinfo.flag
   jdstr,jdmid,3,jdmids

   idx=reverse(sort(xtrack))
   team=team[idx]
   xtrack=xtrack[idx]
   jdmid=jdmid[idx]
   jdmids=jdmids[idx]
   flag=flag[idx]

   xr=axextend(xrange)
   yr=axextend(yrange)
   rcolor='702020'xl
   tcolor='207020'xl
   zcolor='a0a0a0'xl

   blanks=['','','']

   zp=where(flag eq 'y',countp)
   print,'Total number of lightcurves to show is ',strn(countp)
   numleft=countp
   np=0
   repeat begin
      nlen=n_elements(numperpage)
      if np gt nlen-1 then begin
         addnum = maxperpage < (countp-fix(total(numperpage)))
         numperpage=[numperpage,addnum]
      endif
      numleft = numleft-numperpage[np]
      np++
   endrep until numleft le 0

   print,'Plots per page'
   print,numperpage

   ; set the sizes for the full page that defines the "look" of the plots
   fullsz={ $
      np: maxperpage, $ ; everything is copied from the configuration
      dim: dim, $
      left: left, $
      right: right, $
      bottom: bottom, $
      top: top, $
      space: space, $
      labeloffset: labeloffset, $
      sym_size: sym_size, $
      ytickinterval:  ytickinterval, $
      px: 1.0 - left - right, $
      py: (1.0 - bottom - (maxperpage-1)*space - top)/float(maxperpage) $
      }

   if not nosave then begin
      fnkill=file_search(fnstem+'*',count=count)
      for i=0,count-1 do file_delete,fnkill[i],/quiet
   endif

   plotnum=0
   page=0
   for i=0,countp-1 do begin

      if plotnum eq 0 then begin
         ; setup the control parameters for this new page
         if numperpage[page] eq maxperpage then begin
            pinfo=fullsz
         endif else begin
            scalefac = fullsz.py*numperpage[page] + $
                        space*(numperpage[page]-1) + bottom + top
            newdim=[dim[0],round(scalefac*fullsz.dim[1])]
            pinfo={ $
               np: numperpage[page], $
               dim: newdim, $
               left: fullsz.left, $
               right: fullsz.right, $
               bottom: fullsz.bottom/scalefac, $
               top: fullsz.top/scalefac, $
               space: fullsz.space/scalefac, $
               labeloffset: fullsz.labeloffset, $
               sym_size: fullsz.sym_size/scalefac, $
               ytickinterval:  fullsz.ytickinterval/scalefac, $
               px: fullsz.px, $
               py: fullsz.py/scalefac $
               }
         endelse

         ; dummy plot to get the base of the page and set the xtitle
         lefttodo=strn(countp-i)
         print,'Start new page, number of plots left is ',lefttodo, $
               ', max per page ',strn(pinfo.np)
         loc=[pinfo.left,pinfo.bottom,1.0-pinfo.right,1.0-pinfo.top]

         p0=plot([0],xr=xr,yr=[0,1],/nodata,dimension=pinfo.dim, $
                 axis_style=4,position=loc)
         t=text(ylabel_xpos,0.5,'Normalized flux (star+object)',/normal, $
                clip=0,align=0.5,orientation=90,font_size=label_size)
      endif

      loc=[pinfo.left, $
           pinfo.bottom+(pinfo.np-(plotnum+1))*(pinfo.space+pinfo.py), $
           1-pinfo.right, $
           pinfo.bottom+ $
                 (pinfo.np-(plotnum+1))*(pinfo.space+pinfo.py)+pinfo.py]
print,loc

      fnlist=file_search(roccred+addslash(event)+team[zp[i]]+'.*lc', $
                            count=nfound)
      fnlist=strmid(fnlist,strlen(roccred+addslash(event)))

      if nfound gt 1 then begin
         getvalue,info,'lcfig',team[zp[i]],fnone
         if fnone eq '' then begin
            print,'Multiple lightcurve files found:'
            print,fnlist
            print,'You must pick one and add it to the config.ini file'
            goto,bailout
         endif else begin
            if nofile(roccred+addslash(event)+fnone, $
                            'Override lightcurve file') then return
            fnlist=fnone
            nfound=1
         endelse
      endif

      getvalue,info,team[zp[i]],'baseline',baseline,type=4,default=0.0
      getvalue,info,team[zp[i]],'mnpts',mnpts,type=3,default=4000
      getvalue,info,team[zp[i]],'modelt0',mt0,type=4,default=0.
      getvalue,info,team[zp[i]],'modelt1',mt1,type=4,default=0.
      getvalue,info,team[zp[i]],'mark',in_zmark,default='-1 -1'
      words=strsplit(in_zmark,' ',/extract)
      zmark=long(words)
      if min(zmark) ge 0 then model=1 else model=0

      if nfound eq 1 then begin
         print,team[zp[i]],page,pinfo.np,plotnum,format='(a-10,3(1x,i2))'
         readcol,roccred+addslash(event)+fnlist[0],jd,flux, $
            format='x,d,f',count=ndata
         obsinfo,'G**',obsinfo,oinfo.lat_r[zp[i]], $
                               oinfo.lon_r[zp[i]], $
                               oinfo.alt[zp[i]], $
                               oinfo.team[zp[i]]
         obspos=[oinfo.lat_r[zp[i]],oinfo.lon_r[zp[i]],oinfo.alt[zp[i]]]
         ephem,jdmid[zp[i]],obsinfo,72,objectid,eph,pipe=pipe
         ssgeom,eph,sun,earth,phang,elong
         kmscale = 1.0/(1.0/(earth*1.49598e8)*!radeg*3600.0) ; km/arcsec
         jdv=jdmid[zp[i]]+[-1,1]*30.0/60.0/24.0d0
         ephem,jdv,obsinfo,52,objectid,eph,pipe=pipe
         ra1=trimrank(eph[0,0])
         dec1=trimrank(eph[1,0])
         ra2=trimrank(eph[0,1])
         dec2=trimrank(eph[1,1])
         astrd2sn,ra1,dec1,ra,dec,xi1,eta1,/arcsec
         astrd2sn,ra2,dec2,ra,dec,xi2,eta2,/arcsec
         rate=sqrt((xi1-xi2)^2 + (eta1-eta2)^2) ; arcsec/hour
         speed = rate*kmscale/3600.0 ; km/sec
         time = (jd-jdmid[zp[i]])*86400.0d0

         bad=bytarr(ndata)
         z=where(flux lt median(flux)/2.0,count)
         if count ne 0 then bad[z]=1B
         robomean,flux,3.0,0.5,meanval,avgdev,stdev,bad=bad
         flux=flux/meanval
         fluxerr=replicate(stdev,n_elements(flux))/meanval

         z=where(time ge xr[0] and time le xr[1])
         pe=plot(time[z],(flux[z]<yr[1])>yr[0],xr=xr,yr=yr,axis_style=4, $
                     /current,position=loc,symbol='circle',/sym_filled, $
                     sym_size=pinfo.sym_size,clip=0,color='c0c0c0'xl, $
                     sym_color='black',font_size=label_size)
         dt = time[1:-1]-time[0:-2]
         robomean,dt,3.0,0.5,avgdt
;      if eventflag[zp[i]] then begin
;         mflux=replicate(1.0,mnpts)
;         t1=(jd1[zp[i]]-jdmid[zp[i]])*86400.0
;         t2=(jd2[zp[i]]-jdmid[zp[i]])*86400.0
;         z=where(mtime ge t1 and mtime le t2,count)
;         if count gt 0 then mflux[z]=0.0
;         pe1=plot(/overplot,/current,mtime,mflux,color=cpalette(1,/array))
;      endif
;      ax=axis('Y',location=xr[0],target=pe,axis_range=yr,major=3,minor=3,tickdir=1)
      endif else begin
         print,team[zp[i]],page,pinfo.np,plotnum,'no data', $
            format='(a-10,3(1x,i2),1x,a)'
         pe=plot([0],/nodata,xr=xr,yr=yr,axis_style=4,/current, $
                     position=loc,font_size=label_size)
         t=text(mean(xr),mean(yr),'data missing for '+team[zp[i]], $
                /data,align=0.5,target=pe,font_size=label_size)
         avgdt = 0.0
         speed = 0.0
         stdev = 0.0
      endelse

      if model then begin
;         jdstr,jd[zmark],3,jds
;         print,'First marked point ',jds[0]
;         print,'First marked point ',jds[1]
         mdt=double(mt1-mt0)/double(mnpts-1)
         mdtime=findgen(mnpts)*mdt + mt0
         mflux=replicate(1.0,mnpts)
         midval = (1.0+baseline)/2.0
         deltat=(time[zmark[0]+1]-time[zmark[0]-1])/2.0
;         print,'D Time spacing',deltat,' seconds'
         t1=time[zmark[0]]+(flux[zmark[0]]/meanval-midval)*deltat
         deltat=(time[zmark[1]+1]-time[zmark[1]-1])/2.0
;         print,'R Time spacing',deltat,' seconds'
         t2=time[zmark[1]]+(midval-flux[zmark[1]]/meanval)*deltat
         z=where(mdtime ge t1 and mdtime le t2,count)
         if count ne 0 then mflux[z]=baseline
      endif

      label=team[zp[i]]+'!C'+ $
            strn(xtrack[zp[i]],format='(f10.1)')+' km'+'!C'+ $
            strmid(jdmids[zp[i]],11)+'!C'+ $
            '$\sigma$='+strn(stdev,format='(f10.2)')+'!C'+ $
            '$\Delta$t='+strn(avgdt,format='(f10.3)')+' s!C'+ $
            '('+strn(avgdt*speed,format='(f10.3)')+' km)'
      label=repchar(label,'-','$X$')
      label=repchar(label,'$X$','$-$')
      t=text(1.0-pinfo.right+labeloffset,(loc[1]+loc[3])/2.0,label,target=pe, $
             /data,clip=0,align=0.5,vertical_alignment=0.5,/normal)
      ax=axis('Y',location=xr[0],target=pe,tickinterval=ytickinterval,tickdir=1)
      ax=axis('Y',location=xr[1],target=pe,axis_range=yr, $
              major=3,minor=3,tickdir=0,tickname=blanks)
      if plotnum eq pinfo.np-1 then $
         ax=axis('X',location=yr[0]-0.2,target=pe,tickfont_size=label_size, $
                 title='Time (seconds) relative to reference orbit prediction')

      if model then begin
;         print,'times of first and last marked points', $
;            time[zmark[0]],time[zmark[1]]
;         oplot,time[zmark[0]:zmark[1]],flux[zmark[0]:zmark[1]],psym=8, $
;            color=cpalette(35),symsize=1.5
         jd_d=jdmid+double(t1)/86400.0
         jd_r=jdmid+double(t2)/86400.0
;         print,'JD D&R',jd_d,jd_r,format='(a,1x,f16.8,1x,f16.8)'
;         jdstr,jd_d,3,jd_ds
;         jdstr,jd_r,3,jd_rs
;         dstr='D: '+jd_ds
;         rstr='R: '+jd_rs
;         print,dstr,' ',rstr
;         tlen=t2-t1
;         cstr='Chord: '+strn(tlen,format='(f10.3)')+' sec, ' + $
;                        strn(tlen*ginfo.speed,format='(f10.1)')+' km, '
;         xyouts,0.15,0.88,dstr,align=0,color=cpalette(35),/normal,charsize=1.5
;         xyouts,0.15,0.86,rstr,align=0,color=cpalette(35),/normal,charsize=1.5
;         xyouts,0.15,0.84,cstr,align=0,color=cpalette(35),/normal,charsize=1.5
         pm = plot(/current,/overplot,mdtime,mflux,color=cpalette(35,/array))
;         oplot,mdtime,mflux,color=cpalette(35)
      endif

      plotnum++

      if plotnum eq pinfo.np or i eq countp-1 then begin
         if page eq 0 and i eq countp-1 then tag='' $
         else tag=string(byte('a')+byte(page))
         fnroot=fnstem+tag
         fneps=fnroot+'.eps'
         fnpdf=fnroot+'.pdf'
         fnpng=fnroot+'.png'
         if not nosave then begin
            print,'save to ',fnpdf
            p0.save,fnpdf
            print,'save to ',fneps
            p0.save,fneps
            print,'save to ',fnpng
            p0.save,fnpng,width=pinfo.dim[0]
         endif
         plotnum=0
         page++
      endif

   endfor

bailout:
   free_lun,pipe

end
