;+
; NAME:
;  occproc
; PURPOSE:   (one line only)
;  Aperture photometry of occultation data
; DESCRIPTION:
;
;  This tool is for an initial aperture photometry extraction of a single
;    source from a set of FITS images, usually from a stellar occultation
;    observation.  It is currently designed for use on data taken with
;    the QHY174M-GPS camera.  However, the only thing specific to this camera
;    is the use of BACKSUB.PRO that removes a row-by-row level from the
;    image to eliminate the detector bias striping.  The philosophy
;    here is to build a tool that makes it fast and easy to process good
;    quality data that are collected in the recommended fashion.  It is NOT
;    intended to help you through the possibly agonizing process of dealing
;    with challenging data whether due to dodgy weather conditions or
;    errant observing procedures.
;
;  This GUI is designed to work from a master root directory under which
;    all different occultation campaign results are stored.  In this
;    directory there must be a helper program, getddir.pro, that will
;    return a valid location for data given a relative directory.  This
;    is designed to allow off-site and on-site reductions as well as not
;    requiring all data to be co-located.  The calling sequence for this
;    routine is:
;       getddir,rdir,ddir
;    where rdir is the relative directory that will be provided by the user
;    through the GUI.  ddir is then the name of the top level directory
;    where that rdir can be found.
;
;  In the local directory the top level has directories that are named
;    for each project.  Inside each project directory the output files
;    that are generated for that project are stored here.  There is a
;    master configuration file (config.dat) that applies to all data
;    for the event.  This contains the name of the event and the UT date.
;    Each site has its own configuration file (ending in .dat).  There is
;    one file for each time/site combination that has per-site configuration.
;    The included information is an estimate of image quality, the aperture
;    used for photometry, and the position of the target star in the first
;    frame.  The files ending in .lc are the actual lightcurve files.
;    The columns:
;      1 - index number
;      2 - JD of image mid-time
;      3 - normalized flux
;      4 - X position
;      5 - Y position
;      6 - FWHM in pixels
;    The precision of the timing stored in the file is good to about
;      0.1 msec.
;
;  There are two levels for the nomenclature of datasets to process:
;    Project: This refers to a string that will identify the event.
;             There can be many datasets within one project.  Examples
;             are something like 'LE20181114' or 'RECON79'.  However,
;             this string is just a string and not parsed for content.
;             This string must not contain spaces and really shouldn't
;             contain anything other than letters and numbers.
;    Dataset: This refers to the data collected at a single site.  The
;             typical case here will be the name of a camera or a system
;             but could be a site or team name as well.  This depends
;             entirely on the project.  Again, this is just a string
;             with the same restrictions and suggests as for the project
;             string.
;
;  If there is only one site it is automatically selected.  Otherwise, you
;    have to select your site (look until the File menu).  Similarly, if
;    there is only one dataset (time), that is automatically loaded.  More
;    than one you have to select.
;
;  The first 20 frames are used to determine a baseline for the target star
;    signal.  After that is done, there is a threshold set of 10% of the
;    baseline.  If the star drops below this threshold, it's position is not
;    updated based on the previous exposure.  This lets it step over a short
;    occultation signal without losing positional lock on the target star.
;    Otherwise, the program will track the star as is moves slightly from
;    frame to frame.  If it moves too much between frames it will lose lock.
;    Data like this falls into the category of "difficult" and isn't (yet)
;    supported.  Likewise, if the occultation is very long and the field
;    drifts too much while the star is missing, it will lose lock.
;
; CATEGORY:
;  Photometry
; CALLING SEQUENCE:
;  occproc
; INPUTS:
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
; OUTPUTS:
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  Written by Marc W. Buie, Southwest Research Institute, 2020/01/13
;  2022/02/14, MWB, Small tweak to show the sky signal level from a probe
;  2022/08/10, MWB, major overhaul to add comp star measurements, some
;                     prior support files may no longer be compatible,
;                     (mostly the .dat for a given dataset).
;  2022/08/27, MWB, significant cleanup and debugging
;  2022/09/22, MWB, add a tool to mark all measurements as invalid to
;                     reset back to the beginning on the current object
;  2022/09/25, MWB, bug fixes on tracking, adding tool to step by measured
;                     positions
;  2023/02/13, MWB, minor bug fixes on display and setting maxfac
;  2023/03/14, MWB, split bad flags on raw data off into its own file, this
;                      will quietly convert the old files into the new format
;                      if encountered.
;  2023/03/15, MWB, added toffset to config.ini file options
;  2023/04/03, MWB, fixed a minor bug in the window menu event handler.
;  2023/11/07, MWB, introduced a minimum noise level in the image, set at 3 DN.
;                     This bypasses odd behavior when the background noise
;                     in the image isn't properly sampled.
;  2024/01/26, MWB, added a tool to build a PSF for each good image in the
;                     dataset.
;-
pro occproc_cleanup, tlb

   ; get pointer to state structure
   widget_control, tlb, get_uvalue=state

   ; Free up any dynamically allocated space with the state structure
   ptr_free,(*state).imlist
   ptr_free,(*state).bad
   ptr_free,(*state).sitelist
   ptr_free,(*state).timelist
   ptr_free,(*state).index
   ptr_free,(*state).jd
   ptr_free,(*state).flux
   ptr_free,(*state).fwhm
   ptr_free,(*state).x0
   ptr_free,(*state).y0
   ptr_free,(*state).type0
   ptr_free,(*state).xpos
   ptr_free,(*state).ypos
   ptr_free,(*state).sky
   ptr_free,(*state).valid
   ptr_free,(*state).srclist

   ; Free up the state structure itself.
   ptr_free, state

end

; Create a new source to measure
; set ctype to 'o' |--perhaps this should be set by a multi-select tool to
; set ptype to 'o' |    get this out of the way all at once
; add new column to cx0,cy0,c0type,cfwhm,cflux,cerr,cx,cy
;  initialize c0type to 'x'
pro occproc_addsource,state

   ; nothing exists, make the first source which is always the target star.
   if (*state).nsrc eq 0 then begin
;print,'addsource: init first source'

      (*state).nsrc=1
      (*state).idxsrc=0

      ptr_free,(*state).flux
      ptr_free,(*state).fwhm
      ptr_free,(*state).x0
      ptr_free,(*state).y0
      ptr_free,(*state).type0
      ptr_free,(*state).xpos
      ptr_free,(*state).ypos
      ptr_free,(*state).sky
      ptr_free,(*state).valid

      (*state).flux =ptr_new(fltarr((*state).nimages))
      (*state).fwhm =ptr_new(fltarr((*state).nimages))
      (*state).x0 =ptr_new(replicate(-1,(*state).nimages))
      (*state).y0 =ptr_new(replicate(-1,(*state).nimages))
      (*state).type0 =ptr_new(replicate('x',(*state).nimages))
      (*state).xpos =ptr_new(fltarr((*state).nimages))
      (*state).ypos =ptr_new(fltarr((*state).nimages))
      (*state).sky  =ptr_new(fltarr((*state).nimages))
      (*state).valid =ptr_new(bytarr((*state).nimages))

      text='add target for dataset'

   endif else begin
      (*state).nsrc=(*state).nsrc+1
      (*state).idxsrc=(*state).nsrc-1

      flux  = (*(*state).flux)
      fwhm  = (*(*state).fwhm)
      x0    = (*(*state).x0)
      y0    = (*(*state).y0)
      type0 = (*(*state).type0)
      xpos  = (*(*state).xpos)
      ypos  = (*(*state).ypos)
      sky   = (*(*state).sky)
      valid = (*(*state).valid)

      ptr_free,(*state).flux
      ptr_free,(*state).fwhm
      ptr_free,(*state).x0
      ptr_free,(*state).y0
      ptr_free,(*state).type0
      ptr_free,(*state).xpos
      ptr_free,(*state).ypos
      ptr_free,(*state).sky
      ptr_free,(*state).valid

      (*state).flux =ptr_new([[flux],[fltarr((*state).nimages)]])
      (*state).fwhm =ptr_new([[fwhm],[fltarr((*state).nimages)]])
      (*state).x0   =ptr_new([[x0],[fltarr((*state).nimages)]])
      (*state).y0   =ptr_new([[y0],[fltarr((*state).nimages)]])
      (*state).type0=ptr_new([[type0],[strarr((*state).nimages)]])
      (*state).xpos =ptr_new([[xpos],[fltarr((*state).nimages)]])
      (*state).ypos =ptr_new([[ypos],[fltarr((*state).nimages)]])
      (*state).sky  =ptr_new([[sky],[fltarr((*state).nimages)]])
      (*state).valid=ptr_new([[valid],[bytarr((*state).nimages)]])

      text='add source '+strn((*state).nsrc-1)+'for dataset'

   endelse

   occproc_postinfo,state,text

   occproc_savedataset,state
   occproc_managetarg,state

end

pro occproc_apextract,state,START=start

   if (*state).nimages le 0 then return

   if (*state).trackref gt 0 and (*state).idxsrc ne (*state).trackref then begin
      z=where((*(*state).valid)[*,(*state).trackref] ne 1 and $
              (*(*state).bad) eq 0,count)
      if count ne 0 then begin
         text=['The reference target has not been fully processed.', $
                strn(count)+' frames are still marked invalid that', $
                'are marked good.  You may need to run the lightcurve', $
                'extraction on the reference comp first.']
         res=qannounc(text,falselabel='',truelabel='Continue', $
                      title='Warning: reference comp stars not valid')
         return
      endif
      trackit=1
   endif else begin
      trackit=0
   endelse
;print,'trackit',trackit

   if (*state).objrad le 1.5 then begin
      text=['The objrad is unreasonbly small.', $
            'Refusing to run lightcurve extraction.']
      res=qannounc(text,falselabel='',truelabel='Continue', $
                   title='Error: invalid objrad')
      return
   endif

   if badpar(start,[0,2,3],0,caller='xxx',default=0) then return

   doit=replicate(0B,(*state).nimages)
   i0=start
   if (*state).xlimit eq 0 then begin
      i1=(*state).nimages-1
   endif else begin
      i1=i0+(*state).xlimit < (*state).nimages-1
   endelse
   doit[i0:i1]=1B
   z=where(doit,count)
   if count ne 0 then $
      (*(*state).valid)[i0:i1,(*state).idxsrc] = 0B

   z=where((*(*state).type0)[*,(*state).idxsrc] ne 'm' and doit,count)
   if count ne 0 then (*(*state).type0)[z,(*state).idxsrc] = 'x'

   mkcircle,0,0,1.0,xc,yc

   objrad = (*state).objrad/(*state).binfac
   ncheck=20
   occproc_postinfo,state, $
      'Start lightcurve extraction on '+strn((*state).nimages)+' frames',/clear
   nobs=0+start
   for i=i0,i1 do begin
      if (*(*state).bad)[i] eq 1 or doit[i] eq 0 then continue
      nobs++
      if nobs eq ncheck and (*state).idxsrc eq 0 then begin
;print,(*(*state).flux)[0:ncheck-1]
         robomean,(*(*state).flux)[0:ncheck-1,(*state).idxsrc],3.0,0.5,avg
;print,'avg',avg
         (*state).thresh=0.1*avg
         str='Threshold set to '+strn(long((*state).thresh))
         occproc_postinfo,state,str
      endif
      occproc_loadimage,state,i,image,hdr,error
      if error then begin
         print,'error loading image ',strn(i)
         return
      endif
      if (*(*state).jd)[i] eq 0. then begin
         exptime=float(sxpar(hdr,'EXPTIME'))
         (*(*state).jd)[i]=jdparse(sxpar(hdr,'DATE-OBS'))+ $
                              exptime/86400.0d0/2.0d0
      endif
      xt=0.
      yt=0.
      if trackit then begin
         if (*(*state).type0)[(*state).imidx,(*state).idxsrc] eq 'm' then begin
            x0 = fix(round((*(*state).x0)[(*state).imidx,(*state).idxsrc]))
            y0 = fix(round((*(*state).y0)[(*state).imidx,(*state).idxsrc]))
;print,'A',i,x0,y0
         endif else begin
            bad=replicate(1B,(*state).nimages)
            zg=where((*(*state).bad) eq 0 and $
                    (*(*state).valid)[*,(*state).trackref] eq 1 and $
                    (*(*state).valid)[*,(*state).idxsrc] eq 1 and $
                    (*(*state).index) ne (*state).imidx,countg)
            if countg eq 0 then begin
               text=['This is an unexpected error.  Nothing valid to work', $
                     'with could be found.']
               res=qannounc(text,falselabel='',truelabel='Continue', $
                            title='Warning: no comp stars defined yet')
               return
            endif
            bad[zg]=0B
            x0 = (*(*state).xpos)[*,(*state).idxsrc]
            y0 = (*(*state).ypos)[*,(*state).idxsrc]
            xt = (*(*state).xpos)[*,(*state).trackref]
            yt = (*(*state).ypos)[*,(*state).trackref]
            dx = x0-xt
            dy = y0-yt
            if countg eq 1 then begin
               dx = trimrank(dx[zg])
               dy = trimrank(dy[zg])
            endif else if countg lt 3 then begin
               dx = mean(dx[zg])
               dy = mean(dy[zg])
            endif else if countg lt 50 then begin
               robomean,dx,3.0,0.5,avgdx,bad=bad
               robomean,dy,3.0,0.5,avgdy,bad=bad
               robomean,dx,3.0,0.5,avgdx,bad=bad
               dx = avgdx
               dy = avgdy
            endif else begin
               dt=((*(*state).jd)-(*(*state).jd)[0]) * 86400.0
               cx=goodpoly(dt,dx,1,3.0,bad=bad)
               cy=goodpoly(dt,dy,1,3.0,bad=bad)
               cx=goodpoly(dt,dx,1,3.0,bad=bad)
               dx=trimrank(poly(dt[(*state).imidx],cx))
               dy=trimrank(poly(dt[(*state).imidx],cy))
            endelse

            xt = (*(*state).xpos)[(*state).imidx,(*state).trackref]
            yt = (*(*state).ypos)[(*state).imidx,(*state).trackref]
            x0 = fix(round(xt + dx))
            y0 = fix(round(yt + dy))
            (*(*state).x0)[(*state).imidx,(*state).idxsrc] = x0
            (*(*state).y0)[(*state).imidx,(*state).idxsrc] = y0
            (*(*state).type0)[(*state).imidx,(*state).idxsrc] ='t'
         endelse
      endif else begin
         if (*(*state).type0)[(*state).imidx,(*state).idxsrc] eq 'm' then begin
            x0 = (*(*state).x0)[(*state).imidx,(*state).idxsrc]
            y0 = (*(*state).y0)[(*state).imidx,(*state).idxsrc]
         endif else begin
            x0 = fix(round((*(*state).xpos)[(*state).imidx-1,(*state).idxsrc]))
            y0 = fix(round((*(*state).ypos)[(*state).imidx-1,(*state).idxsrc]))
            (*(*state).x0)[(*state).imidx,(*state).idxsrc] = x0
            (*(*state).y0)[(*state).imidx,(*state).idxsrc] = y0
            (*(*state).type0)[(*state).imidx,(*state).idxsrc] ='p'
         endelse
      endelse
      x0 = x0/(*state).binfac
      y0 = y0/(*state).binfac
      basphote,1.0,image,1.0,x0,y0,objrad,(*state).sky1,-(*state).sky2, $
         /nolog,/silent,flux=flux0,xcen=xpos0,ycen=ypos0,fwhm=fwhm0, $
         boxmrad=objrad*(*state).maxfac,max=peaksig,skymean=sky0
      diff=sqrt((x0-xpos0)^2+(y0-ypos0)^2)

      x0    = x0*(*state).binfac
      y0    = y0*(*state).binfac
      xpos0 = xpos0*(*state).binfac
      ypos0 = ypos0*(*state).binfac

      (*(*state).flux)[(*state).imidx,(*state).idxsrc] = flux0
      (*(*state).fwhm)[(*state).imidx,(*state).idxsrc] = fwhm0
      if flux0 gt (*state).thresh or (*state).idxsrc ne 0 then begin
         (*(*state).xpos)[(*state).imidx,(*state).idxsrc] = fix(round(xpos0))
         (*(*state).ypos)[(*state).imidx,(*state).idxsrc] = fix(round(ypos0))
      endif else begin
         (*(*state).xpos)[(*state).imidx,(*state).idxsrc] = x0
         (*(*state).ypos)[(*state).imidx,(*state).idxsrc] = y0
      endelse
      (*(*state).sky)[(*state).imidx,(*state).idxsrc]  = sky0
      (*(*state).valid)[(*state).imidx,(*state).idxsrc] = 1B

      occproc_showproject,state

      oplot,objrad*xc+xpos0,objrad*yc+ypos0,color=cpalette(1)

      if (i mod 100) eq 0 then occproc_showlc,state,/silent

   endfor

   occproc_showlc,state
   occproc_savesource,state

end

pro occproc_buildpsf,state,START=start

   if (*state).nimages le 0 then return

   if (*state).trackref eq 0 then return

   z=where((*(*state).valid)[*,(*state).trackref] ne 1 and $
           (*(*state).bad) eq 0,count)
   if count ne 0 then begin
      text=['The reference target has not been fully processed.', $
             strn(count)+' frames are still marked invalid that', $
             'are marked good.  You may need to run the lightcurve', $
             'extraction on the reference comp first.']
      res=qannounc(text,falselabel='',truelabel='Continue', $
                   title='Warning: reference comp star not valid')
      return
   endif

   if badpar(start,[0,2,3],0,caller='xxx',default=0) then return

   doit=replicate(0B,(*state).nimages)
   i0=start
   if (*state).xlimit eq 0 then begin
      i1=(*state).nimages-1
   endif else begin
      i1=i0+(*state).xlimit < (*state).nimages-1
   endelse
   doit[i0:i1]=1B
   z=where(doit,count)

   ; check for pre-existing PSF files, unmark doit if found
; XXX TBD

   objrad = (*state).objrad/(*state).binfac
   occproc_postinfo,state, $
      'Start PSF extraction on '+strn((*state).nimages)+' frames',/clear
   nobs=0+start
   for i=i0,i1 do begin
      if (*(*state).bad)[i] eq 1 or doit[i] eq 0 then continue
      psfdir=(*state).project+'/PSF'
      if not exists(psfdir) then file_mkdir,psfdir
      if (*state).binfac eq 1 then begin
         fnpsf=(*state).project+'/PSF/'+(*state).site+'.'+ $
               (*state).time+'.'+strn((*state).idxsrc)+ $
               '.'+string(i,format='(i5.5)')+'.psf'
      endif else begin
         fnpsf=(*state).project+'/PSF/'+(*state).site+'.'+ $
              (*state).time+'.'+strn((*state).binfac)+'.'+ $
              strn((*state).idxsrc)+'.'+string(i,format='(i5.5)')+'.psf'
      endelse
      if exists(fnpsf) then continue
      nobs++
      occproc_loadimage,state,i,image,hdr,error
      if error then begin
         print,'error loading image ',strn(i)
         return
      endif

      findsrc,image,exptime=(*state).exptime,gap=round(objrad), $
         maxphotsig=80000L,/nodisplay, $
         /nocrs,/noinfo,results=res,/silent
      if res.nobj le 3 then begin
         occproc_postinfo,state, $
            'Image '+strn(i)+' has 3 or less detected sources.'
         continue
      endif

      maxphotsig = 59000.0
      minfwhm = 1.5
      snrlimit = 20.0
      srcbad=bytarr(res.nobj)
      peak=image[round(res.xc),round(res.yc)]
      z=where(peak ge maxphotsig,count)
      if count ne 0 then srcbad[z]=1B
      zg=where(srcbad eq 0B,countg)
      if countg le 3 then begin
         occproc_postinfo,state, $
            'Image '+strn(i)+' has 3 or less non-saturated sources.'
         continue
      endif
      z=where(res.fwhm lt minfwhm,count)
      if count ne 0 then srcbad[z]=1B
      zg=where(srcbad eq 0B,countg)
      if countg le 3 then begin
         occproc_postinfo,state, $
            'Image '+strn(i)+' has 3 or less sources with good FWHM.'
         continue
      endif
      z=where(res.snr le snrlimit and srcbad eq 0,count)
      if count ne 0 then srcbad[z]=1B
      zg=where(srcbad eq 0B,countg)
      if countg le 3 then begin
         occproc_postinfo,state, $
            'Image '+strn(i)+' has 3 or less sources with good SNR.'
         continue
      endif
      meanerr2,res.fwhm[zg],res.snr[zg],avgfwhmw,sig
      
      srcdir=(*state).project+'/Src'
      if not exists(srcdir) then file_mkdir,srcdir
      if (*state).binfac eq 1 then begin
         fnsrc=(*state).project+'/Src/'+(*state).site+'.'+ $
               (*state).time+'.'+strn((*state).idxsrc)+ $
               '.'+string(i,format='(i5.5)')+'.src'
      endif else begin
         fnsrc=(*state).project+'/Src/'+(*state).site+'.'+ $
              (*state).time+'.'+strn((*state).binfac)+'.'+ $
              strn((*state).idxsrc)+'.'+string(i,format='(i5.5)')+'.src'
      endelse

      data=[[res.xc[zg]],[res.yc[zg]],[res.fwhm[zg]], $
            [res.mag[zg]],[res.err[zg]],[res.snr[zg]]] 
      mkhdr,hdr,data
      sxaddpar,hdr,'SITE',(*state).site,' site identification'
      sxaddpar,hdr,'EVENT',(*state).project,' Event identification'
      sxaddpar,hdr,'GAP',round(objrad),' Object gap size, is approximately the FWHM'
      sxaddpar,hdr,'OBJRAD',round(objrad),' Object aperture radius for photometry'
      sxaddpar,hdr,'MAXSIG',maxphotsig,' Saturated above this DN level'
      sxaddpar,hdr,'MEANFWHM',avgfwhmw,' Mean FWHM in pixels'
      sxaddpar,hdr,'SKYLEVEL',res.avgsky,' Average sky signal level counts/pixel'
      sxaddpar,hdr,'SKYSIGMA',res.skysg,' Standard deviation of the sky signal'
      sxaddpar,hdr,'OBSCURA1',res.obscura1, $
         ' Fraction of image obscured by 50*skysig bright pixels'
      sxaddpar,hdr,'OBSCURA2',res.obscura2, $
         ' Fraction of image obscured by 5*skysig bright pixels'

print,'Saving ',fnsrc
      writefits,fnsrc,data,hdr

      ; extract PSF and save
      psfstack,image,res.xc[zg],res.yc[zg],psf,dw=2*round(avgfwhmw),/silent
      sz=size(psf,/dimen)
      nx=sz[0]
      ny=sz[1]
print,'Saving ',fnpsf
      writefits,fnpsf,psf
   endfor

end

pro occproc_loadbadfile,state

   if (*state).binfac eq 1 then begin
      fnbad=(*state).project+'/Raw/'+(*state).site+'.'+ $
            (*state).time+'.bad'
      fnraw=(*state).project+'/Raw/'+(*state).site+'.'+ $
            (*state).time+'.0.raw'
   endif else begin
      fnbad=(*state).project+'/Raw/'+(*state).site+'.'+ $
           (*state).time+'.'+strn((*state).binfac)+'.bad'
      fnraw=(*state).project+'/Raw/'+(*state).site+'.'+ $
           (*state).time+'.'+strn((*state).binfac)+'.0.raw'
   endelse

   if exists(fnbad) then begin
      readcol,fnbad,idx,bad,format='l,i'
      (*(*state).bad)[idx]=bad
      return
   endif

   ; if the file isn't found there are two possibilities:
   ;  1) no raw data yet (in this case just return, nothing to do)
   if not exists(fnraw) then return

   ;  2) Read the raw source file for target (src=0), pull bad flag
   ;      out here and push to structure, we'll leave the bad flag in
   ;      the file but no new files will be written with that data.
   readcol,fnraw,index,x0,y0,type0,jd,flux,xpos,ypos,skymean,fwhm,bad, $
      format='l,i,i,a,d,f,f,f,f,f,b',count=nimages
   (*(*state).bad)[index]=bad

end

pro occproc_loadrawdata,state

   if (*state).nsrc eq 0 then return

;print,'loadrawdata: nsrc=',(*state).nsrc

   occproc_loadbadfile,state

   nsrc = (*state).nsrc

   (*state).nsrc=0
   for i=0,nsrc-1 do begin

;print,i,' call occproc ****************'
      occproc_addsource,state
;
      if (*state).binfac eq 1 then begin
         fnraw=(*state).project+'/Raw/'+(*state).site+'.'+ $
               (*state).time+'.'+strn((*state).idxsrc)+'.raw'
      endif else begin
         fnraw=(*state).project+'/Raw/'+(*state).site+'.'+ $
              (*state).time+'.'+strn((*state).binfac)+'.'+ $
              strn((*state).idxsrc)+'.raw'
      endelse
;print,fnraw

      if not exists(fnraw) then begin
         text=['no prior data '+fnraw, $
              'creating empty target']
         occproc_postinfo,state,text
         flux=fltarr((*state).nimages)
         fwhm=fltarr((*state).nimages)
         x0=intarr((*state).nimages)
         y0=intarr((*state).nimages)
         type0=replicate('x',(*state).nimages)
         xpos=fltarr((*state).nimages)
         ypos=fltarr((*state).nimages)
         skymean=fltarr((*state).nimages)
         skymean=fltarr((*state).nimages)
         bad=replicate(0B,(*state).nimages)
         valid=0B
      endif else begin

         occproc_postinfo,state,'load '+fnraw
         readcol,fnraw,index,x0,y0,type0,jd,flux,xpos,ypos,skymean,fwhm, $
            format='l,i,i,a,d,f,f,f,f,f',count=nimages

         if (*state).nimages ne nimages then begin
            print,'mismatch in number of files and length of saved raw data'
            print,'Expecting ',strn((*state).nimages)
            print,'Found ',strn(nimages),' in the data file.'
            return
         endif

         if i eq 0 then (*(*state).jd)[*] = jd
         valid=1B
      endelse

      (*(*state).flux)[*,(*state).idxsrc]=flux
      (*(*state).fwhm)[*,(*state).idxsrc]=fwhm
      (*(*state).x0)[*,(*state).idxsrc]=x0
      (*(*state).y0)[*,(*state).idxsrc]=y0
      (*(*state).type0)[*,(*state).idxsrc]=type0
      (*(*state).xpos)[*,(*state).idxsrc]=xpos
      (*(*state).ypos)[*,(*state).idxsrc]=ypos
      (*(*state).sky)[*,(*state).idxsrc]=skymean
      (*(*state).valid)[*,(*state).idxsrc]=valid

   endfor
   (*state).idxsrc=0

;print,'loadrawdata end: nsrc=',(*state).nsrc,' set to source 0'
end

pro occproc_loadproject,state,newproject,RELOAD=reload

   if newproject eq '' then return
;print,'loadproject'

   (*state).project=newproject
   fnproject=(*state).project+'/config.dat'

   text='loadproject: '+(*state).project
   if keyword_set(reload) then text=text+' reload'
   occproc_postinfo,state,text,/clear

   occproc_cleardisplay,state

   if keyword_set(reload) then begin
;print,'loadproject: reload'
      ; Set project information to starting empty values in case this project
      ;   hasn't been fully configured
      (*state).rdir=''
      (*state).date=''
      (*state).nsites=0
      (*state).site=''
      (*state).time=''
      (*state).ntimes=0
      (*state).nimages=0
      (*state).seeing=-1.0
      (*state).objrad=-1.0
      (*state).nsrc=0
      widget_control,(*state).idxsliderid,set_value=0,set_slider_max=1
   endif

   if exists(fnproject) then begin
      occproc_postinfo,state,'load existing project file '+fnproject
      openr,lun,fnproject,/get_lun
      if eof(lun) then begin
         free_lun,lun
         return
      endif
      line=''
      readf,lun,line,format='(a)'
      (*state).rdir=strtrim(line,2)
      getddir,(*state).rdir,ddir
      (*state).ddir=ddir
      if eof(lun) then begin
         free_lun,lun
         return
      endif
      readf,lun,line,format='(a)'
      (*state).date=strtrim(line,2)
      free_lun,lun
   endif

   ; first get the list of sites that are seen here, not all may be relevant
   ddir=(*state).ddir+(*state).rdir
   if ddir eq '' then begin
      text=['No data found for project,', $
            'dir:['+(*state).ddir+(*state).rdir+']']
      occproc_postinfo,state,text
      return
   endif
   if (*state).nsites eq 0 then begin
      text=['Search for site data directories in',ddir+'*']
      occproc_postinfo,state,text
      sitelist=file_search(ddir+'*',/test_directory,count=nsites)
      if nsites eq 0 then return
      sitelist=strmid(sitelist,strlen(ddir))
      good=bytarr(nsites)
      for i=0,nsites-1 do begin
         dircheck=ddir+sitelist[i]+'/'+(*state).date
         good[i]=exists(dircheck)
      endfor
      z=where(good eq 1,nsites)
      (*state).nsites=nsites
      if nsites eq 0 then return
      sitelist=sitelist[z]
      ptr_free,(*state).sitelist
      (*state).sitelist=ptr_new(sitelist)
      if nsites eq 1 then begin
         widget_control,(*state).setsiteid, set_value=(*(*state).sitelist)[0]
         (*state).site=(*(*state).sitelist)[0]
      endif else begin
         widget_control,(*state).setsiteid, set_value=''
         (*state).site=''
      endelse
   endif
   text=strjoin((*(*state).sitelist),' ')
   occproc_postinfo,state,text
   if (*state).site eq '' then begin
      occproc_showproject,state
      return
   endif

   if (*state).ntimes eq 0 then begin
      dircheck=ddir+(*state).site+'/'+(*state).date+'/'
      occproc_postinfo,state,dircheck
      timelist=file_search(dircheck+'*',/test_directory,count=ntimes)
      (*state).ntimes=ntimes
      if ntimes eq 0 then begin
         widget_control,(*state).settimeid,sensitive=0
         return
      endif
      timelist=strmid(timelist,strlen(dircheck))
      ptr_free,(*state).timelist
      (*state).timelist=ptr_new(timelist)
      if ntimes eq 1 then begin
         widget_control,(*state).settimeid,set_value=(*(*state).timelist)[0]
         (*state).time=(*(*state).timelist)[0]
      endif else begin
         widget_control,(*state).settimeid,set_value=''
         (*state).time=''
      endelse
   endif
   text=(*state).site+': '+strjoin((*(*state).timelist),' ')
   occproc_postinfo,state,text
   if (*state).time eq '' then begin
      occproc_showproject,state
      return
   endif
;print,'loadproject, continue, nsrc=',(*state).nsrc

   if (*state).nimages eq 0 then begin
      dircheck=ddir+(*state).site+'/'+(*state).date+'/'+(*state).time+'/'
      occproc_postinfo,state,dircheck
      (*state).path=dircheck
      imlist=file_search(dircheck+'*.fits',count=nimages)
      (*state).nimages=nimages
      if nimages eq 0 then return
      ; load list of bad images, if not found, assume all good
      ;  XXX to do
      bad=bytarr(nimages)
      imlist=strmid(imlist,strlen(dircheck))
      ptr_free,(*state).imlist
      ptr_free,(*state).index
      ptr_free,(*state).jd
      ptr_free,(*state).bad
      (*state).imlist=ptr_new(imlist)
      (*state).index=ptr_new(lindgen(nimages))
      (*state).jd=ptr_new(dblarr(nimages))
      (*state).bad=ptr_new(bad)
      widget_control,(*state).idxsliderid,set_slider_max=nimages-1
   endif

;print,'loadproject: b',(*state).nsrc
   occproc_loaddataset,state
;print,'loadproject: c',(*state).nsrc
   occproc_loadimage,state,0,image,hdr,error,/reset_scroll
;print,'loadproject: d',(*state).nsrc
   if error then return

   if (*state).seeing le 0 then begin
      text='Measuring first frame for seeing.'
      occproc_postinfo,state,text
      seeing,image,fwhm,objrad,nstars,fwhmguess=6,/nodisplay
      text=strn(nstars)+' sources found'
      occproc_postinfo,state,text
      if fwhm gt 0 then begin
         fwhm=fwhm*(*state).binfac
         objrad=objrad*(*state).binfac
      endif
      (*state).seeing=fwhm
      (*state).objrad=objrad
      occproc_savedataset,state
   endif
;print,'loadproject: e',(*state).nsrc

;print,'occproc_loadproject partial convertion, not done yet'

   str='FWHM   = '+string((*state).seeing,format='(f4.1)')
   widget_control,(*state).seeingid,set_value=str
   str='ObjRad = '+string((*state).objrad,format='(f4.1)')
   widget_control,(*state).objradid,set_value=str
   str='Binfac = '+string((*state).binfac,format='(f4.1)')+ $
        ', maxfac = '+ strn((*state).maxfac,format='(f10.1)')
   widget_control,(*state).binfacid,set_value=str

;print,'loadproject: f',(*state).nsrc

   occproc_loadrawdata,state
;print,'loadproject: g',(*state).nsrc
;print,'xxx showproject'
   occproc_showproject,state
;print,'refresh plot'
   occproc_showlc,state
;print,'loadproject: h',(*state).nsrc

end

pro occproc_managetarg,state

;print,'managetarg: nsrc=',(*state).nsrc

   if (*state).nimages eq 0 then begin
      widget_control,(*state).srcid,sensitive=0
      return
   endif

   if (*state).nsrc eq 1 and (*state).doratio then begin
      (*state).doratio=0
      widget_control,(*state).ratioid,set_value=0
   endif

   if (*state).idxsrc ge (*state).nsrc then begin
;print,'managetarg; rest idxsrc, >=nsrc  nsrc,idxsrc', $
;   (*state).nsrc,(*state).idxsrc
      (*state).idxsrc = 0
   endif

   ; nsrc and idxsrc are assumed to be as desired, build srclist as needed
   ;   and update the srcid dropdown widget value and state

;print,'managetarg: srclist size=',n_elements((*state).srclist)
   if (*state).nsrc ne n_elements((*state).srclist) then begin
      srclist=strarr((*state).nsrc)
      for i=0,(*state).nsrc-1 do begin
         if i eq 0 then begin
            str='Target'
         endif else begin
            str='Comp'+strn(i-1)
         endelse
         srclist[i]=str
      endfor
      ptr_free,(*state).srclist
      (*state).srclist=ptr_new(srclist)
      if (*state).nsrc eq 1 then widget_control,(*state).srcid,sensitive=0 $
      else widget_control,(*state).srcid,sensitive=1
   endif

   widget_control,(*state).srcid,set_value=srclist, $
      set_combobox_select=(*state).idxsrc

;print,'managetarg end: nsrc=',(*state).nsrc
end

; adds text to the running log window
pro occproc_postinfo,state,text,CLEAR=clear
   if keyword_set(clear) then begin
      widget_control,(*state).noticeid,set_value=text
   endif else begin
      widget_control,(*state).noticeid,get_value=msg
      if n_elements(msg) eq 1 and msg[0] eq '' then begin
         msg=text
      endif else begin
         msg=[msg,text]
      endelse
      widget_control,(*state).noticeid,set_value=msg
   endelse
end

pro occproc_showlc,state,SILENT=silent

   if (*state).nimages eq 0 then return

   occproc_makefinal,state,lc,zg,countg,zb,countb
   if countg eq 0 then return

   widget_control,(*state).plotwin1,get_value=winnum
   wset,winnum
   if (*state).doratio and (*state).idxsrc ne 1 then begin
      ytitle='Normalized flux'
   endif else begin
      ytitle='Flux (DN)'
   endelse
   if (*state).idxsrc eq 0 then begin
      title='Target'
   endif else begin
      title='Comp'+strn((*state).idxsrc-1)
   endelse
   plot,(*(*state).index)[zg],lc[zg], $
      xr=[0,(*state).nimages-1],yr=[0,max(lc[zg])], $
      psym=3,background='ffffff'xl,color=0, $
      xtitle='Frame index number',ytitle=ytitle,title=title
   if countb ne 0 then $
      oplot,(*(*state).index)[zb],lc[zb], $
         psym=8,symsize=0.8,color=cpalette(1)

   if not keyword_set(silent) then begin

      z=where(lc eq 0 or (*(*state).bad) eq 1 or $
              (*(*state).valid)[*,(*state).idxsrc] eq 0,count)
      bad=bytarr((*state).nimages)
      if count ne 0 then bad[z]=1B
      robomean,lc,3.0,0.5,avgflux,dummy,stdev,bad=bad
      robomean,(*(*state).fwhm)[*,(*state).idxsrc],3.0,0.5,avgfwhm,bad=bad
      robomean,(*(*state).sky)[*,(*state).idxsrc],3.0,0.5,avgsky,bad=bad

      text=['Average FWHM = '+string(avgfwhm*(*state).binfac), $
            'Average signal = '+string(avgflux), $
            'Signal/noise ratio = '+string(avgflux/stdev), $
            'Average sky = '+string(avgsky)]
      occproc_postinfo,state,text

   endif

end

pro occproc_loaddataset,state
   if (*state).project ne '' and (*state).site ne '' $
                             and (*state).time ne '' then begin
;print,'loaddataset'
      fndataset=(*state).project+'/'+(*state).site+'.'+(*state).time+'.dat'
      if not exists(fndataset) then begin
         (*state).seeing = -1.0
         (*state).objrad = -1.0
         (*state).binfac = 1 
         (*state).maxfac = 1
         (*state).nsrc   = 0
         (*state).trackref = -1
      endif else begin

         text='Load '+fndataset
         occproc_postinfo,state,text

         openr,lun,fndataset,/get_lun
         readf,lun,seeing
         (*state).seeing=seeing
         readf,lun,objrad
         (*state).objrad=objrad

         binfac=1
         maxfac=1.0
         nsrc=0
         trackref=0
         if not eof(lun) then readf,lun,binfac
         if not eof(lun) then readf,lun,maxfac
         if not eof(lun) then readf,lun,nsrc
         if not eof(lun) then readf,lun,trackref
         (*state).binfac=binfac
         (*state).maxfac=maxfac
         (*state).nsrc  =nsrc
         (*state).trackref  =trackref
         free_lun,lun
      endelse
      (*state).idxsrc=0
      if (*state).nsrc eq 0 then begin
         occproc_addsource,state
         occproc_savedataset,state
      endif
;print,'loaddataset: nsrc',(*state).nsrc
      occproc_managetarg,state
   endif
;print,'loaddataset end: nsrc',(*state).nsrc
end

pro occproc_cleardisplay,state

   widget_control,(*state).setprojid,set_value=''
   widget_control,(*state).setrdirid,set_value=''
   widget_control,(*state).setdateid,set_value=''
   widget_control,(*state).setsiteid,set_value=''
   widget_control,(*state).settimeid,set_value=''
   widget_control,(*state).seeingid,set_value=' '
   widget_control,(*state).objradid,set_value=' '
   widget_control,(*state).imageid,set_value=' '
   widget_control,(*state).targvalidid,set_value=' '
   widget_control,(*state).drawwin,get_value=winnum
   wset,winnum
   erase,0
   widget_control,(*state).plotwin1,get_value=winnum
   wset,winnum
   erase,'ffffff'xl

end

pro occproc_loadimage,state,index,image,hdr,error,RESET_SCROLL=reset_scroll

   error=1
   if (*state).nimages eq 0 then return
   if index lt 0 or index ge (*state).nimages then return

   (*state).imidx=index
   widget_control,(*state).imageid, $
      set_value=(*(*state).imlist)[(*state).imidx]
   widget_control,(*state).frameidxid,set_value=strn((*state).imidx)
   widget_control,(*state).idxsliderid,set_value=(*state).imidx
   image=readfits((*state).path+(*(*state).imlist)[(*state).imidx],hdr)
   image=float(image)
   skysclim,image,lowval,hival,skymean,npts=20000
   exptime=float(sxpar(hdr,'EXPTIME'))
   (*state).skymean = skymean
   (*state).exptime = exptime
   backsub,image,/row
   image += skymean
   sz=size(image,/dimen)

   if (*state).binfac gt 1 then begin
      binfac=(*state).binfac
      newnx=sz[0]/binfac
      newny=sz[1]/binfac
      image=rebin(image[0:binfac*newnx-1,0:binfac*newny-1],newnx,newny)* $
               float(binfac)^2
      sz=size(image,/dimen)
   endif
   (*state).nx=sz[0]
   (*state).ny=sz[1]
   widget_control,(*state).drawwin,draw_xsize=sz[0],draw_ysize=sz[1]
   if keyword_set(reset_scroll) then begin
      cx=(*state).nx/2
      cy=(*state).ny/2
      sx = cx-(*state).xviewsize/2
      sy = cy-(*state).yviewsize/2
      sx = ((sx>0) < ((*state).nx-(*state).xviewsize-1))>0
      sy = ((sy>0) < ((*state).ny-(*state).yviewsize-1))>0
      widget_control,(*state).drawwin, $
         set_draw_view=([sx,sy])
   endif
   widget_control,(*state).drawwin,get_value=winnum
   wset,winnum
   skysclim,image,lowval,hival,meanval,sigma,npts=30000,lowclip=0.2,hiclip=0.8
   sigma = sigma > 3.0
   (*state).sky1 = meanval
   (*state).sky2 = sigma/sqrt(30000.)
   lowsig = -3.0
   hisig  =  8.0
   lowval = max([meanval+lowsig*sigma,min(image)])
   hival  = min([meanval+hisig*sigma,max(image)])
   bim=bytscl(image,min=lowval,max=hival,top=255)
   tv,bim

;print,'loadimage, nx,ny',(*state).nx,(*state).ny,sz[0],sz[1]
   plot,[0],[1],/nodata,xmargin=[0,0],ymargin=[0,0], $
      xr=[0,float((*state).nx*(*state).binfac)], $
      yr=[0,float((*state).ny*(*state).binfac)],xstyle=5,ystyle=5,/noerase

   error=0

end

pro occproc_makefinal,state,lc,zg,countg,zb,countb

   if (*state).nimages eq 0 then return

   zg=where((*(*state).valid)[*,(*state).idxsrc] and $
            (*(*state).bad) eq 0,countg)
   if countg eq 0 then return

   if (*state).doratio and (*state).idxsrc ne 1 then begin
      rawlc = trimrank((*(*state).flux)[*,(*state).idxsrc])
      reflc = trimrank((*(*state).flux)[*,1])
      lc = rawlc/reflc
      zb=where(lc eq 0 or (*(*state).bad) eq 1 or $
              (*(*state).valid)[*,(*state).idxsrc] eq 0,countb)
      bad=bytarr((*state).nimages)
      if countb ne 0 then bad[zb]=1B
      robomean,lc,3.0,0.5,avgflux,dummy,stdev,bad=bad
      robomean,(*(*state).fwhm)[*,(*state).idxsrc],3.0,0.5,avgfwhm,bad=bad
      lc = lc/avgflux
      countb = 0
   endif else begin
      lc = trimrank((*(*state).flux)[*,(*state).idxsrc])
      ; flag low flux points
      if (*state).idxsrc eq 0 then begin
         zb=where((*(*state).flux)[*,(*state).idxsrc] lt (*state).thresh and $
                 (*(*state).valid)[*,(*state).idxsrc] eq 1,countb)
      endif else begin
         countb=0
      endelse
   endelse

end

pro occproc_savefinal,state

   if (*state).nimages eq 0 then return

   occproc_makefinal,state,lc,zg,countg,zb,countb
   if countg eq 0 then return

   if (*state).binfac eq 1 then begin
      fnlc=(*state).project+'/'+(*state).site+'.'+ $
            (*state).time+'.lc'
   endif else begin
      fnlc=(*state).project+'/'+(*state).site+'.'+ $
           (*state).time+'.'+strn((*state).binfac)+'.lc'
   endelse

   bad=(*(*state).bad)
   robomean,(*(*state).flux)[*,0],3.0,0.5,avgflux,bad=bad

   if (*state).doratio eq 0 then lc=lc/avgflux

   text='Save final lightcurve to '+fnlc
   occproc_postinfo,state,text

   openw,lun,fnlc,/get_lun
   for i=0,countg-1 do begin
      printf,lun,i,(*(*state).jd)[zg[i]],lc[zg[i]], $
            (*(*state).flux)[zg[i],0],(*(*state).xpos)[zg[i],0], $
            (*(*state).ypos)[zg[i],0],(*(*state).fwhm)[zg[i],0], $
         format='(i5,1x,f17.9,1x,f8.5,1x,f10.1,2(1x,f7.2),1x,f5.2)'
   endfor
   free_lun,lun

end

pro occproc_savedataset,state
   fndataset=(*state).project+'/'+(*state).site+'.'+(*state).time+'.dat'
   text='Save '+fndataset
   occproc_postinfo,state,text
   openw,lun,fndataset,/get_lun
   printf,lun,(*state).seeing
   printf,lun,(*state).objrad
   printf,lun,(*state).binfac
   printf,lun,(*state).maxfac
   printf,lun,(*state).nsrc
   printf,lun,(*state).trackref
   free_lun,lun
end

pro occproc_savesource,state
   if (*state).nimages eq 0 then return

   if (*state).binfac eq 1 then begin
      fnbad=(*state).project+'/Raw/'+(*state).site+'.'+ $
            (*state).time+'.bad'
      fnraw=(*state).project+'/Raw/'+(*state).site+'.'+ $
            (*state).time+'.'+strn((*state).idxsrc)+'.raw'
   endif else begin
      fnbad=(*state).project+'/Raw/'+(*state).site+'.'+ $
           (*state).time+'.'+strn((*state).binfac)+'.bad'
      fnraw=(*state).project+'/Raw/'+(*state).site+'.'+ $
           (*state).time+'.'+strn((*state).binfac)+'.'+ $
           strn((*state).idxsrc)+'.raw'
   endelse

   if not exists((*state).project+'/Raw') then $
      file_mkdir,(*state).project+'/Raw'

   occproc_postinfo,state,'Save to '+fnbad
   openw,lun,fnbad,/get_lun
   for i=0,(*state).nimages-1 do $
      printf,lun,(*(*state).index)[i],(*(*state).bad)[i], $
         format='(i5,1x,i1)'
   free_lun,lun

   j=(*state).idxsrc
   occproc_postinfo,state,'Save to '+fnraw
   openw,lun,fnraw,/get_lun
   for i=0,(*state).nimages-1 do begin
      printf,lun,(*(*state).index)[i],(*(*state).x0)[i,j], $
         (*(*state).y0)[i,j],(*(*state).type0)[i,j], $
         (*(*state).jd)[i],(*(*state).flux)[i,j], $
         (*(*state).xpos)[i,j],(*(*state).ypos)[i,j], $
         (*(*state).sky)[i,j], $
         (*(*state).fwhm)[i,j], $
         format='(i5,2(1x,i4),1x,a1,1x,f16.8,1x,f10.2,1x,2(1x,f7.2),1x,f8.2,1x,f5.2)'
   endfor
   free_lun,lun

end

pro occproc_saveproject,state
   fnproject=(*state).project+'/config.dat'
   text='Save '+fnproject
   occproc_postinfo,state,text
   openw,lun,fnproject,/get_lun
   if (*state).rdir eq '' then begin
      free_lun,lun
      return
   endif
   printf,lun,(*state).rdir
   if (*state).date eq '' then begin
      free_lun,lun
      return
   endif
   printf,lun,(*state).date
   free_lun,lun
end

pro occproc_showproject,state

;print,'showproject, nsrc=',(*state).nsrc

   widget_control,(*state).setprojid,set_value=(*state).project
   widget_control,(*state).setrdirid,set_value=(*state).rdir
   widget_control,(*state).setdateid,set_value=(*state).date
   widget_control,(*state).setsiteid,set_value=(*state).site
   widget_control,(*state).settimeid,set_value=(*state).time

   if (*state).date eq '' then begin
      str=''
   endif else begin
      str=strn((*state).nsites)+' site'
      if (*state).nsites ne 1 then str=str+'s'
   endelse
   widget_control,(*state).shownsitesid,set_value=str
   if (*state).site eq '' then begin
      str=''
   endif else begin
      str=strn((*state).ntimes)+' times'
      if (*state).ntimes ne 1 then str=str+'s'
   endelse
   widget_control,(*state).showntimesid,set_value=str
   if (*state).time eq '' then begin
      str=''
   endif else begin
      str=strn((*state).nimages)+' images'
      if (*state).nimages ne 1 then str=str+'s'
   endelse
   widget_control,(*state).shownimagesid,set_value=str

   if (*state).nimages eq 0 then return

   occproc_managetarg,state

;print,'showproject, source index',(*state).idxsrc
   zg=where((*(*state).valid)[*,(*state).idxsrc] eq 1,countg)
   str=strn(countg)+' valid meas'
   widget_control,(*state).targvalidid,set_value=str
   mkcircle,0,0,1.0,xc,yc

   for i=0,(*state).nsrc-1 do begin

      x0 = (*(*state).x0)[(*state).imidx,i]
      y0 = (*(*state).y0)[(*state).imidx,i]
      x  = (*(*state).xpos)[(*state).imidx,i]
      y  = (*(*state).ypos)[(*state).imidx,i]

      if i eq (*state).idxsrc then begin
         if (*(*state).bad)[(*state).imidx] eq 1 then btag=' bad' else btag=''
         str='Initial '+strn(x0)+','+strn(y0)+' '+ $
             (*(*state).type0)[(*state).imidx,(*state).idxsrc]+btag
         widget_control,(*state).srcposid,set_value=str
         if (*state).trackref le 0 then begin
            str='no tracking'
         endif else begin
            if (*state).idxsrc eq (*state).trackref then begin
               str='Tracking reference'
            endif else begin
               str='Tracking on Comp'+strn((*state).trackref-1)
            endelse
         endelse
         widget_control,(*state).refinfoid,set_value=str
      endif

      if i eq 0 then begin
         icolor=60
      endif else begin
         icolor=54
      endelse

      if x0 gt 0 and y0 gt 0 then begin
;print,'showproject, A) source, i,x0,y0',i,x0,y0,icolor
         oplot,(*state).objrad*xc*3+x0, $
               (*state).objrad*yc*3+y0, $
               color=cpalette(icolor)
      endif

      if (*(*state).valid)[(*state).imidx,i] then begin
;print,'showproject, B) source, i,x,y',i,x,y
         oplot,(*state).objrad*xc+x, $
               (*state).objrad*yc+y, $
               color=cpalette(1)
         if i eq (*state).idxsrc then begin
            str='Measured '+strn(x,format='(f10.1)')+','+ $
                            strn(y,format='(F10.1)')
            widget_control,(*state).measposid,set_value=str
         str='Flux '+strn((*(*state).flux)[(*state).imidx,(*state).idxsrc], $
                          format='(f10.1)')+','+ $
             ' FWHM '+strn((*(*state).fwhm)[(*state).imidx,(*state).idxsrc], $
                          format='(F10.1)')+ $
             ' sky '+strn((*(*state).sky)[(*state).imidx,(*state).idxsrc], $
                          format='(F10.1)')
         widget_control,(*state).srcinfoid,set_value=str
         endif
      endif else begin
         widget_control,(*state).measposid,set_value=''
      endelse
   endfor

;print,'showproject end, nsrc=',(*state).nsrc

end

pro occproc_eve, event

   widget_control, event.top, GET_UVALUE=state

   if event.id eq (*state).mainbase then $
      event_name = 'Mainbase' $
   else $
      widget_control, event.id,  GET_UVALUE=event_name, /HOURGLASS

   exit = event_name eq 'THE_MENU'
   if exit then exit = event.value eq 'Exit'

   case event_name of

      'THE_MENU': begin
         case event.value of

            'Full Image': begin
               if (*state).project eq '' then return
               if (*state).nimages le 0 then return
               occproc_loadimage,state,(*state).imidx,image,hdr,error
               if error then return
               showsrc,image,window=0,hisig=8
            end

            'Add star': begin
               if (*state).project eq '' then return
               if (*state).nimages le 0 then return
;               occproc_loadimage,state,0,image,hdr,error
               occproc_addsource,state
            end

            'Aperture LC': begin
               if (*state).project eq '' then return
               if (*state).nimages le 0 then return
               occproc_apextract,state
            end

            'Aperture LC (from current)': begin
               if (*state).project eq '' then return
               if (*state).nimages le 0 then return
               occproc_apextract,state,start=(*state).imidx
            end

            'Build PSF': begin
               if (*state).project eq '' then return
               if (*state).nimages le 0 then return
               occproc_buildpsf,state
            end

            'Flag Bad Images': begin
               if (*state).project eq '' then return
               if (*state).nimages le 0 then return
               bad = (*(*state).bad)
               x = (*(*state).index)
               y = (*(*state).flux)[*,(*state).idxsrc]
               markdata,x,y,bad,group=(*state).mainbase, $
                  title='Mark frames as unusable'
               (*(*state).bad) = bad
               ; need to save all sources, reset to current
               idxsrc = (*state).idxsrc
               for i=0,(*state).nsrc-1 do begin
                  (*state).idxsrc = i
                  occproc_savesource,state
               endfor
               (*state).idxsrc = idxsrc
               occproc_showlc,state
            end

            'Manual Aperture': begin
               objrad=qinput(/floating,title='Manual set of FWHM/Objrad', $
                             prompt='Enter FWHM (used for Objrad too)', $
                             default=(*state).seeing,cancelled=cancelled)
               if cancelled then return
               (*state).seeing=objrad
               (*state).objrad=objrad
               occproc_savedataset,state
               str='FWHM   = '+string((*state).seeing,format='(f4.1)')
               widget_control,(*state).seeingid,set_value=str
               str='ObjRad = '+string((*state).objrad,format='(f4.1)')
               widget_control,(*state).objradid,set_value=str
            end

            'Mark Invalid': begin
               info=['This operation will mark all measurements'+ $
                     ' of the currently selected target', $
                     'as invalid as if they were never measured.'+ $
                     '  This is not a normal operation', $
                     'but happens occasionally with very marginal data.'+ $
                     '  This operation cannot', $
                     'be undone.  You will need to regenerate the lightcurve.']
               if qannounc(info,false='Continue, mark invalid', $
                            title='Confirmation', $
                            true='Abort') then begin
                  return
               endif
               (*(*state).valid)[*,(*state).idxsrc]=0
               occproc_showproject,state
            end

            'Probe': begin
               (*state).probe=1B
            end

            'Save Final LC': begin
               occproc_savefinal,state
            end

            'Select Existing Project': begin
               plist=file_search('*',/test_directory,count=count)
               if count eq 0 then begin
                  text=['There are no projects in the current directory.', $
                        'You have to start a project before this selector', $
                        'will work.  Use the editable area to the right of', $
                        '"Project" to create a new project.']
                  result=dialog_message(text,/error)
                  return
               endif
               newproject=picker(plist,title='Project list')
               if newproject eq '[[[CANCEL]]]' then return
               occproc_loadproject,state,newproject,/reload
            end

            'Select Site': begin
               if (*state).project eq '' or (*state).rdir eq '' then begin
                  text=['You must select a project and a relative directory', $
                        'before you can chose a site']
                  result=dialog_message(text,/error)
               endif else if (*state).nsites eq 0 then begin
                  text=['There do not appear to be any sites available for', $
                        'your chosen project.  This usually happens if there', $
                        'is no data available in your defined location.', $
                        'Unable to proceed.','',$
                        'Looking in '+(*state).ddir+(*state).rdir+' for sites']
                  result=dialog_message(text,/error)
               endif else begin
                  newsite=picker((*(*state).sitelist),title='Site list')
                  if newsite eq '[[[CANCEL]]]' then return
                  (*state).site=newsite
                  (*state).ntimes=0
                  (*state).nimages=0
                  (*state).time=''
                  (*state).seeing=-1.0
                  (*state).objrad=-1.0
                  occproc_loadproject,state,(*state).project
               endelse
            end

            'Select Time': begin
               if (*state).project eq '' or (*state).rdir eq '' or $
                                            (*state).site eq '' then begin
                  text=['You must select a project, relative directory, andsite', $
                        'before you can chose a time']
                  result=dialog_message(text,/error)
               endif else if (*state).ntimes eq 0 then begin
                  text=['There do not appear to be any times available for', $
                        'your chosen site.  This usually happens if there', $
                        'is no data available in your defined location.', $
                        'Unable to proceed.']
                  result=dialog_message(text,/error)
               endif else begin
                  newtime=picker((*(*state).timelist),title='Time list')
                  if newtime eq '[[[CANCEL]]]' then return
                  (*state).time=newtime
                  (*state).nimages=0
                  (*state).seeing=-1.0
                  (*state).objrad=-1.0
                  occproc_loadproject,state,(*state).project
               endelse
            end

            'Set Binfac': begin
               binfac=qinput(/integer,title='Set Binning factor', $
                             prompt='Enter new binning factor', $
                             default=(*state).binfac,cancelled=cancelled)
               if cancelled then return
               if binfac le 0 or binfac gt 6 then return
               (*state).binfac=binfac
               str='Binfac = '+strn((*state).binfac)+ $
                    ', maxfac = '+ strn((*state).maxfac,format='(f10.1)')
               widget_control,(*state).binfacid,set_value=str
               occproc_savedataset,state
               occproc_loadproject,state,(*state).project
            end

            'Set Maxfac': begin
               maxfac=qinput(/floating,title='Set Boxmrad factor', $
                             prompt='Enter new maxfac factor', $
                             default=(*state).maxfac,cancelled=cancelled)
               if cancelled then return
               if maxfac lt 0.4 or maxfac gt 20 then return
               (*state).maxfac=maxfac
               str='Binfac = '+strn((*state).binfac)+ $
                    ', maxfac = '+ strn((*state).maxfac,format='(f10.1)')
               widget_control,(*state).binfacid,set_value=str
               occproc_savedataset,state
            end

            'Set Tracking Reference': begin
               if (*state).nsrc le 0 then begin
                  text=['This operation is valid only when there are comparison', $
                        'stars defined.  You must define a comp star first.  This', $
                        'process works much better if the comp star to be selected', $
                        'has a clean extraction before enabling this feature.']
                  res=qannounc(text,falselabel='',truelabel='Continue', $
                               title='Warning: no comp stars defined yet')
                  return
               endif
               list = (*(*state).srclist)
               list[0]='None'
               res=picker(list,group=(*state).mainbase,index=index)
               if res eq '[[[CANCEL]]]' then return
               (*state).trackref=index
               occproc_showproject,state
               occproc_savedataset,state
            end

            'Exit' : begin
               widget_control, event.top, /DESTROY
               return
            end

            else: begin
               message, 'Unknown menu event:', /INFO
               help, event, /STRUCTURE
            end

         endcase

      end ; THE_MENU

;      'Mainbase': begin

         ; Use if you have other widgets on screen, need to take off their
         ;   size from the event x,y size.
;         info=widget_info((*state).colbaseid,/geometry)
;         widget_control,(*state).drawwin,xsize=event.x,ysize=event.y-info.ysize

         ; Use if draw window is only thing in the tool.
;         widget_control,(*state).drawwin,xsize=event.x,ysize=event.y
;         widget_control,(*state).drawwin,xsize=event.x,ysize=event.y-info.ysize

;        refresh window here
;      end

      'Frame+m': begin
         if (*state).nimages le 0 then return
         if (*state).imidx eq (*state).nimages-1 then return
         z=where( (*(*state).type0)[*,(*state).idxsrc] eq 'm' and $
                  (*(*state).index) gt (*state).imidx, count )
         if count eq 0 then return
         occproc_loadimage,state,z[0],image,hdr,error
         occproc_showproject,state
      end

      'Frame-m': begin
         if (*state).nimages le 0 then return
         if (*state).imidx eq 0 then return
      end

      'Frame+1': begin
         if (*state).nimages le 0 then return

         newidx=(*state).imidx+1

         if newidx ge (*state).nimages then return

         occproc_loadimage,state,newidx,image,hdr,error
         occproc_showproject,state
      end

      'Frame-1': begin
         if (*state).nimages le 0 then return

         newidx=(*state).imidx-1

         if newidx lt 0 then return

         occproc_loadimage,state,newidx,image,hdr,error
         occproc_showproject,state
      end

      'idx slider': begin
         if (*state).nimages le 0 then return

         newidx=event.value

         occproc_loadimage,state,newidx,image,hdr,error
         occproc_showproject,state
      end

      'LC all': begin
         if (*state).project eq '' then return
         if (*state).nimages le 0 then return
         occproc_apextract,state
      end

      'LC cur': begin
         if (*state).project eq '' then return
         if (*state).nimages le 0 then return
         occproc_apextract,state,start=(*state).imidx
      end

      'Ratio Control': begin
         (*state).doratio = event.value
         occproc_showlc,state
      end

      'Set Date': begin
         widget_control,(*state).setdateid,get_value=date
         date=date[0]
         if date eq '' then return

         (*state).date = date

         occproc_saveproject,state
         occproc_loadproject,state,(*state).project,/reload
      end

      'Set Frame Index': begin
         widget_control,(*state).frameidxid,get_value=newidx
         if newidx eq '' then return
         newidx_val=long(newidx)
         if strn(newidx_val) ne newidx then begin
            if (*state).imidx lt 0 then begin
               widget_control,(*state).frameidxid,set_value=''
            endif else begin
               widget_control,(*state).frameidxid,set_value=(*state).imidx
            endelse
         endif
         occproc_loadimage,state,newidx_val,image,hdr,error
         occproc_showproject,state
      end

      'Set Limit': begin
         widget_control,(*state).limitid,get_value=newlimit
         if newlimit eq '' then newlimit=0 else newlimit=long(newlimit)
         if newlimit lt 0 or newlimit ge (*state).nimages then begin
            newlimit=0
         endif
         (*state).xlimit = newlimit
         widget_control,(*state).limitid,set_value=strn(newlimit)
      end

      'Set Project': begin
         widget_control,(*state).setprojid,get_value=newproject
         newproject=newproject[0]
         if not exists(newproject) then begin
            text=['Project "'+newproject+'" does not exist.'+ $
                  '  Please confirm that you want to create a new', $
                  'project directory.  A new directory will be created,'+ $
                  ' the current project will be', $
                  'closed and the new one will be opened.']
            if qannounc(text,false='Create directory', $
                         title='Confirmation', $
                         true='Abort') then begin
               return
            endif
            file_mkdir,newproject
         endif
         (*state).site=''
         (*state).time=''
         (*state).seeing=-1.0
         occproc_loadproject,state,newproject,/reload

      end

      'Set Rdir': begin
         widget_control,(*state).setrdirid,get_value=rdir
         rdir=rdir[0]
         if (*state).rdir ne rdir then begin
            text=['Project "'+(*state).project+ $
                  '" already has a defined relative directory.'+ $
                  '  It is currently set', $
                  'to '+(*state).rdir+'.', $
                  'Please confirm that you want to change this.'+ $
                  '  This is not a typical thing to do.', $
                  'It is far more likely that you edited the wrong field.']
            if qannounc(text,false='Change directory', $
                         title='Change Confirmation', $
                         true='Abort') then begin
               return
            endif

         endif

         ; validate the proposed directory name
         getddir,rdir,ddir
         if ddir eq 'not found' then begin
            text=['The requested relative directory cannot be'+ $
                  ' found in the configured data areas..', $
                        'Check the relative directory name or getddir.pro' ]
            result=dialog_message(text,/error)
            return
         endif

         (*state).ddir=addslash(ddir)
         (*state).rdir=addslash(rdir)

         occproc_saveproject,state
         occproc_loadproject,state,(*state).project,/reload

      end

      'Set Site': begin
         widget_control,(*state).setsiteid,get_value=newsite
         newsite=newsite[0]
         z=where(newsite eq (*(*state).sitelist),count)
         if count eq 0 then begin
            text=['Site '+newsite+' is not valid for this project.', $
                  'Here are the sites that are currently known:', $
                  strjoin((*(*state).sitelist),' '), $
                  'Reverting back to the prior setting for the site.']
            result=dialog_message(text,/error)
            widget_control,(*state).setsiteid,set_value=(*state).site
         endif else begin
            (*state).site=newsite
            (*state).ntimes=0
            (*state).nimages=0
            (*state).time=''
            (*state).seeing=-1.0
            (*state).objrad=-1.0
            occproc_loadproject,state,(*state).project
         endelse
      end

      'Set Time': begin
         widget_control,(*state).settimeid,get_value=newtime
         newtime=newtime[0]
         z=where(newtime eq (*(*state).timelist),count)
         if count eq 0 then begin
            text=['Time '+newtime+' is not valid for this site.', $
                  'Here are the times that are currently known:', $
                  strjoin((*(*state).timelist),' '), $
                  'Reverting back to the prior setting for the time.']
            result=dialog_message(text,/error)
            widget_control,(*state).settimeid,set_value=(*state).time
         endif else begin
            (*state).time=newtime
            occproc_loadproject,state,(*state).project
         endelse
      end

      'Source Select': begin
         (*state).idxsrc = event.index
;print,'Source Select: calling showproject',event.index
         occproc_showproject,state
;print,'Source Select: calling showlc'
         occproc_showlc,state
      end

      'Window': begin
         if (*state).nimages eq 0 then return
         if event.press ne 1 then return

;         if (*state).probe then begin
;            if (*state).objrad lt 0 then return
;            occproc_loadimage,state,(*state).imidx,image,hdr,error
;            if error then return
;            x0=event.x
;            y0=event.y
;            boxm,image,x0,y0,3,3,xmax,ymax
;            sky1=(*state).objrad+10
;            sky2=230
;            basphote,1.0,image,1.0,x0,y0,(*state).objrad,sky1,sky2, $
;               /nolog,/silent,flux=flux0,xcen=xpos0,ycen=ypos0,fwhm=fwhm0, $
;               max=max,boxmrad=(*state).objrad*(*state).maxfac
;            text=['FWHM = '+string(fwhm0), $
;                  'x,y  = '+string(xpos0*(*state).binfac,ypos0*(*state).binfac), $
;                  'peak above sky = '+string(max), $
;                  'sky = '+string((*state).skymean)+' counts/pixel', $
;                  'total counts='+string(flux0)]
;            occproc_postinfo,state,text,/clear
;            (*state).probe=0B
;         endif else begin
            occproc_loadimage,state,(*state).imidx,image,hdr,error
            if error then return
            x0=event.x
            y0=event.y
;print,'Window:',x0,y0
;print,'Window: source',(*state).idxsrc
;print,'Window: current image',(*state).imidx
;print,'Window: current x0,y0,type0', $
;                            (*(*state).x0)[(*state).imidx,(*state).idxsrc], $
;                            (*(*state).y0)[(*state).imidx,(*state).idxsrc], $
;                        ' ',(*(*state).type0)[(*state).imidx,(*state).idxsrc]

;            boxm,image,x0,y0,3,3,xmax,ymax
            md=(*state).objrad * (*state).maxfac
            boxm,image,x0,y0,md,md,xmax,ymax
;print,'Window: max x,y',xmax,ymax
            (*(*state).x0)[(*state).imidx,(*state).idxsrc]=xmax*(*state).binfac
            (*(*state).y0)[(*state).imidx,(*state).idxsrc]=ymax*(*state).binfac
            (*(*state).type0)[(*state).imidx,(*state).idxsrc]='m'
            ; set all following measures to be 
            z=where((*(*state).type0)[*,(*state).idxsrc] ne 'm' and $
                   (*(*state).index) gt (*state).imidx,count)
            if count ne 0 then (*(*state).type0)[z,(*state).idxsrc]='x'
;print,'Window: ',(*(*state).x0)[(*state).imidx,(*state).idxsrc], $
;                 (*(*state).y0)[(*state).imidx,(*state).idxsrc],' ', $
;                 (*(*state).type0)[(*state).imidx,(*state).idxsrc]
            occproc_showproject,state

;         endelse
      end

      else: begin
         print,'EVENT NAME: ',event_name
         message, 'Unknown event:', /INFO
         help, event, /STRUCTURE
      end

   endcase

end ; end of event handler

pro occproc

   ; optional
   if xregistered('occproc') then return

   if (!d.flags and 256) eq 0 then begin
      print, 'Error. No windowing device. OCCPROC cannot be started.'
      return
   endif

   ;Define the main base.
   mainbase = widget_base( TITLE='OCCPROC: Occultation Data Processing Tool', $
                           /COLUMN, UVALUE=0, MBAR=bar, /TLB_SIZE_EVENTS )

   menu = CW_PdMenu(bar, /RETURN_NAME, $
                    ['1\File',$
                     '0\Select Existing Project',$
                     '0\Select Site',$
                     '0\Select Time',$
                     '0\Save Final LC',$
                     '2\Exit',$
                     '1\Stars', $
                     '0\Add star', $
                     '0\Set Tracking Reference', $
                     '2\tool 1',$
                     '1\Tools',$
                     '0\Aperture LC', $
                     '0\Aperture LC (from current)', $
                     '0\Manual Aperture', $
                     '0\Set Binfac', $
                     '0\Set Maxfac', $
                     '0\Probe', $
                     '0\Build PSF', $
                     '0\Full Image', $
                     '0\Mark Invalid', $
                     '0\Flag Bad Images', $
                     '2\tool 3'], UVALUE='THE_MENU', /MBAR)

   base = widget_base(mainbase,/row)

   xviewsize=600
   yviewsize=600
   cbase=widget_base(base,/column)

   win1 = widget_draw( cbase, XSIZE=1000, YSIZE=1000, RETAIN=2, $
                       X_SCROLL_SIZE=xviewsize, Y_SCROLL_SIZE=yviewsize, $
                       KEYBOARD_EVENTS=0, $
                       /BUTTON_EVENTS, UVALUE='Window' )
   b1=widget_base(cbase,/row)
   imageid=widget_label(b1,value='',/dynamic_resize,/frame)
   t1=widget_label(b1,value='Frame index')
   frameidxid=widget_text(b1,value='',/editable,xsize=5,uvalue='Set Frame Index')
   button1=widget_button(b1,value='-m', uvalue='Frame-m')
   button1=widget_button(b1,value='-', uvalue='Frame-1')
   button1=widget_button(b1,value='+', uvalue='Frame+1')
   button1=widget_button(b1,value='+m', uvalue='Frame+m')
   idxsliderid=widget_slider(b1,minimum=0,maximum=1,value=0,uvalue='idx slider')
   t1=widget_label(b1,value='X-limit')
   limitid=widget_text(b1,value='',/editable,xsize=5,uvalue='Set Limit')

   b1=widget_base(cbase,/row)
   button1=widget_button(b1,value='LC from 0',uvalue='LC all')
   t1=widget_label(b1,value='LCgen')
   button1=widget_button(b1,value='LC from cur',uvalue='LC cur')

   cbase=widget_base(base,/column)

   b1=widget_base(cbase,/row)
   t1=widget_label(b1,value='Project')
   setprojid=widget_text(b1,value='',/editable,xsize=10,uvalue='Set Project')

   b1=widget_base(cbase,/row)
   t1=widget_label(b1,value='Reldir')
   setrdirid=widget_text(b1,value='',/editable,xsize=30,uvalue='Set Rdir')

   b1=widget_base(cbase,/row)
   t1=widget_label(b1,value='Date')
   setdateid=widget_text(b1,value='',/editable,xsize=10,uvalue='Set Date')
   shownsitesid=widget_label(b1,value='',/dynamic_resize)

   b1=widget_base(cbase,/row)
   t1=widget_label(b1,value='Site')
   setsiteid=widget_text(b1,value='',/editable,xsize=10,uvalue='Set Site')
   showntimesid=widget_label(b1,value='',/dynamic_resize)

   b1=widget_base(cbase,/row)
   t1=widget_label(b1,value='Time')
   settimeid=widget_text(b1,value='',/editable,xsize=10,uvalue='Set Time')
   shownimagesid=widget_label(b1,value='',/dynamic_resize)

   b1=widget_base(cbase,/col,/frame)
   b2=widget_base(b1,/row)
   seeingid=widget_label(b2,value='',/dynamic_resize)
   objradid=widget_label(b2,value='',/dynamic_resize)
   binfacid=widget_label(b1,value='',/dynamic_resize)

   b1=widget_base(cbase,/col)
   plotwin1=widget_draw(b1,XSIZE=350,YSIZE=200,RETAIN=2)

   b1=widget_base(cbase,/col,/frame)
   b2=widget_base(b1,/row,/frame)
   srcid=widget_combobox(b2,value=' ',uvalue='Source Select',sensitive=0, $
                            /dynamic_resize)
   targvalidid=widget_label(b2,value='',/dynamic_resize)
   srcposid=widget_label(b1,value='',/dynamic_resize)
   measposid=widget_label(b1,value='',/dynamic_resize)
   srcinfoid=widget_label(b1,value='',/dynamic_resize)
   refinfoid=widget_label(b1,value='nothing yet',/dynamic_resize)
   values=['Raw','Ratio']
   ratioid=cw_bgroup(b1,values,uvalue='Ratio Control', $
                     /row,/exclusive,set_value=0)

   b1=widget_base(cbase,/col)
   noticeid=widget_text(b1,ysize=6,xsize=50,/scroll)

   state = ptr_new({ $

      ; Data and information in the widget

      ; project information
      project: '', $
      ddir: '', $          ; This is the system-dependent part of the path
      rdir: '', $          ; System-independent part of the path
      date: '', $          ; Date (string for dir name)

      ; dataset information
      site: '', $          ; Name of the site (data sub-dir, actually)
      time: '', $          ; Time (string for dir name)
      nsites: 0, $
      sitelist: ptr_new(), $
      ntimes: 0, $
      timelist: ptr_new(), $
      path: '', $
      imlist: ptr_new(), $
      bad: ptr_new(), $
      seeing: -1.0, $      ; initial seeing, FWHM in pixels
      objrad: -1.0, $      ; initial seeing, FWHM in pixels
      binfac: 1, $
      maxfac: 1.0, $
      nstars: 0, $

; other information
; target information saved for all frames

; change type of comp star

; change type of photometric use

; set manual location for star on current frame

; widget items
;   dropdown selection: Target, Comp_0, Comp_1, ...
;     at the start there is only Target, The others get added as new sources
;     are added
;   dropdown selection: 't','r','o'  (tracking type)  (deselect for target)
;   dropdown selection: 'p','s','o'  (phot type) (deselect for target)
;   text label showing cx0,cy0 and c0type
;   text label showing cfwhm,cflux,cerr,cx,cy

; image information
;  quality: replicate(-1,nimages) ; unknown
;                      0          ; nothing useful on image
;                      1          ; useful field star but no target
;                      2          ; all good

; field star information (structure?)
; ncomp: 0, ; number of comparisons stars defined
; ctype: strarr(ncomp) ;  't' = tracking star
;                         'r' = rotation tracking star
;                         'o' = undefined, no other use
; ptype: strarr(ncomp) ;  'p' = primary photometric reference star (0 or 1)
;                         's' = secondary photometric reference star (0+)
;                         'o' = other photometric reference stars (0+)
; cx0: intarr[nimages,ncomp] ; starting location for source
; cy0: intarr[nimages,ncomp]
; c0type: strarr(nimages,ncomp) ; type of starting positiong
;                                  'p' = previous image
;                                  'm' = manual click
;                                  'x' = value not set
; cfwhm: strarr(nimages, ncomp)
; cflux: fltarr(nimages, ncomp)
; cerr : fltarr(nimages, ncomp)
; cx:  : fltarr(nimages, ncomp)
; cy:  : fltarr(nimages, ncomp)

      ; other stuff
      nx: 0, $
      ny: 0, $
      xviewsize: xviewsize, $
      yviewsize: yviewsize, $
      imidx: 0L, $         ; index number of image being displayed
      probe: 0B, $         ; flag, if set will enable manual measure of a source
      skymean: 0.0, $      ; sky mean of last image loaded
      exptime: 0.0, $      ; exposure time of current loaded image
      sky1: 0.0, $
      sky2: 0.0, $
      doratio: 0, $        ; flag, 0=show raw lc, 1=show ratio
      xlimit: 0, $

      ; current lightcurve data
      nimages: 0, $        ; number of images in sequence to process
      index: ptr_new(), $  ; ordinal frame number
      jd:    ptr_new(), $  ; exposure mid-time (JD)
      nsrc:  0, $          ; number of sources measured
      idxsrc:  0, $        ; index number of current active source
      srclist: ptr_new(), $
      trackref: -1, $
      photref: -1, $
      flux:  ptr_new(), $
      fwhm:  ptr_new(), $
      x0:    ptr_new(), $
      y0:    ptr_new(), $
      type0: ptr_new(), $
      xpos:  ptr_new(), $
      ypos:  ptr_new(), $
      sky:   ptr_new(), $
      valid: ptr_new(), $
      thresh: 0.0, $
;      lcvalid: 0B, $

      ; Widget ids
      binfacid:      binfacid, $
      drawwin:       win1, $           ; ID of main draw window
      frameidxid:    frameidxid, $
      idxsliderid:   idxsliderid, $
      imageid:       imageid, $
      noticeid:      noticeid, $
      objradid:      objradid, $
      plotwin1:      plotwin1, $
      seeingid:      seeingid, $
      setdateid:     setdateid, $
      setprojid:     setprojid, $
      setrdirid:     setrdirid, $
      setsiteid:     setsiteid, $
      settimeid:     settimeid, $
      shownsitesid:  shownsitesid, $
      showntimesid:  showntimesid, $
      shownimagesid: shownimagesid, $
      srcid:         srcid, $
      targvalidid:   targvalidid, $
      srcposid:      srcposid, $
      measposid:     measposid, $
      srcinfoid:     srcinfoid, $
      refinfoid:     refinfoid, $
      ratioid:       ratioid, $
      limitid:       limitid, $

      mainbase: mainbase $       ; ID of top level base.

      })

   ;Stash the state structure pointer.
   widget_control, mainbase, SET_UVALUE=state

   ;Realize the main base.
   widget_control, mainbase, /REALIZE

   ; Give control to the XMANAGER.
   xmanager, 'occproc', mainbase, $
             EVENT_HANDLER='occproc_eve',/NO_BLOCK, $
             GROUP_LEADER=mainbase, CLEANUP='occproc_cleanup'

end
