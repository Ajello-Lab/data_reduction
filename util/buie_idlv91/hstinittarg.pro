;+
; NAME:
;  hstinittarg
; PURPOSE:   (one line only)
;  Inititalize data and fitting parameters for a target fit to HST WFC3 data
; DESCRIPTION:
; CATEGORY:
;  Astrometry
; CALLING SEQUENCE:
;  hstinittarg,dblun,idx,hidx,error
; INPUTS:
;  dblun - pre-opened LUN for a database connection, if provided, the LUN
;            is left open..  If connection is not open, then it is opened
;            at the start and closed at the end.
;  idx   - index number of star to be fitted (stars table)
;  hidx  - index number of image to be fitted (header table)
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
; OUTPUTS:
;  error - Flag, set if an error occurred in setting things up
; KEYWORD OUTPUT PARAMETERS:
; CONFIGURATION: (optional, overrides xtrack.in)
;   File: config.ini
;   [global]
;     subdir   - naem of sub-directory  added to base path to find data
;     hstpsf   - directory for psf cache
;     satur    - Saturation signal level, default=80000L
;     filetype - type of file to read ('flt' or 'flc'), default='flc'
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
;  2020/06/29, Written by Marc W. Buie, Southwest Research Institute, cloned
;                 from init_tno.pro in the 15405 reduction directory.
;  2020/09/25, MWB, added SHOWDW keyword
;-
pro hstinittarg,dblun,idx,hidx,error,SHOWDW=showdw

   common com_hsttargfit,info

   error=1

   self='hstinittarg: '
   if badpar(dblun,[0,1,2,3],0,caller=self+'(dblun) ', $
                               type=dbluntype) then return
   if badpar(idx,[2,3],0,caller=self+'(idx) ') then return
   if badpar(hidx,[2,3],0,caller=self+'(idx) ') then return
   if badpar(showdw,[0,2,3],0,caller=self+'(SHOWDW) ',default=0) then return

   getddir,cinfo,ddir
   getvalue,cinfo,'global','subdir',subdir
   getvalue,cinfo,'global','hstpsf',hstpath
   getvalue,cinfo,'global','satur',satur,type=4
   getvalue,cinfo,'global','filetype',value,default='flc'

   error=0

   dbmanualopen=0
   if dbluntype eq 0 then begin
      openmysql,dblun,'hstast'
      dbmanualopen=1
   endif else begin
      dbstat=fstat(dblun)
      if dbstat.open eq 0 then begin
         openmysql,dblun,'hstast'
         dbmanualopen=1
      endif
   endelse

   ; get the info for the image we've got to work on
   cmd=['select root,det,filter,centera1,centera2,xsize,ysize', $
        'from header where idx='+strn(hidx)+';']
   mysqlquery,dblun,cmd,root,det,filter,centera1,centera2,xsize,ysize, $
      format='a,i,a,i,i,i,i',ngood=nhit
   if nhit eq 0 then begin
      print,cmd
      print,'No image information found. Aborting.'
      error=1
      goto,bailout
   endif

   ; get the info for the image we've got to work on
   cmd=['select xnav,ynav,back,backsig,dw,x,y,flux,chisq', $
        'from pos', $
        'where idx='+strn(idx)+';']
;print,cmd
   mysqlquery,dblun,cmd,xnav,ynav,back,backsig,dw,xpos,ypos,flux,chisq, $
      format='i,i,f,f,i,f,f,f,f',ngood=nhit
   if nhit eq 0 then begin
      print,cmd
      print,'No target navigation values found.  Aborting.'
      error=1
      goto,bailout
   endif
   if showdw gt 0 then dw[*]=showdw

   xnav = round(xnav)
   ynav = round(ynav)

   print,'fitting region center is',xnav,ynav,', sky',back,'+/-',backsig, $
      format='(a,2(1x,i4),a,1x,f7.3,1x,a,1x,f6.3)'

   print,'filter ',filter,', ',centera1,centera2,xsize,ysize

   if det eq 1 then exten=4 else exten=1

   rdwfc3,root,det,data,ddir=ddir,type=type,satur=satur
   if data eq !null then goto,bailout

   ; size of the chisq region
   msz=2*dw+1

   ; set the region for computation
   dwc=128
   csz = 2*dwc+1

   ; set the region of the fit in the computation area
   ci1 = dwc-dw
   ci2 = ci1+msz-1
   cj1 = dwc-dw
   cj2 = cj1+msz-1

   ; set the region for the fit in original pixel coordinates
   msz=2*dw+1
   i1 = xnav-dw
   i2 = xnav+dw
   j1 = ynav-dw
   j2 = ynav+dw

   xoff = xnav - dwc
   yoff = ynav - dwc

   fmt='(a,2(1x,f8.3),1x,a,f11.1,a,1x,i2)'
   print,'Initial values:'
   print,'previous chisq ',chisq
   print,'background  ',back
   print,'fitting ',root,', detector:',strn(det),', (Filter:',filter,')'
   print,'   x,y',xpos,ypos,', flux',flux,' dw',dw,format=fmt
   print,'subframe [',strn(i1),':',strn(i2),',', $
                      strn(j1),':',strn(j2),']'

   subarr,data.image,i1,i2,j1,j2,fitimage,error
   sigimage=sqrt(data.image>0) > 3.0  ; read-noise floor
   subarr,sigimage,i1,i2,j1,j2,simage,error

   subarr,data.mask,i1,i2,j1,j2,mask,error
   zg=where(mask eq 0B,goodcount,complement=zb,ncomplement=badcount)

   if badcount ne 0 then begin
      fitimage[zb] = randomn(seed,badcount)*backsig+back
      z=where(data.mask ne 0B,count)
      data.image[z]=randomn(seed,count)*backsig+back
   endif

   z4 = 0.
   jit = 0.04

   ; fit extramural param block
   info = { $
      fn: data.fnim, $       ; Image file name, with path
      fnmsk: data.fnmsk, $ ; Mask file name, with path
      maskhdr: data.maskhdr, $
      root: root, $
      det: det, $ ; detector
      filter: data.filter, $
      bmvnum: 13, $ ; somewhat red
      chicount: 0, $
      chibest: 1.0e9, $
      nbetter: 0L, $
      hstpath: hstpath, $
      goodcount: goodcount, $
      badcount: badcount, $
      exptime: data.exptime, $
      jdmid: data.jdmid, $
      xnav: xnav, $
      ynav: ynav, $
      xsize: xsize, $
      ysize: ysize, $
      msz:msz, $
      i1:i1, $ ; chisq region in the original image
      i2:i2, $
      j1:j1, $
      j2:j2, $
      csz: csz, $   ; size of the region that is computed
      xoff: xoff, $ ; offsets for wfc3 model image
      yoff: yoff, $
      ci1:ci1, $ ; chisq region in the computed image
      ci2:ci2, $
      cj1:cj1, $
      cj2:cj2, $
      xpos:xpos, $     ; model values from last call
      ypos:ypos, $
      flux:flux, $
      back:back, $ ; sky signal
      backsig:backsig, $ ; sky signal
      weight: 1, $ ; 0 for unweighted chisq, 1 for weighted
      magval: 0, $ ; 1 for magnitude input, 0 for flux
      chired: 1, $ ; 0 for chisq, 1 for reduced chisq
      jit:jit, $
      quiet: 0, $
      z4:z4, $
      dw:dw, $
      dwc:dwc, $
      ; full size
      fullimage: data.image, $  ; complete image
      fullmask: data.mask, $
      sigimage:sigimage, $ ; used only if weight is 1 (full size)
      astinfo: data.astinfo, $
      ; calculation region
      modimage:replicate(0.,csz,csz), $ ; last model image computed
      ; just the fitting region
      fitimage:fitimage, $  ; original data
      mimage:replicate(0.,msz,msz), $ ; model
      mimageps:replicate(0.,msz,msz), $ ; model plus sky
      simage:simage, $ ; sigma image cropped to chisq size
      readnoise: 3.0, $ ; [e-]
      noisemod: 1, $ ; noise model, 0=noise from data, 1=noise from model
      dimage:replicate(0.,msz,msz), $
      mask: mask, $
      zg:zg, $ ; points into the fitting region, controlled by the mask
      zb:zb $
      }

bailout:

   if dbmanualopen then free_lun,dblun

end
