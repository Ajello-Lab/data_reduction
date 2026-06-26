;+
; NAME:
;  warpstack
; PURPOSE:   (one line only)
;  Stack a set of images on a given ROI warped to a master image
; DESCRIPTION:
;  This tool is designed to build a reference image for a given image of
;    the sky.  The reference image is intended to be subtracted by a later
;    tool to remove the fixed stellar background.  The template is built
;    by resampling the input reference images to precisely match the
;    master image, which includes its scale, orientation and any optical
;    distortions.  The input list of reference images should be a reasonble
;    list of images to be considered but not all of them are required to be
;    useful for this purpose (ie., no overlap).  However, it is up to the
;    calling program to not propose reference images that contain the object
;    of interest on or near the location of that object in the master image.
;
;  The stack is built from those reference images that have overlap with
;    the ROI on the master image.  The software works to use only the best
;    data from the reference image list to make the template.
;
;  When complete, four files are saved.  1) a log file containg information
;    about the stacking process, 2) the stacked image in FITS format, 3) the
;    cube from which the stack was built, also in FITS format, and 4) a screen
;    shot of the template image saved in .png format
;
;  Construction of templates works best where there are lots of images to
;    work with but this tool strives to make do if the number of files is
;    small.  With 5 or more images, it will do a robust average of the cube.
;    With 4 or less a straight median is used.  If there is only one relevant
;    file you get that one file, resampled to match but it will still have
;    all image blemishes, like cosmic ray strikes.
;
; CATEGORY:
;  CCD data processing
; CALLING SEQUENCE:
;  warpstack,fnim,fnref,error
; INPUTS:
;  fnim    - File name of the master image that the stack will be built to match
;  fnref   - List of files that may be useful for building a stack
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;  FWHM    - Vector of fwhm, in pixels for each image in fnref.  If provided,
;               this can speed up the program significantly.  The default
;               is to determine this on the images internally.
;  GAIN    - Gain of detector, default is 1e-/DN
;  KEYFILE - Header keyword list (see loadkeys.pro)
;  OUTPATH - Directory where output files are to be written to.  Default is
;               to write to the current directory.
;  PATH    - Directory where input images (master and reference) can be
;               found.  You can put paths on the inputs in which case use
;               the default of '' for this keyword.
;  ROI     - Region of interest from master image that stack will be built
;               to.  The default is to do it for the entire image.  Provide
;               a 4-element vector [i0,j0,i1,j1] (lower left hand corner and
;               upper right hand corner of region) to use this option.
;  VERBOSE - Flag, if set will turn on a lot of printed output to see what
;              the program is doing.
;  MINAREA - minimum fraction of area overlap allowed to keep a template
;              image.  Default = 0.65
; OUTPUTS:
;  error - Flag, set to 1 in case of error.  The
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
;  This tool depends on the presence of WCS information in the image headers
;    and expects to find my format information in there (see astinfo.pro).
; PROCEDURE:
; MODIFICATION HISTORY:
;  2016/04/12, Written by Marc W. Buie, Southwest Research Institute
;  2016/04/21, MWB, slight fixes to logic for output file names when
;               input files contain their paths.
;  2016/09/29, MWB, added MINAREA keyword
;-
; Notes, might want to allow an input structure that defines what to stack
;   to instead of an input image.
pro warpstack,fnim,fnref,error, $
       PATH=path,ROI=in_roi,VERBOSE=verbose,GAIN=gain,KEYFILE=keyfile, $
       FWHM=in_fwhm,OUTPATH=outpath,MINAREA=minarea,NX=in_nx,NY=in_ny, $
       FNBASE=fnbase

   self='warpstack: '
   error=1
   if badpar(fnim,[7,8],[0,1],caller=self+'(fnim) ',type=fnim_type) then return
   if badpar(fnref,7,[0,1],caller=self+'(fnref) ',npts=nref) then return
   if badpar(in_roi,[0,2,3],1,caller=self+'(ROI) ',default=-1) then return
   if badpar(in_fwhm,[0,2,3,4,5],1,caller=self+'(FWHM) ', $
                                   type=fwhmtype) then return
   if badpar(path,[0,7],0,caller=self+'(PATH) ',default='') then return
   if badpar(outpath,[0,7],0,caller=self+'(OUTPATH) ',default='') then return
   if badpar(verbose,[0,1,2,3],0,caller=self+'(VERBOSE) ',default=1) then return
   if badpar(gain,[0,2,3,4,5],0,caller=self+'(GAIN) ',default=1.0) then return
   if badpar(minarea,[0,4,5],0,caller=self+'(MINAREA) ',default=0.65) then return
   if badpar(keyfile,[0,7],0,CALLER=self+'(KEYFILE) ', $
                              default='[[DEFAULT]]') then return
   if path ne '' then path=addslash(path)
   if outpath ne '' then outpath=addslash(outpath)

   if fnim_type eq 7 then begin
      if verbose then begin
         print,'Master image ',fnim
         print,strn(nref),' candidate images for reference'
      endif

      if not exists(path+fnim) then begin
         print,path+fnim
         print,'Unable to locate master file.  Aborting.'
         return
      endif

      hdr=headfits(path+fnim)
      nx=sxpar(hdr,'NAXIS1')
      ny=sxpar(hdr,'NAXIS2')

      pos=strpos(fnim,'/',/reverse_search)
      if pos gt 0 then fnlog=strmid(fnim,pos)+'.log' else fnlog=fnim+'.log'
   ;   fnlog=fnim+'.log'
      openw,lun,outpath+fnlog,/get_lun,width=132
      printf,lun,'Template image creation for ',fnim

      astinfo,hdr,info,ast_error
      if ast_error then begin
         print,path+fnim
         print,'Astrometric information not found in image.  Aborting.'
         return
      endif

   endif else begin
      if verbose then begin
         print,'Reference supplied externally rather than from an image'
      endif
      if badpar(in_nx,[2,3],0,caller=self+'(NX) ') then return
      if badpar(in_ny,[2,3],0,caller=self+'(NY) ') then return
      if badpar(fnbase,7,0,caller=self+'(FNBASE) ') then return
      nx=in_nx
      ny=in_ny
      info=fnim
      mkhdr,hdr,4,[nx,ny]
      astinfo,hdr,info,/toheader
      fnlog=fnbase+'.log'
      openw,lun,outpath+fnlog,/get_lun,width=132
      printf,lun,'Template image creation for manual reference'
   endelse

   if in_roi[0] lt 0 then roi=[0,0,nx-1,ny-1] else roi=in_roi
   str='Processing region ['+strn(roi[0])+':'+strn(roi[1])+','+ $
                             strn(roi[2])+':'+strn(roi[3])+']'
   if verbose then print,str
   printf,lun,str

   ; Compute the ra/dec of the corners of the ROI
   xc=roi[[0,2,2,0]]
   yc=roi[[1,1,3,3]]
   astcvt,'xy',xc,yc,info,'rd',cra,cdec
   rastr,cra,1,cras
   decstr,cdec,0,cdecs
   if verbose then begin
      print,'Corners of roi are at'
      for i=0,3 do print,cras[i],' ',cdecs[i]
   endif
   printf,lun,'Corners of roi are at'
   for i=0,3 do printf,lun,cras[i],' ',cdecs[i]
   nxr=roi[2]-roi[0]+1
   nyr=roi[3]-roi[1]+1

   ; Loop over the reference images to find all those that are truly
   ;  relevant to building a stacked image.
   goodref=bytarr(nref)
   for i=0,nref-1 do begin
      if not exists(path+fnref[i]) then begin
         print,path+fnref[i]
         print,'Unable to locate candidate reference file.  Aborting.'
         return
      endif
      rhdr=headfits(path+fnref[i])
      astinfo,rhdr,rinfo,ast_error
      if ast_error then begin
         print,path+fnref[i]
         print,'Astrometric information not found in reference image. Aborting.'
         return
      endif
      astcvt,'rd',cra,cdec,rinfo,'xy',xr,yr
      xrclip = (xr>0)<(nx-1)
      yrclip = (yr>0)<(ny-1)
      roiarea=poly_area(xr,yr,/signed)
      laparea=poly_area(xrclip,yrclip,/signed)

      if laparea/roiarea gt minarea then goodref[i]=1B

      if verbose then print,fnref[i],laparea,roiarea,laparea/roiarea
      printf,lun,i,' ',fnref[i],laparea,roiarea,laparea/roiarea
   endfor

   zref=where(goodref eq 1,countref)
   if countref eq 0 then begin
      print,'No useful reference images found.  Aborting.'
      return
   endif
   if verbose then $
      print,strn(countref),' images have useful overlap with the ROI'
   printf,lun,''
   printf,lun,strn(countref),' images have useful overlap with the ROI'

   ; Filter down the lists to concentrate on those that appear to be good.
   fnref_f = fnref[zref]

   loadkeys,keyfile,keylist

   allcube=fltarr(nxr,nyr,countref)
   allmask=bytarr(nxr,nyr,countref)
   meansky=fltarr(countref)
   skynoise=fltarr(countref)
   fwhm=fltarr(countref)
   maglim=fltarr(countref)
   fluxref=fltarr(countref)
   if fwhmtype ne 0 then fwhm=in_fwhm[zref]

   for i=0,countref-1 do begin
      refim=readfits(path+fnref_f[i],rhdr)
      parsekey,rhdr,keylist,rhinfo
      astinfo,rhdr,rinfo,ast_error
      photzp=sxpar(rhdr,'ASTPHOZP')
      mag2flx,photzp,0.*photzp,fluxref0
      fluxref[i]=fluxref0

      ; warp the reference image to match the master image
      dewarp,rinfo,refim,info,refatim,nx,ny,count=used,roi=roi
      refatim=refatim[roi[0]:roi[2],roi[1]:roi[3]]
      used=used[roi[0]:roi[2],roi[1]:roi[3]] < 1

      ; fit sky and subtract, save sky value
      skyfit,refatim,skyim,hiclip=0.9,xorder=1,yorder=1, $
         npts=4000,/silent,coeff=skyc,skysig=skysig,mask=~used
      meansky[i]=skyim[nxr/2,nyr/2]
      refatim=refatim-skyim
      skynoise[i]=skysig

      ; get the seeing
      if fwhmtype eq 0 then begin
         seeing,refatim,fwhm0,fwhmguess=6,/nodisplay
         if fwhm0 gt 9 then $
            seeing,refatim,fwhm0,fwhmguess=10,/nodisplay
         fwhm[i]=fwhm0
      endif

      gauss2d,2*fwhm[i]+5,2*fwhm[i]+5,fwhm[i]+2,fwhm[i]+2,fwhm[i],psf
      basphote,gain,psf*skysig*3.0,rhinfo.exptime,fwhm[i]+2.5,fwhm[i]+2.5, $
                  fwhm[i],0.0,0.0,/exact,/nolog,/silent,mag=maglim0
      maglim[i]=maglim0

      if verbose then $
         print,fnref_f[i],photzp,fluxref[i], $
            meansky[i],skysig,fwhm[i],maglim0, $
            format='(a,1x,f6.2,1x,f7.3,1x,f6.1,2(1x,f5.1),1x,f6.2)'
      printf,lun,zref[i],fnref[i],photzp,fluxref[i], $
         meansky[i],skysig,fwhm[i],maglim0, $
         format='(i4,1x,a,1x,f6.2,1x,f7.3,1x,f6.1,2(1x,f5.1),1x,f6.2)'

      allcube[*,*,i]=refatim
      allmask[*,*,i]=used < 1

   endfor

   bad=bytarr(countref)

   ; Filter the list of images to keep only those worthy of inclusion in
   ;  the template.

   ; Filter by FWHM.  If the seeing varies by more than 25%, trim off the
   ;   worst quartile of the frames.
   fwhmthresh = 0.25
   fwhmfom=(fwhm-mean(fwhm))/mean(fwhm)
   fwhmvar = max(fwhmfom)
   if verbose then print,'FWHM fluctuations are ',fwhmvar, $
                         ', threshold is ',fwhmthresh
   printf,lun,''
   printf,lun,'FWHM fluctuations are ',fwhmvar,', threshold is ',fwhmthresh
   if fwhmvar gt fwhmthresh then begin
      idx=reverse(sort(fwhm))
      idx=idx[0:countref/(1/fwhmthresh)]
      z=where(fwhmfom gt fwhmthresh,count2)
      if count2 gt 0 then begin
         bad[idx[z]]=1B
         if verbose then print,'Dropped ',strn(count2),' frames with bad FWHM'
         printf,lun,'Dropped ',strn(count2),' frames with bad FWHM'
      endif
   endif

   zg=where(bad eq 0,countg)

   ; Find the index of the image that will be the flux reference (first pass)
   z=where(maglim[zg] eq max(maglim[zg]))
   idxref = zg[z[0]]
   scalefac = fluxref/fluxref[idxref]

   ; predict the noise from stacking these images and cut off the list when
   ;   there are images that degrade the stack
   idx=reverse(sort(maglim))
   bestnoise=9999.0
   printf,lun,''
   printf,lun,'Stacking details'
   printf,lun,'       i fnref index     scalefac       noise   total noise'
   for i=0,countref-1 do begin
      if bad[idx[i]] then continue
      zg=where(bad[idx[0:i]] eq 0,countg)
      useidx=idx[zg]
      newnoise=sqrt(total((scalefac[useidx]*skynoise[useidx])^2))/countg
      if newnoise le bestnoise then begin
         bestnoise=newnoise
      endif else begin         
         bad[idx[i]] = 1B
      endelse
      if verbose then $
         print,i,idx[i],scalefac[idx[i]], $
            scalefac[idx[i]]*skynoise[idx[i]],newnoise
      printf,lun,i,idx[i],scalefac[idx[i]], $
                 scalefac[idx[i]]*skynoise[idx[i]],newnoise
   endfor

   ; Second pass doesn't filter, just generate numbers and go
   zg=where(bad eq 0,countg)
   z=where(maglim[zg] eq max(maglim[zg]))
   idxref = zg[z[0]]
   scalefac = fluxref/fluxref[idxref]

   cube=fltarr(nxr,nyr,countg)
   mask=bytarr(nxr,nyr,countg)
   for i=0,countg-1 do begin
      cube[*,*,i] = allcube[*,*,zg[i]]*scalefac[i]
      mask[*,*,i] = allmask[*,*,zg[i]]
   endfor

   refsky=median(meansky[zg])
   cube=cube+refsky

   ntopclip=round(0.25*countg)
;   if countg lt 5 then justmedian=1 else justmedia=0
   justmedian=1
   avgclip,cube,stack,bad=~mask,ntopclip=ntopclip, $
      silent=(verbose eq 0),justmedian=justmedian
   if justmedian then begin
      if verbose then print,'Output stack is median combined only.'
      printf,lun,'Output stack is median combined only.'
   endif

   showsrc,stack,window=0

   sxaddpar,hdr,'NAXIS1',nxr
   sxaddpar,hdr,'NAXIS2',nyr
   sxaddpar,hdr,'I0',roi[0]
   sxaddpar,hdr,'J0',roi[1]
   sxaddpar,hdr,'I1',roi[2]
   sxaddpar,hdr,'J1',roi[3]
   sxaddpar,hdr,'NSTACK',countg

   if fnim_type eq 7 then begin
      if pos gt 0 then fnstack=strmid(fnim,pos)+'.ref' else fnstack=fnim+'.ref'
      if pos gt 0 then fncube=strmid(fnim,pos)+'.cube' else fncube=fnim+'.cube'
      if pos gt 0 then fnpng=strmid(fnim,pos)+'.ref.png' else fnpng=fnim+'.ref.png'
   endif else begin
      fnstack=fnbase+'.ref'
      fncube=fnbase+'.cube'
      fnpng=fnbase+'.ref.png'
   endelse

   writefits,outpath+fnstack,stack,hdr
   printf,lun,'Save ',outpath+fnstack

   sxaddpar,hdr,3,'NAXIS'
   sxaddpar,hdr,countg,'NAXIS3'
   writefits,outpath+fncube,cube,hdr
   printf,lun,'Save ',outpath+fncube

   tvgrab,outpath+fnpng,0,/png
   printf,lun,'Save ',outpath+fnpng

   printf,lun,'Processing completed successfully'
   free_lun,lun

   error=0

end
