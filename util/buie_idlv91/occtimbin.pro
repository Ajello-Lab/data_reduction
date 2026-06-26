;+
; NAME:
;  occtimbin
; PURPOSE:   (one line only)
;  Bin QHY174 occultation images in time
; DESCRIPTION:
;  This routine will add images together with no shifting to reduce
;   the time resolution.  Integer factors of binning are required and
;   the read-out overhead is ignored so this is relevant for small amounts
;   of binning.  The data will be read from an input directory and written
;   to an output directory with fewer files (binning by 2 will generate
;   half as many files).  Any residual images at the end that do not fully
;   fill a binned image will be ignored.
; CATEGORY:
;  Occultations
; CALLING SEQUENCE:
;  occtimbin,indir,outdir,binfac
; INPUTS:
;  indir  - string with the name of the input directory to read.  This can
;              be a fully qualified or relative path.  If fully qualified,
;              do not use the PATH keyword since the path is pre-pended to
;              this input.
;  outdir - string with the name of the directory to write to.  This can
;              be a fully qualified or relative path.  If fully qualified,
;              do not use the PATH keyword since the path is pre-pended to
;              this input.  This directory will be created if it doesn't
;              already exist.  You will need to manually delete files if you
;              run this routine multiple times with different binning.
;  binfac - Time binning factor.  A value <= 1 is ignored.  Only integer
;              values are allowed.  An arbitrary upper limit of 10 is imposed.
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;  PATH   - String with a path to look for the input and output directories.
;              Typically, the input will be the original directory but
;              appropriately renamed (eg., 06_04_00 --> 06_04_00-original)
; OUTPUTS:
; KEYWORD OUTPUT PARAMETERS:
; CONFIGURATION:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
;
;  Due to the nature of the readout banding of this camera, each image is
;   run through backsub.pro to remove the banding.  This strips both the
;   banding and the sky signal.  To recover, the mean sky is added back in.
;   There is no way to distinguish the true bias level from the sky.  This
;   bias shouldn't be added more than once but the sky should accumulate.
;   Correcting for this will be left to what ever processes this output.
;
;  The header of the first image of a group is carried to the output data
;   file.  Only EXPTIME is modified (multiplied by binfac).  The output file
;   names are a new sequence starting with frame_00001.fits and advancing
;   from there.  The camera settings file is not carried over.  That will
;   require a manual edit and copying over of the file if needed.
;
; MODIFICATION HISTORY:
;  Written by Marc W. Buie, Southwest Research Institute, 2021/01/15
;-
pro occtimbin,indir,outdir,binfac,PATH=in_path

   self='occtimbin: '
   if badpar(indir,7,0,caller=self+'(indir) ') then return
   if badpar(outdir,7,0,caller=self+'(outdir) ') then return
   if badpar(binfac,[2,3],0,caller=self+'(binfac) ') then return
   if badpar(in_path,[0,7],0,caller=self+'(PATH) ',default='') then return

   if binfac le 1 or binfac gt 10 then begin
      print,'binfac must be between 2 and 10, inclusive.  Nothing done'
      return
   endif

   if in_path ne '' then path=addslash(in_path) else path=in_path

   if not exists(path+indir) then begin
      print,path+indir
      print,'Input directory not found.  Aborting.'
      return
   endif

   dir1 = addslash(path+indir)
   dir2 = addslash(path+outdir)

   if not exists(dir2) then file_mkdir,dir2

   fnin=file_search(dir1+'*.fits',count=nfiles)
   fnin=strmid(fnin,strlen(dir1))

   if nfiles eq 0 then begin
      print,dir1+'*.fits'
      print,'No image files found.  Aborting.'
      return
   endif

   print,strn(nfiles),' input image files found.  Binning by ',strn(binfac)

   fno=0
   for i=0,nfiles-1,binfac do begin
      fnout='frame_'+string(fno,format='(i05.5)')+'.fits'
      if i+binfac gt nfiles then break
      str=strjoin(fnin[i:i+binfac-1],',')
      print,fnout,' ',str
      hdr=headfits(dir1+fnin[i])
      nx=sxpar(hdr,'NAXIS1')
      ny=sxpar(hdr,'NAXIS2')
      exptime=sxpar(hdr,'EXPTIME')
      blacklvl=sxpar(hdr,'BLKLEVEL')
      outim=fltarr(nx,ny)
      sky=0.0
      for j=0,binfac-1 do begin
         im=readfits(dir1+fnin[i+j])
         im=float(im)
         robomean,im,3.0,0.5,meanval
         sky += (meanval-blacklvl)
         backsub,im,/row
         outim += im
      endfor
      outim += (sky+blacklvl)
      showsrc,outim,window=0
      showsrc,im,window=1
      sxaddpar,hdr,'BITPIX',-32
      sxaddpar,hdr,'BZERO',0
      sxaddpar,hdr,'EXPTIME',exptime*binfac
      sxaddpar,hdr,'TIMEBIN',binfac
      sxdelpar,hdr,'ASTINFO'
      writefits,dir2+fnout,outim,hdr
      fno++
   endfor

end
