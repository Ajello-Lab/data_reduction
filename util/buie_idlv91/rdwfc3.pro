;+
; NAME:
;  rdwfc3
; PURPOSE:   (one line only)
;  Read a HST WFC3 image with an associated mask and select header information
; DESCRIPTION:
; CATEGORY:
;  File I/O
; CALLING SEQUENCE:
;  rdwfc3,root,det,data
; INPUTS:
;  root - String with the root of the file name (preceeds '_')
;  det  - which detector 1 = UVIS1 and 2 = UVIS2
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;  DDIR - Directory where data can be found
;  TYPE - Data file type (flt or flc) to read, default='flc'
;  SATUR - saturation level for detectory.  Default=80000.0
;            This is used to build a mask if not is found where all pixels
;            above this level are automatically set to bad.
; OUTPUTS:
;  data - anonymous structure with all the collected information.
;            On failure, data=!null.  The following tags are defined.
;      fnim:  Full file name with path for the image file read
;      fnmsk: Full file name with path for the mask file read
;      image: The image
;      mask:  The mask, same size as the image
;      maskhdr: The header from the mask image
;      nx:    Width of the image in pixels
;      ny:    Height of the image in pixels
;      date:  UT Date at start of exposure (string)
;      time:  UT Time at start of exposures (string)
;      exptime: Exposure time (seconds)
;      filter: Name of the filter used
;      astinfo:  anonymous structure with the wcs solution from the header
;      epoch: Mid-time of exposure in decimal years
;      jdmid: UT JD of the mid-time of the exposure
;
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  2020/06/23, Written by Marc W. Buie, Southwest Research Institute.
;                Generalized from previous prototype versions.
;-
pro rdwfc3,root,det,data,DDIR=ddir,TYPE=type,SATUR=satur

   self='rdwfc3: '
   if badpar(root,7,0,caller=self+'(root) ') then return
   if badpar(det,[2,3],0,caller=self+'(det) ') then return
   if badpar(type,[0,7],0,caller=self+'(TYPE) ', $
                default='flc') then return
   if badpar(ddir,[0,7],0,caller=self+'(DDIR) ',default='') then return
   if badpar(satur,[0,2,3,4,5],0,caller=self+'(SATUR)', $
                                    default=80000.0) then return

   fnim=root+'_'+type+'.fits'
   fnmsk=root+'_msk.fits'

   if exists(ddir+fnim+'.gz') then fnim=fnim+'.gz'

   if not exists(ddir+fnim) then begin
      print,ddir+fnim
      print,'File not found.  Aborting'
      data=!null
      return
   end

   if det eq 1 then exten=4 else exten=1
   fits_read,ddir+fnim,im,hdr,exten=exten

   date=sxpar(hdr,'DATE-OBS')
   time=sxpar(hdr,'TIME-OBS')
   jdstart=fxpar(hdr,'EXPSTART',DATATYPE=0.0d0)+2400000.5d0
   exptime=sxpar(hdr,'EXPTIME')
   filter=strtrim(sxpar(hdr,'FILTER'),2)
   naxis=sxpar(hdr,'NAXIS*')
   nx=naxis[0]
   ny=naxis[1]
   jdmid=jdstart+exptime/2.0/86400.0d0

   extast,hdr,astinfo

   if exists(ddir+fnmsk) then begin
;      print,'Read existing mask ',ddir+fnmsk
      fits_read,ddir+fnmsk,mask,maskhdr,exten=det,/no_pdu
   endif else begin
;      print,'Create default mask'
      mask=bytarr(nx,ny)
      zs=where(im ge satur,satcount)
      if satcount ne 0 then mask[zs]=1B
      maskhdr=['']
   endelse

   z=where(mask ne 0,count)
   if count ne 0 then im[z]=0

   jd2year,jdmid,epoch

   data={ $
      fnim:  ddir+fnim, $
      fnmsk: ddir+fnmsk, $
      image: im, $
      hdr:   hdr, $
      mask:  mask, $
      maskhdr: maskhdr, $
      nx:    nx, $
      ny:    ny, $
      date:  date, $
      time:  time, $
      exptime: exptime, $
      filter: filter, $
      astinfo:  astinfo, $
      epoch: epoch, $
      jdmid: jdmid $
      }

end
