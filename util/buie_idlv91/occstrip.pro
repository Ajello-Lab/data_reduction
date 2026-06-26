;+
; NAME:
;  occstrip
; PURPOSE:   (one line only)
;  Generate a postage stamp strip of images for an occultation
; DESCRIPTION:
; CATEGORY:
;  Occultation
; CALLING SEQUENCE:
;  occstrip,dir,x,y,xhw,yhw,i1,i2
; INPUTS:
;  dir - String, name of the directory where the data files can be found.
;           This directory will be searched for files ending in .fits.
;           The file lits is accessed in normal directory sorting order.
;  x   - x pixel location of center of ROI to extract
;  y   - y pixel location of center of ROI to extract
;  xhw - half-width of ROI in x direction
;  yhw - half-width of ROI in y direction, total width is 2*hw+1
;  i1  - first image to extract ROI from  (0 is the first file in the list)
;  i2  - last image to extract ROI from
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
; OUTPUTS:
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  Written by Marc W. Buie, Southwest Research Institute, 2020/04/06
;-
pro occstrip,dir,x,y,xhw,yhw,f1,f2,BINFAC=binfac,HISIG=hisig,EVENT=event

   self='occstrip: '
   if badpar(dir,7,0,caller=self+'(dir) ') then return
   if badpar(x,[2,3,4,5],0,caller=self+'(x) ') then return
   if badpar(y,[2,3,4,5],0,caller=self+'(y) ') then return
   if badpar(xhw,[2,3],0,caller=self+'(xhw) ') then return
   if badpar(yhw,[2,3],0,caller=self+'(yhw) ') then return
   if badpar(f1,[2,3],0,caller=self+'(f1) ') then return
   if badpar(f2,[2,3],0,caller=self+'(f2) ') then return
   if badpar(binfac,[0,1,2,3],0,caller=self+'(BINFAC) ',default=1) then return
   if badpar(hisig,[0,2,3,4,5],0,caller=self+'(HISIG) ',default=5) then return
   if badpar(event,[0,2,3],1,caller=self+'(EVENT) ',default=[-1,-1]) then return

   fnlist=file_search(addslash(dir)+'*.fits',count=nfiles)

   print,strn(nfiles),' found.'
   if nfiles eq 0 then return

   if f1 lt 0 or f1 ge nfiles then begin
      print,'First frame index number is out of range'
      return
   endif

   if f2 lt 0 or f2 ge nfiles then begin
      print,'Last frame index number is out of range'
      return
   endif

   lowsig = -3

   setwin,0
   erase
   for i=f1,f2 do begin
      im=float(readfits(fnlist[i]))
      backsub,im,/row
      if binfac ge 1 then begin
         sz=size(im,/dimen)
         xsm = sz[0]/binfac
         ysm = sz[1]/binfac
         im = rebin(im[0:xsm*binfac-1,0:ysm*binfac-1],xsm,ysm)
      endif
      i0=round(x)-xhw
      i1=round(x)+xhw
      j0=round(y)-yhw
      j1=round(y)+yhw
      subarr,im,i0,i1,j0,j1,sub,error
      skysclim,im,lowval,hival,meanval,sigma,npts=30000,lowclip=0.2,hiclip=0.8
      lowval = max([meanval+lowsig*sigma,min(im)])
      hival  = min([meanval+hisig*sigma,max(im)])

      bsub=bytscl(sub,min=lowval,max=hival,top=255)
      bsubx=bsub
      if event[0] ge 0 and event[1] ge 0 then begin
         if i ge event[0] and i le event[1] then doborder=0 else doborder=1
if doborder eq 0 then print,i,' ',fnlist[i]
      endif else begin
         doborder=1
      endelse
      if doborder then begin
         bsubx[0,*]=255
         bsubx[-1,*]=255
         bsubx[*,0]=255
         bsubx[*,-1]=255
      endif else begin
         bsubx=bsub
         bsub[0,*]=255
         bsub[-1,*]=255
         bsub[*,0]=255
         bsub[*,-1]=255
      endelse
      setwin,0
      tv,[[[bsubx]],[[bsub]],[[bsub]]],i-f1,true=3
showsrc,im
oplot,[x],[y],psym=4,symsize=3,color=cpalette(1)
   endfor
;tv,bsub,1
;showsrc,im
;oplot,[x],[y],psym=4,symsize=3,color=cpalette(1)

end
