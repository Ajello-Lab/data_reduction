;+
; NAME:
;  hstmodstar
; PURPOSE: (one line)
;  Calculate chi-square value for trailed star fit
; DESCRIPTION:
;  Chi-sq function for Powell or Ameoba fitting and showmodel.
; CATEGORY:
;  Astrometry
; CALLING SEQUENCE:
;  hstmodstar, vals
; INPUTS:
;  vals - array of parameters to fit, It is a three element vector = [x,y,flux]
;           The common block includes the fit keywords that controls which
;           variables are included in vals.
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
; OUTPUTS:
;  return the chisq for the model computed from vals
; COMMON BLOCKS:
;  com_hststarfit:  Used for communication of data
; SIDE EFFECTS:
;  Uses HST PSF images (hstpsf.pro) which may cause the memory and disk PSF
;     caches to be populated.
;
; RESTRICTIONS:
; PROCEDURE:
;
; MODIFICATION HISTORY:
;  2011/03/24, MWB, cloned from hst15 routine of the same name.
;  2011/05/07, MWB, internal restructuring to keep more intermediate
;                      results including full arrays of everything
;  2014/07/03, MWB, cloned from chimodplch
;  2014/07/08, MWB, cloned from chimod.pro
;  2016/12/21, MWB, added option for flux input as a magnitude
;  2020/06/24, MWB, generalized version
;-
function hstmodstar,vals

   common com_hststarfit,info

   iv = 0

   x=vals[iv++]
   y=vals[iv++]
   f=vals[iv++]

   if info.magval then begin
      f = 10.0^((f-24.0)/(-2.5))
   endif

   f=f>0

   dx = x-info.xoff
   dy = y-info.yoff

   wfc3model,info.det,dx,dy,f,info.filter,info.bmvnum, $
             0.0,image,OBJRAD=0.5,z4=info.z4, $
             HSTPATH=info.hstpath,/NEW, $
             xsize=info.csz,ysize=info.csz, $
             xoffset=info.xoff,yoffset=info.yoff,jitter=info.smear

   info.modimage=image

   i1 = info.ci1
   i2 = info.ci2
   j1 = info.cj1
   j2 = info.cj2

   info.mimage = info.modimage[i1:i2,j1:j2]
   info.mimageps[*,*]=info.mimage[*,*]+info.back
   info.dimage = info.fitimage-(info.mimage+info.back)

   if info.noisemod eq 0 then begin
      simage = info.simage
   endif else begin
      simage = sqrt(info.mimageps+info.readnoise^2)
   endelse

   if info.chired then begin
      if info.weight then begin
         chisq = total((info.dimage[info.zg] / $
                        simage[info.zg])^2 ) / $
                    float(info.goodcount)
      endif else begin
         chisq = total(info.dimage[info.zg]^2)/float(info.goodcount)
      endelse
   endif else begin
      if info.weight then begin
         chisq = total((info.dimage[info.zg] / $
                        simage[info.zg])^2 )
      endif else begin
         chisq = total(info.dimage[info.zg]^2)
      endelse
   endelse

   if not info.quiet then begin
      if chisq lt info.chibest then begin
         print,info.chicount,chisq,info.goodcount,vals
         info.chibest=chisq
         info.nbetter++
      endif
      info.chicount++
   endif

   return,chisq

end
