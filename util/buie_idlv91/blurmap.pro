;+
; NAME:
;  blurmap
; PURPOSE:   (one line only)
;  Apply spherical blurring to a map
; DESCRIPTION:
; CATEGORY:
;  Numerical
; CALLING SEQUENCE:
;  blurmap,inmap,blurradius,outmap
; INPUTS:
;  inmap - Rectangular array which contains a map of a spherical body.  The
;            coordinate scheme used is documented in RENDER.PRO
;  blurradius - radius of the blurring function in degrees (<90)
;                 a gaussian filter is used and this is the FWHM of the filter
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;  OUTSIZE - two element vector that gives the size of the output map.
;              Default is the same as the input map.
;  MAPID   - String that identifies the map this is computed for.  The default
;              value is 'default'.  This is used to control the cached values.
;              If the MAPID changes, the internal values are recomputed.
;              Also, if the blurradius, inmap size, or outmap size changes
;              the cached is recomputed.
;  RESET   - Flag, if set, forces a cache refresh
; OUTPUTS:
;  outmap - Output map smoothed to desired level
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  Written by Marc W. Buie, Lowell Observatory, 2003/09/18
;  2015/07/01, MWB, added caching and MAPID/RESET keywords
;-
pro blurmap,inmap,blurradius,outmap,OUTSIZE=outsize,MAPID=mapid,RESET=reset

   common com_blurmap,cache

   self='BLURMAP '
   if badpar(inmap,[4,5],2,caller=self+'(inmap) ') then return
   if badpar(blurradius,[2,3,4,5],0,caller=self+'(blurradius) ') then return
   if badpar(mapid,[0,7],0,caller=self+'(MAPID) ',default='default') then return
   if badpar(reset,[0,1,2,3],0,caller=self+'(RESET) ',default=0) then return

   cache_type=size(cache,/type)

   sz = size(inmap,/dimen)
   if badpar(outsize,[0,1,2,3],1,caller=self+'OUTSIZE',default=sz) then return

   if cache_type eq 8 and not reset then begin
      recalc=0
      if cache.mapid ne mapid then recalc=1
      if cache.blurradius ne blurradius then recalc=1
      if cache.sz[0] ne sz[0] then recalc=1
      if cache.sz[1] ne sz[1] then recalc=1
      if cache.outsize[0] ne sz[0] then recalc=1
      if cache.outsize[1] ne sz[1] then recalc=1
   endif else recalc=1

   outmap = fltarr(outsize[0],outsize[1])

   if recalc then begin

      if cache_type eq 8 then begin
         for i=0L,cache.nvals-1 do begin
            ptr_free,cache.wptr[i]
            ptr_free,cache.zptr[i]
         endfor
      ; de-allocate memory
      endif

      blur_r = (blurradius<90.0) / !radeg

      ; compute the unit vectors for each pixel in the input map
      initvc,sz[0],sz[1],xn1,yn1,zn1,area1

      ; compute the unit vectors for each pixel in the output map
      initvc,outsize[0],outsize[1],xn2,yn2,zn2,area2

      ; setup caching array
      nvals=long(outsize[0])*long(outsize[1])
      wptr=ptrarr(nvals)
      zptr=ptrarr(nvals)
      wtotal=fltarr(nvals)

      ; loop over all tiles in the output map
      idx=0L
      for i=0L,outsize[0]-1 do begin  ; x/lon
         for j=0L,outsize[1]-1 do begin ; y/lat
            arg = ( (xn2[i,j]*xn1+yn2[i,j]*yn1+zn2[i,j]*zn1) < 1.0 ) > (-1.0)
            ang = acos(arg)
            arg = -(ang/blur_r)^2
            z=where(arg gt -20 and ang lt !pi/2.0,count)
            w = exp(arg[z])*area1[z]
            wptr[idx]=ptr_new(w)
            zptr[idx]=ptr_new(z)
            wtotal[idx]=total(w)
            outmap[i,j] = total(inmap[z]*w)/wtotal[idx]
            idx++
         endfor
      endfor

      cache = { $
         mapid:      mapid, $
         blurradius: blurradius, $
         blur_r:     blur_r, $
         sz:         sz, $
         outsize:    outsize, $
         wptr:       wptr, $
         zptr:       zptr, $
         wtotal:     wtotal, $
         nvals:      nvals $
         }

   endif else begin

      idx=0L
      for i=0L,outsize[0]-1 do begin  ; x/lon
         for j=0L,outsize[1]-1 do begin ; y/lat
            outmap[i,j] = total(inmap[(*cache.zptr[idx])] * $
                                      (*cache.wptr[idx])) / $
                               cache.wtotal[idx]
            idx++
         endfor
      endfor

   endelse

end
