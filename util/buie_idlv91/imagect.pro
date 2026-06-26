;+
; NAME:
;  imagect
; PURPOSE:   (one line only)
;  Convert a byte image to a rgb image using a standard color table
; DESCRIPTION:
;  This tool lets you re-create the illusion of using decomposed color
;    that was our only option many years ago.  24-bit color is generally
;    a lot more useful but false color image display is still handy.   The
;    result of this function is to return a 3-plane rgb color image where
;    the input byte image has been converted to rgb according to intensity
;    into the chosen color table.
; CATEGORY:
;  Image display
; CALLING SEQUENCE:
;  result=imagect(image,table)
; INPUTS:
;  image - Byte scaled image to convert
;  table - IDL standard color table number
;            -or-
;          A string which is the name of a local color table in your
;          path.  These files are compatible with cpalette.pro.  The files
;          are named color.<table>.dat.  If you give the table name it will
;          be loaded.  However, not all of these local color tables have
;          256 colors.  Those that are not are expanded by replication to
;          this length meaning some indexes will be duplicated.
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;  BLACK - Flag, if set will replace the first color in the lookup table
;             with black so that 0 in the image comes out as black on the
;             display.  The lookup table itself is not modified, only
;             the resulting output.
; OUTPUTS:
;  return value is a [nx,ny,3] byte cube where nx,ny is the size of the
;    input image.
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
;  Makes use of mwb_palette_com managed by cpalette.pro
; SIDE EFFECTS:
;  If you use a local palette, the loaded (and thus default) palette may
;    change.
; RESTRICTIONS:
; PROCEDURE:
;
;   Here's an example.  Given "image" is a byte-scaled image (use bytscl
;     ahead of time if you need to):
;
;   tv,imagect(image,34,/black),true=3
;
;   This creates a false-color image where intensity has been mapped to color
;     using the Rainbow2 standard IDL color table.  (see loadct documentation
;     for a list of color tables).
;
; MODIFICATION HISTORY:
;  2015/11/08, Written by Marc W. Buie, Southwest Research Institute
;  2016/01/12, MWB, added option of local color tables.
;-
function imagect,image,table,BLACK=black

   common mwb_palette_com,info

   self='imagect: '
   if badpar(image,1,2,caller=self+'(image) ') then return,0
   if badpar(table,[2,3,7],0,caller=self+'(table) ', $
                             type=table_type) then return,0
   if badpar(black,[0,1,2,3],0,caller=self+'(BLACK) ',default=0) then return,0

   sz=size(image,/dimen)
   r=bytarr(sz[0],sz[1])
   g=bytarr(sz[0],sz[1])
   b=bytarr(sz[0],sz[1])

   if table_type eq 7 then begin
      result=cpalette(0,table)
      rgb=bytarr(256,3)
      if info.ncolors eq 256 then begin
         rgb[*,0]=info.r
         rgb[*,1]=info.g
         rgb[*,2]=info.b
      endif else begin
         rgb[*,0]=congrid(info.r,256)
         rgb[*,1]=congrid(info.g,256)
         rgb[*,2]=congrid(info.b,256)
      endelse
   endif else begin
      loadct,table,rgb_table=rgb
   endelse

   if black then i0=1 else i0=0

   for i=i0,255 do begin
      z=where(image eq i,count)
      if count ne 0 then begin
         r[z] = rgb[i,0]
         g[z] = rgb[i,1]
         b[z] = rgb[i,2]
      endif
   endfor

   return,[[[r]],[[g]],[[b]]]

end
