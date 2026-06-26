;+
; NAME:
;  cpalette
; PURPOSE:   (one line only)
;  Choose a color from a selection of palettes
; DESCRIPTION:
;
;  This routine will look up a color out of a palette.  You give the name
;    of the palette.  A file named color.<set>.dat is expected to be
;    somewhere in your path.  These files contain RGB triplets.  The input
;    index must be in the file.  If there are any errors, a color of '7f7f7f'xl
;    is returned.
;
;  The file format needed is an ASCII tabular file that can be read by readcol.
;    Each line must contain a set of three values, R G B, as integers (will
;    be read in and used as byte data).  You are allowed to have comments
;    in the file.  Text after the three values will be ignored.  You can have
;    as many or as few colors in the table as you like but practically you
;    should have at least 2 and no more than 256.  More colors than 256
;    are allowed but will be less useful in some contexts.   The index for
;    each color in the file counts up from 0 being the index of the first
;    triplet.
;
; CATEGORY:
;  Miscellaneous
; CALLING SEQUENCE:
;  color=palette(index,set)
; INPUTS:
;  index - index number of the color in the set to return
; OPTIONAL INPUT PARAMETERS:
;  set - string, name of the set of colors to pull from, the default is
;           the last set loaded and if none are loaded the crayola set is used.
; KEYWORD INPUT PARAMETERS:
;  ARRAY - Flag, if set forces the output to be in a three-element array
; OUTPUTS:
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
;  mwb_palette_com
;    contains the last palette that was read in.  This is to allow multiple
;    calls within the same palette avoid repeated reading of the file.
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  2016/01/10, Written by Marc W. Buie, Southwest Research Institute
;  2016/01/18, MWB, added ARRAY keyword
;-
function cpalette,index,set,ARRAY=array

   common mwb_palette_com,info

   self='palette: '
   if badpar(index,[1,2,3],0,caller=self+'(index) ') then return,'7f7f7f'xl
   if badpar(set,[0,7],0,caller=self+'(set) ',default='') then return,'7f7f7f'xl
   if badpar(array,[0,1,2,3],0,caller=self+'(ARRAY) ',default=0) then return,'7f7f7f'xl

   type=size(info,/type)

   loadit=0
   if type eq 0 then loadit=1
   if type ne 0 then begin
      if info.set ne set and set ne '' then loadit=1
   endif

   if loadit then begin
      if set eq '' then begin
         in_file='color.crayola.dat'
      endif else begin
         in_file='color.'+set+'.dat'
      endelse
      file=find_with_def(in_file,!path)
      if file eq '' then begin
         print,self,'Palette file ',in_file,' not found.'
         return,'7f7f7f'xl
      endif

      readcol,file,r,g,b,format='b,b,b',count=ncolors

      info={ $
         set: set, $
         r: r, $
         g: g, $
         b: b, $
         ncolors: ncolors}

   endif

   if index lt 0 or index ge info.ncolors then return,'7f7f7f'xl

   if array then begin
      return,[info.r[index],info.g[index],info.b[index]]
   endif else begin
      lvalue = ishft(long(info.b[index]),16) + $
                  ishft(long(info.g[index]),8) + $
                  long(info.r[index])
      return,lvalue
   endelse

end
