;+
; NAME: 
;  tvmaps
; PURPOSE:
;  Display a full set of Pluto/Charon model .til maps on the current display.
; DESCRIPTION:
;  This is a simple tool to help view the version 2 MaxEnt map tiling format
;    that is more fully described in readtil.pro.  Both the default image
;    and the reconstructed image are shown.
; CATEGORY:
;  Miscellaneous
; CALLING SEQUENCE:
;  tvmaps,root,scale,pluto,charon,pldef,chdef
; INPUTS:
;  root - String, with the root of the file names to be read.  This program
;           expects to file a .til and .def file pair and you are providing
;           the common part of the name.
;  scale - Scaling factor, positive definite integer.
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
; OUTPUTS:
;  pluto - This is the Pluto map in the tiled format.  The length of the array
;             defines the attributes of the map and the value is usually the
;             single-scattering albedo.
;  charon - Charon map, similar to Pluto.  May or may not be the same size as
;             Pluto.
;  pldef - This is the default map for Pluto.  This is used by the MaxEnt
;             fitting program to indicate what should be used in regions where
;             there are little to no constraints.
;  chdef - This is the default map for Charon.
;
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  1997/06/21, Written by Marc W. Buie, Lowell Observatory
;  2023/02/27, MWB, added documentation and some cosmetic improvements.
;-
pro tvmaps,root,scale,pluto,charon,pldef,chdef,WINDOW=win

   self='tvmaps: '
   if badpar(root,7,0,caller=self+'(root) ') then return
   if badpar(scale,[2,3],0,caller=self+'(scale) ') then return
   if badpar(win,[0,2,3],0,caller=self+'(win) ',default=0) then return

   erase,0

   if nofile(root+'.til','Tile map') then return
   if nofile(root+'.def','Default map') then return

   readtil,root+'.til',pluto,charon
   readtil,root+'.def',pldef,chdef

   ; Compute the number of annuli for each object based on the length of the map
   ;  it is expected that the map and default are the same length
   nann1 = fix(sqrt(n_elements(pluto)/4))
   plann=nann1*2
   nann1 = fix(sqrt(n_elements(charon)/4))
   chann=nann1*2

   plmin=min(pluto)
   plmax=max(pluto)
   chmin=min(charon)
   chmax=max(charon)

   width = ((plann/2)*4)*scale*2
   height = (plann*2 + chann*2)*scale

   setwin,win,xsize=width,ysize=height

   x = 0
   y = (plann+2*chann)*scale
   tvtil,bytscl(pluto,min=0,max=1,top=255),x,y,scale
   str='Pluto [0,1]'
   xyouts,x+1,y+1,str,align=0,color=cpalette(1),/device

   x = 2*plann*scale
   tvtil,bytscl(pluto,min=plmin,max=plmax,top=255),x,y,scale
   str='Pluto ['+strn(plmin,format='(f10.2)')+','+ $
                 strn(plmax,format='(f10.2)')+']'
   xyouts,x+1,y+1,str,align=0,color=cpalette(1),/device

   x = plann*scale
   y = (plann+chann)*scale
   tvtil,bytscl(charon,min=0,max=1,top=255),x,y,scale
   str='Charon [0,1]'
   xyouts,x+1,y+1,str,align=1,color=cpalette(1),/device

   x = 3*plann*scale
   tvtil,bytscl(charon,min=chmin,max=chmax,top=255),x,y,scale
   str='Charon ['+strn(chmin,format='(f10.2)')+','+ $
                 strn(chmax,format='(f10.2)')+']'
   xyouts,x+1,y+1,str,align=1,color=cpalette(1),/device

   x = 0
   y = chann*scale
   tvtil,bytscl(pldef,min=0,max=1,top=255),x,y,scale
   str='P default [0,1]'
   xyouts,x+1,y+1,str,align=0,color=cpalette(1),/device

   x = 2*plann*scale
   tvtil,bytscl(pldef,min=plmin,max=plmax,top=255),x,y,scale
   str='P default ['+strn(plmin,format='(f10.2)')+','+ $
                     strn(plmax,format='(f10.2)')+']'
   xyouts,x+1,y+1,str,align=0,color=cpalette(1),/device

   x = plann*scale
   y = 0
   tvtil,bytscl(chdef,min=0,max=1,top=255),x,y,scale
   str='Charon [0,1]'
   xyouts,x+1,y+1,str,align=1,color=cpalette(1),/device

   x = 3*plann*scale
   tvtil,bytscl(chdef,min=chmin,max=chmax,top=255),x,y,scale
   str='Charon ['+strn(chmin,format='(f10.2)')+','+ $
                 strn(chmax,format='(f10.2)')+']'
   xyouts,x+1,y+1,str,align=1,color=cpalette(1),/device

end
