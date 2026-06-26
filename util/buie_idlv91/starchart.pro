;+
; NAME:
;  starchart
; PURPOSE:   (one line only)
;  Generate a star chart graphic
; DESCRIPTION:
; CATEGORY:
;  Astronomy
; CALLING SEQUENCE:
;  starchart,ra,dec,width,height,scale,outfile
; INPUTS:
;  ra    - Right ascension of center of plot (J2000, radians)
;  dec   - Declination of center of plot (J2000, radians)
;  width - width of plot in units native to output format
;            use pixels for any bitmap output and use centimeters for
;            postscript
;  height - height of plot (same units as width)
;  scale  - scale of image in arcsec/plot unit (eg., arcsec/pixel or arcsec/cm)
;            The scale is forced to be the same in both directions.
;  outfile - Name of the file where the graphic is to be saved.  The suffix
;              of the file will determine the output file type.
;              To get the plot shown on the display, use '<DISPLAY>'.
;              Uses plot function graphic calls.
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;  CATALOG - Name of the catalog to use
;              GDR2 (default)
;              UCAC2 (not implemented)
;              USNOB (not implemented)
;              URAT1 (not implemented)
;              USNO (implemented)
;  MAGLIM  - Limiting magnitude of finder chart (default=20)
;  EWFLIP  - Flag, if set, plot is flipped east-west
;  ORIENT  - Position angle of north on plot measured
;               (angle is CCW from up), value given in degrees.
;  OBJMARK - If specified, this is a [2,N] array of ra,dec.  Each position
;              is marked on the plot.
;  FOV     - If provided, specifies an anonymous structure that defines a
;              camera field of view.   The following tags must be defined.
;                 type='R'   (rectangular)
;                   width: width of FOV in arcsec
;                   height: height of FOV in arcsec
;                   pang: position angle of up in FOV, degrees measured
;                           eastward from north
;                 type='C'  (circular)
;                   radius: radius of FOV in arcsec
; OUTPUTS:
;  The generated graphic is saved to the output file desired.
;    This routine uses the hidden graphics mode of the plot functions and
;    can be run in detached batch processes.  Using the display mode only
;    work interactively.
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  Written by Marc W. Buie, Southwest Research Institute, 2015/04/14
;  2016/06/16, MWB, changed plot algorithm to discretize the star symbol
;                   sizes and make fewer plot calls.  This speeds the program
;                   up a phenomenal amount.
;  2019/01/23, RL, Implementation of CATALOG (including support to Gaia DR2),
;                  EWFLIP and ORIENT
;  2020/01/06, RL, Remove plot of additional bright stars when catalog=GDR2 
;  2024/11/19, MWB, changed to protect input ra,dec from being modified
;-
pro starchart,in_ra,in_dec,width,height,scale,outfile, $
       CATALOG=catalog,MAGLIM=maglim,JD=jd,TITLE=title,ORIENT=orient, $
       EWFLIP=ewflip

   self='starchart: '
   if badpar(in_ra,[4,5],0,caller=self+'(ra) ') then return
   if badpar(in_dec,[4,5],0,caller=self+'(dec) ') then return
   if badpar(width,[2,3,4,5],0,caller=self+'(width) ') then return
   if badpar(height,[2,3,4,5],0,caller=self+'(height) ') then return
   if badpar(scale,[2,3,4,5],0,caller=self+'(scale) ') then return
   if badpar(outfile,7,0,caller=self+'(outfile) ') then return

   if badpar(title,[0,7],0,caller=self+'(TITLE) ',default='') then return
   if badpar(catalog,[0,7],0,caller=self+'(CATALOG) ', $
                             default='GDR2') then return
   if (catalog ne 'USNO') && (catalog ne 'GDR2') then begin
      print,catalog,' not implemented'
      return
   endif
   
   if badpar(maglim,[0,2,3,4,5],0,caller=self+'(MAGLIM) ', $
                                  default=20.0) then return
   if badpar(EWFLIP,[0,1,2,3],0,caller=self+'(EWFLIP) ',default=0) then return
   if badpar(orient,[0,2,3,4,5],0,caller=self+'(ORIENT) ', $
                                  default=0.0) then return
   if badpar(jd,[0,4,5],0,caller=self+'(JD) ', $
                                  default=systime(/julian,/ut)) then return

   if outfile eq '<DISPLAY>' then buffer=0 else buffer=1

   ra=in_ra
   dec=in_dec

   xr=[-0.5,0.5]*width*scale
   yr=[-0.5,0.5]*height*scale

   xsize=1000
   ysize=1000

   p=plot([0],buffer=buffer,xrange=xr,yrange=yr,/nodata, $
          axis_style=0,margin=[0.05,0.05,0.05,0.05],dimensions=[xsize,ysize])

   ; plot a border
   xbox=[xr[0],xr[1],xr[1],xr[0],xr[0]]
   ybox=[yr[0],yr[0],yr[1],yr[1],yr[0]]
   jdstr,jd,-12,jds
   p1=plot(/overplot,xbox,ybox,thick=4,color='black')

   ; Collect the list of stars.
   if (catalog eq 'USNO') then begin
      refnet,ra,dec,width*scale,height*scale,maglim,maglim,'tmp.cat'
      rdstarc,'tmp.cat',sra,sdec,bmag,rmag,nstars,/noconvert,/silent
      file_delete,'tmp.cat',/quiet
   endif else if (catalog eq 'GDR2') then begin
      epoch=float(STRMID(systime(),3,4,/reverse_offset))
      refnet,ra,dec,width*scale,height*scale,maglim,maglim,'tmp.cat',gaia=epoch
      rdstarc,'tmp.cat.gcat',sra,sdec,bmag,rmag,nstars,/noconvert,/silent   
      file_delete,'tmp.cat',/quiet
      file_delete,'tmp.cat.gcat',/quiet
   endif

   

   ; Get a supplemental list of bright stars
   radius=sqrt((height*scale)^2+(width*scale)^2)/3600.0/!radeg
   openmysql,dblun,'obs'
   sc_nearest, dblun, ra, dec, radius, id, bra, bdec, dra, ddec, brmag
   free_lun,dblun

   ; convert to xi/eta
   astrd2sn,sra,sdec,ra,dec,sxi,seta,/arcsec
   astrd2sn,bra,bdec,ra,dec,bsxi,bseta,/arcsec

   ; flip East/West
   if ewflip eq 1 then begin
      sxi=-sxi
   endif

   ; rotate
   ;orient in degrees, East of North
   ; convert rotation to rad
   rotrad = orient*!pi/180
   sxirot =  sxi * cos(rotrad) - seta * sin(rotrad)
   setarot = sxi * sin(rotrad) + seta * cos(rotrad)
   
   sxi =sxirot
   seta=setarot
   
   ; filter to only those plottable
   z=where(sxi ge xr[0] and sxi le xr[1] and $
           seta gt yr[0] and seta le yr[1],nstars)
   if nstars eq 0 then begin
      print,self,'No stars in plotting region.'
      return
   endif
   sra=sra[z]
   sdec=sdec[z]
   sxi=sxi[z]
   seta=seta[z]
   bmag=bmag[z]
   rmag=rmag[z]

   z=where(bsxi ge xr[0] and bsxi le xr[1] and $
           bseta gt yr[0] and bseta le yr[1],nbstars)
   if nbstars gt 0 then begin
      bra=bra[z]
      bdec=bdec[z]
      bsxi=bsxi[z]
      bseta=bseta[z]
      brmag=brmag[z]
   endif

   ; plot
   ;topclip=3.0
   topclip=1.5
   botclip=0.1

   ; faint end should be the size of botclip, bright end should be
   ;  the size of topclip and bright=faint-10
   mag0=maglim-0.9
   mag1=maglim-8
   tmag=[mag0,mag1]
   tsize=[botclip,topclip]
   c=poly_fit(tmag,tsize,1)

   ssz=(poly(rmag,c) < topclip) > botclip

   ; discretize the sizes
   ssz = long(ssz*10.0)/10.0

   ussz=ssz[uniq(ssz,sort(ssz))]

   if nbstars gt 0 then begin
      bssz=(poly(brmag,c) < topclip) > botclip
   endif

   lmag=findgen(18)
   lssz=(poly(lmag,c) < topclip) > botclip

   for i=0,n_elements(ussz)-1 do begin
      z=where(ssz eq ussz[i],count)
      if count gt 0 then begin
         p2=plot([sxi[z[0:count-1]]],[seta[z[0:count-1]]], $
                 /overplot,linestyle='none', $
                 symbol='circle',/sym_filled,sym_size=ussz[i])
      endif
   endfor
;   for i=0,nstars-1 do begin
;      p2=plot([sxi[i]],[seta[i]],/overplot,linestyle='none', $
;              symbol='circle',/sym_filled,sym_size=ssz[i])
;;if i ge 100 then break
;   endfor
   if catalog ne 'GDR2' then begin
      for i=0,nbstars-1 do begin
         p2=plot([bsxi[i]],[bseta[i]],/overplot,linestyle='none', $
                 symbol='circle',/sym_filled,sym_size=bssz[i],color='black')
      endfor
   endif

   j=0
   for i=0,n_elements(lmag)-1 do begin
      if lmag[i] le mag0 and lmag[i] ge mag1 then begin
         p2=plot([j*85]-700,[-height*scale/2-30],/overplot,linestyle='none', $
                 symbol='circle',/sym_filled,sym_size=lssz[i], $
                 color='black',clip=0)
         t=text([j*85]-698+10*lssz[i],[-height*scale/2-43], $
                  strn(round(lmag[i])),/data,clip=0)
         j++
      endif
   endfor

   p3=plot([0,0],[10,30],/overplot,color='magenta')
   p3=plot([10,30],[0,0],/overplot,color='magenta')
   p3=plot([0,0],[-10,-30],/overplot,color='magenta')
   p3=plot([-10,-30],[0,0],/overplot,color='magenta')

   ; RECON 1.0 FOV rectangle
   bx=[-0.5,0.5,0.5,-0.5,-0.5]
   by=[-0.5,-0.5,0.5,0.5,-0.5]
   p4=plot(bx*1020,by*765,/overplot,color='dark green')

   rastr,ra,1,ras
   decstr,dec,0,decs
   str=ras+' '+decs
   jd2year,jd,eqofdate
   ra_od=ra
   dec_od=dec
   precess,ra_od,dec_od,2000.0,eqofdate,/radian
   rastr,ra_od,1,ras
   decstr,dec_od,0,decs
   str2=ras+'  '+decs
   t=text(310,-height*scale/2-30,'J2000',/data,clip=0)
   t=text(420,-height*scale/2-30,str,/data,clip=0)
   t=text(310,-height*scale/2-60,'of date',/data,clip=0)
   t=text(420,-height*scale/2-60,str2,/data,clip=0)
   t=text(-width*scale/2,height*scale/2+20,title, $
           /data,clip=0,font_size=16,align=0.0)
   t=text(width*scale/2,height*scale/2+20,jds+' UT', $
           /data,clip=0,font_size=16,align=1.0)

   ; N/E compass
   ; origin of compass
   xarri = -width*scale/2+width*scale/8
   yarri = -height*scale/2+width*scale/8
   ; 'Arrow' length
   arrlen = width*scale/16
   ; Text position and end of arrows
   
   ; North arrow
   xarrnf = -arrlen * sin(rotrad) + xarri
   yarrnf =  arrlen * cos(rotrad) + yarri
   ; N text
   xarrnft = -arrlen*1.5 * sin(rotrad) + xarri
   yarrnft =  arrlen*1.5 * cos(rotrad) + yarri
   
   ; East arrow
   xarref = arrlen * cos(rotrad)
   yarref = arrlen * sin(rotrad)
   ; East text
   xarreft =  arrlen*1.5 * cos(rotrad)
   yarreft =  arrlen*1.5 * sin(rotrad)
   ; Flip
   if ewflip eq 1 then begin
      xarref  = -xarref 
      yarref  = -yarref 
      xarreft = -xarreft
      yarreft = -yarreft
   endif
   ; Origin offset
   xarref  = xarref + xarri 
   yarref  = yarref + yarri 
   xarreft = xarreft + xarri
   yarreft = yarreft + yarri
   
   tn=text(xarreft,yarreft,'E',font_size=20,align=0.5,clip=0,/data,color='red')
   te=text(xarrnft,yarrnft,'N',font_size=20,align=0.5,clip=0,/data,color='red')
   arrown=plot([xarri,xarrnf],[yarri,yarrnf],/overplot,color='red',thick=2)
   arrowe=plot([xarri,xarref],[yarri,yarref],/overplot,color='red',thick=2)

   
;bailout:
   ; save
   if outfile ne '<DISPLAY>' then begin
      p.save,outfile,width=xsize
   endif

end
