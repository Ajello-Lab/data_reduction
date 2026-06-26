;+
; NAME:
;  occshowgrid
; PURPOSE:   (one line only)
;  Show a grid of postage stamp images from an occultation dataset
; DESCRIPTION:
; CATEGORY:
;  Occultations
; CALLING SEQUENCE:
;  occshowgrid,team,nx,ny,dw
; INPUTS:
;  team - String with the name of the team/dataset to read
;  nx   - width of image grid
;  ny   - height of image grid
;  dw   - half-width of the thumbnail to extract
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;  HISIG - sigma level for upper stretch threshold (low is -3 sigma),
;             (default = 5)
;  ACTUAL - Flag, if set, use the D&R times (from chords.dat) to establish
;             the reference time, otherwise just used the prediction
;             (from crosstrack.dat).
;  WINDOW - direct graphics window to use for display (default=0)
;  SAVEPNG - Flag, if set, causes the display to be saved to a .png file.
;              The file name will be [team]_grid.png.
;  POS     - Optional two element vector with the position of the target [x,y]
;            If supplied will bypass trying to get this from the database.
;  LCFILE  - To bypass using the database for the PSF reductions, supply
;              a lightcurve file as written by occproc.pro.  It's up to
;              you to provide a file that is consistent with the team
;              provided.  This will use the x,y position in the file unless
;              you also provide the POS keyword.
;  ZF      - zoom factor for thumbnails, default=2
; OUTPUTS:
; KEYWORD OUTPUT PARAMETERS:
; CONFIGURATION:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  Written by Marc W. Buie, Southwest Research Institute, 2020/09/23
;  2022/01/25, MWB, Added code to handle SAVEPNG keyword optional input
;                    argument.  Added POS and LCFILE keywords.
;-
pro occshowgrid,team,nx,ny,dw,HISIG=hisig,ACTUAL=actual,WINDOW=windnum, $
        SAVEPNG=savepng,ZF=zf,BINFAC=binfac,POS=pos,LCFILE=lcfile

   self='occshowgrid: '
   if badpar(team,7,0,caller=self+'(team) ') then return
   if badpar(nx,[2,3],0,caller=self+'(nx) ') then return
   if badpar(ny,[2,3],0,caller=self+'(ny) ') then return
   if badpar(dw,[2,3],0,caller=self+'(dw) ') then return
   if badpar(hisig,[0,2,3,4,5],0,caller=self+'(HISIG) ',default=5) then return
   if badpar(windnum,[0,1,2,3],0,caller=self+'(WINDOW) ',default=0) then return
   if badpar(actual,[0,1,2,3],0,caller=self+'(ACTUAL) ',default=0) then return
   if badpar(savepng,[0,1,2,3],0,caller=self+'(SAVEPNG) ',default=0) then return
   if badpar(zf,[0,1,2,3],0,caller=self+'(ZF) ',default=2) then return
   if badpar(binfac,[0,1,2,3],0,caller=self+'(BINFAC) ',default=1) then return
   if badpar(pos,[0,2,3],1,caller=self+'(POS) ',default=[-1,-1]) then return
   if badpar(lcfile,[0,7],0,caller=self+'(LCFILE) ',default='') then return

   getddir,cinfo,root
   getvalue,cinfo,'global','event',event
   getvalue,cinfo,'global','date',date

   if actual then begin
      if nofile('chords.dat','Site location and times') then return
      readcol,'chords.dat',cteam,middate,dtime,rtime,format='a,x,x,x,x,a,a,a'
      zm=trimrank(where(team eq cteam,countm))
      if countm eq 0 then begin
         print,'Team ',team,' not found in chords.dat'
         return
      endif
      if dtime[zm] eq 'x' or rtime[zm] eq 'x' then begin
         print,'Team ',team,' does not have a chord, cannot use /actual'
         return
      endif
      jd1=jdparse(middate[zm]+' '+dtime[zm])
      jd2=jdparse(middate[zm]+' '+rtime[zm])
      jdref = (jd1+jd2)/2.0d0
      print,'reference time from chords.dat',jdref
   endif else begin
      if nofile('crosstrack.dat','Ephemeris support data') then return
      readcol,'crosstrack.dat',fteam,xtrack,middate,midtime,format='a,f,a,a'
      zm=trimrank(where(team eq fteam,countm))
      if countm eq 0 then begin
         print,'Team ',team,' not found in crosstrack.dat'
         return
      endif
      jdref=jdparse(middate[zm]+' '+midtime[zm])
      jd1=jdref
      jd2=jdref
      print,'reference time from crosstrack.dat',jdref
   endelse

print,jdref

   if lcfile eq '' then begin
      openmysql,dblun,'occlc'

      cmd=['select idx,jd,filename,stemdir,sky,skysig', $
           'from info', $
           'where event='+quote(event), $
           'and binfac='+strn(binfac), $
           'and team='+quote(team), $
           'order by jd;']
      mysqlquery,dblun,cmd,idx,jd,filename,stemdir,sky,skysig, $
         format='l,d,a,a,f,f',ngood=nfiles
      if nfiles eq 0 then begin
         print,cmd
         print,'No files found in database'
         free_lun,dblun
         return
      endif
   endif else begin
      if nofile(lcfile,'Lightcurve file.') then return
      readcol,lcfile,idx,jd,flux,counts,xmeas,ymeas,fwhm, $
         format='l,d,f,f,f,f,f',count=nfiles
      if nfiles eq 0 then begin
         print,lcfile
         print,'No valid data found in file'
         return
      endif
      binfac=1
      getvalue,cinfo,team,'stemdir',stemdir
      getvalue,cinfo,team,'dirtime',dirtime
      fdir=root+stemdir+'/'+dirtime+'/'
      filename=file_search(fdir+'*.fits',count=nfiles2)
      print,fdir
      filename=strmid(filename,strlen(fdir))
      if nfiles ne nfiles2 then begin
         print,strn(nfiles2),' found, should be ',strn(nfiles)
         return
      endif
   endelse

   if binfac eq 1 then bindir='' else bindir='binned_'+strn(binfac)+'/'

   color=replicate(1,nfiles)
   if jd1 eq jd2 then begin
      z=where(abs(jd-jdref) eq min(abs(jd-jdref)))
      z=z[0]
      color[z]=2
      print,'crosstrack.dat reference',z
   endif else begin
      z=where(jd ge jd1 and jd le jd2,count)
      if count ne 0 then begin
         color[z]=2
      endif
      print,'chords.dat reference',z
   endelse

   zref=trimrank(where( abs(jd-jdref) eq min(abs(jd-jdref)) ))
print,zref,idx[zref],' ',filename[zref]

   nsubs=nx*ny
   zsub=lindgen(nsubs)-nsubs/2+zref
print,zsub

   if zref eq 0 or zref eq nfiles-1 then begin
      print,'WARNING: reference time puts you at the start or stop of data'
   endif

   fw= 2L*dw+1
   xsize = nx*fw*zf
   ysize = ny*fw*zf

   setwin,windnum,xsize=xsize,ysize=ysize
   erase
   plot,[0],[1],/nodata,xmargin=[0,0],ymargin=[0,0], $
      xr=[0,xsize],yr=[0,ysize],xstyle=5,ystyle=5,/noerase

   cell=0
   for j=0,ny-1 do begin
      for i=0,nx-1 do begin
         if zsub[cell] ge 0 and zsub[cell] lt nfiles then begin
            if lcfile eq '' then begin
               fn=addslash(root)+stemdir[cell]+bindir+filename[zsub[cell]]
               cmd=['select idx,x,y from target', $
                    'where imidx='+strn(idx[zsub[cell]])+';']
               mysqlquery,dblun,cmd,targidx,x,y,format='l,f,f'
            endif else begin
               x=xmeas[zsub[cell]]
               y=ymeas[zsub[cell]]
               targidx=idx[zsub[cell]]
               fn=fdir+filename[zsub[cell]]
               print,filename[zsub[cell]],x,y,counts[zsub[cell]]
            endelse
            if pos[0] gt 0 then begin
               x=pos[0]
               y=pos[1]
            endif
            print,cell,targidx,idx[zsub[cell]],' ',bindir,filename[zsub[cell]]
            x=round(x)
            y=round(y)
            im=readfits(fn)
            im=float(im)
            backsub,im,/row

            subarr,im,x-dw,x+dw,y-dw,y+dw,sub
            if lcfile eq '' then begin
               lowval = -3.0*skysig[zsub[cell]]
               hival  = hisig*skysig[zsub[cell]]
            endif else begin
               skysclim,im,lowval,hival,sky,skysig,npts=20000
               lowval = -3.0*skysig
               hival  = hisig*skysig
            endelse
            bsub=bytscl(sub,min=lowval,max=hival,top=255)
            bsub=rebin(bsub,fw*zf,fw*zf,/sample)
            tv,bsub,cell
            xr=[0,fw*zf-1]
            yr=[0,fw*zf-1]
            oplot,xr+i*fw*zf,[1,1]*(ny-j-1)*fw*zf, $
               color=cpalette(color[zsub[cell]])
            oplot,xr+i*fw*zf,[1,1]*(ny-j)*fw*zf-1, $
               color=cpalette(color[zsub[cell]])

            oplot,[1,1]*i*fw*zf,yr+(ny-j-1)*fw*zf, $
               color=cpalette(color[zsub[cell]])
            oplot,[1,1]*(i+1)*fw*zf-1,yr+(ny-j-1)*fw*zf, $
               color=cpalette(color[zsub[cell]])
;print,xr+i*fw*zf,[1,1]*(ny-j)*fw*zf-1,xsize,ysize
         endif
         cell++
      endfor
;break
   endfor

   if lcfile eq '' then free_lun,dblun

   if savepng then begin
      fnpng=team+'_grid.png'
      tvgrab,fnpng,windnum,/png
   endif

end
