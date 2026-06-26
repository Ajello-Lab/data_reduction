;+
; NAME:
;  addstar
; PURPOSE:   (one line only)
;  Add a reference star to the data base for fitting
; DESCRIPTION:
;  This tool is really designed to be run on one visit at a time.  It
;    locates all the Gaia reference stars in the image and posts them
;    to the database for subsequent fitting.   What's saved at this step
;    is basically the starting point for fitting.  This program can be a
;    bit interactive and is pretty fast.  The fitting can be slow and is
;    left for the next step.
; CATEGORY:
;  Astrometry
; CALLING SEQUENCE:
;  hstaddstar,filestem,det
; INPUTS:
;  filestem - String that is used to select a set of file for processing.
;             The search string for files will be
;               filestem+'*.flc.fits.gz'
;  visitid - Two character string for the visit id
;  det     - Detector number (1 or 2)
;  pattstep - integer that picks out the image in the sequence for that visit
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;  BOXMRAD - box half-width to search for the local maximum,
;               default=value from config.ini
;  DW - half-width of fitting region centered on local max,
;               default=value from config.ini
;  POST - Flag, if set will cause results to be saved to the database
;           It is really important to test one file from a visit until
;           it looks good then the rest can be run safely without supervision
;           The default is to NOT save anything.
; OUTPUTS:
;  the hstast.stars database table is modified
; KEYWORD OUTPUT PARAMETERS:
; CONFIGURATION: (optional, overrides xtrack.in)
;   File: config.ini
;   [global]
;     boxmrad  - search half-width for peak of source, default=3
;     sdw      - Fitting region half-width for stars, default=40
;     subdir   - naem of sub-directory  added to base path to find data
;     filetype - type of file to read ('flt' or 'flc'), default='flc'
;     satur    - Saturation signal level, default=80000L
;     ndet     - Number of detectors, default=2
;     fov      - FOV of instrument in arcsec, default=140
;     addstars - mode for adding stars to fit list, 'manual' or 'auto'
;                  default='manual'
;   [ddir]   - section to list potential root directories to find data
;     XXX - <value>
;       XXX is a unique string (eg., D01)
;       <value> is a directory path
;       for additional details see getddir.pro
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  2014/07/08, Written by Marc W. Buie, Southwest Research Institute
;  2015/11/24, MWB, modified to put full precision catalog position
;                into database
;  2016/07/15, MWB, added PROPID keyword
;  2020/06/23, MWB, generalized version
;  2021/02/03, MWB, fixed bug in getting file type from config.ini
;-
pro hstaddstar,filestem,det,DW=dw,BOXMRAD=boxmrad,POST=post

compile_opt strictarrsubs

   pname='hstaddstar'
   self=pname+': '
   if badpar(filestem,7,0,caller=self+'(filestem) ') then return
   if badpar(det,[2,3],0,caller=self+'(det) ') then return

   if badpar(post,[0,1,2,3],0,caller=self+'(POST) ', $
                                  default=0) then return

   getddir,info,ddir

   getvalue,info,'global','boxmrad',boxmrad_default,type=2,default=3
   if badpar(boxmrad,[0,2,3],0,caller=self+'(BOXMRAD) ', $
                                  default=boxmrad_default) then return

   getvalue,info,'global','sdw',dw_default,type=2,default=40
   if badpar(dw,[0,2,3],0,caller=self+'(DW) ', $
                                  default=dw_default) then return

   getvalue,info,'global','subdir',subdir
   getvalue,info,'global','filetype',filetype,default='flc'
   getvalue,info,'global','satur',satur,default=80000L,type=3
   getvalue,info,'global','ndet',ndet,type=2,default=2
   getvalue,info,'global','fov',fov,type=2,default=140
   getvalue,info,'global','addstars',addmode,default='manual'

   if ndet eq 2 then begin
      if det eq 2 then exten=1 else exten=4
   endif else begin
      exten=1
   endelse

   patt=filestem+'*'+filetype+'.fits*'
   fnlist=file_search(ddir+patt,count=nfiles)
   if nfiles eq 0 then begin
      print,'data directory: ',ddir
      print,'No files found.  Looked for ',patt
      return
   endif
   fnlist=strmid(fnlist,strlen(ddir))

   openmysql,dblun,'hstast'
   c=','
   dirty=0
   nadded=0

   for i=0,nfiles-1 do begin
      print,'Process ',fnlist[i],' detector ',det
      words=strsplit(fnlist[i],'_',/extract)
      root=words[0]
      cmd=['select idx from header', $
           'where root='+quote(root), $
           ' and det='+strn(det)+';']

      mysqlquery,dblun,cmd,hidx,format='l',ngood=ncheck
      if ncheck eq 0 then begin
         print,'ERROR! image ',fnlist[i],' is not yet posted to database'
         print,cmd
         goto,bailout
      endif

      cmd=['select count(*) from stars,header', $
           'where stars.hidx=header.idx', $
           'and root='+quote(root)+';']
      mysqlquery,dblun,cmd,nposted,format='i'
      cmd=['select count(*) from stars,header', $
           'where stars.hidx=header.idx', $
           'and root='+quote(root), $
          ' and info like '+quote('auto%')+';']
      mysqlquery,dblun,cmd,nfitted,format='i'

      print,strn(nposted),' stars posted for ',root,' and ', $
            strn(nfitted),' fitted'

      if nposted eq 1 and nfitted eq 0 then begin
         print,'You must fit one star first before adding other stars'
         continue
      endif

      print,'Loading ',fnlist[i]
      rdwfc3,root,det,data,ddir=ddir,type=filetype,satur=satur

      if nfitted gt 0 then begin
         cmd='select dcrval0,dcrval1,nstars,ngood from wcs'+ $
             ' where root='+quote(root)+';'
         mysqlquery,dblun,cmd,dcrval0,dcrval1,wcs_stars,wcs_good, $
            format='d,d,i,i',ngood=ncheck
         if ncheck eq 0 then begin
            print,'No WCS update found for ',root
            print,'Maybe you forgot to run wcsupdate first?'
            goto,bailout
         endif
         print,'Loading WCS update',dcrval0,dcrval1
         data.astinfo.crval=data.astinfo.crval+[dcrval0,dcrval1]/3600.0d0

         cmd='select idx from header where root='+quote(root)+';'
         mysqlquery,dblun,cmd,hidx_dw,format='l',ngood=ncheck
         dwlist=replicate(-1,ncheck)
         for j=0,ncheck-1 do begin
            cmd='select max(dw) from stars'+ $
                ' where hidx='+strn(hidx_dw[j])+';'
            mysqlquery,dblun,cmd,thisdw,format='i',ngood=ncheck
            if ncheck eq 1 then dwlist[j]=thisdw
         endfor
         z=where(dwlist gt 0,count)
         if count ne 0 then begin
            dw=max(dwlist[z])
            print,'Setting dw to ',strn(dw),' based on prior entries'
         endif
      endif

      fncat=strmid(root,0,6)+'.cat'
      if not exists(fncat) then begin
         if ndet eq 1 then begin
            xtp = data.nx/2.0
            ytp = data.ny/2.0
         endif else begin
            xtp = data.nx/2.0
            if det eq 1 then ytp = -16.0 else ytp = data.ny+16.0
         endelse
         print,'Tangent point pixel at ',xtp,ytp
         xy2ad,xtp,ytp,data.astinfo,rtp,dtp
         rtp_r=rtp/180.0d0*!dpi
         dtp_r=dtp/180.0d0*!dpi
         rastr,rtp_r,1,ras
         decstr,dtp_r,0,decs
         print,'Tangent point ',ras,' ',decs
         print,'Generate sub-catalog ',fncat
         refnet,rtp_r,dtp_r,fov,fov,30.0,30.0,fncat,gaia=data.epoch
      endif
      if not exists(fncat) then begin
         print,'Star catalog generation failed'
         goto,bailout
      endif
      print,'Reading catalog ',fncat
      ;rdstarc,fncat,sra,sdec,bmag,rmag,nstars,info=starinfo,/noconvert
      rdstarc,fncat+'.gcat',info=starinfo,/noconvert
      sra = starinfo.ra + starinfo.rapm*(data.epoch-starinfo.epoch)
print,starinfo.ra[0],starinfo.raerr[0],starinfo.rapm[0],starinfo.rapmerr[0]
      sdec = starinfo.dec + starinfo.decpm*(data.epoch-starinfo.epoch)
      bmag = starinfo.bmag
      rmag = starinfo.rmag
      nstars=starinfo.nstars
      raerr = sqrt(starinfo.raerr^2 + $
                    (starinfo.rapmerr*(data.epoch-starinfo.epoch))^2) ; radians
      raerr = raerr*1000.0d0*3600.0d0*180.0d0/!dpi
      decerr = sqrt(starinfo.decerr^2 + $
                    (starinfo.decpmerr*(data.epoch-starinfo.epoch))^2) ; radians
      decerr = decerr*1000.0d0*3600.0d0*180.0d0/!dpi
      print,strn(nstars),' stars found in catalog'
      if nstars eq 0 then continue

      ; compute the ra,dec of the border of the chip
      npts=50
      lside = findgen(npts+1)/npts * data.ny
      sside = findgen(npts+1)/npts * data.nx
      strut = fltarr(npts+1)
      xpos=[strut,sside,   strut+data.nx,  reverse(sside)]
      ypos=[lside,strut+data.ny,reverse(lside),     strut]
      xy2ad,xpos,ypos,data.astinfo,rabord,decbord
      setwin,1,xsize=500,ysize=500
      plot,rabord,decbord,/iso
      oplot,rabord[[0,npts-1]],decbord[[0,npts-1]],psym=8
      oplot,[rabord[0]],[decbord[0]],psym=8,color=cpalette(1)
      oplot,[rabord[npts-1]],[decbord[npts-1]],psym=8,color=cpalette(2)
      print,'red dot is pixel 0,0 and cyan dot is pixel 0,',strn(data.ny-1)

      rabord=rabord*!dpi/180.0d0
      decbord=decbord*!dpi/180.0d0

      ; filter down the star list to match what could be on the chip
      z=where(sra ge min(rabord) and sra le max(rabord) and $
              sdec ge min(decbord) and sdec le max(decbord),count)
      if count eq 0 then begin
         print,'No valid stars found.  Aborting.'
         goto,bailout
      endif
      nstars=count
      print,strn(nstars),' stars found near field.'

      sra=sra[z]
      sdec=sdec[z]
      raerr=raerr[z]
      decerr=decerr[z]
      bmag=bmag[z]
      rmag=rmag[z]
      oplot,sra*!radeg,sdec*!radeg,psym=8,color=cpalette(3)

      pad=50
      sra_d  = sra*180.0d0/!dpi
      sdec_d = sdec*180.0d0/!dpi
      ad2xy,sra_d,sdec_d,data.astinfo,sx,sy
      z=where(sx gt pad and sx lt data.nx-pad and $
              sy gt pad and sy lt data.ny-pad,nstars)
      sra=sra[z]
      sdec=sdec[z]
      raerr=raerr[z]
      decerr=decerr[z]
      bmag=bmag[z]
      rmag=rmag[z]
      sx=sx[z]
      sy=sy[z]
      print,strn(nstars),' stars found on field.'
      if nstars eq 0 then goto,bailout

      showsrc,data.image,hisig=50,window=0
      oplot,sx,sy,psym=4,color='0000ff'xl,symsize=4

      if nfitted gt 0 then begin
         cmd='select idx,x,y from stars where hidx='+strn(hidx)+';'
         mysqlquery,dblun,cmd,sidx,xfit,yfit,format='l,d,d',ngood=nshow
      endif else nshow=0

      color4='0000ff'xl
      mkcircle,0.,0.,1.,xcirc,ycirc
      sky1=long(dw)
      sky2=350L

      for i=0,nshow-1 do begin
         oplot,[-1,1,1,-1,-1]*dw+xfit[i],[-1,-1,1,1,-1]*dw+yfit[i], $
            color='00ffff'xl
         xyouts,xfit[i]+dw+2,yfit[i]+dw+2,strn(sidx[i]), $
            align=0.,color='00ffff'xl,/data,charsize=1.5
      endfor

      if addmode eq 'manual' then begin
         if nposted eq 0 then begin

            print,''
            print,'Select the first star to fit.'
            print,'     Click left on symbol for first anchor star (right=quit).'
            cursor,xs1,ys1,3
            if !mouse.button eq 4 then goto,bailout
            print,'Click at ',xs1,ys1
            dist = sqrt((xs1-sx)^2+(ys1-sy)^2)
            zs1 = where(dist eq min(dist))
            zs1 = zs1[0]
            oplot,[sx[zs1]],[sy[zs1]],psym=4,color=color4,symsize=2
            print,'     Click left on the star in the image (right=quit).'
            cursor,xs1,ys1,3
            if !mouse.button eq 4 then goto,bailout
            print,'Click at ',xs1,ys1
            boxm,data.image,xs1,ys1,boxmrad,boxmrad,xmax,ymax
            print,'Max at   ',xmax,ymax,' is ',data.image[xmax,ymax]
            oplot,[xmax],[ymax],psym=4,color=color4,symsize=2
            getannul,data.image,xmax,ymax,sky1,sky2,skysmpl
            robomean,skysmpl,3.0,0.5,sky,stdmean=skysig
            oplot,xcirc*sky1+xmax,ycirc*sky1+ymax,color='5032c8'xl
            oplot,xcirc*sky2+xmax,ycirc*sky2+ymax,color='5032c8'xl

            oplot,[-1,1,1,-1,-1]*dw+xmax,[-1,-1,1,1,-1]*dw+ymax,color='00ff70'xl

         endif else begin
            print,''
            print,'Adding extra stars, look for catalog stars not already used'
            print,'     Click left on the star in the image (right=quit).'
            cursor,xs1,ys1,3
            if !mouse.button eq 4 then goto,bailout
            print,'Click at ',xs1,ys1
            boxm,data.image,xs1,ys1,boxmrad,boxmrad,xmax,ymax
            print,'Max at   ',xmax,ymax,' is ',data.image[xmax,ymax]
            oplot,[xmax],[ymax],psym=4,color=color4,symsize=2
            getannul,data.image,xmax,ymax,sky1,sky2,skysmpl
            robomean,skysmpl,3.0,0.5,sky,stdmean=skysig
            oplot,xcirc*sky1+xmax,ycirc*sky1+ymax,color='5032c8'xl
            oplot,xcirc*sky2+xmax,ycirc*sky2+ymax,color='5032c8'xl

            oplot,[-1,1,1,-1,-1]*dw+xmax,[-1,-1,1,1,-1]*dw+ymax,color='00ff70'xl

            dist = sqrt((xs1-sx)^2+(ys1-sy)^2)
            zs1 = where(dist eq min(dist))
            oplot,[sx[zs1]],[sy[zs1]],psym=4,color=color4,symsize=2
            
         endelse

         cmd=['insert into stars values (', $
              'NULL,', $
              strn(hidx)+c, $
              strn(xs1)+c, $
              strn(ys1)+c, $
              strn(data.image[xmax,ymax])+c, $
              strn(sky)+c, $
              strn(skysig)+c, $
              strn(sra[zs1],format='(f14.10)')+c, $
              strn(sdec[zs1],format='(f14.10)')+c, $
              strn(raerr[zs1],format='(f14.10)')+c, $
              strn(decerr[zs1],format='(f14.10)')+c, $
              strn(bmag[zs1])+c, $
              strn(rmag[zs1])+c, $
              '-1,-1,', $
              strn(dw)+c, $
              strn(xmax)+',-1'+c, $
              strn(ymax)+',-1'+c, $
              strn(data.image[xmax,ymax]/0.18)+',-1'+c, $
              quote('g')+c, $
              quote('refit')+c, $
              quote('DR2')+c, $
              '-1,NULL', $
              ');']
         print,cmd
         if post then begin
            mysqlcmd,dblun,cmd
            dirty=1
            nadded++
         endif else begin
            print,'nothing saved'
            break
         endelse
      endif else begin
         print,'addmode unrecognized: addstars=',addmode
      endelse

   endfor

   if dirty then begin
      context=subdir
      action='Added '+strn(nadded)+' stars'
      jdcur=systime(/julian,/ut)
      jdstr,jdcur,300,jds
      cmd=['insert into history set', $
           'posted='+jds+c, $
           'context='+quote(context)+c, $
           'tool='+quote(pname)+c, $
           'action='+quote(action)+';']
      print,cmd
      mysqlcmd,dblun,cmd
   endif

bailout:

   free_lun,dblun

end
