;+
; NAME:
;  hstmeasure
; PURPOSE:   (one line only)
;  Measure a science target position in an HST WFC3 image
; DESCRIPTION:
;  This tool uses extracted sub-arrays to permit measuring a source
;    location.  It is designed for moving targets and the position is
;    saved for later use either as is, or, as a starting location for
;    some other fitting program.
; CATEGORY:
;  Astrometry
; CALLING SEQUENCE:
;  hstmeasure,fn
; INPUTS:
;  fn - file name for the extracted sub-array to be measured
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
; OUTPUTS:
; KEYWORD OUTPUT PARAMETERS:
; CONFIGURATION: (optional, overrides xtrack.in)
;   File: config.ini
;   [global]
;     subdir   - name of sub-directory  added to base path to find data
;     tdw      - Fitting region half-width for stars, default=15
;   [ddir]   - section to list potential root directories to find data
;     XXX - <value>
;       XXX is a unique string (eg., D01)
;       <value> is a directory path
;       for additional details see getddir.pro
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
;   Uses the hstast mysql/mariadb database table
; PROCEDURE:
; MODIFICATION HISTORY:
;  Written by Marc W. Buie, Southwest Research Institute, 2020/10/05
;-
pro hstmeasure,fnim,objname,objid,SHOW=show,PEAK=peak

   pname='hstmeasure'
   self=pname+': '
   if badpar(fnim,7,0,caller=self+'(fnim) ') then return
   if badpar(objname,7,0,caller=self+'(objname) ') then return
   if badpar(objid,7,0,caller=self+'(objid) ') then return
   if badpar(show,[0,1,2,3],0,caller=self+'(SHOW) ',default=0) then return
   if badpar(peak,[0,1,2,3],0,caller=self+'(PEAK) ',default=0) then return

   if not exists(fnim) then begin
      print,fnim,' not found, aborting.'
      return
   endif

   getddir,info,ddir
   getvalue,info,'global','subdir',subdir
   getvalue,info,'global','tdw',dw

   if not exists(fnim) then begin
      print,'file ',fnim,' not found, quitting.'
      return
   endif
   fits_read,fnim,image,hdr
   image=float(image)
   x0=sxpar(hdr,'SUBARRI0')
   y0=sxpar(hdr,'SUBARRJ0')
   exptime=sxpar(hdr,'EXPTIME')

   if not exists('plots') then file_mkdir,'plots'

   words=strsplit(fnim,'_',/extract)
   root=words[0]
   det=fix(words[1])

   sz=size(image,/dimen)

   zf=10
   wd=2*dw+1
   i0=sz[0]/2-dw
   i1=sz[0]/2+dw
   j0=sz[1]/2-dw
   j1=sz[1]/2+dw

   sub=image[i0:i1,j0:j1]
   skysclim,image,loval,hival,meanval,sigma,npts=10000
   if peak then begin
      hival = max(sub)
   endif else begin
      hival=meanval+sigma*20.0
   endelse
   bim=rebin(image[i0:i1,j0:j1],wd*zf,wd*zf,/sample)
   bim=bytscl(bim,min=loval,max=hival,top=255)

   mkcircle,0.,0.,1.,xcirc,ycirc

   c=','
   openmysql,dblun,'hstast'

   if not show then begin

      showsrc,image,window=0,hisig=8,lowval=loval,hival=hival
      setwin,1,xsize=wd*zf,ysize=wd*zf
      tv,bim
      plot,[0],[1],/nodata,xmargin=[0,0],ymargin=[0,0], $
         xr=[i0-0.5,i1+0.5],yr=[j0-0.5,j1+0.5],xstyle=5,ystyle=5,/noerase

      dirty=0
      !mouse.button=0
      print,'left=auto, middle=manual, right=exit'
      while !mouse.button ne 4 do begin
         cursor,x,y,2,/data,/down
         if !mouse.button eq 1 then begin
print,'left',x,y,image[round(x),round(y)]
            basphote,1.0,image,exptime,x,y,2.0,10,100,/nolog,rdnoise=3.0, $
               xcen=xcen,ycen=ycen,zpoint=2.6852-0.8222,boxmrad=2, $
               max=maxval,flux=flux
            dirty=1
         endif else if !mouse.button eq 2 then begin
print,'midd',x,y,image[round(x),round(y)]
            basphote,1.0,image,exptime,x,y,2.0,10,100,/nolog,rdnoise=3.0, $
               zpoint=2.6852-0.8222,boxmrad=2,/exact,max=maxval,flux=flux
            xcen=x
            ycen=y
            dirty=1
         endif
         if dirty then begin
            tv,bim
            plots,xcirc*2+xcen,ycirc*2+ycen,/data,color='0000ff'xl
         endif
      endwhile

      if dirty then begin
         cmd=['select pos.idx,header.idx from pos,header', $
              'where pos.hidx=header.idx', $
              'and root='+quote(root), $
              'and det='+strn(det), $
              'and objname='+quote(objname)+';']
print,cmd
         mysqlquery,dblun,cmd,idx,hidx,format='l,l',ngood=nhit

         if nhit eq 0 then begin
            cmd=['select idx from header', $
                 'where root='+quote(root), $
                 'and det='+strn(det)+';']
            mysqlquery,dblun,cmd,hidx,format='l',ngood=nhit
print,cmd
            if nhit eq 0 then begin
               print,cmd
               print,root,' ',strn(det),' not found in database'
               goto,bailout
            endif
            cmd=['insert into pos set', $
                 'idx=NULL,', $
                 'hidx='+strn(hidx)+c, $
                 'objname='+quote(objname)+c, $
                 'objid='+quote(objid)+c, $
                 'info='+quote('initial'), $
                 ';']
print,cmd
            mysqlcmd,dblun,cmd
            cmd=['select idx from pos', $
                 'where hidx='+strn(hidx), $
                 'and objname='+quote(objname)+';']
print,cmd
            mysqlquery,dblun,cmd,idx,format='l',ngood=nhit
            if nhit eq 0 then begin
               print,'unexpected error, this should not happen'
               goto,bailout
            endif
print,cmd
         endif

         cmd=['update pos set', $
              'xnav='+strn(xcen+x0)+c, $
              'ynav='+strn(ycen+y0)+c, $
              'maxval='+strn(maxval)+c, $
              'back='+strn(meanval)+c, $
              'backsig='+strn(sigma)+c, $
              'dw='+strn(dw)+c, $
              'x='+strn(xcen+x0)+c, $
              'xerr=-1'+c, $
              'y='+strn(ycen+y0)+c, $
              'yerr=-1'+c, $
              'flux='+strn(flux*exptime)+c, $
              'fluxerr=-1'+c, $
              'info='+quote('refit')+c, $
              'chisq=10000.0', $
              'where idx='+strn(idx)+';']
print,cmd
         mysqlcmd,dblun,cmd

         context=subdir
         action=objname+' position for '+root+'_'+strn(det)
         jdcur=systime(/julian,/ut)
         jdstr,jdcur,300,jds
         cmd=['insert into history set', $
              'posted='+jds+c, $
              'context='+quote(context)+c, $
              'tool='+quote(pname)+c, $
              'action='+quote(action)+';']
         print,cmd
         mysqlcmd,dblun,cmd

;         tag=strmid(fnim,0,9)
;         line=string(det,xcen+x0,ycen+y0,format='(1x,i1,2(1x,f8.3))')
;         repwrite,fnnav,tag,tag+line
      endif

   endif

   cmd=['select pos.idx,header.idx from pos,header', $
        'where pos.hidx=header.idx', $
        'and root='+quote(root), $
        'and det='+strn(det), $
        'and objname='+quote(objname)+';']
   mysqlquery,dblun,cmd,idx,hidx,format='l,l',ngood=nhit
   if nhit eq 0 then begin
      print,cmd
      print,'object not found in database, quitting.'
      goto,bailout
   endif

   cmd='select xnav,ynav,x,y from pos where idx='+strn(idx)+';'
   mysqlquery,dblun,cmd,xnav,ynav,xpos,ypos,format='f,f,f,f'

   showsrc,image,window=0,hisig=8,lowval=loval,hival=hival
   plot,[0],[1],/nodata,xmargin=[0,0],ymargin=[0,0], $
      xr=[i0-0.5,i1+0.5],yr=[j0-0.5,j1+0.5],xstyle=5,ystyle=5,/noerase

   setwin,1,xsize=wd*zf,ysize=wd*zf
   tv,bim

   help,image
   print,'subarray corner: ',x0,y0
   print,'region: ',i0,i1,j0,j1
   fmt1='(a,2(1x,f8.3))'

   print,'nav: ',xnav,ynav,format=fmt1
   oplot,xcirc*2+(xnav-x0),ycirc*2+(ynav-y0),color=cpalette(1),thick=2
   astmark,xnav-x0,ynav-y0,/data,gap=3,len=5,color='0000ff'xl
   xyouts,0.20,0.1,'!5nav!3',color=cpalette(1),/normal,size=3

   print,'pos: ',xpos,ypos,format=fmt1
   oplot,xcirc*2+(xpos-x0),ycirc*2+(ypos-y0),color='00ff00'xl,thick=2
   astmark,xpos-x0,ypos-y0,/data,gap=3,len=5,color='00ff00'xl,rotang=45
   xyouts,0.5,0.1,'!5pos!3',color='00ff00'xl,/normal,size=3

   fnpng='plots/'+'T'+strn(idx)+'_stamp.png'
   tvgrab,fnpng,1,/png

bailout:
   free_lun,dblun

end
