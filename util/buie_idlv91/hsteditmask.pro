;+
; NAME:
;  hsteditmask
; PURPOSE:   (one line only)
;  Edit the bad pixel mask for a WFC3 image
; DESCRIPTION:
; CATEGORY:
;  Astrometry
; CALLING SEQUENCE:
;  hsteditmask,idx,hidx
; INPUTS:
;  idx   - index number of star or target to be fitted
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;  STAR - Flag, if set then the idx input applies to a star, otherwise
;           idx applies is for a target (pos table)
;  RESETPOS - Flag, if set will reset xmax,ymax,imax based on the fitted
;               x,y position.  The values for xmax,ymax come from rounding
;               the fitted position and the value for imax is then taken
;               from that image pixel.  This flag will be honored only if
;               info equals 'auto' or 'autopdf' at the start.  If this
;               is modified then info will be reset to 'refit' even if you
;               do not edit any pixels.
;  NEWDW - Use this to change the DW value for this object in the database.
;            Default = no change.  If the values does change, the info
;            flag is set to 'refit' when done even if no other changes are
;            made by this tool.
; OUTPUTS:
;   the mask file is modified and the status of the target or star is changed
;     in the database
; KEYWORD OUTPUT PARAMETERS:
; CONFIGURATION: (optional, overrides xtrack.in)
;   File: config.ini
;   [global]
;     subdir   - naem of sub-directory  added to base path to find data
;     satur    - Saturation signal level, default=80000L
;     filetype - type of file to read ('flt' or 'flc'), default='flc'
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
;  2020/06/29, Written by Marc W. Buie, Southwest Research Institute, cloned
;                 from the 15405 reduction directory.
;  2020/07/15, MWB, added RESETPOS and NEWDW keywords
;-
pro hsteditmask,idx,STAR=star,RESETPOS=resetpos,NEWDW=newdw

   pname='hsteditmask'
   self=pname+': '
   if badpar(idx,[2,3],0,caller=self+'(idx) ') then return
   if badpar(star,[0,1,2,3],0,caller=self+'(STAR) ',default=0) then return
   if badpar(resetpos,[0,1,2,3],0,caller=self+'(RESETPOS) ', $
                                  default=0) then return
   if badpar(newdw,[0,1,2,3],0,caller=self+'(NEWDW) ', $
                                  default=-1) then return

   getddir,cinfo,ddir
   getvalue,cinfo,'global','subdir',subdir
   getvalue,cinfo,'global','satur',satur,type=4
   getvalue,cinfo,'global','filetype',value,default='flc'

   openmysql,dblun,'hstast'
   c=','

   ;get info on the fitting region to edit
   if star then begin
      cmd=['select hidx from stars where idx='+strn(idx)+';']
   endif else begin
      cmd=['select hidx from pos where idx='+strn(idx)+';']
   endelse
   mysqlquery,dblun,cmd,hidx,format='i',ngood=ncheck

   if ncheck eq 0 then begin
      print,cmd
      print,'No object information found. Aborting.'
      error=1
      goto,bailout
   endif

   cmd=['select root,det from header', $
        'where idx='+strn(hidx)+';']
   mysqlquery,dblun,cmd,root,det,format='a,i',ngood=ncheck
   if ncheck eq 0 then begin
      print,cmd
      print,'No image information found. Aborting.'
      error=1
      goto,bailout
   endif

   rdwfc3,root,det,data,ddir=ddir,type=type,satur=satur
   if data eq !null then goto,bailout

   if resetpos then begin
      if star then begin
         cmd=['select xmax,ymax,x,y,info from stars where idx='+strn(idx)+';']
         mysqlquery,dblun,cmd,xmax,ymax,x,y,info,format='i,i,f,f,a',ngood=nhit
         newxmax=round(x)
         newymax=round(y)
         if newxmax ne xmax or newymax ne ymax then begin
            cmd=['update stars set', $
                 'xmax='+strn(newxmax)+c, $
                 'ymax='+strn(newymax)+c, $
                 'imax='+strn(data.image[newxmax,newymax])+c, $
                 'info='+quote('refit'), $
                 'where idx='+strn(idx)+';']
            mysqlcmd,dblun,cmd
         endif
      endif else begin
         cmd=['select xnav,ynav,x,y,info from pos where idx='+strn(idx)+';']
         mysqlquery,dblun,cmd,xmax,ymax,x,y,info,format='f,f,f,f,a',ngood=nhit
         newxmax=round(x)
         newymax=round(y)
         if newxmax ne round(xmax) or newymax ne round(ymax) then begin
            cmd=['update pos set', $
                 'xnav='+strn(newxmax)+c, $
                 'ynav='+strn(newymax)+c, $
                 'imax='+strn(data.image[newxmax,newymax])+c, $
                 'info='+quote('refit'), $
                 'where idx='+strn(idx)+';']
            mysqlcmd,dblun,cmd
         endif
      endelse
   endif

   if newdw gt 0 then begin
      if star then begin
         cmd=['select dw from stars where idx='+strn(idx)+';']
         mysqlquery,dblun,cmd,dw,format='i',ngood=nhit
         if newdw ne dw then begin
            cmd=['update stars set', $
                 'dw='+strn(newdw)+c, $
                 'info='+quote('refit'), $
                 'where idx='+strn(idx)+';']
            mysqlcmd,dblun,cmd
         endif
      endif else begin
         cmd=['select dw from pos where idx='+strn(idx)+';']
         mysqlquery,dblun,cmd,dw,format='i',ngood=nhit
         if newdw ne dw then begin
            cmd=['update pos set', $
                 'dw='+strn(newdw)+c, $
                 'info='+quote('refit'), $
                 'where idx='+strn(idx)+';']
            mysqlcmd,dblun,cmd
         endif
      endelse
   endif

   ;get info on the fitting region to edit
   if star then begin
      cmd=['select hidx,xmax,ymax,dw from stars where idx='+strn(idx)+';']
      mysqlquery,dblun,cmd,hidx,xnav,ynav,dw,format='l,i,i,i',ngood=nhit
   endif else begin
      cmd=['select hidx,xnav,ynav,dw from pos where idx='+strn(idx)+';']
      mysqlquery,dblun,cmd,hidx,xnav,ynav,dw,format='l,f,f,i',ngood=nhit
   endelse

   xnav=round(xnav)
   ynav=round(ynav)

   ; get the info for the image we need to work on
   cmd=['select filter,centera1,centera2,xsize,ysize', $
        'from header where idx='+strn(hidx)+';']
   mysqlquery,dblun,cmd,filter,centera1,centera2,xsize,ysize, $
      format='a,i,i,i,i',ngood=nhit
   if nhit eq 0 then begin
      print,cmd
      print,'No image information found. Aborting.'
      error=1
      goto,bailout
   endif

   ; set the region for the fit in original pixel coordinates
   msz=2*dw+1
   i1 = xnav-dw
   i2 = xnav+dw
   j1 = ynav-dw
   j2 = ynav+dw

   subarr,data.image,i1,i2,j1,j2,fitimage,error
   subarr,data.mask,i1,i2,j1,j2,mask,error

   names=['Saturated','CRS','star pixels','extra sat', $
          'unused','unused','unused','unused']
   style=[0,1,1,0,0,0,0,0]
   bit=1
   bitmask=ishft(1B,bit)
   newmask=mask
   editmask,fitimage,newmask,bitnum=bit,style=style,bitnames=names
   zd=where(newmask ne mask,countd)
   if countd ne 0 then begin
      print,'Update mask file ',data.fnmsk
      data.mask[i1:i2,j1:j2]=newmask
      if not exists(data.fnmsk) then begin
         print,'Create new mask file'
         mkhdr,pduhdr,1,0,/extend
         sxaddpar,pduhdr,'NAXIS',0
         sxdelpar,pduhdr,'NAXIS1'
         sxaddpar,pduhdr,'NEXTEND',2,after='EXTEND'
         writefits,data.fnmsk,0,pduhdr
         for ie=1,2 do begin
            if ie eq det then begin
               mkhdr,mhdr,1,[data.nx,data.ny],/image
               writefits,data.fnmsk,data.mask,mhdr,/append
            endif else begin
               if ie eq 1 then texten=4 else texten=1
               fits_read,data.fnim,tmpimage,thdr,exten=texten
               tfullmask=bytarr(data.nx,data.ny)
               zs=where(tmpimage ge satur,satcount)
               tfullmask[zs]=1B
               tmpimage=0
               mkhdr,mhdr,1,[data.nx,data.ny],/image
               writefits,data.fnmsk,tfullmask,mhdr,/append
               tfullmask=0
            endelse
         endfor
      endif else begin
         print,'Update existing mask'
         modfits,data.fnmsk,data.mask,data.maskhdr,exten=det
      endelse

      if star then begin
         cmd='update stars set info='+quote('refit')+' where idx='+strn(idx)+';'
      endif else begin
         cmd='update pos set info='+quote('refit')+' where idx='+strn(idx)+';'
      endelse
      mysqlcmd,dblun,cmd
   endif

bailout:
   free_lun,dblun

end
