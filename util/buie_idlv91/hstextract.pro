;+
; NAME:
;  hstextract
; PURPOSE:   (one line only)
;  Define target position and extract sub-frame for processing
; DESCRIPTION:
; CATEGORY:
;  Astrometry
; CALLING SEQUENCE:
;  hstextract,filestem
; INPUTS:
;  filestem - String that is used to select a set of file for processing.
;             The search string for files will be
;               filestem+'*.flc.fits.gz'
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;  SKIPWCS - Flag, if set skips the attempt to apply a WCS update and will
;               not require it to proceed but absolute astrometry will be
;               bad in this case.
; OUTPUTS:
; KEYWORD OUTPUT PARAMETERS:
; CONFIGURATION: (optional, overrides xtrack.in)
;   File: config.ini
;   [global]
;     subdir   - naem of sub-directory  added to base path to find data
;     objectid - object code (see ephem.pro)
;     filetype - type of file to read ('flt' or 'flc'), default='flc'
;     satur    - Saturation signal level, default=80000L
;     tdw      - Fitting region half-width for stars, default=15
; objectid
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
;  2020/06/26, Written by Marc W. Buie, Southwest Research Institute
;  2020/09/24, MWB, added SKIPWCS keyword
;-
pro hstextract,filestem,OFFSET=offset,SKIPWCS=SKIPWCS

   pname='hstextract'
   self=pname+': '
   if badpar(filestem,7,0,caller=self+'(filestem) ') then return
   if badpar(offset,[0,2,3,4,5],1,caller=self+'(OFFSET) ', $
                                default=[0,0]) then return
   if badpar(skipwcs,[0,1,2,3],0,caller=self+'(SKIPWCS) ', $
                                default=0) then return

   getddir,info,ddir
   getvalue,info,'global','subdir',subdir
   getvalue,info,'global','objectid',objectid
   getvalue,info,'global','filetype',type,default='flc'
   getvalue,info,'global','satur',satur,type=4,default=80000
   getvalue,info,'global','tdw',dw,type=2,default=15

   objcode = 'R-48-'+objectid

   subdw=120

   c = ','
   openmysql,dblun,'hstast'

   cmd=['select idx,root,det', $
        'from header', $
        'where root like '+quote(filestem+'%'), $
        'order by root,det;']
   mysqlquery,dblun,cmd,hidx,root,det,format='l,a,i',ngood=nimages

   print,strn(nimages),' images to consider'

   for i=0,nimages-1 do begin
      rdwfc3,root[i],det[i],data,ddir=ddir,type=type,satur=satur
      if data eq !null then goto,bailout

      if not skipwcs then begin
         cmd='select dcrval0,dcrval1 from wcs'+ $
                ' where root='+quote(root[i])+';'
         mysqlquery,dblun,cmd,dcrval0,dcrval1,format='d,d',ngood=ncheck
         if ncheck eq 0 then begin
            print,'No WCS update found for ',root[i]
            print,'Maybe you forgot to run wcsupdate first?'
            goto,bailout
         endif
         data.astinfo.crval=data.astinfo.crval+[dcrval0,dcrval1]/3600.0d0
      endif

      ephem,data.jdmid,'500',2,objcode,eph
      ra=trimrank(eph[0,*])
      dec=trimrank(eph[1,*])
      ra_d=ra*180.0d0/!dpi
      dec_d=dec*180.0d0/!dpi
      jdstr,data.jdmid,0,jds
      rastr,ra,1,ras
      decstr,dec,0,decs
      print,root[i],' ',strn(det[i]),' ',jds,' ',ras,' ',decs

      ad2xy,ra_d,dec_d,data.astinfo,xtarg,ytarg

      xtarg += offset[0]
      ytarg += offset[1]

      if xtarg gt 0 and xtarg lt data.nx and $
         ytarg gt 0 and ytarg lt data.ny then begin
         print,'Predicted location ',xtarg,ytarg
         skysclim,data.image,lowval,hival,meanval,sigma,npts=100000L
         i0=round(xtarg)-subdw
         i1=round(xtarg)+subdw
         j0=round(ytarg)-subdw
         j1=round(ytarg)+subdw
         subarr,data.image,i0,i1,j0,j1,subim,fill=meanval,noise=sigma
         hdr=data.hdr
         sxaddpar,hdr,'SUBARRI0',i0
         sxaddpar,hdr,'SUBARRJ0',j0
         showsrc,data.image,hisig=20,window=0
         oplot,[-1,1,1,-1,-1]*dw+xtarg,[-1,-1,1,1,-1]*dw+ytarg,color='00ff70'xl
         showsrc,subim,hisig=20,window=1
         oplot,[-1,1,1,-1,-1]*dw+xtarg-i0,[-1,-1,1,1,-1]*dw+ytarg-j0, $
            color='00ff70'xl
         fnout=root[i]+'_'+strn(det[i])+'_sub.fits'
         fnpng=root[i]+'_'+strn(det[i])+'_sub.png'
         print,'Saving ',fnout
         writefits,fnout,subim,hdr
         tvgrab,fnpng,1,/png
      endif else begin
         print,'Object not on detector.'
      endelse
   endfor

bailout:
   free_lun, dblun

end
