;+
; NAME:
;  hstfinalpdf
; PURPOSE:   (one line only)
;  Combine target and reference star PDFs for the final astrometry PDF
; DESCRIPTION:
; CATEGORY:
;  Astrometry
; CALLING SEQUENCE:
;  hstfinalpdf,filestem
; INPUTS:
;  filestem - String that is used to select a set of file for processing.
;             The search string for files will be
;               filestem+'*_flc.fits.gz'
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
; OUTPUTS:
; KEYWORD OUTPUT PARAMETERS:
; CONFIGURATION: (optional, overrides xtrack.in)
;   File: config.ini
;   [global]
;     objectid - object code (see ephem.pro)
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
;  2020/06/29, Written by Marc W. Buie, Southwest Research Institute
;-
pro hstfinalpdf,filestem

   self='hstfinalpdf:'
   if badpar(filestem,7,0,caller=self+'(filestem) ') then return

   getddir,info,ddir
   pdfdir=ddir+'pdf/'
   getvalue,info,'global','objectid',objectid

   openmysql,dblun,'hstast'

   cmd=['select pos.idx,hidx,root,det,objid', $
        'from pos,header', $
        'where pos.hidx=header.idx', $
        'and root like '+quote(filestem+'%'), $
        'order by root,det;']
   mysqlquery,dblun,cmd,idx,hidx,root,det,objid, $
      format='l,l,a,i,a',ngood=nobjs


   print,strn(nobjs),' measurements to process'

   objcode = 'R-48-'+objectid

   for i=0,nobjs-1 do begin
      fnwcspdf='W_'+root[i]+'.pdf'
      if not exists(pdfdir+fnwcspdf) then begin
         print,'WCS PDf, ',fnwcspdf,' not found.'
         goto,bailout
      endif
      wcspdf=readfits(pdfdir+fnwcspdf)
      sz=size(wcspdf,/dimen)
      nwsamp=sz[1]
      fnpdf='T'+strn(idx[i])+'.pdf'
      rawpdf=readfits(pdfdir+fnpdf)
      x=trimrank(rawpdf[0,*])
      y=trimrank(rawpdf[1,*])
      sz=size(rawpdf,/dimen)
      nsamp=sz[1]
      pdf=dblarr(2,nsamp)
      if nsamp eq nwsamp then begin
         sidx=lindgen(nsamp)
      endif else begin
         sidx=round(randomu(seed,nsamp)*(nwsamp-1))
      endelse

      cmd=['select stars.idx,x,y,ra,decl', $
           'from stars,header', $
           'where stars.hidx=header.idx', $
           'and root='+quote(root[i]), $
           'order by idx;']
      mysqlquery,dblun,cmd,ridx,sx,sy,sra,sdec,format='l,d,d,d,d',ngood=nstars
      sra=sra*180.0d0/!dpi
      sdec=sdec*180.0d0/!dpi

      spdf=dblarr(2,nstars,nsamp)

      rdwfc3,root[i],det[i],data,ddir=ddir,type='flc'
      orig_crval=data.astinfo.crval
      for j=0,nsamp-1 do begin
         data.astinfo.crval=orig_crval+wcspdf[*,sidx[j]]/3600.0d0
         xy2ad,x[i],y[i],data.astinfo,r,d
         pdf[0,j]=r
         pdf[1,j]=d
         xy2ad,sx,sy,data.astinfo,r,d
         spdf[0,*,j]=r
         spdf[1,*,j]=d
      endfor

      setwin,1,xsize=1024,ysize=1024
      !p.multi=[0,3,3]
      for j=0,nstars-1 do begin
         plot,(spdf[0,j,*]-sra[j])*3600.0d0,(spdf[1,j,*]-sdec[j])*3600.0d0, $
            psym=3,/iso,background='ffffff'xl,color=0,charsize=2
      endfor
      !p.multi=0


      pdf2covar,pdf,covar,means=means
      ; covar starts out as degrees^2
      covar = covar*(3600.0d0^2) ; convert to arcsec
      cosdec=replicate(1.0,2,2)*cos(means[1]*!dpi/180.0d0)
      cosdec[1,1]=1.0
      covar=covar*cosdec
      rmsra = sqrt(covar[0,0])
      rmsdec = sqrt(covar[1,1])
      rmscorr = covar[0,1]/rmsra/rmsdec
;      print,covar
;      print,rmsra,rmsdec,rmscorr

      ephem,data.jdmid,'500',2,objcode,eph
      raeph=trimrank(eph[0,*])
      deceph=trimrank(eph[1,*])
      raeph_d=raeph*180.0d0/!dpi
      deceph_d=deceph*180.0d0/!dpi

      ra=means[0]
      dec=means[1]
      raerr=sqrt(covar[0,0])*3600.0d0
      decerr=sqrt(covar[1,1])*3600.0d0
      jdstr,data.jdmid,3,jds

      dra=(ra-raeph_d)*3600.0d0*cos(means[1]*!dpi/180.0d0)
      ddec=(dec-deceph_d)*3600.0d0
      print,'Offset from ephemeris',dra,ddec

      print,objid[i],jds,ra,dec,rmsra,rmsdec,rmscorr, $
         format='(a,1x,a,2(1x,f14.10),2(1x,f7.5),1x,f7.4)'

      setwin,0,xsize=1500,ysize=512
      cs=2.0
      !p.multi=[0,3,1]
      stats,(pdf[0,*]-mean(pdf[0,*]))*3600.0d0,/silent,nbins=100, $
         xtitle='RA-mean(RA) (arcsec)', $
         title=root[i],charsize=cs
      oplot,[raeph_d-mean([pdf[0,*]])],[0],psym=4,color='0000ff'xl
      stats,(pdf[1,*]-mean(pdf[1,*]))*3600.0d0,/silent,nbins=100, $
         xtitle='Dec-mean(Dec) (arcsec)', $
         charsize=cs
      oplot,[deceph_d-mean([pdf[1,*]])],[0],psym=4,color='0000ff'xl
      xr=maxmin([raeph_d,trimrank(pdf[0,*])])
      yr=minmax([deceph_d,trimrank(pdf[1,*])])
      plot,pdf[0,*],pdf[1,*],psym=3,xr=xr,yr=yr, $
         background='ffffff'xl,color=0,/iso, $
         xtitle='RA (deg)',ytitle='Dec (deg)',charsize=cs
      oplot,[raeph_d],[deceph_d],psym=8,color='0000ff'xl
      !p.multi=0

      pdf = pdf *!dpi/180.0d0

      fnpdf='A'+strn(idx[i])+'.pdf'
      fnpng='A'+strn(idx[i])+'.png'
      hdr=data.hdr
      sxaddpar,hdr,'JDMID',data.jdmid,format='F19.10'

;      print,'Saving ',pdfdir+fnpdf
      writefits,pdfdir+fnpdf,pdf,hdr

      tvgrab,'plots/'+fnpng,0,/png

   endfor

bailout:
   free_lun,dblun

end
