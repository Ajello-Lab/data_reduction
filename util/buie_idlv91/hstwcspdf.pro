;+
; NAME:
;  hstwcspdf
; PURPOSE:   (one line only)
;  Generate the final PDF from the joint constraints for all the good stars
; DESCRIPTION:
; CATEGORY:
;  Astrometry
; CALLING SEQUENCE:
;  hstwcspdf,filestem
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
;     subdir   - naem of sub-directory  added to base path to find data
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
;  2020/07/01, Written by Marc W. Buie, Southwest Research Institute
;-
pro hstwcspdf,filestem

   pname='hstwcspdf'
   self=pname+':'
   if badpar(filestem,7,0,caller=self+'(filestem) ') then return

   getddir,cinfo,ddir
   getvalue,cinfo,'global','subdir',subdir
   getvalue,cinfo,'global','filetype',type,default='flc'
   pdfdir=ddir+'pdf/'

   patt=filestem+'*'+type+'.fits*'
   fnlist=file_search(ddir+patt,count=nfiles)
   if nfiles eq 0 then begin
      print,'data directory: ',ddir
      print,'No files found.  Looked for ',patt
      return
   endif
   fnlist=strmid(fnlist,strlen(ddir))

   dirty=0
   c=','
   ndone=0
   openmysql,dblun,'hstast'

   for i=0,nfiles-1 do begin
      print,'Process ',fnlist[i]
      words=strsplit(fnlist[i],'_',/extract)
      root=words[0]

      rdwfc3,root,1,data1,ddir=ddir
      if data1 eq !null then return
      rdwfc3,root,2,data2,ddir=ddir

      cmd=['select stars.idx,hidx,det,ra,decl,info', $
           'from stars,header', $
           'where stars.hidx=header.idx', $
           'and root='+quote(root), $
           'and flag='+quote('g'), $
           'order by idx;']
      mysqlquery,dblun,cmd,idx,hidx,det,sra,sdec,info, $
         format='l,l,i,d,d,a',ngood=nstars

      ; sra,sdec are the catalog positions in radians

      print,'Found ',strn(nstars),' good stars from hstast.stars'
      z=where(info ne 'autopdf',count)
      if count ne 0 then begin
         print,'Some stars are not full processed.'
         print,idx[z]
         print,'aborting'
         goto,bailout
      endif

      for j=0,nstars-1 do begin

         fnpdf='S'+strn(idx[j])+'.pdf'
         if not exists(pdfdir+fnpdf) then begin
            print,'PDF ',fnpdf,' not found.   Aborting'
            goto,bailout
         endif

         ; The PDF contains x,y,mag of this star on the current field
         ;   but we only use x,y here
print,'Reading ',pdfdir+fnpdf
         pdf=readfits(pdfdir+fnpdf)
         sz=size(pdf,/dimen)

         ; init the working arrays
         if j eq 0 then begin
            nsamp=sz[1]
            wcsra=dblarr(nsamp,nstars)
            wcsdec=dblarr(nsamp,nstars)
         endif

         ; compute the WCS-based position for all the samples in the PDF
         if det[j] eq 1 then $
            xy2ad,pdf[0,*],pdf[1,*],data1.astinfo,ra_d,dec_d $
         else $
            xy2ad,pdf[0,*],pdf[1,*],data2.astinfo,ra_d,dec_d

         ; save to the working arrays, converting from degrees to radians
         wcsra[*,j] = ra_d*!dpi/180.0d0
         wcsdec[*,j] = dec_d*!dpi/180.0d0

;print,'Star',j,idx[i]
;print,pdf[0,0],pdf[1,0]
;print,trimrank(wcsra[0,*])
;print,trimrank(wcsdec[0,*])

      endfor
      print,strn(nsamp),' samples in star PDFs'
      
      ; Derive the WCS update value for each sample
      wcspdf=dblarr(2,nsamp)
      for j=0,nsamp-1 do begin
         dra=(sra-trimrank(wcsra[j,*]))*180.0d0 / !dpi * 3600.0d0
         ddec=(sdec-trimrank(wcsdec[j,*]))*180.0d0 / !dpi * 3600.0d0
         robomean,dra,3.0,0.5,newdra
         robomean,ddec,3.0,0.5,newddec
         wcspdf[*,j]=[newdra,newddec]
;print,idx
;print,hidx
;print,sra
;print,sdec
;print,trimrank(wcsra[j,*])
;print,trimrank(wcsdec[j,*])
;print,dra
;print,ddec
;goto,bailout
      endfor

      pdf2covar,wcspdf,wcscovar
      print,wcscovar

      setwin,0,xsize=1500,ysize=512
      cs=2.0
      !p.multi=[0,3,1]
      stats,wcspdf[0,*],/silent,nbins=100,xtitle='Catalog-WCS RA (arcsec)', $
         title=root,charsize=cs
      stats,wcspdf[1,*],/silent,nbins=100,xtitle='Catalog-WCS Dec (arcsec)', $
         charsize=cs
      plot,wcspdf[0,*],wcspdf[1,*],psym=3, $
         background='ffffff'xl,color=0,/iso, $
         xtitle='dRA (arcsec)',ytitle='dDec (arcsec)',charsize=cs
      !p.multi=0

      fnpdf='W_'+root+'.pdf'
      fnpng='W_'+root+'.png'
      mkhdr,hdr,wcspdf
      print,'Saving ',pdfdir+fnpdf
      writefits,pdfdir+fnpdf,wcspdf,hdr
      tvgrab,'plots/'+fnpng,0,/png

   endfor

bailout:
   free_lun,dblun

end
