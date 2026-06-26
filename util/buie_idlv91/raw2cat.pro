;+
; NAME:
;  raw2cat
;
; PURPOSE:
;  Determine standard magnitudes of crowded sources for the raw2 catalog.
;
; DESCRIPTION:
;  Works on one night's worth of data
;
; CATEGORY:
;  Photometry
;
; CALLING SEQUENCE:
;  raw2cat,catid,rundate
;
; INPUTS:
;  catid   - String, catid of data you wish to use (ex: PL2010)
;  rundate - String, night of data you wish to use (ex. 100704)
;
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;  nosave - Flag, stops writing into database
;
; OUTPUTS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
;  At the moment, this routine is pretty specific to our local data reductions
;   here.  Paths, filter handling, and database setup are all very specific.
;
; PROCEDURES:
;  In a single night's reduced directory, reads the reduc.inf file to get
;   rundate and instrument.
;  Gets coefficients from psf hdr.
;  Reads in data from roboccd for that rundate using objpatt input - for 
;   2010 data, objpatt='PL2010%' - and from .sr2 files.
;  Subtracts the Stage1 catalog from the data.
;  Puts remaining sources through colorsol, which returns final magnitudes.
;  Generates Raw2 catalog using input catid - for 2010 data, id='PL2010' -
;   and writes into the plphot table of phot database.
;
; MODIFICATION HISTORY:
;  2013/12/15 Written by Erin R. George, Southwest Research Institute
;  2014/12/29, MWB, updates for final processing based on new phocal.
;-
pro raw2cat,catid,rundate,NOSAVE=nosave,QUIET=quiet

   compile_opt strictarrsubs

   self='raw2cat: '
   if badpar(catid,7,0,CALLER=self+'(catid) ') then return
   if badpar(rundate,7,0,CALLER=self+'(rundate) ') then return
   if badpar(nosave,[0,1,2,3],0,CALLER=self+'(NOSAVE) ',default=0) then return
   if badpar(quiet,[0,1,2,3],0,CALLER=self+'(QUIET) ',default=0) then return

   s1subid='Stage1'
   r2subid='Raw2'
   objpatt=catid+'%'

   rdir='/net/frakir/raid/buie/Reduced/'
;rdir='/home/buie/Reduced/'
   ndir=rdir+addslash(rundate)
   psfdir=ndir+'psf/'
   sdir=ndir+'Src/'

   keylist=rdir+'roboccd.key'
   loadkeys,keylist,hdrlist,FOUNDIT=foundkey
   if not foundkey then begin
      print,self,'Keylist ',keylist,' could not be loaded. Aborting.'
      return
   endif

   infofile='reduc.inf'
   if not exists(ndir+infofile) then begin
      print,self,'Error: reduc.inf does not exists for this night.'
      return
   endif
   rdreduc,ndir+infofile,instrument,ddir,rundate, $
      rad,sky1,sky2,gain,rdnoise,oplines
;ddir='/home/buie/Data/rawfits/roboccd/'+rundate+'/cal/'
   nddir=ddir+addslash(rundate)
   refid=rundate+'-'+instrument

   dcr=1.5/3600.0d0*!dpi/180.0d0  ; 1.5 arcsec converted to radians

   filter=[1,2]
   filters=['B','V']
   color1=filters[0]
   color2=filters[1]

   ;coordinates for observatory
   ; This is the GPS position for the 42", derived 1993 Sep 08
   lat = (35.0+5.0/60.0+48.740/3600.0)/180.0*!pi
   lon = (111.0+32.0/60.0+10.601/3600.0)/180.0*!pi
   name= 'Lowell Observatory - Anderson Mesa Station'

   openmysql,dblun,'roboccd'
   cmd=['select object from image', $
        'where filename like '+quote(rundate+'%'), $
        'and object like '+quote(objpatt), $
        'group by object;']
   mysqlquery,dblun,cmd,objectlist,ngood=nobjects
   if nobjects eq 0 then begin
      print,'Error: The number of objects for this rundate and catid is 0.'
      goto,bailout
   endif

   gettran,'Nasacam',rundate,filter[0],filter[0],filter[1], $
      tr1,trsig1,jdref1,refcolor1,refam1,found,fitted,nobs,chi2,quality
   if not found or quality ne 'good' then begin
      print,self,'Error getting '+sfilters[0]+' transformation'
      return
   endif

   gettran,'Nasacam',rundate,filter[1],filter[0],filter[1], $
      tr2,trsig2,jdref2,refcolor2,refam2,found,fitted,nobs,chi2,quality
   if not found or quality ne 'good' then begin
      print,self,'Error getting '+sfilters[1]+' transformation'
      return
   endif

   fwhm=[]
   jd=[]
   inst=[]
   instsig=[]
   fil=[]
   chisq=[]
   bad=[]
   am=[]
   trans1=[]
   transig1=[]
   trans2=[]
   transig2=[]
   jdr1=[]
   jdr2=[]
   refc1=[]
   refc2=[]
   refx1=[]
   refx2=[]
   tidx=[]
   idx1=0L
   idx2=0L
   fnlist=[]
   olist=[]
   for ii=0,nobjects-1 do begin
print,'Object ',objectlist[ii],'   ',strn(ii+1),' out of ',strn(nobjects)
      cmd=['select filename from image', $
           'where object='+quote(objectlist[ii]), $
           'and filename like '+quote(rundate+'%'), $
           'group by filename;']
      mysqlquery,dblun,cmd,imagelist,ngood=nimages
      if nimages eq 0 then begin
         print,'Error: There are no images for object ',objectlist[ii]
         goto,bailout
      endif

      for jj=0,nimages-1 do begin
print,'Image ',imagelist[jj],'  ',strn(jj+1),' out of ',strn(nimages)
         fn=string(imagelist[jj])
         if not exists(ddir+fn) then begin
            print,'image file ',ddir+fn,' not found. Aborting.'
            return
         endif
         imhdr=headfits(ddir+fn)
         parsekey,imhdr,hdrlist,iminfo
         if iminfo.filter ne filters[0] and $
            iminfo.filter ne filters[1] then continue

         fnsrf=fn+'.srf'
         if not exists(psfdir+fnsrf) then begin
            print,self,'Error: ',psfdir+fnsrf+' not found'
            goto,bailout
         endif

         fnsr2=fn+'.sr2'
         if not exists(sdir+fnsr2) then begin
            print,self,'Error: ',sdir+fnsr2+' not found'
            goto,bailout
         endif

         fdata=readfits(psfdir+fnsrf,hdrf)
         filt=sxpar(hdrf,'FILTER')
         filts=filters[filt]
         zpt=sxpar(hdrf,'ZEROPT')
         zptsig=sxpar(hdrf,'EZEROPT')
         fx=trimrank(fdata[*,0])
         fy=trimrank(fdata[*,1])
         finst=trimrank(fdata[*,3])
         finstsig=trimrank(fdata[*,4])
         fra=trimrank(fdata[*,5])
         fdec=trimrank(fdata[*,6])

         data2=readfits(sdir+fnsr2,hdr2)
         x2=trimrank(data2[*,0])
         y2=trimrank(data2[*,1])

         ; link the sr2 list with the psf fit list
         srcor,x2,y2,fx,fy,2,ind2,indf,option=1

         ; filter the srf list to keep only those that link
         inst0=finst[indf]
         instsig0=finstsig[indf]
         ra0=fra[indf]
         dec0=fdec[indf]
         nobj=n_elements(fra[indf])
         jd0=replicate(iminfo.jd,nobj)
         fil0=replicate(filts,nobj)
         am0=airmass(jd0,ra0,dec0,lat,lon)
         bad0=bytarr(nobj)

         if filts eq filters[0] then begin
            newtr=tr1
            newtrsig=trsig1
            newtr[3]=zpt
            newtrsig[3]=zptsig
            trans1=[[trans1],[newtr]]
            transig1=[[transig1],[newtrsig]]
            tidx=[tidx,replicate(idx1,nobj)]
            jdr1=[jdr1,jdref1]
            refc1=[refc1,refcolor1]
            refx1=[refx1,refam1]
            idx1++
         endif else begin
            newtr=tr2
            newtrsig=trsig2
            newtr[3]=zpt
            newtrsig[3]=zptsig
            trans2=[[trans2],[newtr]]
            transig2=[[transig2],[newtrsig]]
            tidx=[tidx,replicate(idx2,nobj)]
            jdr2=[jdr2,jdref2]
            refc2=[refc2,refcolor2]
            refx2=[refx2,refam2]
            idx2++
         endelse

         z=where(inst0 gt 20.0,count)
         if count ne 0 then bad0[z]=1
         sigthresh=0.10
         z=where(instsig0 gt sigthresh,count)
         if count gt 0 then bad0[z]=1

         msrcor,set,ra0,dec0,dcr

         inst=[inst,inst0]
         instsig=[instsig,instsig0]
         jd=[jd,jd0]
         fil=[fil,fil0]
         am=[am,am0]
         fnlist=[fnlist,replicate(imagelist[jj],nobj)]
         olist=[olist,replicate(objectlist[ii],nobj)]
         bad=[bad,bad0]

      endfor

   endfor

   nobs=n_elements(ra)

   stand=string(set.objid)
   serial=replicate(0,n_elements(set.objid))

   colorsol,stand,fil,jd,am,serial,inst,instsig, $
            color1,color2,trans1,transig1,jdr1,trans2,transig2,jdr2, $
            object,std1,stdsig1,std2,stdsig2,stdcol,stdcolsig, $
            refcolor=[refcolor1,refcolor2],refam=[refam1,refam2], $
            tidx=tidx,badflags=bad,/noedit,/noprint

   openmysql,dblun,'phot'
   ; The delete statement for the data in the new
   ;  database phot:plphot.
   cmddel=['delete from plphot where', $
           'id='+quote(catid), $
           'and subid='+quote(r2subid), $
           'and refid='+quote(refid)+';']
   if nosave then begin
      print,cmddel
   endif else begin
      mysqlcmd,dblun,cmddel,answer,nlines
   endelse

   nobj=n_elements(object)
   print,strn(nobj),' new objects with good photometry for raw2 catalog'

   c=','
   for kk=0,nobj-1 do begin
      zz=where(stand eq object[kk],count)
      if count lt 4 then continue
      avgjd=mean(jd[z])
      rasig=stdev(set.x[zz],avgra)
      decsig=stdev(set.y[zz],avgdec)
      rasig=rasig*180.0d0/!pi*3600.0d0*cos(avgdec) > 0.01 ; radians-->arcsec
      decsig=decsig*180.0d0/!pi*3600.0d0 > 0.01           ; radians-->arcsec

      zb=where(stand eq object[kk] and $
               fil eq filters[0],nbobs)
      b1=where(stand eq object[kk] and $
               fil eq filters[0] and $
               bad eq 1,nbbad)
      zv=where(stand eq object[kk] and $
               fil eq filters[1],nvobs)
      v1=where(stand eq object[kk] and $
               fil eq filters[1] and $
               bad eq 1,nvbad)

      cmd=['insert into plphot values', $
           '('+quote(catid)+c, $                     ; catalog
           quote(r2subid)+c, $                       ; subid: raw2
           quote(refid)+c, $                         ; yymmdd-instrument
           strn(avgjd,format='(f13.5)')+c, $        ; jdate
           strn(avgra,format='(f11.9)')+c, $        ; ra (radians)
           strn(avgdec,format='(f12.9)')+c, $       ; decl (radians)
           strn(rasig,format='(f11.9)')+c, $  ; ra error (arcsec)
           strn(decsig,format='(f11.9)')+c, $ ; dec error (arcsec)
           strn(std1[kk],format='(f8.5)')+c, $        ; B mag
           strn(stdsig1[kk],format='(f7.5)')+c, $        ; B mag error
           strn(std2[kk],format='(f8.5)')+c, $        ; V mag
           strn(stdsig2[kk],format='(f7.5)')+c, $        ; V mag error
           strn(stdcol[kk],format='(f8.5)')+c, $         ; B-V color
           strn(stdcolsig[kk],format='(f7.5)')+c, $      ; B-V color error
           strn(nbobs)+c, $                       ; # B observations
           strn(nvobs)+c, $                       ; # V observations
           strn(nbbad)+c, $                       ; # bad b measurements
           strn(nvbad)+c, $                       ; # bad v measurements
           strn(1)+c, $                              ; # of nights for b
           strn(1)+');']                             ; # of nights for v
      if nosave then begin
         print,cmd
      endif else begin
         mysqlcmd,dblun,cmd,answer,nlines
      endelse
   endfor

bailout:
   free_lun,dblun

end   ; raw2cat.pro
