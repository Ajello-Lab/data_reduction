;+
; NAME:
;  occbingen
; PURPOSE:   (one line only)
;  Generate a binned version of an occultation dataset
; DESCRIPTION:
; CATEGORY:
;  Occultations
; CALLING SEQUENCE:
;  occbingen,team,fwhm,binfac
; INPUTS:
;  team - string for the team name (must match directories)
;  fwhm - mean fwhm from the unbinned version of the data (or your best guess)
;  binfac - binning factor.  values of 1 do not do anything
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;  NODISPLAY - Flag, if set suppresses the graphical output
; OUTPUTS:
; KEYWORD OUTPUT PARAMETERS:
; CONFIGURATION:
;   File: config.ini
;   [global]
;     event   - string with short ID name of event (ex: OR20200914)
;     dir     - relative directory for this data set added to the root dir
;     date    - YYYY-MM-DD both date and directory name part of path
;     maxphotsig - maximum photometric signal (DN)
;     snrlimit   - lower bound of SNR of detected sources to be kept
;     minfwhm    - Minimum valid FWHM for a source to keep
;   [TEAM]   - name of the team (input variable)
;     dirtime - HH_MM_SS both starting time and directory name part of path
;     maxphotsig - override of value in [global]
;     snrlimit   - override of value in [global]
;     minfwhm    - override of value in [global]
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
;  Written by Marc W. Buie, Southwest Research Institute, 2020/09/23
;  2021/01/18, MWB, changed strategy for saving binned data and reporting
;                to database
;  2021/01/27, MWB, added support for stemdir in team stanza in config.ini
;  2021/05/17, MWB, fixed numerous problems for a binned image that shows no stars.
;  2022/02/11, MWB, added NODISPLAY keyword
;  2023/04/04, MWB, added tweak to stemdir logic
;-
pro occbingen,team,in_fwhm,binfac,NODISPLAY=nodisplay

   pname='occbingen'
   self=pname+': '
   if badpar(team,7,0,caller=self+'(team) ') then return
   if badpar(in_fwhm,[2,3,4,5],0,caller=self+'(in_fwhm) ') then return
   if badpar(binfac,[2,3],0,caller=self+'(binfac) ') then return
   if badpar(nodisplay,[0,1,2,3],0,caller=self+'(NODISPLAY) ', $
                                     default=0) then return

   if binfac eq 1 then begin
      print,'Nothing to do for binfac=1'
      return
   endif

   getddir,cinfo,ddir,team=team
   getvalue,cinfo,'global','event',event
   getvalue,cinfo,'global','date',date
   getvalue,cinfo,team,'dirtime',dirtime

   getvalue,cinfo,team,'stemdir',stemdir
   if stemdir eq '' then begin
      getvalue,cinfo,team,'dir',gdir
      if gdir eq '' then begin
         stemdir=event+'/'+team+'/'+date+'/'+dirtime+'/'
      endif else begin
         stemdir=gdir+'/'+team+'/'+date+'/'+dirtime+'/'
      endelse
   endif else begin
      stemdir=addslash(stemdir)+dirtime+'/'
   endelse

   dir=ddir+stemdir
   if not exists(dir) then begin
      print,dir
      print,'Data directory does not exist'
      return
   endif
   print,dir

   getvalue,cinfo,team,'maxphotsig',in_maxphotsig,type=4
   getvalue,cinfo,team,'snrlimit',snrlimit,type=4
   getvalue,cinfo,team,'minfwhm',minfwhm,type=4

   fwhmguess=float(in_fwhm)/binfac

;   getddir,root,event,stemdir
;   rootdir=root+event+'/'

   fninfo=team+'_'+strn(binfac)+'_info.dat'
   maxphotsig=in_maxphotsig*long(binfac)^2

   bdir='binned_'+strn(binfac)

   if not exists(dir+bdir) then file_mkdir,dir+bdir
   if not exists('Src/'+team) then file_mkdir,'Src/'+team

   bdir=addslash(bdir)

   fnlist=file_search(dir+'*.fits',count=nfiles)
   print,strn(nfiles),' images found.'

   nbin=0
   for i=0,nfiles-1 do begin
      good=1
      fn=strmid(fnlist[i],strlen(dir))
      fnout=dir+bdir+fn
      fnsrc='Src/'+team+'/'+fn+'.'+strn(binfac)+'.src'

      if exists(fnout) and exists(fnsrc) then continue

      if not exists(fnout) then begin
         print,'Load ',fnlist[i]
         image=float(readfits(fnlist[i],imhdr))
         skysclim,image,loval,hival,meanval,sigma,npts=10000
         backsub,image,/row
         image += meanval
         sz=size(image,/dimen)
         nx=sz[0]
         ny=sz[1]
         newnx = nx/binfac
         newny = ny/binfac
         bimage=rebin(image[0:binfac*newnx-1,0:binfac*newny-1],newnx,newny)* $
                   float(binfac)^2
         sxaddpar,imhdr,'NAXIS1',newnx
         sxaddpar,imhdr,'NAXIS2',newny
         sxaddpar,imhdr,'BINFAC',binfac
         sxdelpar,imhdr,'O_BZERO'
         sxdelpar,imhdr,'EXTEND'
         sxdelpar,imhdr,'BSCALE'
         sxdelpar,imhdr,'BZERO'
         sxdelpar,imhdr,'ASTINFO'
         jds=sxpar(imhdr,'DATE-OBS')
         exptime=double(sxpar(imhdr,'EXPTIME'))
         jdmid=jdparse(jds)+exptime/86400.0d0/2.0d0
         print,'sky 1  ',meanval,' +/-',sigma
         savebinnedfile = 1
      endif else begin
         bimage=readfits(fnout,imhdr)
         exptime=double(sxpar(imhdr,'EXPTIME'))
         jds=sxpar(imhdr,'DATE-OBS')
         jdmid=jdparse(jds)+exptime/86400.0d0/2.0d0
         savebinnedfile = 0
      endelse

      print,'Starting FWHM guess',fwhmguess
      findsrc,bimage,exptime=exptime,gap=round(fwhmguess), $
         maxphotsig=80000L*long(binfac)^2,/nodisplay, $
         /nocrs,/noinfo,results=res,/silent
      if res.nobj le 3 then good=0

      if good then begin
         bad=bytarr(res.nobj)
         print,strn(res.nobj),' sources'
         print,'sky 2  ',res.avgsky,' +/-',res.skysg

         peak=bimage[round(res.xc),round(res.yc)]
         z=where(peak ge maxphotsig,count)
         if count ne 0 then bad[z]=1B
         print,strn(count),' saturated sources'
         zg=where(bad eq 0B,countg)
         if countg le 3 then good=0
      endif

      if good then begin
         z=where(res.fwhm lt minfwhm,count)
         if count ne 0 then bad[z]=1B
         print,strn(count),' with FWHM too low'
         zg=where(bad eq 0B,countg)
         if countg le 3 then good=0
      endif

      if good then begin
         robomean,res.fwhm,2.5,0.5,avgfwhm,bad=bad
         print,'Average FWHM',avgfwhm
         print,strn(count),' with FWHM too low'
         zg=where(bad eq 0B,countg)
         if countg le 3 then good=0
      endif

      if good then begin
         z=where(res.snr le snrlimit and bad eq 0,count)
         if count ne 0 then bad[z]=1B
         print,strn(count),' low SNR sources'
         zg=where(bad eq 0B,countg)
         print,strn(countg),' good sources'
         if countg le 3 then good=0
      endif

      if good then begin
         meanerr2,res.fwhm[zg],res.snr[zg],avgfwhmw,sig
         print,'Weighted average FWHM',avgfwhmw,sig

         if not nodisplay then begin
            stats,res.snr[zg],window=1,/silent,xtitle='SNR',nbins=10
            stats,res.fwhm[zg],window=2,/silent,xtitle='FWHM (pixels)',nbins=10
            stats,peak[zg],window=3,/silent,xtitle='Peak signal (DN)',nbins=10
         endif

         avgsky=res.avgsky
         skysig=res.skysg
         obscura1=res.obscura1
         obscura2=res.obscura2
      endif else begin
         skysclim,bimage,lowval,hival,avgsky,skysig
         avgfwhm=0.0
         avgfwhmw=0.0
         sig=0.0
         countg=0
         obscura1=0
         obscura2=0
      endelse

      if not nodisplay then begin
         showsrc,bimage,window=0
         mkcircle,0.,0.,1.,xcirc,ycirc
         for j=0,res.nobj-1 do begin
            if bimage[round(res.xc[j]),round(res.yc[j])] gt maxphotsig then $
               color=cpalette(1) $
            else if bad[j] eq 1B then color=cpalette(5) $
            else color=cpalette(4)
            oplot,xcirc*fwhmguess+res.xc[j],ycirc*fwhmguess+res.yc[j], $
               color=color
         endfor
      endif

      tag=fn
      info=string(jdmid,res.nobj,countg,avgfwhmw,avgsky,skysig,good, $
                  format='(1x,f17.9,1x,i5,1x,i5,1x,f5.2,1x,f9.1,1x,f7.1,1x,i1)')

      repwrite,fninfo,tag,tag+info

      if res.nobj gt 0 then begin
         data=[[res.xc[zg]],[res.yc[zg]],[res.fwhm[zg]], $
               [res.mag[zg]],[res.err[zg]],[res.snr[zg]]]
      endif else begin
         data=0
      endelse
      mkhdr,hdr,data
      sxaddpar,hdr,'TEAM',team,' Team identification'
      sxaddpar,hdr,'EVENT',event,' Event identification'
      sxaddpar,hdr,'GAP',round(fwhmguess),' Object gap size, is approximately the FWHM'
      sxaddpar,hdr,'OBJRAD',round(fwhmguess),' Object aperture radius for photometry'
      sxaddpar,hdr,'MAXSIG',maxphotsig,' Saturated above this DN level'
      sxaddpar,hdr,'MEANFWHM',avgfwhmw,' Mean FWHM in pixels'
      sxaddpar,hdr,'BINFAC',binfac,' Binning factor relative to original pixels'
      sxaddpar,hdr,'SKYLEVEL',avgsky,' Average sky signal level counts/pixel'
      sxaddpar,hdr,'SKYSIGMA',skysig,' Standard deviation of the sky signal'
      sxaddpar,hdr,'OBSCURA1',obscura1,' Fraction of image obscured by 50*skysig bright pixels'
      sxaddpar,hdr,'OBSCURA2',obscura2,' Fraction of image obscured by 5*skysig bright pixels'
      print,'save source file ',fnsrc
      writefits,fnsrc,data,hdr

      if savebinnedfile then begin
         ; write out the binned data
         print,'Write',fnout
         writefits,fnout,bimage,imhdr
      endif

      if good then fwhmguess=avgfwhmw

      nbin++
   endfor

   if nbin gt 0 then begin
      c=','
      openmysql,dblun,'occlc'
      action=team+': binned '+strn(nbin)+' images out of '+strn(nfiles)+ $
             ' with binfac='+strn(binfac)+'.'
      jdcur=systime(/julian,/ut)
      jdstr,jdcur,300,jds
      cmd=['insert into history set', $
           'posted='+jds+c, $
           'event='+quote(event)+c, $
           'tool='+quote(pname)+c, $
           'action='+quote(action)+';']
      print,cmd
      mysqlcmd,dblun,cmd
      free_lun,dblun
   endif

end
