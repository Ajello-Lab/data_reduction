;+
; NAME:
;  occinfogen
; PURPOSE:   (one line only)
;  Generate information about an occultation dataset
; DESCRIPTION:
; CATEGORY:
;  Occultations
; CALLING SEQUENCE:
;  occinfogen,team
; INPUTS:
; OPTIONAL INPUT PARAMETERS:
;  team - string for the team name (must match directories)
; KEYWORD INPUT PARAMETERS:
;  FWHMGUESS - Initial guess for the FWHM, default=10, this only used if
;                 the guess is not provided inthe configuration file.
;  TEST      - Flag, set to run but not save anything.  Do this until you find
;                a reasonable value for FWHMGUESS
;  NODISPLAY - Flag, if set suppresses the graphical output
; OUTPUTS:
;  Nothing is saved, only plots are generated
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
;     fwhmguess  - Starting guess value for FWHM (pixels)
;   [TEAM]   - name of the team (input variable)
;     dirtime - HH_MM_SS both starting time and directory name part of path
;     maxphotsig - override of value in [global]
;     snrlimit   - override of value in [global]
;     minfwhm    - override of value in [global]
;     fwhmguess  - override of value in [global]
;     stemdir    - override of internal stemdir logic, this provides the
;                    bit in between the root directory picked up in [ddir]
;                    and dirtime.
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
;  Written by Marc W. Buie, Southwest Research Institute, 2020/09/24
;  2021/01/27, MWB, added support for stemdir in team stanza in config.ini
;  2022/02/11, MWB, added NODISPLAY keyword
;  2023/04/04, MWB, adjustment to scheme for finding data.
;-
pro occinfogen,team,FWHMGUESS=def_fwhmguess,TEST=test,NODISPLAY=nodisplay

   self='occinfogen: '
   if badpar(team,7,0,caller=self+'(team) ') then return
   if badpar(def_fwhmguess,[0,2,3,4,5],0,caller=self+'(FWHMGUESS) ', $
                                     default=10.0) then return
   if badpar(test,[0,1,2,3],0,caller=self+'(TEST) ', $
                                     default=0) then return
   if badpar(nodisplay,[0,1,2,3],0,caller=self+'(NODISPLAY) ', $
                                     default=0) then return
   getddir,cinfo,ddir,team=team
   getvalue,cinfo,'global','event',event
   getvalue,cinfo,'global','date',date
   getvalue,cinfo,team,'dirtime',dirtime
   getvalue,cinfo,team,'maxphotsig',maxphotsig,type=4
   getvalue,cinfo,team,'snrlimit',snrlimit,type=4
   getvalue,cinfo,team,'minfwhm',minfwhm,type=4
   getvalue,cinfo,team,'fwhmguess',fwhmguess
   if fwhmguess eq '' then fwhmguess=def_fwhmguess $
   else fwhmguess=float(fwhmguess)

   if dirtime eq '' then begin
      print,'No directory (dirtime) provided for team ',team
      return
   endif

   getvalue,cinfo,team,'stemdir',stemdir
   if stemdir eq '' then begin
      getvalue,cinfo,'global','dir',gdir
      if gdir eq '' then begin
         stemdir=event+'/'+team+'/'+date+'/'+dirtime+'/'
      endif else begin
         stemdir=gdir+'/'+team+'/'+date+'/'+dirtime+'/'
      endelse
   endif else begin
      stemdir=addslash(stemdir)+dirtime+'/'
   endelse

   dir=ddir+stemdir
   print,dir

   if not exists(dir) then begin
      print,'Directory does not exist.'
      return
   endif

   fninfo=team+'_info.dat'

   fnlist=file_search(dir+'*.fits',count=nfiles)
   print,strn(nfiles),' images found.'

   for i=0,nfiles-1 do begin
      good=1
      print,'Load ',fnlist[i]
      fn=strmid(fnlist[i],strlen(dir))
      image=float(readfits(fnlist[i],imhdr))
      jds=sxpar(imhdr,'DATE-OBS')
      exptime=double(sxpar(imhdr,'EXPTIME'))
      jdmid=jdparse(jds)+exptime/86400.0d0/2.0d0
      skysclim,image,loval,hival,meanval,sigma,npts=10000
      print,'Exposure time ',exptime,' seconds'
      print,'Raw sky',meanval,' +/-',sigma
      backsub,image,/row
      image += meanval
      skysclim,image,loval,hival,meanval,sigma,npts=10000
      print,'sky 1  ',meanval,' +/-',sigma

      print,'Starting FWHM guess',fwhmguess
      findsrc,image,exptime=exptime,gap=round(fwhmguess), $
         maxphotsig=80000L,/nodisplay, $
         /nocrs,/noinfo,results=res,/silent
      if res.nobj le 3 then good=0
      
      if good then begin
         bad=bytarr(res.nobj)
         print,strn(res.nobj),' sources'
         print,'sky 2  ',res.avgsky,' +/-',res.skysg

         peak=image[round(res.xc),round(res.yc)]
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

      endif else begin
         avgfwhm=0.0
         avgfwhmw=0.0
         sig=0.0
         countg=0
      endelse

      if not nodisplay then begin
         showsrc,image,window=0
         mkcircle,0.,0.,1.,xcirc,ycirc
         for j=0,res.nobj-1 do begin
            if image[round(res.xc[j]),round(res.yc[j])] gt maxphotsig then $
               color=cpalette(1) $
            else if bad[j] eq 1B then color=cpalette(5) $
            else color=cpalette(4)
            oplot,xcirc*fwhmguess+res.xc[j],ycirc*fwhmguess+res.yc[j], $
               color=color
         endfor
      endif

      tag=fn
      info=string(jdmid,res.nobj,countg,avgfwhmw,res.avgsky,res.skysg,good, $
                  format='(1x,f17.9,1x,i5,1x,i5,1x,f5.2,1x,f7.1,1x,f7.1,1x,i1)')

      if test then begin
;         itool,image,/block
         if good then break else continue
      endif else begin
         repwrite,fninfo,tag,tag+info

         if not exists('Src/'+team) then $
            file_mkdir,'Src/'+team
         fnsrc='Src/'+team+'/'+fn+'.src'

         if res.nobj gt 0 then begin
            data=[[res.xc[zg]],[res.yc[zg]],[res.fwhm[zg]], $
                  [res.mag[zg]],[res.err[zg]],[res.snr[zg]]]
         endif else begin
            data=[]
         endelse
         mkhdr,hdr,data
         sxaddpar,hdr,'TEAM',team,' Team identification'
         sxaddpar,hdr,'EVENT',event,' Event identification'
         sxaddpar,hdr,'GAP',round(fwhmguess),' Object gap size, is approximately the FWHM'
         sxaddpar,hdr,'OBJRAD',round(fwhmguess),' Object aperture radius for photometry'
         sxaddpar,hdr,'MAXSIG',maxphotsig,' Saturated above this DN level'
         sxaddpar,hdr,'MEANFWHM',avgfwhmw,' Mean FWHM in pixels'
         sxaddpar,hdr,'SKYLEVEL',res.avgsky,' Average sky signal level counts/pixel'
         sxaddpar,hdr,'SKYSIGMA',res.skysg,' Standard deviation of the sky signal'
         sxaddpar,hdr,'OBSCURA1',res.obscura1,' Fraction of image obscured by 50*skysig bright pixels'
         sxaddpar,hdr,'OBSCURA2',res.obscura2,' Fraction of image obscured by 5*skysig bright pixels'
         if good then goods='T' else goods='F'
         sxaddpar,hdr,'DATAGOOD',goods,' Image is deemed to be worthy of reduction'

         print,'save source file ',fnsrc
         writefits,fnsrc,data,hdr

      endelse

      if good then fwhmguess=avgfwhmw

   endfor

   if test and good eq 0 then print,'Nothing was good in the dataset'

end
