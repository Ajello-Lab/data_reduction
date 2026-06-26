;+
; NAME:
;  occplotinfo
; PURPOSE:   (one line only)
;  Plot information for the source extraction from an occultation dataset
; DESCRIPTION:
; CATEGORY:
;  Occultations
; CALLING SEQUENCE:
;  occplotinfo,team
; INPUTS:
;  team - string for the team name (must match directories)
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;  BINFAC - Binning factor for the data, default=1
; OUTPUTS:
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  Written by Marc W. Buie, Southwest Research Institute, 2020/09/24
;  2021/01/14, MWB, changed window assignments
;  2021/01/27, MWB, added logic for RECON files converted from MallinCAM data
;  2021/01/30, MWB, added saving graphics
;-
pro occplotinfo,team,BINFAC=binfac

   self='occplotinfo: '
   if badpar(binfac,[0,2,3],0,caller=self+'(BINFAC) ',default=1) then return

   if binfac eq 1 then begin
      fninfo=team+'_info.dat'
   endif else begin
      fninfo=team+'_'+strn(binfac)+'_info.dat'
   endelse

   print,'Reading ',fninfo
   if not exists(fninfo) then begin
      print,'info file not found.'
      return
   endif
   readcol,fninfo,fn,jd,nsrc,ngood,fwhm,sky,skysig,good, $
      format='a,d,i,i,f,f,f,i',count=nlines
   if nlines eq 0 then begin
      print,'No valid information found in file.'
      return
   endif
   print,strn(nlines),' entries found.'

   if strpos(fn[0],'_') ge 0 then begin
      words=strsplit(fn[0],'_',/extract)
      fno=strmid(fn,strlen(words[0])+1,5)
      fno=fix(fno)
   endif else begin
      fno=fix(fn)
   endelse

   z=where(good eq 1)
   avgfwhm=mean(fwhm[z])
   medsrc=long(median(nsrc))

   sug_binfac= floor(avgfwhm/3.0) > 1

   if not exists('Plots') then file_mkdir,'Plots'

   if binfac eq 1 then btag='' else btag='_'+strn(binfac)
   fn1='Plots/'+team+'_fwhm'+btag+'.png'
   fn2='Plots/'+team+'_sky'+btag+'.png'
   fn3='Plots/'+team+'_stars'+btag+'.png'

   setwin,1,xsize=1024,ysize=768
   plot,fno,fwhm,xtitle='Frame number',ytitle='FWHM (pixels)', $
      background='ffffff'xl,color=0,charsize=2, $
      title='mean FWHM '+strn(avgfwhm,format='(f10.1)')
   if binfac eq 1 then begin
      xyouts,0.2,0.9,'Suggested binning factor is '+strn(sug_binfac), $
         /normal,color=cpalette(1),charsize=2
   endif else begin
      xyouts,0.2,0.9,'BINFAC='+strn(binfac), $
         /normal,color=cpalette(1),charsize=2
   endelse
   xyouts,0.2,0.87,'min FWHM '+strn(min(fwhm),format='(f10.1)')+ $
      ', max FWHM '+strn(max(fwhm),format='(f10.1)'), $
      /normal,color=cpalette(1),charsize=2
   setwin,2,xsize=1024,ysize=768
   plot,fno,sky,xtitle='Frame number',ytitle='Sky (DN)', $
      background='ffffff'xl,color=0,charsize=2, $
      title='mean sky '+strn(mean(sky),format='(f10.1)')
   setwin,3,xsize=1024,ysize=768
   plot,fno,nsrc,psym=8,yr=[0,max(nsrc)], $
      background='ffffff'xl,color=0,charsize=2, $
      xtitle='Frame number',ytitle='Number of sources detected', $
      title=team+'  '+strn(fix(total(good)))+' good frames out of '+strn(nlines)
   oplot,fno,ngood,psym=8,color=cpalette(2)
   xyouts,0.2,0.9,'median number of sources '+strn(medsrc), $
      /normal,color=cpalette(1),charsize=2

   tvgrab,fn1,1,/png
   tvgrab,fn2,2,/png
   tvgrab,fn3,3,/png

end
