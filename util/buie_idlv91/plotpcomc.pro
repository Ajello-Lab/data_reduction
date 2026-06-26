;+
; NAME:
;  plotpcomc
; PURPOSE:   (one line only)
;  Plot model fit residual files for Pluto-Charon map fitting
; DESCRIPTION:
;  The goal is to plot only the data from the same apparition or same mutual
;    event on the same page.  The input file is a little complex.
;  This program will plot Pluto-Charon photometric data with the model
;    predictions.  The data file for input to this program is generate by
;    a run of the spot modeling program SPOTS.  The input file contains
;    several parts.  The first line is always an identification line so
;    that the program can intelligently disregard old or incorrect format
;    files.  There are three keywords that will appear in the rest of the
;    file:  VALUES, TEXT, and DATA.  VALUES contain one number per line
;    plus a short description of the value.  These are values intended for
;    the use of this program and is made to be easy to read by the
;    program.  TEXT indicates that 'free form text' follows.  This is the
;    information that will appear in the annotation window on the hardcopy
;    plots.  This program makes now attempt to understand the information
;    contained within, it is intended to be easily read by a human looking
;    at the plot.  Within this window there are three fields of
;    information.  Column 1 is an attribute, of which there are 3: s -
;    skip a line, l - left justify a line, and c - center line.  Column 2
;    contains a single digit number.  s# ==> skip # lines.  l# ==> left
;    justify with # chars indentation.  # is ignored if line is to be
;    centered.  Column 3 thru 42 contain the text.  Anything beyond 42 is
;    silently ignored.  The end of text is flagged by DATA appearing in
;    the text window.  For this reason all of the keywords begin in column
;    three, I didn't want to put in fancy code for parsing the input
;    line.  Once DATA begins it is to continue until the end of the file.
;  As the file is read the data are classified as out-of-eclipse or
;    mutual event data.  Data will be flagged as event data when the following
;    conditions are met: 1) longitude is within 10 degrees of either 0 or 180
;    degrees and 2) two points that fall within 1 hour of each other.  All
;    subsequent data will be taken as part of the event until the time jumps
;    by more than 6 hours.  If the data is not recognized as event data, then
;    it is taken to be out-of-eclipse photometry and plotted against the
;    entire longitude range.  Event data, on the other hand, is plotted
;    against time.
; CATEGORY:
;  2-D plotting
; CALLING SEQUENCE:
;  plotpcomc,file
; INPUTS:
;  file - name of residual file
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
; OUTPUTS:
;  Screen grabs of each plot (there will be many).
; KEYWORD OUTPUT PARAMETERS:
; CONFIGURATION:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  Written by Marc W. Buie, Southwest Research Institute, 2023/05/08,
;    cloned from an ancient program written in Fortran using the Mongo
;    plotting library
;-
pro plotpcomc,file

   self='file: '
   if badpar(file,7,0,caller=self+'(file) ') then return

   if nofile(file,'input data file') then return

   words=strsplit(file,'.',/extract)
   root=words[0]
   pdir=root+'p'
   if not exists(pdir) then file_mkdir,pdir
   pdir=addslash(pdir)

   openr,lun,file,/get_lun

   tag=''
   readf,lun,tag,format='(2x,a)'
   if tag ne 'SPOTS v0' then begin
      print,'Error! the input file is not valid for this program'
      goto,bailout
   endif

   repeat begin
      readf,lun,tag,format='(2x,a)'
      if eof(lun) then goto,bailout
   endrep until tag eq 'VALUES'

   nfloat=0L
   readf,lun,nfloat,format='(2x,i)'

   repeat begin
      readf,lun,tag,format='(2x,a)'
      if eof(lun) then goto,bailout
   endrep until tag eq 'TEXT'

   ntext=0
   flag=''
   num=0
   att=[]
   arg=[]
   info=[]
   repeat begin
      readf,lun,flag,num,tag,format='(a1,i1,a)'
      if tag ne 'DATA' then begin
         att=[att,flag]
         arg=[arg,num]
         info=[info,tag]
      endif
      if eof(lun) then goto,bailout
   endrep until tag eq 'DATA'
   ninfo=n_elements(info)

   jd=[]
   clat=[]
   clong=[]
   mag=[]
   err=[]
   model=[]
   omc=[]

   jd0=0.0d0
   clat0=0.0
   clong0=0.0
   mag0=0.0
   err0=0.0
   model0=0.0
   omc0=0.0

   while not eof(lun) do begin
      readf,lun,jd0,clat0,clong0,mag0,err0,model0,omc0, $
         format='(F13.5,1X,5X,1X,F9.5,1X,F9.5,1X,F7.4,1X,F6.4,1X,F7.4,1X,F7.4)'
      jd=[jd,jd0]
      clat=[clat,clat0]
      clong=[clong,clong0]
      mag=[mag,mag0]
      err=[err,err0]
      model=[model,model0]
      omc=[omc,omc0]
   endwhile
   npts=n_elements(jd)
   free_lun,lun
   print,strn(npts),' data point read from file'

   allchi = total((omc/err)^2)
   chired = allchi/float(npts-nfloat)
   allscat = total(abs(omc))/float(npts)
   sqrtchir = sqrt(chired)
   jdstr,jd,0,jds
   jd2year,jd,year
   year=fix(year)
   print,allchi,' Weighted chisq'
   print,chired,' Reduced chisq'
   print,allscat,' post-fit scatter [mag]'

   dt=[jd[1:*]-jd[0:-2],0.]*24.0
   backdt=[0.,dt]
   forwarddt=[dt,0.]

   setnum=intarr(npts)
   me=bytarr(npts)

   z=where(abs(forwarddt) lt 0.04)
   mestart = z[0]
   print,strn(mestart),' first close point'
   me[z[0]:*] = 1

   yrs=year[0:mestart-1]
   rotyears = yrs[uniq(yrs,sort(yrs))]
   nrot=n_elements(rotyears)

   set=0
   for i=mestart,npts-1 do begin
      setnum[i] = set
      if forwarddt[i] gt 6.0 then set++
   endfor
   nsets = max(setnum)+1

;   for i=0,npts-1 do begin
;      if i lt 380 then continue
;      print,jds[i],min([backdt[i],forwarddt[i]]),me[i],clong[i],clat[i], $
;         year[i],setnum[i], $
;         format='(a,1x,f5.1,1x,i1,1x,f5.1,1x,f5.1,1x,i4,1x,i2)'
;if i eq 450 then break
;   endfor

   ; Handle exceptionns for grouping
   z=where(year eq 1954,count)
   if count ne 0 then year[z]=1955
   z=where(year eq 1971,count)
   if count ne 0 then year[z]=1972
   z=where(year eq 1973,count)
   if count ne 0 then year[z]=1972

   ; Annual Rotational lightcurves
   setwin,0,xsize=512,ysize=512
   print,'----------lightcurves---------------'
   for i=0,nrot-1 do begin
      z=where(me eq 0 and year eq rotyears[i],count)
      if count eq 0 then continue
      chi = total((omc[z]/err[z])^2)
      chir = chi/float(count)
      resid= total(omc[z])/float(count)
      scat = total(abs(omc[z]))/float(count)
      yr=maxmin([mag[z]+err[z],mag[z]-err[z],model[z]])
      ploterror,clong[z],mag[z],err[z],psym=8,xr=[0,360],yr=yr, $
         title=strn(rotyears[i])
      oplot,clong[z],model[z],color=cpalette(1),psym=4
      print,rotyears[i],count,chi,chir,resid,scat,chi/allchi,chi/allchi/count, $
         format='(i4,7x,i3,1x,f7.1,1x,f6.2,2(1x,f6.3),3x,f5.3,1x,f8.4)'
      fnpng=strn(rotyears[i])+'_lc.png'
      tvgrab,pdir+fnpng,0,/png
   endfor

   ; Mutual events

   print,'----------mutual-events-------------'
   print,'Number of mutual events',nsets
   for i=0,nsets-1 do begin
      z=where(me eq 1 and setnum eq i,count)
      jd0=double(long(jd[z[0]]-0.5d0))+0.5d0
      jdstr,jd0,100,jd0s,timesep='-'
      time=(jd[z]-jd0)*24.0d0
      lon=clong[z]
      if max(lon)-min(lon) gt 180 then begin
         zz=where(lon gt 180,countzz)
         if countzz ne 0 then lon[zz]=lon[zz]-360.0d0
      endif
      chi = total((omc[z]/err[z])^2)
      chir = chi/float(count)
      resid= total(omc[z])/float(count)
      scat = total(abs(omc[z]))/float(count)
      ploterror,lon,mag[z],err[z],psym=8,yr=maxmin([mag[z],model[z]]), $
         title=jd0s,xr=maxmin(lon)
      oplot,lon,model[z],color=cpalette(1),psym=-8
      print,jd0s,count,chi,chir,resid,scat,chi/allchi,chi/allchi/count, $
         format='(a,1x,i3,1x,f7.1,1x,f6.2,2(1x,f6.3),3x,f5.3,1x,f8.4)'
      if min(lon) gt 90 then metype='S' else metype='I'
      fnpng=jd0s+'_'+metype+'.png'
      tvgrab,pdir+fnpng,0,/png
   endfor
goto,bailout

;   istart=0
;   time=dblarr(npts)
;   for i=0,npts-1 do begin
;      if istart eq i then begin
;         if (abs(clong[i]-180.0) lt 25.0 or $
;            clong[i] gt 335.0 or $
;            clong[i] lt 25.0) and clat[i] lt 10.0) then begin
;            event = 1
;            time[i] = (jd[i]-long(jd[i]-0.5d0)+0.5d0)*24.0d0
;         endif else begin
;            event = 0
;         endelse
;      endif else begin
;
;      endelse
;   endfor

   for i=0,ninfo-1 do begin
      print,info[i]
   endfor
   print,''
   print,'---------------------------------------------------'
   print,'  Total number of data points = ',strn(npts), $
         '  Scatter = ',strn(allscat,format='(f10.4)'),' mag'
   print,'Total chi-sq = ',strn(allchi,format='(f10.3)'),'  ', $
         'Reduced chi-sq = ',strn(chired,format='(f10.3)'),'  ', $
         'Reduced chi = ',strn(sqrtchir,format='(f10.3)')
   print,''
;   print,'Description       N    +  0  -      chi2   chi2r    chir   avgerr avgres  scat'

   return

bailout:
   free_lun,lun

end
