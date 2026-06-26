;+
; NAME:
;  qhygpsavg
; PURPOSE:   (one line only)
;  Average one or more files of GPS log data from a QHY174GPS camera
; DESCRIPTION:
; CATEGORY:
;  Miscellaneous
; CALLING SEQUENCE:
;  qhygpsavg,date
; INPUTS:
;  date - string with a date to process (YYYY-MM-DD)
;             ignored if FILE keyword is provided
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;  DIR  - Directory to scan for data (default is current)
;  FILE - Specific list of files to process (overrides date input variable)
; OUTPUTS:
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  2018/11/25, Written by Marc W. Buie, Southwest Research Institute
;-
pro qhygpsavg,date,FILE=file,DIR=in_dir

   self='qhygpsavg: '
   if badpar(date,[0,7],0,caller=self+'(date) ',default='') then return
   if badpar(file,[0,7],[0,1],caller=self+'(FILE) ',default='') then return
   if badpar(in_dir,[0,7],0,caller=self+'(DIR) ',default='') then return


   if in_dir ne '' then dir=addslash(in_dir) else dir=in_dir

   if date eq '' and file[0] eq '' then begin
      print,self,'Error!  One of date or FILE must be specified'
      return
   endif

   if file[0] ne '' then begin
      fnlist=dir+file
      nfiles=n_elements(fnlist)
   endif else begin
      fnpatt=dir+'*_'+date+'T*.log'
      fnlist=file_search(fnpatt,count=nfiles)
      if nfiles eq 0 then begin
         print,self,'No files found for pattern',fnpatt
         return
      endif
   endelse

   jd=[]
   lat=[]
   lon=[]
   alt=[]
   nvals=0L
   ntotvals=0L
   for i=0,nfiles-1 do begin
      if not exists(fnlist[i]) then continue
      openr,lun,fnlist[i],/get_lun
      line=''
      readf,lun,line,format='(a)'
      while not eof(lun) do begin
         readf,lun,line,format='(a)'
         ntotvals++
         words=strtrim(strsplit(line,',',/extract),2)
         if n_elements(words) ge 12 then begin
            if words[1] eq 'Locked' then begin
               if nvals eq 0 then begin
                  jd=[jd,jdparse(words[0])]
                  lat=[lat,double(words[5])]
                  lon=[lon,double(words[6])]
                  alt=[alt,double(words[11])]
                  lastlat=words[5]
                  lastlon=words[6]
                  lastalt=words[11]
                  nvals++
               endif else if words[5] ne lastlat and words[6] ne lastlon $
                             and words[11] ne lastalt then begin
                  jd=[jd,jdparse(words[0])]
                  lat=[lat,double(words[5])]
                  lon=[lon,double(words[6])]
                  alt=[alt,double(words[11])]
                  lastlat=words[5]
                  lastlon=words[6]
                  lastalt=words[11]
                  nvals++
               endif
            endif
         endif else if n_elements(words) eq 9 or $
                       n_elements(words) eq 10 then begin
            if words[1] eq 'Locked' then begin
               if nvals eq 0 then begin
                  jd=[jd,jdparse(words[0])]
                  lat=[lat,double(words[5])]
                  lon=[lon,double(words[6])]
                  alt=[alt,0.]
                  lastlat=words[5]
                  lastlon=words[6]
                  nvals++
               endif else if words[5] ne lastlat and $
                             words[6] ne lastlon then begin
                  jd=[jd,jdparse(words[0])]
                  lat=[lat,double(words[5])]
                  lon=[lon,double(words[6])]
                  alt=[alt,0.]
                  lastlat=words[5]
                  lastlon=words[6]
                  nvals++
               endif
            endif
         endif
      endwhile
      free_lun,lun
      print,nvals,' ',fnlist[i]
   endfor

   if nvals eq 0 then begin
      print,self,'No valid data found.'
      return
   endif

   print,strn(ntotvals),' total samples, ',strn(nvals),' are unique.'

   bad=bytarr(nvals)
   robomean,lat,3.0,0.5,meanlat,dummy,latsigval,stdmean=latmsig,bad=bad
   robomean,lon,3.0,0.5,meanlon,dummy,lonsigval,stdmean=lonmsig,bad=bad
   robomean,alt,3.0,0.5,meanalt,dummy,altsigval,stdmean=altmsig,bad=bad
   robomean,lat,3.0,0.5,meanlat,dummy,latsigval,stdmean=latmsig,bad=bad
   robomean,lon,3.0,0.5,meanlon,dummy,lonsigval,stdmean=lonmsig,bad=bad
   z=where(bad eq 1,countbad)
   print,strn(countbad),' values rejected'
   z=where(bad eq 0,count)
   print,strn(count),' good values'

   print,'Time span of position is ',(max(jd[z])-jd[0])*24.0,' hours'
   jdstr,jd[0],0,jd0s
   jdstr,jd[-1],0,jd1s
   print,'Time: ',jd0s,' ',jd1s

   print,meanlat,latsigval,latmsig, $
      format='("Latitude ",1x,f11.6,2(1x,f8.6))'
   print,meanlon,lonsigval,lonmsig, $
      format='("Longitude",1x,f11.6,2(1x,f8.6))'
   print,meanalt,altsigval,altmsig, $
      format='("Altitude ",1x,f11.1,2(1x,f8.6))'

   setwin,0
   !p.multi=[0,1,3]
   plot,(jd[z]-jd[0])*24.0,(lat[z]-meanlat)*3600.0,psym=3, $
      background='ffffff'xl,color=0, $
      xtitle='Time from first sample (hours)', $
      ytitle='Latitute from mean (arcsec)', $
      charsize=1.5

   plot,(jd[z]-jd[0])*24.0,(lon[z]-meanlon)*3600.0,psym=3, $
      background='ffffff'xl,color=0, $
      xtitle='Time from first sample (hours)', $
      ytitle='Longitude from mean (arcsec)', $
      charsize=1.5

   plot,(jd[z]-jd[0])*24.0,alt[z]-meanalt,psym=3, $
      background='ffffff'xl,color=0, $
      xtitle='Time from first sample (hours)', $
      ytitle='Altitude from mean (m)', $
      charsize=1.5

   !p.multi=0

   setwin,1
   stats,(lat[z]-meanlat)*3600.0,nbins=100,/silent
   setwin,2
   stats,(lon[z]-meanlon)*3600.0,nbins=100,/silent
   setwin,3
   stats,(alt[z]-meanalt)*3600.0,nbins=100,/silent

end
