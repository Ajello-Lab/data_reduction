;+
; NAME:
;  kboplan
; PURPOSE:
;  KBO observing planning table generation
; DESCRIPTION:
;
; CATEGORY:
;  Astronomy
; CALLING SEQUENCE:
;  kboplan,objcode,obs,date
; INPUTS:
;  objcode - String array of standard object codes (see ephem.pro), if the
;              input is a single string with @ as the first character, then
;              the rest of the string is interpreted as a file to read the
;              object codes from.
;  obs     - Integer Marsden code of the observatory
;               688 - Lowell Observatory
;               500 - Geocentric
;               If you provide an invalid code, 688 is assumed.
;  date    - UT date and time vector near midnight for night,
;                 [year,month,day,hour]
;     
; OPTIONAL INPUT PARAMETERS:
;
; KEYWORD INPUT PARAMETERS:
;  FWHM    - Seeing to assume for the night, default=1.2 arcsec
;  OUTPUT  - String, if supplied, indicates the name of a file to save the
;               output to.  The default is to print the information on the
;               terminal.
;  AMLIMIT - Air mass limit for observations (default=2.5)
;  DESCRIPTION - Optional string array with a descriptive label for each
;                   object.  If supplied it should match the length of the
;                   objcode input array.
;  MINSEP  - Minimum motion desired between successive visits in arcsec.
;              default=3
;  OBSLIST - List of planned observations.  This is a string which contains
;              the file name.  In the file, the first blank-delimited field
;              is the name of the object.  This is the only thing read from
;              the file.  The rest of the line can be anything you like and
;              can be used to document the sequence.  If this is not supplied
;              the observing sequence output is not generated.
;
; OUTPUTS:
;
; KEYWORD OUTPUT PARAMETERS:
;
; COMMON BLOCKS:
;
; SIDE EFFECTS:
;
; RESTRICTIONS:
;
; PROCEDURE:
;
; MODIFICATION HISTORY:
;  Written by Marc W. Buie, Lowell Observatory, 1997/05/23
;  2015/05/15, MWB, Massive overhaul and upgrade for more functionality
;  2015/06/11, MWB, added FWHM keyword
;  2024/07/06, MWB, rework for new obs tools
;-
pro kboplan,objcode,in_obs,date,OUTPUT=output,AMLIMIT=amlimit,DESCRIPTION=desc, $
       MINSEP=minsep,OBSLIST=obslist,WATCHDIR=watchdir,KEYFILE=keyfile, $
       FWHM=fwhm,REFMAG=refmag,REFFWHM=reffwhm,REFEXP=refexp

   if n_params() eq 0 then begin
      print,'kboplan,objcode,obs,date'
      return
   endif

   self='KBOPLAN: '
   if badpar(objcode,7,[0,1],caller=self+'(objcode) ',npts=nobj) then return
   if badpar(in_obs,[2,3,7,8],[0,1],caller=self+'(obs) ',type=codetype) then return
   if badpar(date,[2,3,4,5],[0,1],caller=self+'(date) ', $
                                  npts=datelen) then return
   if badpar(output,[0,7],0,caller=self+'(OUTPUT) ', $
                            default='<screen>') then return
   if badpar(watchdir,[0,7],0,caller=self+'(WATCHDIR) ', $
                            default='') then return
   if badpar(keyfile,[0,7],0,caller=self+'(KEYFILE) ', $
                            default='[[DEFAULT]]') then return
   if badpar(amlimit,[0,2,3,4,5],0,caller=self+'(AMLIMIT) ', $
                                   default=2.5) then return
   if badpar(minsep,[0,2,3,4,5],0,caller=self+'(MINSEP) ', $
                                   default=3.0) then return
   if badpar(fwhm,[0,2,3,4,5],0,caller=self+'(FWHM) ', $
                                   default=1.2) then return
   if badpar(desc,[0,7],[0,1],caller=self+'(DESCRIPTION) ', $
                          default=replicate('',nobj),npts=ndesc) then return
   if badpar(obslist,[0,7],[0,1],caller=self+'(OBSLIST) ', $
                                 default='') then return
   if badpar(refmag,[0,2,3,4,5],0,caller=self+'(REFMAG) ', $
                                   default=23.0) then return
   if badpar(reffwhm,[0,2,3,4,5],0,caller=self+'(REFFWHM) ', $
                                   default=1.5) then return
   if badpar(refexp,[0,2,3,4,5],0,caller=self+'(REFEXP) ', $
                                   default=300.0) then return

   if ndesc ne nobj then begin
      print,self,'The DESCRIPTION array length must ', $
                 'match the length of the objcode array'
      return
   endif

   if codetype eq 8 then begin
      obs = in_obs
   endif else begin
      obsinfo,in_obs,obs
   endelse

   loadkeys,keyfile,keylist

   ; Normal Mosaic values
   ;refmag=23.0
   ;reffwhm=1.5
   ;refexp=300.0
   ;slewrate = 0.45 ; deg/second, Mayall
   ;guider=70.0 ; seconds
   ;readout=22.0 ; seconds Mosaic 1.1
   ;readout=44.0 ; seconds Mosaic 3

   readout=11.0
   guider=25.0 ; seconds
   slewrate = 0.90 ; deg/second

   ; Convert dates to JD.
   if datelen eq 4 then begin
      jdcnv,date[0],date[1],date[2],date[3],jd
   endif else if datelen eq 1 then begin
      jd=date
   endif else begin
      print,self,'length of date must be 1 or 4'
      return
   endelse

   duplicates=0
   for i=0,nobj-1 do begin
      z=where(objcode[i] eq objcode,count)
      if count ne 1 then begin
         duplicates=1
         print,objcode[i],' is duplicated in objcode input vector',count
         print,desc[z]
      endif
   endfor
   if duplicates then return

   if obslist ne '' then begin
      if not exists(obslist) then begin
         print,self,'obslist file ',obslist,' not found.  Aborting.'
         return
      endif
      readcol,obslist,objlist,format='a',count=nlist
      objlist='A'+objlist
      for i=0,nlist-1 do begin
         z=where(objlist[i] eq objcode,count)
         if count eq 0 then begin
            print,'WARNING! object ',objlist[i],' is in OBSLIST and not objcode'
            objcode=[objcode,objlist[i]]
            desc=[desc,'EXTRA']
            nobj++
         endif
      endfor
   endif else begin
      objlist=['']
      nlist=0
   endelse

   if output eq '<screen>' then begin
      print,'Observatory ',obs.name,' selected.'
   endif else begin
      openw,lun,output,/get_lun
      printf,lun,'Observatory ',obs.name,' selected.'
   endelse

   ; Set the critical altitude and airmass for observability.
   crital = 0.5*!pi - acos(1.0/amlimit)

   ; Compute ephemeris for object
   ephem,replicate(jd,nobj),obs,23,objcode,eph
   ra=trimrank(eph[0,*])
   dec=trimrank(eph[1,*])
   ephem,replicate(jd+1.0d0/24.0d0,nobj),obs,23,objcode,eph1
   ra1=trimrank(eph1[0,*])
   dec1=trimrank(eph1[1,*])
   ssgeom,eph[0:7,*],sun,earth,phang,elong
   ;idx=sort(sun)
   idx=sort(eph[0,*])
   motion=angsep(ra,dec,ra1,dec1)*!radeg*3600.0
   time = 2*minsep/motion

   ; Sun position at input JD
   sunpos,jd,sunra,sundec,/radian
   moonpos,jd,moonra,moondec,/radian

   ; Define night, Sun set to sun rise.
   am  = airmass(jd,sunra,sundec,obs.lat,obs.wlon,alt=alt,lha=lha,lst=lst)
   hatojd,!dpi,sunra,lst,jd,jdlclmid ; jd of nearest local midnight
   lsidtim,jdlclmid,obs.wlon,midlst       ; LST at local midnight
   jdofmid = float(long(jdlclmid+0.5d0))-0.5d0
   jdstr,jdofmid,100,thisdate

   ; Hour angle of Sun at sunset, AT, NT, CT
   altoha,-18.0/!radeg,sundec,obs.lat,sunatha,sunattype
   altoha,-12.0/!radeg,sundec,obs.lat,sunntha,sunnttype
   altoha,-6.0/!radeg,sundec,obs.lat,sunctha,suncttype
   altoha,-0.5/!radeg,sundec,obs.lat,sunhorzha,sunhorztype

   ; JD of sunset/sunrise, AT, NT, CT
   jdatset  = jdlclmid - (!dpi-sunatha)/2.0d0/!dpi
   jdatrise = jdlclmid + (!dpi-sunatha)/2.0d0/!dpi
   jdntset  = jdlclmid - (!dpi-sunntha)/2.0d0/!dpi
   jdntrise = jdlclmid + (!dpi-sunntha)/2.0d0/!dpi
   jdctset  = jdlclmid - (!dpi-sunctha)/2.0d0/!dpi
   jdctrise = jdlclmid + (!dpi-sunctha)/2.0d0/!dpi
   jdsset   = jdlclmid - (!dpi-sunhorzha)/2.0d0/!dpi
   jdsrise  = jdlclmid + (!dpi-sunhorzha)/2.0d0/!dpi

   jdstr,jdsset,-12,jdssetstr
   jdstr,jdsrise,-12,jdsrisestr
   jdstr,jdatset,-12,jdatsetstr
   jdstr,jdatrise,-12,jdatrisestr
   jdstr,jdntset,-12,jdntsetstr
   jdstr,jdntrise,-12,jdntrisestr
   jdstr,jdlclmid,-12,jdlclmidstr

   ; Moonrise/set
   obswind,midlst,obs.lat,moonra,moondec,jdatrise,jdatset, $
      moonrtime,moonrkind,moonstime,moonskind,moontype
   mphase,jdlclmid,moonphase
   jdstr,moonrtime,-12,jdmrisestr
   jdstr,moonstime,-12,jdmsetstr

   if moontype eq 2 or moontype eq -1 then begin
      moonstr='Moon is not up this night'
      moonrkind=''
      moonskind=''
   endif else if moontype eq 1 then begin
      moonstr='Moon is up all night'
   endif else begin
      moonstr=''
   endelse

   header = 'Object           N '+ $
            ' Rise/Set    '+ $
            ' RA    Dec'+ $
            '  rate    perr   '+ $
            ' V    '+ $
            'Tran   X   '+ $
            '  r    d    ph   sel mel sinc  arc nobs'

   nstr1=' Sset   NT    AT  '
   nstr2=strmid(jdssetstr,12)+' '+strmid(jdntsetstr,12)+' '+ $
         strmid(jdatsetstr,12)
   if moonrtime lt jdlclmid and  moonrkind eq 'rises' then begin
      nstr1 += 'Mrise '
      nstr2 += ' '+strmid(jdmrisestr,12)
   endif
   if moonstime lt jdlclmid and  moonskind eq 'sets' then begin
      nstr1 += ' Mset '
      nstr2 += ' '+strmid(jdmsetstr,12)
   endif
   nstr1 += 'Midni '
   nstr2 += ' '+strmid(jdlclmidstr,12)
   if moonrtime ge jdlclmid and  moonrkind eq 'rises' then begin
      nstr1 += 'Mrise '
      nstr2 += ' '+strmid(jdmrisestr,12)
   endif
   if moonstime ge jdlclmid and  moonskind eq 'sets' then begin
      nstr1 += ' Mset '
      nstr2 += ' '+strmid(jdmsetstr,12)
   endif
   nstr1 += '  AT  '
   nstr2 += ' '+strmid(jdatrisestr,12)
   nstr1 += 'Srise'
   nstr2 += ' '+strmid(jdsrisestr,12)

   if output eq '<screen>' then begin
      print,''
      print,'Night centered on UT:',jdlclmidstr, $
            (jdatrise-jdatset)*24.0,'hours long', $
            format='(a,1x,a,", ",f4.1,1x,a)'
      print,''
      print,nstr1
      print,nstr2
      print,''
      if moonstr ne '' then print,moonstr
      print,'Lunar phase is ',round(moonphase*100),format='(a,i3,"%")'
      print,''
      print,header
   endif else begin
      printf,lun,''
      printf,lun,'Night centered on UT: ',jdlclmidstr
      printf,lun,''
      printf,lun,nstr1
      printf,lun,nstr2
      printf,lun,''
      if moonstr ne '' then printf,lun,moonstr
      printf,lun,'Lunar phase is ',round(moonphase*100),format='(a,i3,"%")'
      printf,lun,''
      printf,lun,header
   endelse

   spawn,'getinfo',unit=pipe

   rmag=fltarr(nobj)
   for j=0,nobj-1 do begin
      i = idx[j]
      printf,pipe,objcode[i]
      obname=''
      readf,pipe,obname,h,g,format='(a32,1x,f6.2,1x,f5.2)'
      if strmid(obname,0,5) eq 'XXXXX' then begin
         obname = objcode[i]
         h=40
         g=0.2
      endif
      if sun[i] eq 0. then begin
         print,objcode[i],' not recognized'
         goto,bailout
      endif
      disphase,0.,sun[i],earth[i],phang[i],g,hmag
      rmag[i] = h-hmag - 0.3   ; assumes (V-R) = 0.3

      melong=sphdist(eph[0,i],eph[1,i],moonra,moondec)*180.0d0/!dpi

      am  = airmass(jd,sunra,sundec,obs.lat,obs.wlon,alt=alt,lha=lha,lst=lst)
      hatojd,!dpi,sunra,lst,jd,jdlclmid
      lsidtim,jdlclmid,obs.wlon,midlst
      hatojd,0.0d0,eph[0,i],midlst,jdlclmid,jdtrans
      jdstr,jdtrans,-12,objtransstr
      transam = airmass(jdtrans,eph[0,i],eph[1,i],obs.lat,obs.wlon,alt=transalt)
      altoha,crital,eph[1,i],obs.lat,horzha,type
      if type eq 0 then begin
         jdrise  = jdtrans - horzha/2.0d0/!dpi
         jdset   = jdtrans + horzha/2.0d0/!dpi
;         jdstr,jdrise,-12,objrisestr
;         jdstr,jdset,-12,objsetstr
      endif
      obswind,midlst,obs.lat,eph[0,i],eph[1,i],jdatrise,jdatset,rtime,rkind, $
         stime,skind,objtype
      if objtype eq -1 or objtype eq 2 then continue

      len = (stime-rtime)*24.0 ; hours
      if len lt time[i] then continue

      epherr = eph[20,i]
      jdlastast=eph[22,i]
      arclen=round(eph[23,i]/365.25)
      nobs=long(eph[24,i])
      timetolast=jdatset-jdlastast
      if timetolast lt 99 then begin
         sincestr=string(round(timetolast),format='(i3)')+'d'
      endif else if timetolast lt 180 then begin
         sincestr=string(round(timetolast/30.0),format='(i3)')+'m'
      endif else begin
         sincestr=string(round(timetolast/365.25),format='(i3)')+'y'
      endelse

      if epherr gt 1.0e10 then epherr = -1.0
;          'f4.1,1x,f5.1,2x,f6.1,2x,f5.1,2x,'+ $
;          'f4.1,1x,f5.1,2x,e13.5,2x,f5.1,2x,'+ $

      fmt='(a-16,1x,a,1x,a,1x,a,2x,' + $
          'f4.1,1x,f5.1,2x,f4.1,2x,f6.1,2x,f5.1,2x,'+ $
          'a,1x,f3.1,2x,'+ $
          'f4.1,1x,f4.1,1x,f5.1,2x,i3,1x,i3,1x,a,1x,i3,"y",1x,i4)'

      if objtype ne -1 and objtype ne 2 then begin
         jdstr,rtime,-12,risestr
         jdstr,stime,-12,setstr
         z=where(objcode[i] eq objlist,count)
         if count eq 0 then obstag=' ' else obstag=strn(count)
         info=string(obname,obstag,strmid(risestr,12,5),strmid(setstr,12,5), $
                     eph[0,i]*!radeg/15.0,eph[1,i]*!radeg, $
                     motion[i],epherr, $
                     h-hmag, $
                     strmid(objtransstr,12,5),transam, $
                     sun[i],earth[i],phang[i], $
                     fix(ceil(elong[i])), $
                     fix(ceil(melong)), $
                     sincestr,arclen,nobs<9999, $
                     format=fmt)
         if desc[i] ne '' then info += ' '+desc[i]
      endif else begin
         info=string(strmid(obname,0,16),' object not up at night.')
      endelse

      if output eq '<screen>' then begin
         print,info
      endif else begin
         printf,lun,info
      endelse

   endfor

   magfac = 10.0^((refmag-rmag)/(-2.5))
   seefac = (fwhm/reffwhm)^2
   pexp = magfac*seefac*refexp
   pexp = ceil(pexp/10)*10 > 30

   nrep=replicate(0,nobj)
   jdlast=dblarr(nobj)

   if nlist gt 0 then begin
      if exists('linkup.dat') then begin
         readcol,'linkup.dat',donefn,donetype,doneobj,doneexp,donenum,donejd, $
            format='a,a,a,i,i,d',count=nlinked
      endif else begin
         nlinked=0
         doneobj=[]
         donetype=[]
         donenum=[]
         donefn=[]
         donejd=[]
         doneexp=[]
      endelse
      watchdir=addslash(watchdir)
      fitslist=file_search(watchdir+'*.fits',count=numfits)
      fitslist=strmid(fitslist,strlen(watchdir))
      for i=0,numfits-1 do begin
         if nlinked gt 0 then begin
            z=where(fitslist[i] eq donefn,count)
            if count gt 0 then continue
         endif
         hdr=headfits(watchdir+fitslist[i],exten=0)
         parsekey,hdr,keylist,hdrinfo
         words=strsplit(hdrinfo.object,' ',/extract)
         doneobj=[doneobj,words[0]]
         htype=strcompress(strtrim(sxpar(hdr,'OBSTYPE'),2))
         donetype=[donetype,htype]
         donejd=[donejd,hdrinfo.jd-(hdrinfo.exptime/2.0)/86400.0]
         doneexp=[doneexp,round(hdrinfo.exptime)]
         donenum=[donenum,0]
         donefn=[donefn,fitslist[i]]
         nlinked++
print,donefn[i],donetype[i],doneobj[i],doneexp[i],donenum[i],donejd[i], $
   format='(a,1x,a-8,1x,a-10,1x,i4,1x,i2,1x,f13.5)'
      endfor
      jdobs=dblarr(nlist)
      amobs=dblarr(nlist)
      objobs=strarr(nlist)
      alt=fltarr(nlist)
      azi=fltarr(nlist)
      print,''
      print,'Observing sequence'
      print,''
      print,'  N Exposure Start (UT) Object', $
            '    R   Exp  X      HA', $
            '    Slew Ovrhd #  Dis'
      curjd=jdatset
      excess_slew=0.0
      cumtimeslip=0L
      for i=0,nlist-1 do begin
         idx=trimrank(where(objlist[i] eq objcode,/null))
         nrep[idx]++

         zo=where(strmid(objlist[i],1) eq doneobj and nrep[idx] eq donenum, $
                  counto)
         if counto eq 1 then begin
            zo=zo[0]
            oinfo=donefn[zo]
            curjd=donejd[zo]
            exptime=doneexp[zo]
         endif else begin
            z=where(strmid(objlist[i],1) eq doneobj and donenum eq 0,count)
            if count gt 0 then begin
               donenum[z[0]] = nrep[idx]
               oinfo=donefn[z[0]]
               curjd=donejd[z[0]]
               exptime=doneexp[z[0]]
            endif else begin
               oinfo=''
               exptime=pexp[idx]
            endelse
         endelse


         jdstr,curjd,0,jds
         am=airmass(curjd,ra[idx],dec[idx],obs.lat,obs.wlon,lha=lha,alt=alt0,azi=azi0)
         alt[i]=alt0
         azi[i]=azi0
         if i eq 0 then begin
            lastra=ra[idx]
            lastdec=dec[idx]
         endif
         if am gt 2.5 then amtag='*' $
         else if am gt 2.0 then amtag='x' $
         else amtag=' '
         slew=angsep(ra[idx],dec[idx],lastra,lastdec)*!radeg
         overhead = ((slew/slewrate+guider) > readout)
         if i eq nlist-1 then overhead=0.0
         excess_slew += (slew/slewrate - readout) > 0
         hastr,lha,-2,lhas
         if i eq 0 then slew=0.
         moved=motion[idx]*(curjd-jdlast[idx])*24.0
         if nrep[idx] eq 1 then moved=0.
         if moved gt 0. and moved lt 3.0 then mtag='*' else mtag=' '
         if oinfo ne '' then begin
            if i ne 0 then begin
               timeslip=curjd-jdobs[i-1]-(exptime+overhead)/86400.0d0
            endif else begin
               timeslip=curjd-jdatset
            endelse
            timeslip=round(timeslip*24.0*3600.0)
            cumtimeslip+=timeslip
            oinfo=oinfo+string(timeslip,format='(i6)')+' sec '+ $
                        string(cumtimeslip,format='(i6)')+' cum'
         endif

         print,i+1,jds,strmid(objlist[i],1),rmag[idx],exptime,am,amtag,lhas, $
            slew,overhead,nrep[idx], $
            moved,mtag,desc[idx],oinfo, $
            format='(i3,1x,a,1x,a-8,1x,f4.1,1x,i3,1x,f4.2,' + $
                   'a,1x,a,1x,f5.1,1x,f5.1,1x,i1,1x,f4.1,a,1x,a-10,1x,a)'
         jdlast[idx]=curjd
         jdobs[i]=curjd
         objobs[i]=strmid(objlist[i],1)
         amobs[i]=am
         curjd += (exptime+overhead)/86400.0d0
         lastra=ra[idx]
         lastdec=dec[idx]
      endfor
      timeleft=(jdatrise-curjd)*24.0
      if timeleft gt 1. then begin
         print,timeleft,'hours left in night',format='(f3.1,1x,a)'
      endif else if timeleft gt 0. then begin
         print,timeleft*60.0,'minutes left in night',format='(f4.1,1x,a)'
      endif else begin
         print,'Plan is too long by',timeleft,'hours',format='(a,1x,f4.1,1x,a)'
      endelse
      print,'Extra time lost to long slews',excess_slew,' seconds'

      if nlinked gt 0 then begin
         openw,lun,'linkup.dat',/get_lun
         for i=0,nlinked-1 do begin
            printf,lun,donefn[i],donetype[i],doneobj[i], $
               doneexp[i],donenum[i],donejd[i], $
               format='(a,1x,a-8,1x,a-10,1x,i4,1x,i2,1x,f13.5)'
               format='(a,1x,a-8,1x,a-10,1x,i2,1x,f13.5)'
         endfor
         free_lun,lun
      endif

      setwin,0
      jd0=double(long(jdlclmid+0.5))-0.5
      jdstr,jd0,100,jd0s
      xr=([jdatset,jdatrise]-jd0)*24.0
      time=(jdobs-jd0)*24.0
      plot,time,amobs,psym=8,xr=xr,color='000000'xl, $
         background='ffffff'xl,xtitle='UT Time',ytitle='Airmass', $
         yr=maxmin(amobs)-[0,.1],charsize=2,title=jd0s+'  '+obs.name
      for i=0,nlist-1 do begin
         xyouts,time[i],amobs[i],'  '+objobs[i],align=0.,color='000000'xl, $
            orient=90.,charsize=1.2
      endfor

      setwin,1,xsize=1520,ysize=1300
      px=(90-alt*!radeg)/90*sin(azi)
      py=-(90-alt*!radeg)/90*cos(azi)
      ang=findgen(101)/100*!pi*2.0
      sf=1.1
      plot,px,py,psym=8,xstyle=5,ystyle=5,xr=[-0.80,0.80]*sf,yr=[-0.60,0.80]*sf, $
         xmargin=[0,0],ymargin=[0,0],position=[0.,0.,1.,1.],/iso, $
         color='000000'xl,background='ffffff'xl
      for dralt=10.0,70,10 do begin
         oplot,dralt/90*sin(ang),dralt/90*cos(ang),color=0
         xyouts,0,dralt/90,strtrim(string(fix(90-dralt)),2),align=0.5,color=0
         xyouts,0,-dralt/90,strtrim(string(fix(90-dralt)),2),align=0.5,orient=180,color=0
      endfor
      altlim=20.
      for draz=-180.,170,15 do begin
         dpx=[0.,(90-altlim)/90*sin(draz/!radeg)]
         dpy=[0.,-(90-altlim)/90*cos(draz/!radeg)]
         oplot,dpx,dpy,color=0
      endfor
      for draz=15,80,15 do begin
         xyouts,1.02*(90-altlim)/90*sin(draz/!radeg),-1.02*(90-altlim)/90*cos(draz/!radeg),strtrim(string(draz),2), $
            align=0.5,orient=draz,color=0
      endfor
      for draz=105,170,15 do begin
         xyouts,1.02*(90-altlim)/90*sin(draz/!radeg),-1.02*(90-altlim)/90*cos(draz/!radeg),strtrim(string(draz),2), $
            align=0.5,orient=draz,color=0
      endfor
      for draz=195,260,15 do begin
         xyouts,1.02*(90-altlim)/90*sin(draz/!radeg),-1.02*(90-altlim)/90*cos(draz/!radeg),strtrim(string(draz),2), $
            align=0.5,orient=draz,color=0
      endfor
      for draz=285,350,15 do begin
         xyouts,1.02*(90-altlim)/90*sin(draz/!radeg),-1.02*(90-altlim)/90*cos(draz/!radeg),strtrim(string(draz),2), $
            align=0.5,orient=draz,color=0
      endfor
      xyouts,0.80,0,'W',align=0.5,orient=90,color=0
      xyouts,-0.80,0,'E',align=0.5,orient=270,color=0
      xyouts,0,0.80,'S',align=0.5,orient=0,color=0
      xyouts,0,-0.80,'N',align=0.5,orient=180,color=0
      for i=0,nlist-2 do begin
         oplot,px[i:i+1],py[i:i+1],color='0000ff'xl
      endfor
      for i=0,nlist-1 do begin
         xyouts,px[i],py[i],' '+strn(i+1),color=0
      endfor
   endif

bailout:
   free_lun,pipe

   if output ne '<screen>' then begin
      free_lun,lun
   endif

   z=where(nrep gt 0,count)
   if count ne 0 then print,strn(count),' unique objects observed.'

end
