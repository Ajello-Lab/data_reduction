;+
; NAME:
;  occtargfit
; PURPOSE:   (one line only)
;  PSF fitting of the occultation target star
; DESCRIPTION:
; CATEGORY:
;  Occultations
; CALLING SEQUENCE:
; INPUTS:
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
; OUTPUTS:
; KEYWORD OUTPUT PARAMETERS:
; CONFIGURATION:
;   File: config.ini
;   [global]
;     event   - string with short ID name of event (ex: OR20200914)
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
;-
pro occtargfit,MAXTODO=maxtodo,SHOWIDX=showidx

   pname='occtargfit'
   self=pname+': '
   if badpar(maxtodo,[0,2,3],0,caller=self+'(MAXTODO) ',default=0) then return
   if badpar(showidx,[0,2,3],0,caller=self+'(SHOWIDX) ',default=-1) then return

   if exists('occtargfitdone.ctl') then file_delete,'occtargfitdone.ctl'

   cleanup=1
   gulp=120

   openmysql,dblun,'occlc'
   c=','

   ; this gives a (hopefully) unique string to identify this process.
   ;   used to facilitate multiple cpus/machines running on the fitting
   ;   at one time.
   hosttag=idstring(seed)

   done=0
   ndone=0L

   lastfile=''
   lastteam=''
   lastevent=''
;   getddir,cinfo,root
   loadini,cinfo
   getvalue,cinfo,'global','event',event

   repeat begin
      if showidx lt 0 then begin
         cmd=['update target set status='+quote('C:'+hosttag), $
                 'where status='+quote('refit'), $
                 'and event='+quote(event), $
                 'order by idx limit '+strn(gulp)+';']
         print,cmd
         mysqlcmd,dblun,cmd

         cmd=['select info.idx,target.idx,team,stemdir,filename,', $
               'fwhm,x,y,binfac', $
              'from info,target', $
              'where status='+quote('C:'+hosttag), $
              'and info.idx=target.imidx', $
              ';']
         fitit=1
      endif else begin
         cmd=['select info.idx,target.idx,team,stemdir,filename,', $
               'fwhm,x,y,binfac', $
              'from info,target', $
              'where target.idx='+strn(showidx), $
              'and info.idx=target.imidx', $
              ';']
         fitit=0
      endelse

      print,cmd
      mysqlquery,dblun,cmd,idx,taidx,team,stemdir,filename,fwhm, $
         x,y,binfac, $
         format='l,l,a,a,a,f,i,i,a,a,i',ngood=nhit
      done = nhit eq 0
      if done then continue
      xref=round(x)
      yref=round(y)
      if not fitit then print,taidx,idx,xref,yref

      for i=0,nhit-1 do begin
         if lastteam ne team[i] then begin
            getddir,cinfo,root,team=team[i]
         endif
         if filename[i] ne lastfile or team[i] ne lastteam then begin
            ddir=root+stemdir[i]
            psfdir='PSF/'+team[i]+'/'

            if binfac[i] eq 1 then begin
               fnpsf=psfdir+filename[i]+'.psf'
               fnim=ddir+filename[i]
            endif else begin
               fnpsf=psfdir+filename[i]+'.'+strn(binfac[i])+'.psf'
               fnim=ddir+'binned_'+strn(binfac[i])+'/'+filename[i]
            endelse

            if not fitit then print,fnim

            im=float(readfits(fnim,hdr))
            ;exptime=double(sxpar(hdr,'GPS_ExpU'))*1.0d-6
            exptime=double(sxpar(hdr,'EXPTIME'))
            backsub,im,/row
            psf=readfits(fnpsf)
            psf=psf/total(psf)

            lastfile=filename[i]
            lastteam=team[i]
         endif

         if not fitit then begin
            showsrc,im,window=9,hisig=10
            tvscl,psf
         endif

         x1 = xref[i]-round(fwhm[i])
         x2 = xref[i]+round(fwhm[i])
         y1 = yref[i]-round(fwhm[i])
         y2 = yref[i]+round(fwhm[i])

         if not fitit then print,fwhm[i],x1,x2,y1,y2
         if not fitit then print,im[xref[i],yref[i]]

         psffit2,im,psf,[x1,x2,y1,y2],xref[i],yref[i],counts, $
            chisq=chisq,meansky=0.,skyerr=0.,exptime=exptime, $
            gain=0.002,rdnoise=1.5,silent=(fitit eq 1)

         if showidx ge 0 then begin
            cleanup=0
            ndone=0
            goto,bailout
         endif

         if not finite(counts) then counts=0.0

         if counts gt 0 then $
            mag = -2.5 * alog10(counts) + 24.0 $
         else $
            mag = 99.999

         cmd=['update target', $
              'set', $
              'counts='+strn(counts,format='(f20.3)')+c, $
              'mag='+strn(mag,format='(f10.4)')+c, $
              'chisq='+strn(chisq,format='(f10.2)')+c, $
              'status='+quote('auto'), $
              'where idx='+strn(taidx[i]), $
              ';']

         print,cmd
         mysqlcmd,dblun,cmd

         ndone++

         if maxtodo gt 0 and ndone ge maxtodo then begin
            print,'Early termination from MAXTODO'
            goto,bailout
         endif

      endfor

      if exists('targetfitdone.ctl') then goto,bailout

   endrep until done
   cleanup=0

bailout:
   if cleanup then begin
      cmd='update target set status='+quote('refit')+ $
          ' where status='+quote('C:'+hosttag)+';'
      print,'Cleanup from early termination'
      print,cmd
      mysqlcmd,dblun,cmd
   endif
   if ndone gt 0 then begin
      action='Completed '+strn(ndone)+' target fits - C:'+hosttag
      jdcur=systime(/julian,/ut)
      jdstr,jdcur,300,jds
      cmd=['insert into history set', $
           'posted='+jds+c, $
           'event='+quote(event)+c, $
           'tool='+quote(pname)+c, $
           'action='+quote(action)+';']
      print,cmd
      mysqlcmd,dblun,cmd
   endif
   free_lun,dblun

end
