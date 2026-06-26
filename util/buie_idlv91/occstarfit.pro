;+
; NAME:
;  occstarfit
; PURPOSE:   (one line only)
;  Fit a numerical PSF to each star image in an occultation dataset
; DESCRIPTION:
; CATEGORY:
;  Occultations
; CALLING SEQUENCE:
;  occstarfit
; INPUTS:
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;  MAXTODO - maixmum number to process, default is to do all
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
pro occstarfit,MAXTODO=maxtodo

   pname='occstarfit'
   self=pname+': '
   if badpar(maxtodo,[0,2,3],0,caller=self+'(MAXTODO) ',default=0) then return

   if exists('starfitdone.ctl') then file_delete,'starfitdone.ctl'

   cleanup=1
   gulp=300

   openmysql,dblun,'occlc'
   c=','

   ; this gives a (hopefully) unique string to identify this process.
   ;   used to facilitate multiple cpus/machines running on the fitting
   ;   at one time.
   hosttag=idstring(seed)

   done=0
   ndone=0

   lastfile=''
   lastteam=''
;   getddir,cinfo,root
   loadini,cinfo
   getvalue,cinfo,'global','event',event

   repeat begin
      cmd=['update stars set status='+quote('C:'+hosttag), $
              'where status='+quote('refit'), $
              'and event='+quote(event), $
              'order by imidx,idx limit '+strn(gulp)+';']
      print,cmd
      mysqlcmd,dblun,cmd

      cmd=['select info.idx,stars.idx,team,stemdir,filename,', $
           'fwhm,xref,yref,x,y,binfac', $
           'from info,stars', $
           'where status='+quote('C:'+hosttag), $
           'and info.idx=stars.imidx', $
           ';']

      print,cmd
      mysqlquery,dblun,cmd,idx,stidx,team,stemdir,filename, $
                           fwhm,xref,yref,x,y,binfac, $
                           format='l,l,a,a,a,f,i,i,a,a,i',ngood=nhit
      done = nhit eq 0
      if done then continue

      for i=0,nhit-1 do begin
         if lastteam ne team[i] then begin
            getddir,cinfo,root,team=team[i]
            lastteam=team[i]
            psfdir='PSF/'+team[i]+'/'
            ddir=root+stemdir[i]
         endif
         if filename[i] ne lastfile then begin

            if binfac[i] eq 1 then begin
               fnpsf=psfdir+filename[i]+'.psf'
               fnim=ddir+filename[i]
            endif else begin
               fnpsf=psfdir+filename[i]+'.'+strn(binfac[i])+'.psf'
               fnim=ddir+'binned_'+strn(binfac[i])+'/'+filename[i]
            endelse

            im=float(readfits(fnim,hdr))
            ;exptime=double(sxpar(hdr,'GPS_ExpU'))*1.0d-6
            exptime=double(sxpar(hdr,'EXPTIME'))
            backsub,im,/row
            psf=readfits(fnpsf)
            psf=psf/total(psf)

            lastfile=filename[i]

         endif

         if x[i] eq 'NULL' then startx=float(xref[i]) else startx=float(x[i])
         if y[i] eq 'NULL' then starty=float(yref[i]) else starty=float(y[i])

         x1 = xref[i]-round(fwhm[i])
         x2 = xref[i]+round(fwhm[i])
         y1 = yref[i]-round(fwhm[i])
         y2 = yref[i]+round(fwhm[i])

         psffit,im,psf,[x1,x2,y1,y2],startx,starty,xnew,ynew,counts, $
            chisq=chisq,meansky=0.,skyerr=0.,exptime=exptime,/silent

         if counts gt 0 then $
            mag = -2.5 * alog10(counts) + 24.0 $
         else $
            mag = 99.999

         cmd=['update stars', $
              'set', $
              'x='+strn(xnew,format='(f10.3)')+c, $
              'y='+strn(ynew,format='(f10.3)')+c, $
              'mag='+strn(mag,format='(f10.4)')+c, $
              'chisq='+strn(chisq<10000,format='(f10.2)')+c, $
              'status='+quote('auto'), $
              'where idx='+strn(stidx[i]), $
              ';']

         print,cmd
         mysqlcmd,dblun,cmd

         ndone++

         if ndone ge maxtodo and maxtodo ne 0 then goto,bailout

      endfor

      if exists('starfitdone.ctl') then goto,bailout

   endrep until done
   cleanup=0

bailout:
   if cleanup then begin
      cmd='update stars set status='+quote('refit')+ $
          ' where status='+quote('C:'+hosttag)+';'
      print,'Cleanup from early termination'
      print,cmd
      mysqlcmd,dblun,cmd
   endif
   if ndone gt 0 then begin
      action='Completed '+strn(ndone)+' star fits - C:'+hosttag
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
