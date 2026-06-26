;+
; NAME:
;  occloadstars
; PURPOSE:   (one line only)
;  Load stars for each image in an occultation dataset into the database
; DESCRIPTION:
; CATEGORY:
;  Occultations
; CALLING SEQUENCE:
;  occloadstars,team
; INPUTS:
;  team - string for the team name (must match directories)
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
; OUTPUTS:
; KEYWORD OUTPUT PARAMETERS:
; CONFIGURATION:
;   File: config.ini
;   [global]
;     event   - string with short ID name of event (ex: OR20200914)
;     dir     - relative directory for this data set added to the root dir
;     date    - YYYY-MM-DD both date and directory name part of path
;   [TEAM]   - name of the team (input variable)
;     dirtime - HH_MM_SS both starting time and directory name part of path
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
;  2021/01/18, MWB, added new handling of binfac
;-
pro occloadstars,team,binfac

   pname='occloadstars'
   self=pname+': '
   if badpar(team,7,0,caller=self+'(team) ') then return
   if badpar(binfac,[2,3],0,caller=self+'(binfac) ') then return

   getddir,cinfo,ddir,team=team
   getvalue,cinfo,'global','event',event
   getvalue,cinfo,'global','date',date
   getvalue,cinfo,team,'dirtime',dirtime
   if not exists(ddir) then begin
      print,ddir
      print,'Data directory does not exist'
      return
   endif
   print,ddir

   openmysql,dblun,'occlc'
   c=','

   cmd=['select idx,stemdir,filename', $
        'from info', $
        'where team='+quote(team), $
        'and binfac='+strn(binfac), $
        'and event='+quote(event)+';']
   mysqlquery,dblun,cmd,idx,stemdir,fn,format='l,a,a',ngood=nfiles
   ddir=ddir+stemdir

   dirty=0
   nfupdate=0
   nstarsadded=0
   if nfiles eq 0 then begin
      print,'No files found in database for team ',team,' and event ',event
      goto,bailout
   endif

   print,strn(nfiles),' source files found to be loaded.'

   for i=0,nfiles-1 do begin

      cmd='select count(*) from stars where imidx='+strn(idx[i])+';'
      mysqlquery,dblun,cmd,ncheck,format='l'
      if ncheck ne 0 then begin
         print,fn[i]
         print,'already loaded in stars, skipping'
         continue
      endif

      if binfac eq 1 then begin
         fnsrc='Src/'+team+'/'+fn[i]+'.src'
         fnim=ddir[i]+fn[i]
      endif else begin
         fnsrc='Src/'+team+'/'+fn[i]+'.'+strn(binfac)+'.src'
         fnim=ddir[i]+'binned_'+strn(binfac)+'/'+fn[i]
      endelse
      if not exists(fnsrc) then begin
         print,'Source file ',fnsrc,' not found.'
         goto,bailout
      endif
      print,fnsrc
      rdsource,fnsrc,sinfo

      print,fnim
      if not exists(fnim) then begin
         print,'Image file not found, index=',strn(idx[i])
         goto,bailout
      endif
      im=float(readfits(fnim,hdr))

      dirty=1
      xref=round(sinfo.xpos)
      yref=round(sinfo.ypos)
      obslat=double(sxpar(hdr,'OBSLAT'))
      obslon=double(sxpar(hdr,'OBSLONG'))
      obsalt=double(sxpar(hdr,'GPS_H'))
      cmd=['update info set', $
           'lat='+strn(obslat,format='(f20.10)')+c, $
           'lon='+strn(obslon,format='(f20.10)')+c, $
           'alt='+strn(obsalt,format='(f20.10)'), $
           'where idx='+strn(idx[i]), $
           ';']
      mysqlcmd,dblun,cmd
      nfupdate++
      if sinfo.nsrcs gt 0 then begin

         for j=0,sinfo.nsrcs-1 do begin
            cmd=['insert into stars set', $
                 'event='+quote(event)+c, $
                 'imidx='+strn(idx[i])+c, $
                 'xref='+strn(xref[j])+c, $
                 'yref='+strn(yref[j])+c, $
                 'peak='+strn(im[xref[j],yref[j]],format='(f10.1)')+c, $
                 'status='+quote('needpsf')+';']
            mysqlcmd,dblun,cmd
         endfor
         nstarsadded=nstarsadded+sinfo.nsrcs

      endif else begin
         cmd='update info set qual='+quote('bad')+'where idx='+strn(idx[i])+';'
         mysqlcmd,dblun,cmd
      endelse

   endfor

bailout:
   if dirty then begin
      action=team+'['+strn(binfac)+']: Loaded '+strn(nstarsadded)+ $
             ' stars from '+strn(nfupdate)+' images.'
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
