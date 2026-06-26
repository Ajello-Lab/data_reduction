;+
; NAME:
;  occloadinfo
; PURPOSE:   (one line only)
;  Load image information from an occultation dataset into the database
; DESCRIPTION:
; CATEGORY:
;  Occultations
; CALLING SEQUENCE:
;  occloadinfo,team
; INPUTS:
;  team - string for the team name (must match directories)
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;  BINFAC - binning factor, default=1, you only get to load one option
;  UPDATE - flag, set to load missing images from an earlier run
;            this routine never deletes or modifies anything already in
;            the database
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
;  2021/01/18, MWB, added new handling of binned data
;  2021/01/27, MWB, added support for stemdir in team stanza in config.ini
;  2023/04/04, MWB, added new logic for stemdir
;-
pro occloadinfo,team,BINFAC=binfac,UPDATE=update

   pname='occloadinfo'
   self=pname+': '
   if badpar(team,7,0,caller=self+'(team) ') then return
   if badpar(binfac,[0,2,3],0,caller=self+'(BINFAC) ', $
                           default=1) then return
   if badpar(update,[0,1,2,3],0,caller=self+'(UPDATE) ', $
                           default=0) then return

   if binfac eq 1 then begin
      fninfo=team+'_info.dat'
   endif else begin
      fninfo=team+'_'+strn(binfac)+'_info.dat'
   endelse

   if not exists(fninfo) then begin
      print,fninfo
      print,'information file not found.'
      return
   endif
   readcol,fninfo,fn,jd,nsrc,ngood,fwhm,sky,skysig,good, $
      format='a,d,i,i,f,f,f,i',count=nfiles
   print,strn(nfiles),' files loaded from ',fninfo

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

   if binfac ne 1 then dir=dir+'binned_'+strn(binfac)+'/'
   if not exists(dir) then begin
      print,dir
      print,'data directory does not exist'
      return
   endif

   openmysql,dblun,'occlc'
   c=','
   nload=0

   if not update then begin
      cmd=['select count(*) from info', $
           'where event='+quote(event), $
           'and binfac='+strn(binfac), $
           'and team='+quote(team)+';']
      mysqlquery,dblun,cmd,ncheck,format='i'
      if ncheck ne 0 then begin
         print,'Team ',team,', binfac=',strn(binfac), $
               ' has already been loaded for event ',event
         goto,bailout
      endif
   endif

   nprev=0
   for i=0,nfiles-1 do begin
      if update then begin
         cmd=['select count(*) from info', $
              'where event='+quote(event), $
              'and filename='+quote(fn[i]), $
              'and binfac='+strn(binfac), $
              'and team='+quote(team)+';']
         mysqlquery,dblun,cmd,ncheck,format='i'
         if ncheck ne 0 then begin
            nprev++
            continue
         endif
      endif
;      if good[i] eq 0 then continue
      cmd=['insert into info set', $
           'event='+quote(event)+c, $
           'jd='+strn(jd[i],format='(f17.9)')+c, $
           'filename='+quote(fn[i])+c, $
           'stemdir='+quote(stemdir)+c, $
           'team='+quote(team)+c, $
           'fwhm='+strn(fwhm[i],format='(f10.2)')+c, $
           'sky='+strn(sky[i],format='(f10.1)')+c, $
           'skysig='+strn(skysig[i],format='(f10.1)')+c, $
           'binfac='+strn(binfac)+c, $
           'nstars='+strn(nsrc[i])+c, $
           'ngood='+strn(ngood[i])+c]
      if good[i] eq 0 then $
         cmd=[cmd,'qual='+quote('bad')+c]

      cmd=[cmd, $
           'good='+strn(good[i]), $
           ';']
      mysqlcmd,dblun,cmd
      nload++
   endfor

   print,'Loaded ',strn(nload),' images out of ',strn(nfiles)
   if nprev gt 0 then $
      print,'There were ',strn(nprev),' images previously loaded.'

bailout:
   if nload gt 0 then begin
      action=team+'['+strn(binfac)+']: Loaded '+strn(nload)+ $
             ' images out of '+strn(nfiles)+'.'+ $
             '  Previously there were '+strn(nprev)+'.'
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
