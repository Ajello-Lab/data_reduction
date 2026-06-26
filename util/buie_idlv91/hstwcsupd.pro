;+
; NAME:
;  hstwcsupd
; PURPOSE:   (one line only)
;  Update the WCS from a HST WFC3 image
; DESCRIPTION:
; CATEGORY:
;  Astrometry
; CALLING SEQUENCE:
;  hstwcsupd,filestem
; INPUTS:
;  filestem - String that is used to select a set of file for processing.
;             The search string for files will be
;               filestem+'*_flc.fits.gz'
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
; OUTPUTS:
; KEYWORD OUTPUT PARAMETERS:
; CONFIGURATION: (optional, overrides xtrack.in)
;   File: config.ini
;   [global]
;     subdir   - naem of sub-directory  added to base path to find data
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
;  2020/07/01, Written by Marc W. Buie, Southwest Research Institute
;-
pro hstwcsupd,filestem,FORCE=force,VERBOSE=verbose

   pname='hstwcsupd'
   self=pname+': '
   if badpar(filestem,7,0,caller=self+'(filestem) ') then return
   if badpar(force,[0,1,2,3],0,caller=self+'(FORCE) ',default=0) then return
   if badpar(verbose,[0,1,2,3],0,caller=self+'(VERBOSE) ',default=0) then return

   getddir,info,ddir
   getvalue,info,'global','subdir',subdir
   
   if verbose then begin
      showcall,pname,filestem,{name:'FORCE',value:force}, $
         {name:'VERBOSE',value:verbose}
      print,'ddir=',ddir
      print,'subdir=',subdir
   endif

   c=','
   dirty=0
   openmysql,dblun,'hstast'

   cmd=['select root,count(*) from header,stars', $
        'where stars.hidx=header.idx', $
        'and root like '+quote(filestem+'%'), $
        'group by root;']
   if verbose then print,cmd
   mysqlquery,dblun,cmd,root,nstars,format='a,l',ngood=nfields
   if verbose then print,strn(nfields),' candidate fields to process'

   nupdate=0
   for i=0,nfields-1 do begin
      cmd='select dcrval0,dcrval1,nstars from wcs'+ $
          ' where root='+quote(root[i])+';'
      mysqlquery,dblun,cmd,olddra,oldddec,nwcs,format='d,d,l',ngood=ncheck
      if verbose then begin
         print,cmd
         if ncheck eq 1 then print,olddra,oldddec,nwcs $
         else print,'not processed yet'
      endif
      if ncheck eq 1 and not force then begin
         if nwcs eq nstars[i] then continue
      endif

      print,'update wcs for ',root[i]

      cmd=['select stars.idx,stars.ra,stars.decl,wcsra,wcsdec', $
           'from stars,header', $
           'where stars.hidx=header.idx', $
           'and root='+quote(root[i]), $
           'and flag='+quote('g'), $
           'and catalog='+quote('DR2'), $
           'order by idx;']
      if verbose then print,cmd
      mysqlquery,dblun,cmd,idx,ra,dec,wcsra,wcsdec,format='l,d,d,d,d',ngood=nval
      if nval eq 0 then begin
         print,root[i],' has no fitted stars yet, skipping'
         continue
      endif

      dra=(ra-wcsra)*180.0d0 / !dpi * 3600.0d0
      ddec=(dec-wcsdec)*180.0d0 / !dpi * 3600.0d0

      if nval eq 1 then begin
         newdra  = dra
         newddec = ddec
         nval    = 1
         nfinal  = 1
      endif else begin
         print,strn(nval),' calibration sources found.'
         print,'Star index ',idx
         robomean,dra,3.0,0.5,newdra,avgdev,newdrasig,var,skew,kurt,nfinal, $
            stdmean=newdrasigm
         print,'Mean ra  offset',newdra,newdrasig,newdrasigm, $
            ' from ',strn(nfinal),' points'
         print,'Residuals'
         print,dra-newdra
         robomean,ddec,3.0,0.5,newddec,avgdev,newddecsig,var,skew,kurt,nfinal, $
            stdmean=newddecsigm
         print,'Mean dec offset',newddec,newddecsig,newddecsigm, $
            ' from ',strn(nfinal),' points'
         print,ddec-newddec
         print,'Old offset',olddra,oldddec
         print,'New offset',newdra,newddec
         print,'Old-new   ',olddra-newdra,oldddec-newddec
      endelse

      if ncheck eq 1 then begin
         cmd='delete from wcs where root='+quote(root[i])+';'
         mysqlcmd,dblun,cmd
         dirty=1
      endif

      cmd=['insert into wcs values(', $
          quote(root[i])+c, $
          strn(newdra,format='(f14.10)')+c, $
          strn(newddec,format='(f14.10)')+c, $
          strn(nval)+c, $
          strn(nfinal), $
          ');']
      print,cmd
      mysqlcmd,dblun,cmd
      dirty=1
      nupdate++

   endfor

   if dirty then begin
      context=subdir
      action='Updated '+strn(nupdate)+' WCS entries'
      jdcur=systime(/julian,/ut)
      jdstr,jdcur,300,jds
      cmd=['insert into history set', $
           'posted='+jds+c, $
           'context='+quote(context)+c, $
           'tool='+quote(pname)+c, $
           'action='+quote(action)+';']
      print,cmd
      mysqlcmd,dblun,cmd
   endif

bailout:
   free_lun,dblun

end
