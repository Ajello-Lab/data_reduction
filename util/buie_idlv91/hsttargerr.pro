;+
; NAME:
;  hsttargerr
; PURPOSE:   (one line only)
;  Determine fit errors for PSF fits to point source targets
; DESCRIPTION:
; CATEGORY:
;  Astrometry
; CALLING SEQUENCE:
;  hsttargerr,filestem
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
;     hstpsf   - directory for psf cache
;     satur    - Saturation signal level, default=80000L
; objectid
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
;  2020/06/29, Written by Marc W. Buie, Southwest Research Institute
;-
pro hsttargerr,filestem, $
             DISPLAY=display, $
             QUIET=quiet, $
             MAXTODO=maxtodo, $
             MULTITHREAD=multithread

   compile_opt strictarrsubs
   common com_hsttargfit,info

   pname='hsttargerr'
   self=pname+': '
   if badpar(filestem,7,0,caller=self+'(filestem) ') then return

   if badpar(display,[0,1,2,3],0,caller=self+'(DISPLAY) ',default=0) then return
   if badpar(quiet,[0,1,2,3],0,caller=self+'(QUIET) ',default=0) then return
   if badpar(maxtodo,[0,1,2,3],0,caller=self+'(MAXTODO) ',default=0) then return
   if badpar(multithread,[0,1,2,3],0,caller=self+'(MULTITHREAD) ', $
                                     default=0) then return

   if not multithread then cpu,tpool_nthreads=1 else cpu,/reset

   if exists('quit.ctl') then file_delete,'quit.ctl'

   verbose = quiet eq 0

   getddir,info,ddir
   getvalue,info,'global','subdir',subdir
   getvalue,info,'global','hstpsf',hstpath
   getvalue,info,'global','satur',satur,type=4

   if not exists(ddir+'/pdf') then file_mkdir,ddir+'/pdf'

   c = ','
   cleanup=0

   print,'start ',systime()

   openmysql,dblun,'hstast'

   ; this gives a (hopefully) unique string to identify this process.
   ;   used to facilitate multiple cpus/machines running on the fitting
   ;   at one time.
   hosttag=idstring(seed)

;   rootid=propid

   done=0
   ndone=0
   repeat begin

      ; Get the next image to be worked on.  This is done with a query-set
      ;  command to tag the next available record as belonging to this
      ;  process.
      cmd=['update pos,header set info='+quote('E:'+hosttag), $
           'where pos.hidx=header.idx', $
           'and root like '+quote(filestem+'%'), $
           'and info ='+quote('auto'), $
           'order by root limit 1;']
      mysqlcmd,dblun,cmd

      ; get the name of the image we've got to work on
      cmd='select idx,hidx from pos where info='+quote('E:'+hosttag)+';'
      mysqlquery,dblun,cmd,idx,hidx,format='l,l',ngood=nhit

      done = nhit eq 0
      if done then continue

      print,'Processing image ',strn(hidx),' for pos index ',strn(idx)

      hstinittarg,dblun,idx,hidx,error
      if error then begin
         cleanup=1
         goto,bailout
      endif

      print,'Processing ',info.root,' to get errors for the target.'

      info.magval=1
      info.quiet=1
      info.chired=0

      flx2mag,info.flux,info.flux/10.0,mag,magerr,zeropt=24.0

      vals = [info.xpos,info.ypos,mag]
      scale = [0.1,0.1,magerr] ; *0.25
      region = scale*5
      domain = [[vals-0.6],[vals+0.6]]
      domain[2,*]=[-1,1]*!values.f_infinity
      nsteps=10000
;nsteps=301

      ; Run the MC sampler
      jd1=systime(/julian)
      cputime,time1

      mcmcsamp,vals,scale,region,pdf,function_name='hstmodtarg',nsteps=nsteps, $
         verbose=verbose,display=display,acceptance=keeprate,domain=domain

      jd2=systime(/julian)
      cputime,time2

      print,time2-time1,' seconds, execution time for ',strn(nsteps),' steps'
      print,(jd2-jd1)*86400.0d0,' seconds, wall clock time'

      ; save the pdf
      fnpdf=ddir+'pdf/T'+strn(idx)+'.pdf'
      mkhdr,hdr,pdf
      sxaddpar,hdr,'KEEPRATE',keeprate
      writefits,fnpdf,pdf,hdr

      cmd='update pos set info='+quote('autopdf')+ $
          ' where info='+quote('E:'+hosttag)+';'
      print,cmd
      mysqlcmd,dblun,cmd

      ndone++
      if maxtodo ne 0 and ndone ge maxtodo then break
      if exists('quit.ctl') then break
   endrep until done

bailout:
   if cleanup then begin
      cmd='update pos set info='+quote('auto')+ $
          ' where info='+quote('E:'+hosttag)+';'
      print,cmd
      mysqlcmd,dblun,cmd
   endif
   free_lun, dblun
   cpu,/reset

end
