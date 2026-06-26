;+
; NAME:
;   hststarfit
; PURPOSE: (one line)
;   Fit a model to single source in HST WFC3 data.
; DESCRIPTION:
; Using approximate (or previous fit) positions from database table stars,
; and model psf's from wfc3model, we fit the x,y and flux for a source
; using POWELL. Results are saved in the pos data table.  If the position isn't
; in pos, the starting locations are pulled from nav.
; CATEGORY:
;   Astrometry
; CALLING SEQUENCE:
;   hststarfit, visitid, pattstep
;
; INPUTS:
;  filestem - String that is used to select a set of file for processing.
;             The search string for files will be
;               filestem+'*.flc.fits.gz'
; OPTIONAL INPUT PARAMETERS:
;
; KEYWORD PARAMETERS:
;  ASK   - Flag, if set, user will be prompted after each fit- once per
;          rootname- whether to update the data base with the new fit values.
;  FORCE   - Flag, if set, a rootname that has already been fit will be fit
;              again. If the 'info' field in the pos table is 'auto', it is
;              assumed the fit was done.
;  FTOL  - tolerance value passed to POWELL, by default .0001
;  MAXTODO - How many images to process, default=0 which means all of them.
;  NOFIT - Flag, if set, will suppress the fitting call and just calculate
;            the model based on what is known and print out the chisq.
; OUTPUTS:
;   Everything is saved to the database
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
;    com_fithstpsf  :  Used for communication of data between this routine and
;                    the chi-square function used for the Powell
;                    fit..
; SIDE EFFECTS:
; Uses HST PSF images which may cause the memory and disk PSF caches to be
; populated. Modifies entries in hst1i data base table pos. These entries
; reflect the most recent values obtained from the fit,
;
; RESTRICTIONS:
; Requires hst1i database table header and plchpos.
; nav must be populated
;
; Currently can fit for the following parameters:
; x, y, flux
;
; The sky background is measured with robomean local to the object.
;
; PROCEDURE:
;
; MODIFICATION HISTORY:
;  2011/03/24, Written by Marc W. Buie, Southwest Research Institute, cloned
;                 from autofit.pro in the hst15 reduction directory.
;  2011/05/07, Added NOFIT keyword, restructuring for changes to chimodplch
;  2011/09/15, MWB, added mask support
;  2012/07/06, MWB, added ONEPASS keyword
;  2014/07/04, MWB, cloned from pcfit.pro from hst20
;  2015/11/24, MWB, changed to save full precision WCS position to database
;  2016/07/15, MWB, added PROPID keyword
;  2017/01/03, MWB, heavily restructured to pull init out into init_star.pro
;  2020/06/23, MWB, generalized version
;  2024/08/06, MWB, logic fix to /NOFIT case
;-
pro hststarfit,filestem, $
            ASK=ask, $
            FTOL=ftol, $
            NOFIT=nofit, $
            MAXTODO=maxtodo

   compile_opt strictarrsubs
   common com_hststarfit,info

   pname='starfit'
   self=pname+': '
   if badpar(filestem,7,0,caller=self+'(filestem) ') then return

   if badpar(ask,[0,1,2,3],0,caller=self+'(ASK) ',default=0) then return
   if badpar(maxtodo,[0,1,2,3],0,caller=self+'(MAXTODO) ',default=0) then return
   if badpar(nofit,[0,1,2,3],0,caller=self+'(NOFIT) ',default=0) then return
   if badpar(ftol,      [4,5,0],        0, caller=self + '(FTOL)', $
             default=.0001)        then return

   getddir,cinfo,ddir
   getvalue,cinfo,'global','subdir',subdir
   getvalue,cinfo,'global','hstpsf',hstpath
   getvalue,cinfo,'global','satur',satur

   c = ','
   fitoken='auto'
   cleanup=0
   dirty=0

   openmysql,dblun,'hstast'

   ; this gives a (hopefully) unique string to identify this process.
   ;   used to facilitate multiple cpus/machines running on the fitting
   ;   at one time.
   hosttag=idstring(seed)

   done=0
   ndone=0
   repeat begin

      if not nofit then begin
         ; Get the next image to be worked on.  This is done with a query-set
         ;  command to tag the next available record as belonging to this
         ;  process.
         cmd=['update stars,header set info='+quote('C:'+hosttag), $
              'where stars.hidx=header.idx', $
              'and root like '+quote(filestem+'%'), $
              'and info ='+quote('refit'), $
              'and flag ='+quote('g'), $
              'order by root limit 1;']
print,cmd
         mysqlcmd,dblun,cmd

         ; get the info for the image we've got to work on
         cmd='select idx,hidx'+ $
             ' from stars where info='+quote('C:'+hosttag)+';'
;print,cmd
         mysqlquery,dblun,cmd,idx,hidx,format='l,l',ngood=nhit
         done = nhit eq 0
         if done then continue

      endif else begin
         cmd=['select stars.idx,hidx', $
              'from stars,header', $
              'where root like '+quote(filestem+'%'), $
              'and stars.hidx=header.idx', $
              'order by root limit 1;']
print,cmd
         mysqlquery,dblun,cmd,idx,hidx,format='l,l',ngood=nhit
         if nhit ne 1 then begin
            print,cmd
            print,'No data found.  Aborting.'
            goto,bailout
         endif
      endelse

      print,'Processing image ',strn(hidx),' to fit for star index ',strn(idx)

      hstinitstar,dblun,idx,hidx,error
      if error then begin
         cleanup=1
         goto,bailout
      endif

      ; If we're ready, it's time to fit this one.
      vals = [info.xpos,info.ypos,info.flux]
      scale = [0.1,0.1,info.flux/100.0]

      xi = identity(n_elements(vals))
      info.chicount = 0

      ; solve for best vals.
      jd1=systime(/julian)
      cputime,time1

      chisq=hstmodstar(vals)
      info.chibest=chisq ; redundant, safety for pathological case

      if not nofit then begin

         print,'Initial chisq ',chisq
         print,'Fit Starting ',systime()

print,vals
         vals = amoeba(ftol, FUNCTION_NAME='hstmodstar', NCALLS=ncalls, $
                       P0=vals, SCALE=scale, $
                       FUNCTION_VALUE=fvals)
print,vals
         chsq = fvals[0]

         cputime,time2
         jd2=systime(/julian)
         dt = (jd2-jd1)*86400.0

         ; fit's over get the values back
         xpos = vals[0]
         ypos = vals[1]
         flux = vals[2]

         xy2ad,xpos,ypos,info.astinfo,ra,dec
         ra=ra*!dpi/180.0d0
         dec=dec*!dpi/180.0d0

         print,'Fit completed ',systime()
         print,' cpu   time ',time2-time1, $
               ' sec, per iteration=',(time2-time1)/info.chicount,' sec', $
               format='(a,f7.1,a,f7.2,a)'
         print,' clock time ',dt, $
               ' sec, per iteration=',dt/info.chicount,' sec', $
               format='(a,f7.1,a,f7.2,a)'

         print, ' fit for ',info.root, ncalls, ' iterations ', $
                  info.chicount, ' chisq evaluations.'

         print,'New fitted values:'
         print,'   x,y',xpos,ypos,', flux',flux,' dw',info.dw,format=fmt
         print,'background  ',info.back,' focus',info.z4
         print,'new chisq ',chsq,' cpu time ',time2-time1, $
            ' sec, per iteration=',(time2-time1)/info.chicount,' sec'
         if ask then begin
            ans=''
            read,prompt='Do you want to enter this fit into the database [y,n,q]?', $
                  ans
            ans =strupcase(ans)
         endif else ans='Y' 

         if ans eq 'Q' then break

         if ans eq 'Y' or ans eq 'YES' then begin
            print, ' updating ',info.root, ' in database.'

            curjd=systime(/julian)
            jdstr,curjd,300,jds
            newinfo='auto'
            cmd =['update stars set', $
                  'x='+strn(xpos,FORMAT='(F8.3)')+c, $
                  'xerr=-1,', $
                  'y='+strn(ypos,FORMAT='(F8.3)')+c, $
                  'yerr=-1,', $
                  'flux='+strn(flux,FORMAT='(F15.3)')+c, $
                  'fluxerr=-1,', $
                  'wcsra='+strn(ra,format='(f14.10)')+c, $
                  'wcsdec='+strn(dec,format='(f14.10)')+c, $
                  'info='+quote(newinfo)+c, $
                  'chisq='+strn(chsq,FORMAT='(F10.4)')+c, $
                  'tstamp='+jds, $
                  'where idx='+strn(idx)+';']
print,cmd
;print,'ending early for testing.'
;cleanup=1
;goto,bailout
            mysqlcmd, dblun, cmd, answer
            dirty=1

         endif else begin
            print,'Fit Values dropped, will NOT be placed in database.'
            cleanup=1
            goto,bailout
         endelse
      endif else begin
         print,'Current chisq ',chisq
         showsrc,info.fullimage,lowsig=-3,hisig=8,window=0
         oplot,[info.i1,info.i2,info.i2,info.i1,info.i1], $
               [info.j1,info.j1,info.j2,info.j2,info.j1],color='00ff00'xl
         zf=2
         psz=size(info.smear,/dimen)
         setwin,2,xsize=psz[0]*zf,ysize=psz[1]*zf
         tvscl,rebin(info.smear,psz[0]*zf,psz[1]*zf,/sample)
         setwin,1,xsize=3*info.msz*zf,ysize=info.msz*zf
         skysclim,info.fitimage,loval,hival,mval,sigma
         zm=where(info.mask ne 0,countm)
         if countm ne 0 then begin
            info.fitimage[zm]=randomn(seed,countm)*sigma+mval
         endif
         bim=rebin(info.fitimage,info.msz*zf,info.msz*zf,/sample)
         tv,bytscl(bim,min=mval-3*sigma,max=mval+8*sigma,top=255),0
         bim=rebin(info.mimage+info.back,info.msz*zf,info.msz*zf,/sample)
         tv,bytscl(bim,min=mval-3*sigma,max=mval+8*sigma,top=255),1
         bim=rebin(info.fitimage-(info.mimage),info.msz*zf,info.msz*zf,/sample)
         tv,bytscl(bim,min=mval-3*sigma,max=mval+8*sigma,top=255),2
         done=1
      endelse
      ndone++
      if maxtodo ne 0 and ndone ge maxtodo then break
   endrep until done

   if dirty then begin
      context=subdir
      action='Fitted '+strn(ndone)+' stars -- C:'+hosttag
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
   if cleanup then begin
      cmd='update stars set info='+quote('refit')+ $
          ' where info='+quote('C:'+hosttag)+';'
      mysqlcmd,dblun,cmd
   endif
   free_lun, dblun
end
