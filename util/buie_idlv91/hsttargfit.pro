;+
; NAME:
;    hsttargfit
; PURPOSE: (one line)
;    Fit a model of a single point source target to HST WF3 data.
; DESCRIPTION:
; Using approximate (or previous fit) positions from database table plchpos,
; and model psf's from wfc3model, we fit the x,y and flux for a TNO
; using POWELL. Results are saved in the pos data table.  If the position isn't
; in pos, the starting locations are pulled from nav.
; CATEGORY:
;  Astrometry
; CALLING SEQUENCE:
;    hsttargfit, filestem
;
; INPUTS:
;  filestem - String that is used to select a set of file for processing.
;             The search string for files will be
;               filestem+'*_flc.fits.gz'
; OPTIONAL INPUT PARAMETERS:
;
; KEYWORD PARAMETERS:
;  ASK   - Flag, if set, user will be prompted after each fit- once per
;          object- whether to update the data base with the new fit values.
;  FORCE   - Flag, if set, a image that has already been fit will be fit
;              again. If the 'info' field in the pos table is 'auto', it is
;              assumed the fit was done.
;  FTOL  - tolerance value passed to POWELL, by default .0001
;  MAXTODO - How many images to process, default=0 which means all of them.
;  NOFIT - Flag, if set, will suppress the fitting call and just calculate
;            the model based on what is known and print out the chisq.
;  RESETPOS - If in the NOFIT mode, this will copy x,y from nav to pos and
;               will also ask for new flux value.
;  ONEPASS - Flag, if set just does a single fitting pass.  Watch out,
;              if you do this it won't be fully converged, though it
;              will be close.
;  SHOWDW - If NOFIT is set, this keyword allows you to override the fitting
;              dw value set for the object and let you temporarily show a
;              different (usually larger) region.
;  EXTRAFUNC - String with the name of an external function to call at the end
;                if NOFIT is set.  This is a procedure that takes one input
;                argument which will be the internal information structure
;                and does something.  The default is an empty string in which
;                case this option is ignored.
;
; OUTPUTS:
;
; CONFIGURATION: (optional, overrides xtrack.in)
;   File: config.ini
;   [global]
;     subdir   - naem of sub-directory  added to base path to find data
; objectid
;   [ddir]   - section to list potential root directories to find data
;     XXX - <value>
;       XXX is a unique string (eg., D01)
;       <value> is a directory path
;       for additional details see getddir.pro
; COMMON BLOCKS:
;    com_hsttargfit  :  Used for communication of data between this routine and
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
;  2014/07/09, MWB, added RESETPOS
;  2020/06/29, MWB, generalized version
;  2020/09/25, MWB, added SHOWDW keyword
;  2020/10/01, MWB, added EXTRAFUNC keyword
;-
pro hsttargfit,filestem, $
            ASK=ask, $
            FTOL=ftol, $
            NOFIT=nofit, $
            ONEPASS=onepass, $
            RESETPOS=resetpos, $
            MAXTODO=maxtodo, $
            SHOWDW=showdw, $
            EXTRAFUNC=extrafunc

   common com_hsttargfit,info

   pname='hsttargfit'
   self=pname+': '
   if badpar(filestem,7,0,caller=self+'(filestem) ') then return

   if badpar(ask,[0,1,2,3],0,caller=self+'(ASK) ',default=0) then return
   if badpar(onepass,[0,1,2,3],0,caller=self+'(ONEPASS) ',default=0) then return
   if badpar(resetpos,[0,1,2,3],0,caller=self+'(RESETPOS) ',default=0) then return
   if badpar(maxtodo,[0,1,2,3],0,caller=self+'(MAXTODO) ',default=0) then return
   if badpar(nofit,[0,1,2,3],0,caller=self+'(NOFIT) ',default=0) then return
   if badpar(ftol,      [4,5,0],        0, caller=self + '(FTOL)', $
             default=.0001)        then return
   if badpar(showdw,[0,2,3],0,caller=self+'(SHOWDW) ',default=0) then return
   if badpar(extrafunc,[0,7],0,caller=self+'(EXTRAFUNC) ',default='') then return

   if nofit eq 0 then showdw=0

   getddir,cinfo,ddir
   getvalue,cinfo,'global','subdir',subdir

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
; this one is for FORCE=0, visitid provided, no pattstep
         cmd=['update pos,header set info='+quote('C:'+hosttag), $
              'where pos.hidx=header.idx', $
              'and root like '+quote(filestem+'%'), $
              'and info ='+quote('refit'), $
              'order by root limit 1;']
         mysqlcmd,dblun,cmd

         ; get the name of the image we've got to work on
         cmd='select idx,hidx from pos where info='+quote('C:'+hosttag)+';'
;print,cmd
         mysqlquery,dblun,cmd,idx,hidx,format='l,l',ngood=nhit
         done = nhit eq 0
         if done then continue

      endif else begin
         cmd=['select pos.idx,hidx', $
             'from pos,header', $
              'where pos.hidx=header.idx', $
              'and root like '+quote(filestem+'%'), $
              'limit 1;']
         mysqlquery,dblun,cmd,idx,hidx,format='l,l',ngood=nhit
         if nhit ne 1 then begin
            print,cmd
            print,'No data found.  Aborting.'
            goto,bailout
         endif
         if resetpos then begin
            cmd='select xnav,ynav from pos' + $
                ' where idx='+strn(idx)+';'
            mysqlquery,dblun,cmd,xnav,ynav,format='f,f',ngood=nhit
            if nhit ne 1 then begin
               print,cmd
               print,'Object has no nav entries.  Aborting.'
               goto,bailout
            endif
            cmd='update pos set'+ $
                ' x='+strn(xnav)+c+ $
                ' y='+strn(ynav)+ $
                ' where idx='+strn(idx)+';'
            mysqlcmd,dblun,cmd
            newflux=0.
            read,prompt='New flux value> ',newflux
            cmd='update pos set'+ $
                ' flux='+strn(newflux)+c+ $
                ' info='+quote('refit')+ $
                ' where idx='+strn(idx)+';'
            mysqlcmd,dblun,cmd
         endif
      endelse

      hstinittarg,dblun,idx,hidx,error,showdw=showdw
      if error then begin
         cleanup=1
         goto,bailout
      endif

      print,'Processing ',info.root,' to fit for the TNO.'

      ; If we're ready, it's time to fit this one.
      vals = [info.xpos,info.ypos,info.flux]
      scale = [0.1,0.1,info.flux/100.0]

      xi = identity(n_elements(vals))
      info.chicount = 0

      ; solve for best vals.
      jd1=systime(/julian)
      cputime,time1

      chisq=hstmodtarg(vals)
      info.chibest=chisq ; redundant, safety for pathological case

      if not nofit then begin
         print,'Initial chisq ',chisq
         print,'Fit Starting ',systime()

         vals = amoeba(ftol, FUNCTION_NAME='hstmodtarg', NCALLS=ncalls, $
                       P0=vals, SCALE=scale, $
                       FUNCTION_VALUE=fvals)
         chsq = fvals[0]

         cputime,time2
         jd2=systime(/julian)
         dt = (jd2-jd1)*86400.0

         ; fit's over get the values back
         xpos = vals[0]
         ypos = vals[1]
         flux = vals[2]

         print,'Fit completed ',systime()
         print,' cpu   time ',time2-time1, $
               ' sec, per iteration=',(time2-time1)/info.chicount,' sec', $
               format='(a,f7.1,a,f7.2,a)'
         print,' clock time ',dt, $
               ' sec, per iteration=',dt/info.chicount,' sec', $
               format='(a,f7.1,a,f7.2,a)'

         print, ' fit for ', info.root, ncalls, ' iterations ', $
                  info.chicount, ' chisq evaluations.'

         print,'New fitted values:'
         print,'   x,y',xpos,ypos,', flux',flux,' dw',info.dw,format=fmt
         print,'background  ',info.back,' focus',info.z4
         print,'new chisq ',chsq,' cpu time ',time2-time1, $
            ' sec, per iteration=',(time2-time1)/info.chicount,' sec'
         if ask then begin
            ans=''
            read,prompt='Do you want to enter this fit into pos table [y,n,q]?', $
                  ans
            ans =strupcase(ans)
         endif else ans='Y' 

         if ans eq 'Q' then break

         if ans eq 'Y' or ans eq 'YES'then begin
            print, ' updating ',info.root, ' in database.'
            ; go replace values in plchpos

            curjd=systime(/julian)
            jdstr,curjd,300,jds
            newinfo='auto'
            cmd =['update pos set', $
                  'x='+strn(xpos,FORMAT='(F8.3)')+c, $
                  'xerr=-1,', $
                  'y='+strn(ypos,FORMAT='(F8.3)')+c, $
                  'yerr=-1,', $
                  'flux='+strn(flux,FORMAT='(F15.3)')+c, $
                  'fluxerr=-1,', $
                  'info='+quote(newinfo)+c, $
                  'chisq='+strn(chsq,FORMAT='(F10.4)')+c, $
                  'tstamp='+jds, $
                  'where idx='+strn(idx)+';']
print,cmd
            mysqlcmd, dblun, cmd, answer

         endif else begin
            print,'Fit Values dropped, will NOT be placed in database.'
            cleanup=1
            goto,bailout
         endelse
      endif else begin
         print,'Current chisq ',chisq
;         showsrc,info.fullimage,lowsig=-3,hisig=8,window=0
;         oplot,[info.i1,info.i2,info.i2,info.i1,info.i1], $
;               [info.j1,info.j1,info.j2,info.j2,info.j1],color='00ff00'xl
         zf=6
         setwin,1,xsize=3*info.msz*zf,ysize=info.msz*zf
         skysclim,info.fitimage,loval,hival,mval,sigma
         zm=where(info.mask ne 0,countm)
         if countm ne 0 then begin
            info.fitimage[zm]=randomn(seed,countm)*sigma+mval
         endif
         bim=rebin(info.fitimage,info.msz*zf,info.msz*zf,/sample)
         tv,bytscl(bim,min=mval-3*sigma,max=mval+8*sigma,top=255)
         bim=rebin(info.mimage[*,*]+info.back, $
                      info.msz*zf,info.msz*zf,/sample)
         tv,bytscl(bim,min=mval-3*sigma,max=mval+8*sigma,top=255),1
         bim=rebin(info.fitimage-(info.mimage[*,*]+info.back), $
                       info.msz*zf,info.msz*zf,/sample)
         tv,bytscl(bim,min=-5*sigma,max=5*sigma,top=255),2

         if extrafunc ne '' then begin
            cmd=extrafunc+',info'
            tst = execute(cmd)
         endif
         done=1
      endelse
      ndone++
      if maxtodo ne 0 and ndone ge maxtodo then break
   endrep until done

   if dirty then begin
      context=subdir
      action='Fitted '+strn(ndone)+' targets -- C:'+hosttag
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
   cmd=['select count(*) from pos,header', $
        'where pos.hidx=header.idx',$
        'and root like '+quote(filestem+'%')+';']
   mysqlquery,dblun,cmd,ncheck,format='i'
   if ncheck eq 0 then begin
      print,'There are no entries for ',filestem+'%',' that can be fitted.'
   endif else begin
      print,'There are a total of ',strn(ncheck),' entries in pos for ',filestem+'%'
   endelse
   if cleanup then begin
      cmd='update pos set info='+quote('refit')+ $
          ' where info='+quote('C:'+hosttag)+';'
      mysqlcmd,dblun,cmd
   endif
   free_lun, dblun
end
