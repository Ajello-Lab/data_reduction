;+
; NAME:
;  reconstmail
; PURPOSE:   (one line only)
;  Scan RECON database and send a summary email about sign-up status.
; DESCRIPTION:
; CATEGORY:
;  Miscellaneous
; CALLING SEQUENCE:
;  reconstmail
; INPUTS:
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;  DEBUG - Flag, if set will suppress email and print the message to the
;            screen instead.  Some extra debugging information may be printed
;            as well.
; OUTPUTS:
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
;
;  DO NOT RUN THIS MORE THAN ONCE PER DAY
;   it could generate too much reminder email on reminder day.
;
; PROCEDURE:
; MODIFICATION HISTORY:
;  2016/11/17, Written by Marc W. Buie, Southwest Research Institute
;  2016/11/30, MWB, added reminder email
;  2016/12/12, MWB, added campaign result summary
;  2017/01/03, MWB, refinements to report, less detail if event is not close
;  2017/02/02, MWB, slight formatting and information change to report
;  2022/11/12, MWB, changed to only report on active sites
;-
pro reconstmail,DEBUG=debug

compile_opt strictarrsubs

   self='reconstmail: '
   if badpar(debug,[0,1,2,3],0,caller=self+'(DEBUG) ',default=0) then return

   jdstr,systime(/ut,/julian),0,jdreps
   print,self,'execution starting at ',jdreps

   openmysql,dblun,'recon'

   ; This first block deals with reporting on signups

   jdend=systime(/ut,/julian)+30.0
   jdstr,jdend,100,jdends,timesep='-'

   if debug then print,'Signup: Search window end at   ',jdends

   jdnow=systime(/ut,/julian)
   jdstr,jdnow,100,jdnows,timesep='-'

   if debug then print,'        Search window start at ',jdnows

   cmd='select evidx,evtype,utdate,object from campaign'+ $
       ' where utdate>='+quote(jdnows)+ $
       ' and utdate<='+quote(jdends)+ $
       ' order by utdate;'
   mysqlquery,dblun,cmd,evidx,evtype,utdate,object, $
      format='l,a,a,a',ngood=nevents
   msg=[]

   if nevents gt 0 then begin
      msg=[msg,'Campaign signup status summary generated at '+jdreps+' UT','']
      if nevents eq 1 then ess='' else ess='s'
      msg=[msg,strn(nevents)+' active campaign'+ess,'']
      for i=0,nevents-1 do begin
         if i ne 0 then msg=[msg,'','-----------------------------------','']
         msg=[msg,'Event index '+strn(evidx[i])+ $
                  ', '+utdate[i]+' UT, '+object[i]+'  ['+evtype[i]+']']
         jdev=jdparse(utdate[i])
         daysuntil = fix(jdev-jdnow)
         if daysuntil eq 1 then ess='' else ess='s'
         msg=[msg,strn(daysuntil)+' day'+ess+' until event']

         if evtype[i] eq 'RECON' or evtype[i] eq 'RECON-Rio' then begin
            if daysuntil eq 7 or daysuntil eq 3 then $
               reconnotice,evidx[i],debug=debug
            cmd='select sitecode,name,response from signup,sites'+ $
                ' where sitecode=code'+ $
                ' and evidx='+strn(evidx[i])+ $
                ' and sitecode not like '+quote('V%')+ $
                ' and sitecode not like '+quote('L%')+ $
                ' and active=1'+ $
                ' order by lat desc;'
            mysqlquery,dblun,cmd,sitecode,name,response, $
               format='a,a,a',ngood=noresp

            msg=[msg,'']
            z=where(response eq 'x',count)
            if count eq 1 then ess='' else ess='s'
            msg=[msg,string(count)+' non-responsive site'+ess+'.']
            z=where(response eq '?',count)
            if count eq 1 then ess='' else ess='s'
            msg=[msg,string(count)+' site'+ess+' with uncertainty.']
            z=where(response eq 'n',count)
            if count eq 1 then ess='' else ess='s'
            msg=[msg,string(count)+' site'+ess+' unable to participate.']
            z=where(response eq 'y',count)
            if count eq 1 then ess='' else ess='s'
            msg=[msg,string(count)+' site'+ess+' signed up and ready to go.']

            if daysuntil lt 13 then begin

               msg=[msg,'']

               types=['x','?','n']

               for k=0,n_elements(types)-1 do begin
                  z=where(response eq types[k],count)
                  if count eq 0 then continue
                  for j=0,count-1 do begin
                     cmd=['select member.id,first,value from member,contact', $
                          'where code='+quote(sitecode[z[j]]), $
                          'and (role='+quote('MR1')+ $
                             ' or role='+quote('MR2')+')', $
                          'and status='+quote('MS1'), $
                          'and member.id=contact.id', $
                          'and contact.descr='+quote('email'), $
                          ';']
                     mysqlquery,dblun,cmd,id,first,email, $
                        format='l,a,a',ngood=nlead
                     if nlead eq 0 then begin
                        email='NO LEADER!'
                        str=string(sitecode[z[j]],name[z[j]], $
                                   response[z[j]],email, $
                                   format='(a4,1x,a-30,1x,a,1x,a)')
                        msg=[msg,str]
                     endif else begin
                        for kl=0,nlead-1 do begin
                           if kl eq 0 then begin
                              str=string(sitecode[z[j]],name[z[j]], $
                                         response[z[j]],email[kl], $
                                         format='(a4,1x,a-30,1x,a,1x,a)')
                           endif else begin
                              str=string(email[kl],format='(38x,a)')
                           endelse
                           msg=[msg,str]
                        endfor
                     endelse
                  endfor
               endfor
            endif

            msg=[msg,'']

         endif else begin
            cmd='select count(*) from signup where evidx='+strn(evidx[i])+ $
                ' and response='+quote('y')+';'
            mysqlquery,dblun,cmd,numyes,format='i'
            msg=[msg,strn(numyes)+' sites have signed up for this']
         endelse
         cmd='select sitecode,name,reporter,email from signup,sites'+ $
             ' where sitecode=code and sub = '+quote('y')+ $
             ' and evidx='+strn(evidx[i])+ $
             ' order by lat desc;'
         mysqlquery,dblun,cmd,sitecode,name,reporter,semail, $
            format='a,a,a,a',ngood=numsub
         if numsub eq 0 then begin
            msg=[msg,'No substitute requests.']
         endif else begin
            if numsub eq 1 then ess='' else ess='s'
            msg=[msg,strn(numsub)+' request'+ess+' for substitute support.']
            for j=0,numsub-1 do begin
               str=string(sitecode[j],name[j],reporter[j],semail[j], $
                          format='(a4,1x,a-30,1x,a,1x,a)')
               msg=[msg,str]
            endfor
         endelse
         msg=[msg,'', $
            'https://delwin.boulder.swri.edu/recon/db/signupsummary.php'+ $
            '?idx='+strn(evidx[i])]
      endfor
   endif

   ; This second part deals with report on status reporting

   jdend=systime(/ut,/julian)
   jdstr,jdend,100,jdends,timesep='-'
   if debug then print,'Results: Search window end at   ',jdends

   jdnow=systime(/ut,/julian)-14.0
   jdstr,jdnow,100,jdnows,timesep='-'
   jdstr,jdnow+14.0,100,jdnows_true,timesep='-'

   if debug then print,'         Search window start at ',jdnows

   cmd='select evidx,evtype,utdate,object from campaign'+ $
       ' where utdate>='+quote(jdnows)+ $
       ' and utdate<='+quote(jdends)+ $
       ' order by utdate;'
   mysqlquery,dblun,cmd,evidx,evtype,utdate,object, $
      format='l,a,a,a',ngood=nevents
   if nevents gt 0 then begin
      msg=[msg,'Campaign report status summary generated at '+jdreps+' UT','']
      if nevents eq 1 then ess='' else ess='s'
      msg=[msg,strn(nevents)+' recently completed campaign'+ess,'']
      for i=0,nevents-1 do begin
         msg=[msg,'Event index '+strn(evidx[i])+ $
                  ', '+utdate[i]+' UT, '+object[i]+'  ['+evtype[i]+']']
         jdev=jdparse(utdate[i])
         dayssince = fix(jdend-jdev)
         if dayssince eq 1 then ess='' else ess='s'
         if (evtype[i] eq 'RECON' or evtype[i] eq 'RECON-Rio') $
                  and dayssince eq 3 then $
            reconnotice,evidx[i],/report,debug=debug
         msg=[msg,strn(dayssince)+' day'+ess+' since event']
         cmd='select sitecode,name,report from result,sites'+ $
             ' where sitecode=code'+ $
             ' and evidx='+strn(evidx[i])+ $
             ' and active=1'+ $
             ' order by lat desc;'
         mysqlquery,dblun,cmd,sitecode,name,reportcode, $
            format='a,a,a',ngood=nsites
         if nsites eq 0 then begin
            msg=[msg,'Campaign not setup in database.']
         endif else begin
            codelist=reportcode[uniq(reportcode,sort(reportcode))]
            ncodes=n_elements(codelist)
            for j=0,ncodes-1 do begin
               if codelist[j] eq 'NULL' then begin
                  descr='No report filed'
               endif else begin
                  cmd='select description from codes' + $
                      ' where tag='+quote(codelist[j])+';'
                  mysqlquery,dblun,cmd,descr,format='a',ngood=ncheck
                  if ncheck ne 1 then begin
                     descr='unrecognized code ('+codelist[j]+')'
                  endif else begin
                     descr=trimrank(descr)
                  endelse
               endelse
               z=where(reportcode eq codelist[j],count)
               msg=[msg, $
                  string(count,format='(i3)')+' '+descr]
            endfor
         endelse
      endfor
   endif

   ; send email
   if msg ne !null then begin
      msg=[msg,'','http://www.boulder.swri.edu/~buie/recon/teamstat.html']
      msg=[msg,'','http://www.boulder.swri.edu/~buie/recon/teamreport.html']
      msg=[msg,'','generated by reconstmail.pro']
      if debug then begin
         print,''
         print,'email message contents'
         print,''
         for i=0,n_elements(msg)-1 do print,msg[i]
      endif else begin
         mailmsg,'buie@boulder.swri.edu','RECON status report '+jdnows_true, $
            msg,ccaddr=['john.m.keller@colorado.edu']
      endelse
   endif

   free_lun,dblun

   jdstr,systime(/ut,/julian),0,jdreps
   print,self,'execution completed at ',jdreps

end
