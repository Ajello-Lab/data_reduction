;+
; NAME:
;  reconnotice
; PURPOSE:   (one line only)
;  Generate a reminder email notice for a missing RECON signup or report
; DESCRIPTION:
; CATEGORY:
;  Miscellaneous
; CALLING SEQUENCE:
;  reconnotice,idx
; INPUTS:
;  idx - Index of event for which to send notice
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;  DEBUG  - Flag, if set will suppress sending any email but will print
;             everything it intends to do to the screen.
;  REPORT - Flag, if true sends a notice for a missing event report.  Otherwise
;             send a reminder for a missing signup.
; OUTPUTS:
;  All output sent via email.
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  2016/12/07, Written by Marc W. Buie, Southwest Research Institute
;-
pro reconnotice,idx,REPORT=report,DEBUG=debug

compile_opt strictarrsubs

   self='reconnotice: '
   if badpar(idx,[2,3],0,caller=self+'(idx) ') then return
   if badpar(report,[0,1,2,3],0,caller=self+'(REPORT) ',default=0) then return
   if badpar(debug,[0,1,2,3],0,caller=self+'(DEBUG) ',default=0) then return

   openmysql,dblun,'recon'

   ; Get basic information about the event
   cmd='select utdate,utgeomid,object from campaign where evidx='+strn(idx)+';'
   mysqlquery,dblun,cmd,utdate,uttime,object,format='a,a,a',ngood=ncheck
   if ncheck eq 0 then begin
      print,'Event ',strn(idx),' not found.  Aborting.'
      return
   endif
print,'Event on ',utdate,' ',uttime,' involving ',object
   jdevent=jdparse(utdate+' '+uttime)
   jdnow=systime(/julian,/ut)
   daysuntil=floor(jdevent-jdnow)

   ; Warning email about a missing campaign report
   if report eq 1 then begin

      subject='RECON reporting reminder for '+utdate+' UT campaign ('+object+')'

      nsend=0

      cmd=['select sitecode,name from result,sites', $
           'where evidx='+strn(idx), $
           'and sitecode=code', $
           'and report is NULL', $
           'and active=1', $
           'and sitecode not like '+quote('L%'), $
           'and sitecode not like '+quote('V%'), $
           ';']
      mysqlquery,dblun,cmd,sitecode,sitename,format='a,a',ngood=nsites
      print,'Number of sites not reporting is ',strn(nsites)

      for i=0,nsites-1 do begin
         cmd=['select value from member,contact', $
              'where member.id=contact.id', $
              'and (role='+quote('MR1')+' or role='+quote('MR2')+')', $
              'and contact.descr='+quote('email'), $
              'and code='+quote(sitecode[i]), $
              'order by role,value;']
         mysqlquery,dblun,cmd,email,format='a',ngood=nleads
         if nleads eq 0 then continue
         ccaddr=''
         if nleads ge 2 then ccaddr=strcompress(email[1:*],/remove_all)
         bccaddr=['buie@boulder.swri.edu', $
                  'john.m.keller@colorado.edu']
         replyto='tnorecon-org@mailman.boulder.swri.edu'

         toaddr=email[0]

         msg=['Dear '+sitename[i]+' team lead(s),', $
              '', $
              'This is a friendly reminder that we are half-way through the'+ $
                 ' weeklong', $
              'reporting window for our recent campaign on '+utdate+' UT.'+ $
                 '  The form will', $
              'close in just '+strn(abs(daysuntil))+' days and our records'+ $
                 ' indicate your community has not yet', $
              'submitted a campaign report.  This report is very important'+ $
                 ' to the project', $
              'regardless of outcome or underlying reasons.  Please take a'+ $
                 ' moment to go to:', $
              '', $
              'http://spikard.boulder.swri.edu/recon/report.php', $
              '', $
              'to let us know how things went.  As always, feel free to'+ $
                 ' contact us if you need', $
              'additional assistance.  Note that it is entirely ok to task a'+ $
                 ' student with', $
              'filling out the report form.', $
              '', $
              'Thank you, RECON!', $
              '', $
              'Marc, John, and the RECON robot reminder system', $
              'tnorecon-org@mailman.boulder.swri.edu' $
              ]
         if debug then begin
            print,'To:  ',toaddr
            print,'Cc:  ',ccaddr[*]
            print,'Bcc: ',bccaddr
            print,'Subject: ',subject
            for j=0,n_elements(msg)-1 do print,msg[j]
         endif else begin
            mailmsg,toaddr,subject,msg, $
               ccaddr=ccaddr,bccaddr=bccaddr,replyto=replyto
         endelse
         nsend++

      endfor

   ; Warning email about a missing signup
   endif else begin

      subject='RECON signup reminder for '+utdate+' UT campaign ('+object+')'

      nsend=0

      cmd=['select sitecode,name from signup,sites', $
           'where evidx='+strn(idx), $
           'and sitecode=code', $
           'and posted is NULL', $
           'and active=1', $
           'and sitecode not like '+quote('L%'), $
           'and sitecode not like '+quote('V%'), $
           ';']
      mysqlquery,dblun,cmd,sitecode,sitename,format='a,a',ngood=nsites

      print,'Number of sites not responding is ',strn(nsites)

      for i=0,nsites-1 do begin
         cmd=['select value from member,contact', $
              'where member.id=contact.id', $
              'and (role='+quote('MR1')+' or role='+quote('MR2')+')', $
              'and contact.descr='+quote('email'), $
              'and code='+quote(sitecode[i]), $
              'order by role,value;']
         mysqlquery,dblun,cmd,email,format='a',ngood=nleads
         if nleads eq 0 then continue
         ccaddr=''
         if nleads ge 2 then ccaddr=strcompress(email[1:*],/remove_all)
         bccaddr=['buie@boulder.swri.edu','john.m.keller@colorado.edu']
         replyto='tnorecon-org@mailman.boulder.swri.edu'

         toaddr=email[0]

         msg=['Dear '+sitename[i]+' team lead(s),', $
              '', $
              'This is a friendly reminder that our next RECON campaign' + $
                 ' scheduled for', $
              utdate+' UT is just '+strn(daysuntil)+' days away.  Our'+ $
                 ' records indicate that you have', $
              'not yet filled out the signup sheet for this event.  Please'+ $
                 ' take a moment', $
              'to go to:', $
              '', $
              'http://spikard.boulder.swri.edu/recon/signup.php', $
              '', $
              'and let us know your status.  As always, feel free to contact'+ $
                 ' us with requests', $
              'for help.  Timely responses are essential so that we will all'+ $
                 ' be ready for', $
              'the upcoming observation.  Note that it is entirely ok to'+ $
                 ' task a student', $
              'with filling out this form.', $
              '', $
              'GO RECON!', $
              '', $
              'Marc, John, and the RECON robot reminder system', $
              'tnorecon-org@mailman.boulder.swri.edu' $
              ]
         if debug then begin
            print,'To:  ',toaddr
            print,'Cc:  ',ccaddr[*]
            print,'Bcc: ',bccaddr
            print,'Subject: ',subject
            for j=0,n_elements(msg)-1 do print,msg[j]
         endif else begin
            mailmsg,toaddr,subject,msg, $
               ccaddr=ccaddr,bccaddr=bccaddr,replyto=replyto
         endelse
         nsend++
      endfor
      print,'Sent ',strn(nsend),' reminder mail messages.'
   endelse

   free_lun,dblun

end
