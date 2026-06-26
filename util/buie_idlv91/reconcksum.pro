;+
; NAME:
;  reconcksum
; PURPOSE:   (one line only)
;  Generate or refresh video data checksums
; DESCRIPTION:
; CATEGORY:
;  Database
; CALLING SEQUENCE:
;  reconcksum
; INPUTS:
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;  MAXTODO - maximum number of updates to perform
; OUTPUTS:
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  2016/03/10, Written by Marc W. Buie, Southwest Research Institute
;  2020/12/30, MWB, added more debug output on bad file mode error
;-
pro reconcksum,MAXTODO=maxtodo

   self='reconcksum: '
   if badpar(maxtodo,[0,1,2,3],0,caller='(MAXTODO) ',default=0) then return

   droot='/net/mandor/raid/buie/'

   jdstr,systime(/julian,/ut),0,str

   print,self,'execution started at ',str

   ndone=0
   msg=[]
   c=','

   openmysql,dblun,'recon'

   ; This first block works on building checksums that do not yet exist.
   cmd=['select idx,dir,filename', $
        'from data', $
        'where cksum is NULL', $
        'and dir is not NULL', $
        'order by idx']
   if maxtodo gt 0 then $
      cmd=[cmd,'limit '+strn(maxtodo)]
   cmd=[cmd,';']
   print,cmd
   mysqlquery,dblun,cmd,idx,dir,filename,format='l,a,a',ngood=nfiles

   print,strn(nfiles),' datasets missing checksums'
   if nfiles gt 0 then begin
      jdstr,systime(/julian,/ut),0,str
      msg=[msg, $
           'Execution started at '+str+' UT', $
           'Adding '+strn(nfiles)+' new checksum entries.']
      for i=0,nfiles-1 do begin
         fn=droot+dir[i]+filename[i]
         finfo=file_info(fn)
         if (finfo.mode and '444'O) eq '444'O then begin
            spawn,'cksum "'+fn+'"',result
            words=strsplit(result,' ',/extract)
            jdstr,systime(/julian,/ut),300,tstr
            cmd=['update data set', $
                 'cksum='+words[0]+c, $
                 'chkdate='+tstr, $
                 'where idx='+strn(idx[i])+';']
            mysqlcmd,dblun,cmd
         endif else begin
            print,'idx[i]=',strn(idx[i])
            print,'droot=',droot
            print,'dir[i]=',dir[i]
            print,'filename[i]=',filename[i]
            print,fn
            print,'skipping: bad file mode ',strn(finfo.mode,format='(O10)')
         endelse
      endfor
   endif

   ; deal with files that have changed since the checksum

bailout:
   free_lun,dblun

   if n_elements(msg) gt 0 then begin
      jdstr,systime(/julian,/ut),0,str
      msg=[msg,'Execution completed at '+str+' UT']
      for i=0,n_elements(msg)-1 do print,msg[i]
      sendto='buie@boulder.swri.edu'
      ccto=['john.m.keller@colorado.edu']
      subject='[RECONCKSUM] '+strn(nfiles)+' checksums added'
      mailmsg,sendto,subject,msg,ccaddr=ccto
   endif

end
