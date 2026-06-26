;+
; NAME:
;  loadbsp
; PURPOSE:   (one line only)
;  Update database information about Spice kernels
; DESCRIPTION:
; CATEGORY:
;  Miscellaneous
; CALLING SEQUENCE:
;  loadbsp
; INPUTS:
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;  MAXTODO - maximum number of files to process, this is a count of files
;              that lead to changes in the database.  Default is to do all.
;  NOSAVE  - Flag, if set will suppress making any changes.  The mysql
;              command that would execute is shown but nothing is done.
; OUTPUTS:
; KEYWORD OUTPUT PARAMETERS:
; CONFIGURATION:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  Written by Marc W. Buie, Southwest Research Institute, 2024/11/06
;-
pro loadbsp,MAXTODO=maxtodo,NOSAVE=nosave

   self='loadbsp: '
   if badpar(maxtodo,[0,2,3],0,caller=self+'(MAXTODO) ',default=-1) then return
   if badpar(nosave,[0,1,2,3],0,caller=self+'(NOSAVE) ',default=0) then return

   naifdir=getenv('NAIF_DIR')
   if naifdir ne '' then naifdir=addslash(naifdir)
   if nofile(naifdir,'NAIF cirectory') then return

   bsplist=file_search(naifdir+'*.bsp*',count=nfiles)
   bsplist=strmid(bsplist,strlen(naifdir))
;   print,strn(nfiles),' files found'

   openmysql,dblun,'recon'
   c=','

   ndone=0
   for i=0,nfiles-1 do begin
      bspinfo,bsplist[i],info
      update=0
      bspfile = bsplist[i]
      if strmid(bspfile,strlen(bspfile)-1) eq 'x' then begin
         bspfile=strmid(bspfile,0,strlen(bspfile)-1)
         active=0
      endif else begin
         active=1
      endelse
      for j=0,info.nobj-1 do begin
         cmd=['select count(*) from kernel', $
             'where bspfile='+quote(bspfile), $
             'and naifid='+quote(info.code[j]), $
             ';']
         mysqlquery,dblun,cmd,ncheck,format='i'
         if ncheck eq 0 then begin
            print,''
            print,'Add new record for ',bspfile
            cmd=['insert into kernel set', $
;                 'idx=NULL'+c, $
                 'naifid='+quote(info.code[j])+c, $
                 'name='+quote(nobname(info.name[j]))+c, $
                 'bspfile='+quote(bspfile)+c, $
                 'vdate='+quote(info.ftime)+c, $
                 'date1='+quote(info.time1s[j])+c, $
                 'date2='+quote(info.time2s[j])+c, $
                 'active='+strn(active), $
                 ';']
            print,cmd
            if not nosave then mysqlcmd,dblun,cmd
            update=1
         endif else begin
            update=0
            ; Is there anything out of sync between the db record and the
            ;   file on disk?  Here are the things to look for:
            ;  kernel.vdate doesn't match bspfile
            ;  kernel.active doesn't match active
            ;  if not saving, skip this check but this doesn't count
            ;    as an update for maxtodo.
            if not nosave then begin
               cmd=['select idx from kernel', $
                    'where bspfile='+quote(bspfile), $
                    'and naifid='+quote(info.code[j]), $
                    'and (vdate != '+quote(info.ftime), $
                    'or active != '+strn(active)+')', $
                    ';']
               mysqlquery,dblun,cmd,idx,format='i',ngood=ncheck
               if ncheck eq 1 then begin
                  cmd=['update kernel set', $
                       'vdate='+quote(info.ftime)+c, $
                       'active='+strn(active), $
                       'where idx='+strn(idx), $
                       ';']
                  print,'Existing record changed.'
                  print,cmd
                  mysqlcmd,dblun,cmd
               endif else if ncheck gt 1 then begin
                  print,'***ERROR***  more than one return found for this command'
                  print,cmd
                  goto,bailout
               endif
            endif
         endelse

         ; check for missing information in this record.
         if not nosave then begin
            cmd=['select idx from kernel', $
                 'where bspfile='+quote(bspfile), $
                 'and naifid='+quote(info.code[j]), $
                 ';']
            mysqlquery,dblun,cmd,idx,format='l'
            cmd='select getephid,name,source from kernel'+ $
                ' where idx='+strn(idx)+';'
            mysqlquery,dblun,cmd,getephid,name,source,format='a,a,a'
            if getephid eq 'NULL' then begin
               cmd=['select getephid from kernel', $
                    'where naifid='+quote(info.code[j]), $
                    'and getephid is not NULL', $
                    'group by getephid;']
               mysqlquery,dblun,cmd,id,format='a',ngood=nfound
               if nfound eq 0 then begin
                  answer=''
                  read,answer,prompt='getephid for '+info.code[j]+' '
                  cmd=['update kernel set', $
                       'getephid='+quote(answer), $
                       'where idx='+strn(idx)+';']
                  print,cmd
                  mysqlcmd,dblun,cmd
               endif else begin
                  cmd=['update kernel set', $
                       'getephid='+quote(id[0]), $
                       'where idx='+strn(idx)+';']
                  print,cmd
                  mysqlcmd,dblun,cmd
               endelse
            endif
            if name eq 'update' then begin
               cmd=['select name from kernel', $
                    'where naifid='+quote(info.code[j]), $
                    'and name != '+quote('update'), $
                    'group by name;']
               mysqlquery,dblun,cmd,kname,format='a',ngood=nfound
               if nfound eq 0 then begin
                  answer=''
                  read,answer,prompt='name for '+info.code[j]+' '
                  cmd=['update kernel set', $
                       'name='+quote(nobname(answer)), $
                       'where idx='+strn(idx)+';']
                  print,cmd
                  mysqlcmd,dblun,cmd
               endif else begin
                  cmd=['update kernel set', $
                       'name='+quote(kname[0]), $
                       'where idx='+strn(idx)+';']
                  print,cmd
                  mysqlcmd,dblun,cmd
               endelse
            endif
            if source eq 'NULL' then begin
               cmd=['select source from kernel', $
                    'where bspfile='+quote(bspfile), $
                    'and source is not NULL', $
                    'limit 1;']
               mysqlquery,dblun,cmd,source,format='a',ngood=nfound
               if nfound eq 0 then begin
                  answer=''
                  read,answer,prompt='Source for '+bspfile+' '
                  cmd=['update kernel set', $
                       'source='+quote(answer), $
                       'where idx='+strn(idx)+';']
                  print,cmd
                  mysqlcmd,dblun,cmd
               endif else begin
                  cmd=['update kernel set', $
                       'source='+quote(source), $
                       'where idx='+strn(idx)+';']
                  print,cmd
                  mysqlcmd,dblun,cmd
               endelse
            endif
         endif
      endfor
      if update then ndone++
      if ndone ge maxtodo and maxtodo gt 0 then break
   endfor

bailout:
   free_lun,dblun

end
