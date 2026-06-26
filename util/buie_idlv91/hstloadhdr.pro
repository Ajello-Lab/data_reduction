;+
; NAME:
;  hstloadhdr
; PURPOSE: 
;  Extract information from HST data headers and put in hstast.header database.
; DESCRIPTION:
;  Reads all xxxxxx_flc.fits.gz files and creates rows in the header table.
; CATEGORY:
;  Database
; CALLING SEQUENCE:
;  hstloadhdr
; INPUTS:
; OPTIONAL INPUT PARAMETERS:
; OUTPUT:
; KEYWORD INPUT PARAMETERS:
;   PATH        - Path to data directory,
;                   default is '/net/amber/raid1/buie/hst/12897/'
;   OVERWRITE   - Will overwrite existing files and data base entries if
;                 specified, otherwise it will issue warnings on duplicates and
;                 leave the existing entries unchanged. If two data base entries
;                 are found for something to be added, it is considered a fatal
;                 error and the loadhdr will terminate  unconditionally.
;            
; COMMON BLOCKS:
; SIDE EFFECTS:
;  Creates entries in the hstast.header table for each _flc.fits.gz file 
;    encountered.
; RESTRICTIONS:
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
; PROCEDURE:
; MODIFICATION HISTORY:
;  2012/07/02, Written by Marc W. Buie, Southwest Research Institute, cloned
;                  from loadhdr.pro from the HST Cycle 17 project.
;  2014/07/01, MWB, cloned for the NH search, program 13633.
;  2016/07/15, MWB, added PROPID keyword
;  2018/01/03, MWB, removed PROPID, now doesn't care what propid is.
;  2020/06/22, MWB, generalized to hstast database and can be used for
;                    all data, regardless of program
;-
pro hstloadhdr,PATH=path,DEBUG=debug,OVERWRITE=overwrite

   pname='loadhdr'
   self = pname+': '
   getddir,info,def_path
   if badpar(path,[0,7],0,caller=self+'(PATH)',default=def_path) then return
   if badpar(debug,[1,2,3,0],     0, caller=self + '(DEBUG)', $
             default=0)            then return
   if badpar(overwrite,[1,2,3,0],     0, caller=self + '(OVERWRITE)', $
             default=0)            then return

   getvalue,info,'global','ndet',ndet,default=-999,type=2
   if ndet eq -999 then begin
      print,'ndet in global section of config file is missing'
      return
   endif
   if ndet le 0 or ndet gt 2 then begin
      print,'Illegal value for ndet in config file must be 1 or 2'
      return
   endif
   getvalue,info,'global','subdir',subdir

;  stats
   newfiles = 0
   newrows = 0
   nofiles = 0
   nodbentries = 0
   dirty=0

   path = addslash(path)

   ; open the data base
   openmysql,dblun,'hstast',nodb
   c=','
   if nodb ne 0 then begin
      print, self, ' cannot open data base hstast'
      return
   endif
            
   fnlist=file_search(path+'*_flc.fits.gz',/test_regular,count=nfiles)
   if debug then begin
      print,strn(nfiles),' files to process.'
      print,'First ',fnlist[0]
      print,'Last  ',fnlist[-1]
   endif
   fnlist=strmid(fnlist,strlen(path))

   for i=0,nfiles-1 do begin
      words=strsplit(fnlist[i],'_',/extract)
      root=words[0]

      if debug then print,' processing ',fnlist[i]

      query = 'select count(*) from header where root='+quote(root)+';'
      if debug then print,query
      mysqlquery,dblun, query, ncheck,format='(i)'
      if debug then print,strn(ncheck),' entries found.'

      if ncheck eq ndet then begin
         if overwrite then begin
            print,'Erase prior values for ',root
            cmd = 'delete from header where root='+quote(root)+';'
            dirty=1
            mysqlcmd,dblun,cmd
            nofiles++
            nodbentries += ncheck
         endif else continue
      endif else if ncheck ne 0 then begin
         print, 'ERROR: ',strn(ncheck), 'entries for ',fnlist[i],' exist in db'
         print, 'THIS IS A FATAL ERROR!!'
         print, 'This must be fixed prior to running '+self +' again'
         return
      endif

      dirty=1
      if ndet eq 1 then begin
         hstloadone,fnlist[i],1,dblun,PATH=path
      endif else begin
         hstloadone,fnlist[i],4,dblun,PATH=path
         hstloadone,fnlist[i],1,dblun,PATH=path
      endelse
      newrows += ndet
      newfiles++

   endfor

   if dirty then begin
      context=subdir
      action=strn(newrows)+' rows added for '+strn(newfiles)+' files'
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

   free_lun,dblun

   print,strn(newrows),' rows added for ',strn(newfiles),' files'
   if overwrite then print,strn(nofiles),' files overwritten, ', $
                           strn(nodbentries), $
                           ' database entries deleted before replacing'

end
