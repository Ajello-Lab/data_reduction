;+
; NAME:
;  bspinfo
; PURPOSE:   (one line only)
;  Parse the output of BRIEF to get information about a spice kernel.
; DESCRIPTION:
; CATEGORY:
;  Miscellaneous
; CALLING SEQUENCE:
;  bspinfo,fnbsp,info
; INPUTS:
;  fnbsp - name of a single spice kernel
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
; OUTPUTS:
;  info - anonymous structure with information pulled from output of brief
; KEYWORD OUTPUT PARAMETERS:
; CONFIGURATION:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  Written by Marc W. Buie, Southwest Research Institute, 2024/11/05
;-
pro bspinfo,fnbsp,info

   self='bspinfo: '
   if badpar(fnbsp,7,0,caller=self+'(fnbsp) ') then return

   naifdir=getenv('NAIF_DIR')
   if naifdir ne '' then naifdir=addslash(naifdir)

   if nofile(naifdir+fnbsp,'Input spice kernel file') then return

   mtime = file_modtime(naifdir+fnbsp)
   jdmtime = jdparse('1970-01-01 0')+mtime/86400.0d0
   jdstr,jdmtime,0,jdmtimes,timesep='-'
;print,mtime
;print,jdmtime
;print,jdmtimes

;print,naifdir

   cmd='brief -t '+naifdir+fnbsp
;print,cmd
   spawn,cmd,result

;print,result

   nobj=0
   start=1
   code=[]
   name=[]
   time1s=[]
   time2s=[]
   for i=0,n_elements(result)-1 do begin
      if start then begin
         if strmid(result[i],0,3) ne '---' then continue
         start=0
         pos1=strpos(result[i],' -')
         pos2=strpos(result[i],' -',pos1+1)
;print,pos1,pos2
;print,result[i]
      endif else begin
         word1=strmid(result[i],0,pos1-1)
         word2=strmid(result[i],pos1+1,pos2-pos1-1)
         word3=strmid(result[i],pos2+1)
         word1=strtrim(word1,2)
         word2=strtrim(word2,2)
         word3=strtrim(word3,2)
;print,'[',word1,'] [',word2,'] [',word3,']'

         pos=strpos(word1,' ')
;print,pos
         if pos gt 0 then begin
            code=[code,'P'+strmid(word1,0,pos)]
            name=[name,strtrim(strmid(word1,pos+1),2)]
         endif else begin
            code=[code,'P'+word1]
            name=[name,'update']
         endelse

         if strpos(word2,'Same') ge 0 then begin
            time1s=[time1s,time1s[-1]]
            time2s=[time2s,time2s[-1]]
         endif else begin
            date=strmid(word2,0,11)
            mon2num,date,res,fulldate=3
            res=repchar(res,' ','-')
            word2=res+strmid(word2,11)
            date=strmid(word3,0,11)
            mon2num,date,res,fulldate=3
            res=repchar(res,' ','-')
            word3=res+strmid(word3,11)
            time1s=[time1s,word2]
            time2s=[time2s,word3]
         endelse
         nobj++

      endelse
   endfor
;print,code
;print,name
;print,time1s
;print,time2s

   info={ $
      nobj: nobj, $
      code: code, $
      name: name, $
      time1s: time1s, $
      time2s: time2s, $
      ftime: jdmtimes $
      }

;help,info,/st

end
