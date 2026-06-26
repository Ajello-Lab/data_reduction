;+
; NAME:
;  getddir
; PURPOSE:   (one line only)
;  Load configuration and resolve source data location
; DESCRIPTION:
; CATEGORY:
;  Miscellaneous
; CALLING SEQUENCE:
;  getdir,info,ddir
; INPUTS:
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;  FILE - Configuration file to read, default='config.ini'
;  VERBOSE - Flag, if set, prints extra information as it goes
; OUTPUTS:
;  info - Configuration information structure (see loadini.pro)
;  ddir - String containing path to data
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  Written by Marc W. Buie, Southwest Research Institute, 2020/06/22
;  2021/01/14, MWB, added VERBOSE option
;  2023/04/04, MWB, added logic to use dir from global for auto path,
;                      stemdir still overrides
;-
pro getddir,info,ddir,FILE=file,VERBOSE=verbose,TEAM=team

   self='getddir: '
   if badpar(team,[0,7],0,caller=self+'(TEAM) ',default='') then return
   if badpar(verbose,[0,1,2,3],0,caller=self+'(VERBOSE) ',default=0) then return

   loadini,info,FILE=file

   if team ne '' then begin
      if verbose then print,'TEAM ',team,' specified'
      getvalue,info,team,'stemdir',stemdir
      getvalue,info,team,'dirtime',dirtime
      if stemdir eq '' then begin
         getvalue,info,'global','dir',gdir
         if gdir eq '' then begin
            getvalue,info,'global','event',event
            getvalue,info,'global','date',date
            stemdir=event+'/'+team+'/'+date+'/'+dirtime+'/'
         endif else begin
            getvalue,info,'global','date',date
            stemdir=gdir+'/'+team+'/'+date+'/'+dirtime+'/'
         endelse
;         print,'automatic stemdir ',stemdir
      endif else begin
         stemdir=addslash(stemdir)+dirtime+'/'
;         print,'manual stemdir ',stemdir
      endelse
   endif else begin
      stemdir=''
   endelse

   getvalue,info,'ddir','',tdir
   ndir=n_elements(tdir)
   if keyword_set(verbose) then begin
      print,strn(ndir),' candidate directories found'
      print,tdir
   endif

   if ndir eq 1 and tdir[0] eq '' then begin
      ddir=''
   endif else begin
      ddir=''
      for i=0,ndir-1 do begin
         if exists(addslash(tdir[i])+stemdir) then begin
            ddir=addslash(tdir[i])
            break
         endif
      endfor
   endelse

   if keyword_set(verbose) then begin
      print,'Directory found: ',ddir
   endif

   getvalue,info,'global','subdir',value
   ddir=addslash(ddir+value)

   if keyword_set(verbose) then begin
      print,'Final directory guess: ',ddir
   endif

end
