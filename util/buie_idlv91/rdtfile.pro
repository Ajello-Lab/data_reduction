;+
; NAME:
;  rdtfile
; PURPOSE:
;  Reads the template file, tfile.dat, for a given night.
; DESCRIPTION:
;  This program reads in the first line of the tfile.dat for a specific
;  observing night and returns the information the file contains.
; CATEGORY:
;  File I/O 
; CALLING SEQUENCE:
;  rdtfile,object,tempobj,marker,fnstack,outsize,degree,error
; INPUTS:
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;  LOWELL  - Flag, if set will require the tfile to be in a Lowell format
;               to successfully return information.
;  PATH    - Optional string for the path to the tfile, default is the
;               current directory.
; OUTPUTS:
;  object  - the object for the image data
;  tempobj - object name of template for the image
;  marker  - unique identifier
;  fnstack - name of image template
;  outsize - size of output difference image
;  degree  - order of variation
;  error   - Flag, set if there was a problem.  This could be because the
;              file does not exist or that the night has been marked bad.
;              In any case, if this comes back set to 1 then you should
;              not proceed with this dataset.
; KEYWORD OUTPUT PARAMETERS:
;  OBSCODE - Observatory code for the data.  This is a guess based on
;               various heuristics and from the file format.
;  COMMENTS- any text after the first line
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURES:
;  Reads in the tfile.dat from an observing night and returns the first
;  line of information.
; MODIFICATION HISTORY:
; 2012/01/24 Written by Erin R. George, Southwest Research Institute
; 2012/03/27, MWB, added support for NMSU data
; 2012/07/25, BLE, add COMMENTS keyword
; 2014/02/04, MWB, added PATH keyword and some clean up
;-
pro rdtfile,object,tempobj,marker,fnstack,outsize,degree,error, $
       obscode=obscode,LOWELL=lowell,COMMENTS=comments,PATH=path

   error=1
   self='(rdtfile): '
   if badpar(lowell,[0,1,2,3],0,caller=self+'(LOWELL) ',default=0) then return
   if badpar(path,[0,7],0,caller=self+'(PATH) ',default='') then return
   if path ne '' then path=addslash(path)

   error=0

   ; Load the template file information
   if not exists(path+'tfile.dat') then begin
      print,self,'The template file descriptor, ',path, $
                 'tfile.dat, does not exist.  Aborting.'
      error=1
      return
   endif

   ; Open the file and read the first line of data.
   openr,lun,path+'tfile.dat',/get_lun
   line=''
   readf,lun,line,format='(a)'

   ; Read any additional lines as comments.
   ; Skip all blank lines until we reach a non-blank one.
   cline=''
   comments=''
   while ~eof(lun) do begin
      readf,lun,cline,format='(a)'
      if strlen(cline) eq 0 and n_elements(comments) eq 1 then continue
      comments=[comments,cline]
   endwhile
   nlines=n_elements(comments)
   if nlines gt 1 then comments=comments[1:nlines-1]
   free_lun,lun

   ; Parse the template file information
   line=strtrim(strcompress(line),2)
   if line eq 'bad' then begin
      if path eq '' then begin
         print,self,'The data in the current directory have been marked bad.', $
                 ' Aborting.'
      endif else begin
         print,self,'The data in ',path,' have been marked bad.', $
                 ' Aborting.'
      endelse
      error=1
      return
   endif

   words=strsplit(line,' ',/extract)

   ; Here's where it gets tricky, the Lowell data will have exactly
   ;   six fields in the data.   The LCO data will have 4.  If no requirements
   ;   are placed on the file then it will adapt to whatever format it finds.

   if lowell and n_elements(words) ne 6 then begin
      print,self,'tfile is not in Lowell format and this was required.'
      error=1
      return
   endif

   if n_elements(words) eq 6 then begin
      object=words[0]
      tempobj=words[1]
      marker=words[2]
      fnstack=words[3]
      outsize=fix(words[4])
      degree=fix(words[5])
      obscode='688'
   endif else if n_elements(words) eq 4 then begin
      object='Pluto'
      tempobj='Pluto'
      marker=words[0]
      fnstack=words[1]
      outsize=fix(words[2])
      degree=fix(words[3])
      obscode='E10'
   endif else if n_elements(words) eq 5 then begin
      object='Pluto'
      tempobj='Pluto'
      marker=words[0]
      fnstack=words[1]
      outsize=fix(words[2])
      degree=fix(words[3])
      obscode=words[4]
   endif else begin
      print,self,'The contents of tfile.dat are incorrect. Aborting.'
      error=1
      return
   endelse

end
