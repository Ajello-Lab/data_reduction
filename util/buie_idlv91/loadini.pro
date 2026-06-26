;+
; NAME:
;  loadini
; PURPOSE:   (one line only)
;  Load configuration information file
; DESCRIPTION:
;  The structure returned is built to support extracting information
;    using the routine, GETVALUE.  You provide a section, name, and can
;    retrieve a value.  For more information on this step, consult the
;    documentation for getvalue.
;
;  One important factor implemented in this routine is to read the information
;    in the configuration file and capture it in an anonymous structure
;    for processing.  The information is stored in sections demarked with
;    [string] at the start where "string" is the name of the section.  Within
;    as section, you have one or more keyword/value pairs separated by '='.
;    This routine does not try to interpret anything in the file.
;
;  Comments are lines that start with #.  Comments and blank lines are
;    ignored by this routine.
;
;  After reading, every line that is a keyword/value pair is associated
;    with its section.  The keyword is set off from the value by =.  The
;    first = seen is used to split the line.  Obviously, = cannot be included
;    in a keyword.  Spaces around this first = are trimmed.  Technically,
;    anything other than = can be in the keyword but common use suggests
;    that a keyword be restricted to alphanumeric (no spaces or special
;    characters).  The value string is kept as it appears in the file
;    except that multiple spaces are compresssed to a single space and
;    leading and trailing spaces are dropped.  There is no formal limit on
;    the length of a value but it's really not intended to be arbitrarily
;    long.
;
; CATEGORY:
;  Miscellaneous
; CALLING SEQUENCE:
;  loadini,info
; INPUTS:
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;  FILE - Configuration file to read, default='config.ini'
; OUTPUTS:
;  info - anonymous structure with information read from file
;           type and version are parsed from configurate in section [id]
;           all values are read as strings (multiple blanks are compressed
;           to one).  The section, name and value are returned as string
;           arrays.  The structure has tages of type, version, section,
;           name, and value.  In case of errors reading the file you will
;           get a scalar 0 back
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  Written by Marc W. Buie, Southwest Research Institute, 2020/06/22
;  2021/01/04, MWB, added support for comment lines (start with #), and
;                     handling empty values (returned as empty strings).
;  2024/07/26, MWB, changed to be more permissive in characters allowed in
;                     the value.  In particular, '=' is now allowed.
;-
pro loadini,info,FILE=file

   self='loadini: '
   if badpar(file,[0,7],0,caller=self+'(FILE) ', $
                          default='config.ini') then return

   info=0

   if not exists(file) then begin
      print,self,'Configuration file ',file,' not found'
      return
   endif

   line=''

   openr,lun,file,/get_lun

   tag=''
   section=[]
   name=[]
   value=[]
   while not eof(lun) do begin
      readf,lun,line,format='(a)'
      if strmid(line,0,1) eq '#' then line=''
      line=strtrim(line,2)
      line=strcompress(line)
      firstchar=strmid(line,0,1)
      if tag eq '' and firstchar ne '[' then continue
      if line eq '' then continue

      if firstchar eq '[' then begin
         tag=strmid(line,1,strlen(line)-2)
      endif else begin
         pos = strpos(line,'=')
         if pos le 0 then continue
         newname=strtrim(strmid(line,0,pos),2)
         newvalue=strtrim(strmid(line,pos+1),2)

         section=[section,tag]
         name=[name,newname]
         value=[value,newvalue]
      endelse
      
   endwhile

   free_lun,lun

   z=where(section eq 'id' and name eq 'type',count)
   if count ne 0 then begin
      type=value[z[0]]
   endif else begin
      type='unknown'
   endelse
   z=where(section eq 'id' and name eq 'version',count)
   if count ne 0 then begin
      version=value[z[0]]
   endif else begin
      version='unknown'
   endelse

   info = { type: type, $
            version: version, $
            section: section, $
            name: name, $
            value: value $
            }

end
