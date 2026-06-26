;+
; NAME:
;  parseop
; PURPOSE:   (one line only)
;  Parse an operation line from photometry reduction, reduc.inf, file
; DESCRIPTION:
; CATEGORY:
;  Photometry
; CALLING SEQUENCE:
;  parseop,str,info
; INPUTS:
;  str - A single string with information to parse
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
; OUTPUTS:
;  info - Structure with information pulled from the string.
;           This structure will always exist with at least one
;           tag, info.error.  This will indicate if the parsing
;           was successful.  A successfully parse operation string
;           will have all of these tags:
;             op    - the name of the operation
;             nvals - the number of values found for the operation
;             vals  - the values found for the operation
;             error - Flag, set if there is an error with the line
;                       zero if the line is parsed successfully
;             ok    - Operation is flagged as "ok", this is used to
;                       tell the calling program that this rule has
;                       already been run and probably doesn't need it
;                       again.
;           The tags above are always relevant.  The tags below are
;           always processed but may or may not be relevant for a
;           given operation.
;             k2    - second order extinction and error (2-element array)
;             ct    - color term and error (2-element array)
;             k     - extinction and error (2-element array)
;             c2    - color^2 term and error (2-element array)
;             x2    - airmass^2 term and error (2-element array)
;             kt    - Flag, if set requests time-dependent extinction term
;             td    - String, rundate of night to use transformation from
;             rc    - Reference color
;             rx    - Reference airmass
;           All of these are given sensible default values (0, false,
;           empty string) as dictated by the variable.
;           
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  Written by Marc W. Buie, Southwest Research Institute, 2014/03/06
;  2018/03/12, MWB, added support for fixed reference airmass and color
;-
pro parseop,str,info

   self='parseop: '
   if badpar(str,7,0,caller=self+'(str) ') then return

   ; break up the line into blank delimited chunks
   lclstr=strcompress(strtrim(str,2))
   words = strsplit(lclstr,' ',/extract)

   ; First, look through the words for the optional information that could
   ;   be present, when found, remove them from the array.
   
   z=trimrank(where(words eq 'k2',count))
   if count eq 0 then begin
      k2=[0.,0.]
      k2fit=1B
   endif else if count gt 1 then begin
      print,str
      print,self,'Error! k2 found more than once'
      goto,bailout
   endif else begin
      k2=float(words[z+1:z+2])
      k2fit=0B
      delelem,words,z,-3
   endelse

   z=trimrank(where(words eq 'ct',count))
   if count eq 0 then begin
      ct=[0.,0.]
      ctfit=1B
   endif else if count gt 1 then begin
      print,str
      print,self,'Error! ct found more than once'
      goto,bailout
   endif else begin
      ct=float(words[z+1:z+2])
      ctfit=0B
      delelem,words,z,-3
   endelse

   z=trimrank(where(words eq 'k',count))
   if count eq 0 then begin
      k=[0.,0.]
      kfit=1B
   endif else if count gt 1 then begin
      print,str
      print,self,'Error! k found more than once'
      goto,bailout
   endif else begin
      k=float(words[z+1:z+2])
      kfit=0B
      delelem,words,z,-3
   endelse

   z=trimrank(where(words eq 'c2',count))
   if count eq 0 then begin
      c2=[0.,0.]
      c2fit=1B
   endif else if count gt 1 then begin
      print,str
      print,self,'Error! c2 found more than once'
      goto,bailout
   endif else begin
      c2=float(words[z+1:z+2])
      c2fit=0B
      delelem,words,z,-3
   endelse

   z=trimrank(where(words eq 'x2',count))
   if count eq 0 then begin
      x2=[0.,0.]
      x2fit=1B
   endif else if count gt 1 then begin
      print,str
      print,self,'Error! x2 found more than once'
      goto,bailout
   endif else begin
      x2=float(words[z+1:z+2])
      x2fit=0B
      delelem,words,z,-3
   endelse

   z=trimrank(where(words eq 'kt',count))
   if count eq 0 then begin
      kt=0B
   endif else if count gt 1 then begin
      print,str
      print,self,'Error! kt found more than once'
      goto,bailout
   endif else begin
      kt=1B
      delelem,words,z
   endelse

   z=trimrank(where(words eq 'td',count))
   if count eq 0 then begin
      td=''
   endif else if count gt 1 then begin
      print,str
      print,self,'Error! td found more than once'
      goto,bailout
   endif else begin
      td=words[z+1]
      delelem,words,z,-2
   endelse

   z=trimrank(where(words eq 'rc',count))
   if count eq 0 then begin
      rc=999.
   endif else if count gt 1 then begin
      print,str
      print,self,'Error! rc found more than once'
      goto,bailout
   endif else begin
      rc=float(words[z+1])
      delelem,words,z,-2
   endelse

   z=trimrank(where(words eq 'rx',count))
   if count eq 0 then begin
      rx=999.
   endif else if count gt 1 then begin
      print,str
      print,self,'Error! rx found more than once'
      goto,bailout
   endif else begin
      rx=float(words[z+1])
      delelem,words,z,-2
   endelse

   z=trimrank(where(words eq 'ok',count))
   if count eq 0 then begin
      ok=0B
   endif else if count gt 1 then begin
      print,str
      print,self,'Error! ok found more than once'
      goto,bailout
   endif else begin
      ok=1B
      delelem,words,z
   endelse

   error=0B

   op=words[0]
   delelem,words,0

   nwords=n_elements(words)
   case op of
      '2c': begin
         if nwords ne 12 then begin
            print,str
            print,self,'Error! wrong number of arguments for 2c rule.'
            goto,bailout
         endif
      end
      'dp': begin
         if nwords ne 7 then begin
            print,str
            print,self,'Error! wrong number of arguments for dp rule.'
            goto,bailout
         endif
      end
      'lc': begin
         if nwords ne 9 then begin
            print,str
            print,self,'Error! wrong number of arguments for lc rule.'
            goto,bailout
         endif
      end
      'sl': begin
         if nwords ne 6 then begin
            print,str
            print,self,'Error! wrong number of arguments for sl rule.'
            goto,bailout
         endif
      end
      'tr': begin
         if nwords ne 5 then begin
            print,str
            print,self,'Error! wrong number of arguments for tr rule.'
            goto,bailout
         endif
      end
      else: begin
         print,str
         print,self,'Error! unrecognized rule.'
         goto,bailout
      end
   endcase

   info={ str:   lclstr, $
          op:    op, $
          nvals: nwords, $
          vals:  words, $
          k2:    k2, $
          k2fit: k2fit, $
          ct:    ct, $
          ctfit: ctfit, $
          k:     k, $
          kfit:  kfit, $
          c2:    c2, $
          c2fit: c2fit, $
          x2:    x2, $
          x2fit: x2fit, $
          kt:    kt, $
          td:    td, $
          rc:    rc, $
          rx:    rx, $
          ok:    ok, $
          error: 0B }

   return

bailout:
   info={error: 1B}

end
