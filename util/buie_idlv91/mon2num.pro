;+
; NAME:
;  mon2num
; PURPOSE:   (one line only)
;  Convert the name of a month to its integer equivalent.
; DESCRIPTION:
; CATEGORY:
;  Utility
; CALLING SEQUENCE:
;  mon2num, name, result
; INPUTS:
;  name - String or string array with the name of a month, the case does
;            not matter and the string need only be long enough to be unique.
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;  FULLDATE - This keyword controls what is done with the input string.  If
;                not supplied, or, set to zero, the input is expected to be
;                just the name of the month.  This is the default behavior.
;                The result will be an integer for the number of the month.
;             If a non-zero value is supplied, the number indicates the
;                maximum number of characters from the name of the month to
;                search to find a match.  The smallest useful value is 3
;                but if you provide a smaller number you may not get a correct
;                answer.  The largest useful value is 9 but if you provide
;                a larger number it will still work.  In this case, the result
;                and an edited version of the input string where the name of
;                the month is replaced with a two digit zero-padded number.
; OUTPUTS:
;  result - Number of the month (1=Jan, 2=Feb, etc.), or, if FULLDATE is set
;              the result is an edited version of the input string.
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
;  If the name you give is not unique, the first match is returned.
;  If the name you give is not correct, you get a 0 back for that entry.
; PROCEDURE:
; MODIFICATION HISTORY:
;  Written by Marc W. Buie, Lowell Observatory, 2008/02/03
;  2013/02/20, MWB, add trimrank call at the end, one element vector will
;                   now be returned as a scalar
;  2014/04/13, MWB, add FULLDATE keyword
;-
pro mon2num,name,num,FULLDATE=fulldate

   self='MON2NUM: '
   if badpar(name,7,[0,1],caller=self+'(name) ') then return
   if badpar(fulldate,[0,1,2,3],0,caller=self+'(FULLDATE) ', $
                                  default=0) then return

   months = [ 'January', 'February', 'March', 'April', 'May', 'June', $
              'July', 'August', 'September', 'October', 'November', 'December' ]

   if n_elements(name) eq 0 then num=0 else num=intarr(n_elements(name))

   if fulldate eq 0 then begin
      sname = name[uniq(name,sort(name))]
      for i=0,n_elements(sname)-1 do begin
         idx = where(strmatch(months,sname[i]+'*',/fold_case) eq 1)
         z=where(sname[i] eq name)
         num[z] = idx[0]+1
      endfor
   endif else begin
      num=strlowcase(name)
      months=strlowcase(months)
      strnum=string(indgen(12)+1,format='(i2.2)')
      for i=0,n_elements(months)-1 do begin
         pos=strpos(num,strmid(months[i],0,fulldate))
         z=where(pos ge 0,count)
         if count gt 0 then $
            num[z]=repchar(num[z],strmid(months[i],0,fulldate),strnum[i])
      endfor
   endelse

   num=trimrank(num)

end
