;+
; NAME:
;  hstloadone
; PURPOSE: 
;  Read one header of a flt file and push select keywords to database
; DESCRIPTION:
;  Opens the file and extracts keywords with READFITS. An appropriate INSERT
;  or optionally, REPLACE command for mysql is performed. 
; CATEGORY:
;  Database
; CALLING SEQUENCE:
;  hstloadone,fn,exten,dblun
; INPUTS:
;  fitsname   - String  which identifies the compressed
;               .gz file to be processed. The value of PATH is prepended.
; OPTIONAL INPUT PARAMETERS:
; OUTPUT:
; KEYWORD INPUT PARAMETERS:
; PATH        - Path to locate fitsname, default is
;               '/net/amber/raid1/buie/hst/12897/'
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  2007/03/28, Written by Peter L. Collins, Lowell Observatory
;  2012/07/02, MWB, cloned for C19 data
;  2014/07/01, MWB, cloned for the NH search, program 13633.
;  2020/06/22, MWB, generalized version
;-
pro hstloadone,fn,exten,dblun,PATH=path

   self = 'hstloadone: '
   if badpar(fn,7,0,caller=self+'(fn)') then return
   if badpar(exten,[2,3],0,caller=self+'(exten)') then return
   if badpar(path,[0,7],0,caller=self+'(PATH)', $
             default='') then return

   words=strsplit(fn,'_',/extract)
   root=words[0]

   ; read the FITS header
   fits_read,path+fn,img,hdr,/header_only,exten=exten
   keywords = [ 'ROOTNAME','PROPOSID','ROOTNAME','ROOTNAME','ROOTNAME', $
                'TARGNAME','CRVAL1', 'CRVAL2', 'CRPIX1','CRPIX2', $
                'CD1_1', 'CD1_2', 'CD2_1', 'CD2_2', 'ORIENTAT', 'PHOTMODE', $
                'PHOTFLAM', 'PHOTZPT', 'PHOTPLAM', 'PHOTBW',  $
                'FILTER', 'ATODGNC', 'READNSEC', 'BIASLEVC', 'PA_V3',  $
                'SUNANGLE', 'MOONANGL', 'SUN_ALT', $
                'DATE-OBS', 'TIME-OBS', 'EXPSTART', 'EXPEND', 'EXPTIME', $
                'SHUTRPOS', 'CENTERA1', 'CENTERA2', 'SIZAXIS1', 'SIZAXIS2', $
                'RA_TARG', 'DEC_TARG', 'POSTARG1', 'POSTARG2', $
                'PATTSTEP', 'LINENUM']

   ; go extract the strings from the fits hdr into values. Text items will
   ; be quoted. This requires that anything that is a char or text item
   ; in the db is also a text item for FITS.
   values = keywords
   for j =0, n_elements(keywords)-1 do begin
      ; header string for next keyword
      keyh = where(strtrim(strmid(hdr,0,8),2) eq keywords[j], count) 
      if count ne 1 then begin
         ; not there or duplicate
         if count eq 0 then begin
            print, 'can''t find keyword ', keywords[j]
            return
         endif else begin
;            print, 'duplicate keyword ', keywords[j] 
;            print,h[keyh]
;            print,'Keeping first reference'
            keyh=keyh[0]
         endelse
      endif
      ; get everything on the other side of '='
      q1 = strsplit(hdr[keyh], '=', /EXTRACT)
      q = strjoin(q1[1:*])

      singlequote = "'"
      forwardslash = "/"
      ; excise the comment field- note '/' inside '' string will break this
      ; The following quick test to mitigate that problem.
      sq = 0
      fs = 0
      for k = 0, strlen(q)-1 do begin
         if strmid(q,k,1) eq singlequote then sq++
         if strmid(q,k,1) eq forwardslash then fs++
         if fs gt 0 and sq eq 1 then begin
            print, 'There is a / in a quoted text string for ', keywords[j]
            print, 'This parser is befuddled, aborting'
            return
         endif else if fs gt 0 and ( sq eq 0 or sq eq 2 ) then break
      endfor
         
      q1 =  strsplit(q, '/', /EXTRACT)
      q = q1[0]

      ; is it a quoted text string.
      nsing = strpos(q, singlequote)
      if nsing ge 0 then begin
         ;yes- find up to the close quote. 
         ;(Does FITS allow embedded single quote??)
         q2 = strsplit(strmid(q,nsing), singlequote,/EXTRACT)
         v = quote(q2[0])
      endif else begin
         ; break on the white space
         q2 = strsplit(q, ' ',/EXTRACT)
         v = q2[0]
      endelse
      values[j] = v
   endfor

   uvis=sxpar(hdr,'CCDCHIP')
   c=','
   ; derive visitid and dateobs derived columns.
   roots = where(keywords eq 'ROOTNAME')

   ; this code is a funky bit written by Peter and the reason for the funk
   ;   is not clear but it works so it's been left alone
   values[roots]=quote(root)
   ; the FIRST ROOTNAME is  ROOT
   ; the SECOND ROOTNAME is for detector number
   ; the THIRD ROOTNAME is for visitid
   ; the FOURTH ROOTNAME is for expid

   pep =  where(keywords eq 'LINENUM')   ; get visitid from this field
   date =  where(keywords eq 'DATE-OBS')  ; this field joined to its successor
   v = strsplit(values[pep],'.',/EXTRACT)
   values[roots[1]]=strn(uvis)
   values[roots[2]]=quote(strmid(dequote(values[roots[0]]),4,2))
   values[roots[3]]=strmid(v[1],0,strlen(v[1])-1) ; drop trailing quote
   ; date time fields- some kludge here
   values[date]= strmid(values[date], 0,strlen(values[date])-1) + ' '
   values[date] += strmid(values[date+1],1)

   ; produce printable value string
   pvalues = [values[0:date], values[date+2:*]]
   ;print, 'VALUES ( ',pvalues[0],  ',' + pvalues[1:*],')'

   ; pre-pend NULL for the index at the start
   pvalues=['NULL',pvalues]

   insertor=['insert into header', $
              'values (',pvalues[0]+c,pvalues[1:-2]+c,pvalues[-1]+');']
   print,fn
   print,insertor
   mysqlcmd, dblun,insertor,resp, nlines                  

end

