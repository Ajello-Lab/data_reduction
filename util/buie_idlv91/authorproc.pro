;+
; NAME:
;     authorproc
; PURPOSE: (one line)
;     Create author lists for LaTeX manuscripts
; DESCRIPTION:
;  This tool automates the building of a AASTeX formatted authorlist for inclusion
;    in a manuscript along with a separate file of acknowledements.  All of the
;    control inputs are carried in a configuration file (usually config.ini) in
;    a directory where the output files are to be written.  Other than control
;    inputs, this program expects to find a tab-separated file containing the
;    raw data for the author list.  This file is usually created via a Google form
;    that the observers fill out with their information.  That form is then
;    exported for use by this program.  The file names are normally named with
;    embedded version date information and the file that sorts alphabetically to
;    the end of the list is used to build the output files.
;
;  This program attempts to validate the information.  Some errors are just noted
;    to the terminal while others will cause the program to quit prematurely and
;    not write any output files.  If there are any errors, you probably should not
;    use the output files until the errors are eliminated.
; CATEGORY:
;     Publications
; CALLING SEQUENCE:
;     authorproc, cfgfile, /verbose
; INPUTS:
;     cfgfile = Configuration file to control behavior (default = 'config.ini')
;               (must contain fields and campaigns keywords)
; OPTIONAL INPUT PARAMETERS:
; KEYWORD PARAMETERS:
;     verbose = Flag, if set will increase number of processing status messages
; OUTPUTS:
;   The primary output of this program is a set of files and are written to the current
;     directory..  Previous versions of these files are silently overwritten.
;     authorlist.tex - Markup text for the author list.  This uses the AASTeX package
;                        commands for building the author list.
;     acktext.tex - A block of text that can be added to the acknowledgement section.
;                      This is built from the list of respondents not requesting to be
;                      authors.
;     master.email - A list of email addresses for the authors, one address per line
;     allraw.txt - A list of all participants, one name per line but with a comma
;                     after each name but the last.  The order is randomized.
;     allauth.txt - A list of all authors, one name per line but with a comma after
;                     each name but the last.  The order matches the publication order.
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
;  It is a bad idea to directly edit the output of this program even though that
;    might seem like the easy thing to do.  The correct process is to edit the
;    source google spreadsheet, re-export the file, and run this program again until
;    the output correct.
; PROCEDURE:
;  grep this file for getvalue to see all the controls from the config file, some
;    of the less obvious or more complicated controls are described here.
;  [authorlist]
;    pubdir - This is the directory where the .tsv files are to be found.  
;    campaigns -  This is a blank delimited list of campaigns for this publication.
;                    It is required that there is one .tsv file for each campaign
;                    in this list.  There must be at least one campaign.  Every
;                    campaign must have its own section in the config file with
;                    information needed to control decoding the information and
;                    varying options.
;
;  [CAMPAIGN]
;    There need to be as many sections as there are campaigns to be aggregated
;      for the final list.  Replace "CAMPAIGN" with the official name of the campaign,
;      usually something like PA20220722 where the first two characters are from the
;      object or project and the rest is the UT date of the event.
;    The following are required items:
;      fields - This is a list of seven integers, blank delimited, that identify
;         which columns of the tsv file are to be used for the main input information
;         from the .tsv file for this campaign.  The seven numbers refer, in turn,
;         to the following conceptual field values:
;            fields[0] = publication order (primary sort key)
;            fields[1] = Last name of the respondent (secondary sort key)
;            fields[2] = First name plus any initials of the respondent
;            fields[3] = Category of respondent (not used by this program)
;            fields[4] = Email address of respondent (required only for authors)
;            fields[5] = Institution code (not the full institution)
;            fields[6] = ORCID
;         There are usually many more columns in the .tsv file than used by this program.
;    The following are optional items:
;       CJK - Name in unicode characters for a Japanese alphabet name, blank delimited
;       Arabic - Name in unicode characters for a name in Arabic form, blank delimited
;
;    The .tsv file is the last file matching the pattern of
;         pubdir+campaigns[i]+'_*.tsv'
;
;  There must also be a file named "address.tsv" in the current directory.  This file
;    is a tab-delimited file with two columns.  The first column is the institution
;    code (fields[5]).  The second column is the full name of the institution to be
;    included in the authorlist.  All authors must have a code that resolves to a
;    value in this file.
;   
; MODIFICATION HISTORY:
;  2023/12/09 - Written by Brian Keeney, SwRI, based on template from Marc Buie
;-
pro authorproc,cfgfile,verbose=verbose

  self='authorproc: '
  if badpar(cfgfile, [0,7], 0, caller=self+'(cfgfile) ', $
            default='config.ini') then return
  if badpar(verbose, [0,1,2,3], 0, caller=self+'(verbose) ', $
            default=0) then return
  
  if nofile(cfgfile,'Configuration') then return
  loadini, file=cfgfile, info

  getvalue, info, 'authorlist', 'pubdir', pubdir, default=''
  getvalue, info, 'authorlist', 'campaigns', campaigns, default='', /array
  if (campaigns[0] eq '') then begin
     print, cfgfile+' must contain campaigns keyword in authorlist stanza.'
     return
  endif

  order=[]
  last=[]
  first=[]
  category=[]
  email=[]
  inst=[]
  orcid=[]
  cjk_name=[]
  cjk_tex=[]
  arabic_name=[]
  arabic_tex=[]
  fns=[]
  
  for i=0,n_elements(campaigns)-1 do begin
     getvalue, info, campaigns[i], 'fields', fields, type=2, /array
     if (n_elements(fields) ne 7) then begin
        print, 'The fields keyword must have 7 elements ('+campaigns[i]+')'
        return
     endif

     getvalue, info, campaigns[i], 'CJK', cjk, default='', /array
     getvalue, info, campaigns[i], 'Arabic', arabic, default='', /array
     
     cmd='ls -rt '+pubdir+campaigns[i]+'_*.tsv | tail -1'
     spawn,cmd,answer
     if (answer[0] eq '') then begin
        print,'Nothing found for file pattern '+pubdir+campaigns[i]+'_*.tsv'
        return
     endif

     fn=trimrank(answer)
     fns=[fns,fn]
     if keyword_set(verbose) then print,'Reading ',fn

     line = ''
     openr, lun, fn, /get_lun
     readf, lun, line

     cols = strsplit(line, string(9b), /extract, /preserve_null)
     if (max(fields) gt n_elements(cols)-1) then begin
        print, 'Not enough columns in '+fn
        return
     endif
     
     if keyword_set(verbose) then begin
        print, 'order    is column '+strn(fields[0])+': '+cols[fields[0]]
        print, 'last     is column '+strn(fields[1])+': '+cols[fields[1]]
        print, 'first    is column '+strn(fields[2])+': '+cols[fields[2]]
        print, 'category is column '+strn(fields[3])+': '+cols[fields[3]]
        print, 'email    is column '+strn(fields[4])+': '+cols[fields[4]]
        print, 'inst     is column '+strn(fields[5])+': '+cols[fields[5]]
        print, 'orcid    is column '+strn(fields[6])+': '+cols[fields[6]]
        if (cjk[0] ne '') then $
          print, 'CJK      is column '+cjk[0]+': '+cols[fix(cjk[0])]
        if (arabic[0] ne '') then $
          print, 'Arabic   is column '+arabic[0]+': '+cols[fix(arabic[0])]
     endif

     table = []
     while ~eof(lun) do begin
        readf, lun, line
        cols  = strsplit(line, string(9b), /extract, /preserve_null)
        table = [[table], [cols]]
     endwhile
     free_lun, lun

     nread = n_elements(table[0,*])
     if keyword_set(verbose) then print,strn(nread),' entries found.'
     if (nread eq 0) then begin
        print,'No valid data found.'
        return
     endif

     order    = [order, fix(reform(table[fields[0],*]))]
     last     = [last,      reform(table[fields[1],*]) ]
     first    = [first,     reform(table[fields[2],*]) ]
     category = [category,  reform(table[fields[3],*]) ]
     email    = [email,     reform(table[fields[4],*]) ]
     inst     = [inst,      reform(table[fields[5],*]) ]
     orcid    = [orcid,     reform(table[fields[6],*]) ]

     if (cjk[0] eq '') then begin
        cjk_name = [cjk_name, strarr(n_elements(table[0,*]))]
        cjk_tex  = [cjk_tex,  strarr(n_elements(table[0,*]))]
     endif else begin
        cjk_name = [cjk_name, reform(table[fix(cjk[0]),*])]
        if (n_elements(cjk) gt 1) then begin
           cjk_tex  = [cjk_tex, $
                       replicate(cjk[1], n_elements(table[0,*]))]
        endif else begin
           cjk_tex  = [cjk_tex, strarr(n_elements(table[0,*]))]
        endelse
     endelse
     
     if (arabic[0] eq '') then begin
        arabic_name = [arabic_name, strarr(n_elements(table[0,*]))]
        arabic_tex  = [arabic_tex,  strarr(n_elements(table[0,*]))]
     endif else begin
        arabic_name = [arabic_name, reform(table[fix(arabic[0]),*])]
        if (n_elements(arabic) gt 1) then begin
           arabic_tex  = [arabic_tex, $
                          replicate(arabic[1], n_elements(table[0,*]))]
        endif else begin
           arabic_tex  = [arabic_tex, strarr(n_elements(table[0,*]))]
        endelse
     endelse
  endfor

  npeople = n_elements(order)
  if keyword_set(verbose) then print,strn(npeople),' names read in.'

  
  ; XXX Remove duplicates!!! XXX


   ; From here on the vast majority of the code is Marc's
   inst=strtrim(inst,2)
   orcid=strtrim(orcid,2)
   last=strtrim(last,2)
   first=strtrim(first,2)
   z=where(orcid eq '0',count)
   if count ne 0 then orcid[z]=''

   z1=sort(strlowcase(first))
   z2=sort(strlowcase(last[z1]))
   z3=sort(order[z1[z2]])
   idx=z1[z2[z3]]

   z=where(order gt 0 and order lt 99,count)
   if keyword_set(verbose) then print,strn(count),' authors'
   z=where(order eq 99,count)
   if keyword_set(verbose) then print,strn(count),' acknowledgements'

   ; some validation
   z=where(email eq '0',count)
   if count ne 0 then begin
      print,strn(count),' entries with blank email'
      print,last[z]
   endif

   z=where(order le 0 or order ge 100,count)
   if count ne 0 then begin
      print,strn(count),' entries with no order value, probably new'
      print,last[z]
   endif

   z=where(last eq '0' and order ne 99,count)
   if count ne 0 then begin
      print,strn(count),' entries with missing last names'
      print,email[z]
      last[z]='missing'
   endif
   z=where(last eq '0',count)
   if count ne 0 then last[z]=''

   z=where(first eq '0' and order ne 99,count)
   if count ne 0 then begin
      print,strn(count),' entries with missing first names'
      print,email[z]
      first[z]='missing'
   endif
   z=where(first eq '0',count)
   if count ne 0 then first[z]=''

   z=where(inst eq '0' and order ne 99,count)
   if count ne 0 then begin
      print,strn(count),' entries with missing institution'
      print,last[z]
      inst[z]='missing'
   endif
   z=where(inst eq '0',count)
   if count ne 0 then inst[z]=''

   ; build a unique set of institutions
   rawinst=[]
   for i=0,npeople-1 do begin
      if inst[i] eq '' then continue
      if order[i] eq 99 then continue
      words=strsplit(inst[i],',',/extract)
      words=strtrim(words,2)
      rawinst=[rawinst,words]
   endfor
   uinst=rawinst[uniq(rawinst,sort(rawinst))]
   nuinst=n_elements(uinst)
   if keyword_set(verbose) then print,strn(nuinst),' unique institutions'
   uaddr=strarr(nuinst)

   ; load the addresses
   if exists('address.tsv') then begin
      readcol,'address.tsv',rawinst,addr,format='a,a', $
                 delim=string(9b),count=naddr,/preserve_null
   endif else begin
      naddr=0
   endelse
   if keyword_set(verbose) then print,strn(naddr),' standard addresses loaded'

   if naddr gt 0 then begin
      for i=0,nuinst-1 do begin
         z=where(uinst[i] eq rawinst,count)
         if count ne 0 then begin
            uaddr[i]=addr[z[0]]
         endif
      endfor
   endif

   z=where(uaddr eq '',count)
   if count ne 0 then begin
      print,strn(count),' institutions that have no address'
      for i=0,count-1 do begin
         print,uinst[z[i]]
      endfor
      print,'You must provide a complete institute list before proceeding.'
      return
   endif

   fn1='authorlist.tex'
   fn2='acktext.tex'
   fn3='master.email'
   fn4='allraw.txt'
   fn5='allauth.txt'

   if exists(fn1) then file_move,fn1,fn1+'.bak',/overwrite
   if exists(fn2) then file_move,fn2,fn2+'.bak',/overwrite
   if exists(fn4) then file_move,fn4,fn4+'.bak',/overwrite

   times=systime(/ut)
   if keyword_set(verbose) then $
      print,'Write ',fn1,'  main latex include file with author list'
   openw,lun,fn1,/get_lun,width=400
   printf,lun,'%'
   printf,lun,'%'
   printf,lun,'%'
   printf,lun,'%'
   printf,lun,'% MACHINE GENERATED FILE, DO NOT EDIT!!!!'
   printf,lun,'%'
   printf,lun,'%'
   printf,lun,'%'
   printf,lun,'%'
   printf,lun,'%'
   printf,lun,'%'
   printf,lun,'% MACHINE GENERATED FILE, DO NOT EDIT!!!!'
   printf,lun,'%'
   printf,lun,'%'
   printf,lun,'%'
   printf,lun,'%'
   printf,lun,'%'
   printf,lun,'%'
   printf,lun,'% MACHINE GENERATED FILE, DO NOT EDIT!!!!'
   printf,lun,'%'
   printf,lun,'%'
   printf,lun,'%'
   printf,lun,'%'
   printf,lun,'%'
   printf,lun,'% built from '
   for i=0,n_elements(fns)-1 do printf,lun,'%   '+fns[i]
   printf,lun,'% generated at ',times,' UTC'
   printf,lun,'%'

   if keyword_set(verbose) then $
      print,'Write ',fn2,'     acknowledgement latex include file'
   openw,lun2,fn2,/get_lun

   if keyword_set(verbose) then $
      print,'Write ',fn3,'    master list of email addresses for co-authors'
   openw,lun3,fn3,/get_lun
   printf,lun3,'%'
   printf,lun3,'% MACHINE GENERATED FILE, DO NOT EDIT!!!!'
   printf,lun3,'%'
   printf,lun3,'% built from '
   for i=0,n_elements(fns)-1 do printf,lun3,'%   '+fns[i]
   printf,lun3,'% generated at ',times,' UTC'
   printf,lun3,'%'

   if keyword_set(verbose) then $
      print,'Write ',fn4,'      raw scrambled list of all participants'
   openw,lun4,fn4,/get_lun

   if keyword_set(verbose) then $
      print,'Write ',fn5,'     list of all co-authors'
   openw,lun5,fn5,/get_lun

   for i=0,npeople-1 do begin
      if order[idx[i]] eq 99 then continue
      printf,lun3,email[idx[i]]
      if orcid[idx[i]] ne '' then orcids='['+orcid[idx[i]]+']' else orcids=''

      str = first[idx[i]]+' '+last[idx[i]]
      if ((cjk_name[idx[i]] ne '') or $
          (arabic_name[idx[i]] ne '')) then begin
         str = str + ' ('
         if (cjk_name[idx[i]] ne '') then begin
            if (cjk_tex[idx[i]] ne '') then begin
               tag = '\'+cjk_tex[idx[i]]
            endif else begin
               tag = ''
            endelse
            str = str + tag + '{' + cjk_name[idx[i]] + '}'
         endif
         if (arabic_name[idx[i]] ne '') then begin
            if (cjk_name[idx[i]] ne '') then str = str + ', '
            if (arabic_tex[idx[i]] ne '') then begin
               tag = '\'+arabic_tex[idx[i]]
            endif else begin
               tag = ''
            endelse           
            str = str + tag + '{' + arabic_name[idx[i]] + '}'
         endif
         str = str + ')'
      endif
      str=strcompress(str)

      printf,lun,'\author',orcids,'{',str,'}'

      words=strsplit(inst[idx[i]],',',/extract)
      words=strtrim(words,2)
      for j=0,n_elements(words)-1 do begin
         z=where(words[j] eq uinst,/null)
         printf,lun,'\affiliation{'+uaddr[z[0]]+'}'
      endfor

      printf,lun,''

   endfor

   z=random_sample(seed,npeople,npeople)

   for i=0,npeople-1 do begin
;      if last[z[idx[i]]] eq 'Buie' then continue
      str=first[z[idx[i]]]+' '+last[z[idx[i]]]
      str=strcompress(str)
      printf,lun4,str,','
   endfor

   for i=0,npeople-1 do begin
      if order[idx[i]] eq 99 then continue
      str=first[idx[i]]+' '+last[idx[i]]
      str=strcompress(str)
      printf,lun5,str,','
   endfor

   z=where(order[idx] eq 99,count)

   printf,lun2,'Thanks to all our other essential contributors for the'
   printf,lun2,'observing campaigns: '
   for i=0,count-1 do begin
      str=first[idx[z[i]]]+' '+last[idx[z[i]]]
      if ((cjk_name[idx[z[i]]] ne '') or $
          (arabic_name[idx[z[i]]] ne '')) then begin
         str = str + ' ('
         if (cjk_name[idx[z[i]]] ne '') then begin
            if (cjk_tex[idx[z[i]]] ne '') then begin
               tag = '\'+cjk_tex[idx[z[i]]]
            endif else begin
               tag = ''
            endelse
            str = str + tag + '{' + cjk_name[idx[z[i]]] + '}'
         endif
         if (arabic_name[idx[z[i]]] ne '') then begin
            if (cjk_name[idx[z[i]]] ne '') then str = str + ', '
            if (arabic_tex[idx[z[i]]] ne '') then begin
               tag = '\'+arabic_tex[idx[z[i]]]
            endif else begin
               tag = ''
            endelse           
            str = str + tag + '{' + arabic_name[idx[z[i]]] + '}'
         endif
         str = str + ')'
      endif
      str=strcompress(str)
      if i eq count-1 then tag='.' $
      else if i eq count-2 then tag=', and ' $
      else tag=', '
      printf,lun2,str,tag
   endfor

bailout:
   free_lun,lun,lun2,lun3,lun4,lun5

end
