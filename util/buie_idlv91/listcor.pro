;+
; NAME:
;  listcor
; PURPOSE:   (one line only)
;  Correlate two string lists
; DESCRIPTION:
; CATEGORY:
;  Miscellaneous
; CALLING SEQUENCE:
;  listcor,list1,list2,ind1,ind2,nind1
; INPUTS:
;  list1 - 1-D array of strings.  The program will run faster if this
;             is the shorter of the two lists.
;  list2 - 1-D array of strings
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;  UNIQUE - flag, if set forces the output mapping to be one-to-one.
;           If there are multiple matches, the first is kept as defined by
;           the order in which the entries are in the lists.  If this is
;           not set, all matches in list2 are reported to the item in list1.
; OUTPUTS:
;  ind1 - Index of matches in the first list
;  ind2 - Index of matches in the second list
;  nind1 - Index into the first list that doesn't match anything in the second
;             If you want to get the opposite result, run this again with the
;                lists reversed.
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  2016/09/12, Written by Marc W. Buie, Southwest Research Institute
;-
pro listcor,list1,list2,ind1,ind2,nind1,UNIQUE=unique

compile_opt strictarrsubs

   self='listcor: '
   if badpar(list1,7,1,caller=self+'(list1) ',npts=len1) then return
   if badpar(list2,7,1,caller=self+'(list2) ',npts=len2) then return
   if badpar(unique,[0,1,2,3],0,caller=self+'(UNIQUE) ',default=0) then return

   ind1=[]
   ind2=[]
   nind1=[]

   for i=0,len1-1 do begin
      z=where(list2 eq list1[i],count)
      if count eq 0 then begin
         nind1=[nind1,i]
      endif else begin
         if unique then begin
            ind1=[ind1,i]
            ind2=[ind2,z[0]]
         endif else begin
            ind1=[ind1,replicate(i,count)]
            ind2=[ind2,z]
         endelse
      endelse
   endfor

end
