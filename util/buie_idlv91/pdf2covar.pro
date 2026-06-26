;+
; NAME:
;  pdf2covar
; PURPOSE:   (one line only)
;  Convert a probability density function to a covariance matrix
; DESCRIPTION:
; CATEGORY:
;  Statistics
; CALLING SEQUENCE:
;  pdf2cover,pdf,covar
; INPUTS:
;  pdf - Probability density function, this is an NxM array where M is 
;          the number of samples in the PDF and N is the number of variables.
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
; OUTPUTS:
;  covar - Covariance matrix, this is an NxN matrix
; KEYWORD OUTPUT PARAMETERS:
;  MEANS - the mean value of each variable computed from the pdf
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  2018/02/01, Written by Marc W. Buie, Southwest Research Institute
;-
pro pdf2covar,pdf,covar,MEANS=means

   self='pdf2covar: '
   if badpar(pdf,[4,5],2,caller=self+'(pdf) ') then return

   sz=size(pdf,/dimen)
   nvar=sz[0]
   nsamp=sz[1]
   pdfval=1.0d0/double(nsamp)

   means=dblarr(nvar)
   for i=0,nvar-1 do begin
      means[i]=mean(pdf[i])
   endfor

   covar=dblarr(nvar,nvar)

   for i=0,nvar-1 do begin
      for j=i,nvar-1 do begin
         sum=total((pdf[i,*]-means[i])*(pdf[j,*]-means[j])*pdfval)
         if i eq j then begin
            covar[i,i]=sum
         endif else begin
            covar[i,j]=sum
            covar[j,i]=sum
         endelse
      endfor
   endfor

end
