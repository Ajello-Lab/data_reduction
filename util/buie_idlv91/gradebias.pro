;+
; NAME:
;  gradebias
; PURPOSE:   (one line only)
;  Automatic quality grading of a set of CCD bias images against a superbias
; DESCRIPTION:
; CATEGORY:
;  CCD data processing
; CALLING SEQUENCE:
;  gradebias,cube,bias,grade
; INPUTS:
;  cube - 3-d cube of original bias images, if you need to subtract overscan
;              and crop, this should be done prior to building the cube
;  bias - 2-d image which is the superbias frame
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;  MAXBAD   - Maximum number of bad rows allowed per frame before flagging
;               the cube as bad.  (default=25)
; OUTPUTS:
;  grade - This is the number of images that fail the test of being consistent
;             with the ensemble of images.
; KEYWORD OUTPUT PARAMETERS:
;  ROWTEST - array with a length equal to the number of rows in the image
;             This contains the number of frames that fail the goodness test
;             for each row.   This metric is rather hard to use but provided
;             for completeness.
;  FILETEST - array with a length equal to the number of images.  This array
;               records the number of rows in each image that fail the
;               goodness test.
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  Written by Marc W. Buie, Southwest Research Institute, 2015/05/03
;-
pro gradebias,cube,bias,grade,DISPLAY=display,MAXBAD=maxbad, $
       FILETEST=filetest,ROWTEST=rowtest

   self='gradebias: '
   if badpar(cube,[4,5],3,caller=self+'(cube) ') then return
   if badpar(bias,[4,5],2,caller=self+'(bias) ') then return
   if badpar(display,[0,1,2,3],0,caller=self+'(DISPLAY) ',default=0) then return
   if badpar(maxbad,[0,2,3],0,caller=self+'(MAXBAD) ',default=25) then return

   sz=size(cube,/dimen)
   nx=sz[0]
   ny=sz[1]
   nf=sz[2]

   ; Generate a scan array, this compresses each image down to an average
   ;   vertical profile, one such profile for each image in the cube
   ;   The superbias is subtracted from each image in the cube before
   ;   taking the average.
   scan=fltarr(ny,nf)
   for i=0,nf-1 do begin
      diff=cube[*,*,i]-bias
      if display then showsrc,diff,window=1
      scan[*,i]=total(diff,1)/float(nx)
   endfor

   ; filetest will be a vector, one entry per image in the cube and will
   ;  contain a count of the number of rows in that image that fall outside
   ;  of a reasonable bound for that set of images.
   filetest=intarr(nf)

   ; rowtest will be a vector, one entry per row in the cube and will contain
   ;  a count of the number of images in that row that fall outside of a
   ;  reasonable bound for that set of images.
   rowtest=intarr(ny)

   ; Loop over the y dimension and build up the filetest and rowtest arrays
   for i=0,ny-1 do begin
      tbad=bytarr(nf)
      robomean,scan[i,*],3.0,0.5,bad=tbad
      z=where(tbad eq 1,count)
      rowtest[i]=count
      if count gt 0 then filetest[z]=filetest[z]+1
   endfor

   if display then begin
      setwin,2
      plot,[0],/nodata,xr=[0,ny-1],yr=[-10,10], $
         xtitle='Row number',ytitle='bias-superbias',title='scan'
      for i=0,nf-1 do $
         oplot,scan[*,i]

      setwin,3
      plot,rowtest,xtitle='row number',ytitle='Number of bad images.', $
         title='rowtest'

      setwin,4
      plot,filetest,xtitle='image number',ytitle='Number of bad rows.', $
         title='filetest'
   endif

   z=where(filetest gt maxbad,grade)

end
