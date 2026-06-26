;+
; NAME:
;  pmcorrdr3
; PURPOSE:   (one line only)
;  Correct for the Gaia DR3 proper motion bias.
; DESCRIPTION:
;  This is an implmentation of the correction published in
;    Cantat-Gaudin and Brandt, A&A 649, A124 (2021)
; CATEGORY:
;  Astrometry
; CALLING SEQUENCE:
;  pmcorrdr3,pmra,pmdec,ra,dec,gmag,newpmra,newpmdec,pmracorr,pmdeccorr
; INPUTS:
;  pmra  - Proper motion in RA from the Gaia DR3 catalog [mas/yr]
;  pmdec - Proper motion in Dec from the Gaia DR3 catalog [mas/yr]
;  ra    - DR3 catalog position of source [deg]
;  dec   - DR3 catalog position of source [deg]
;  gmag  - DR3 catalog G magnitude of source
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
; OUTPUTS:
;  newpmra   - Corrected proper motion, same units as the input
;  newpmdec  - Corrected proper motion, same units as the input
;  pmracorr  - Correction to proper motion [mas]
;  pmdeccorr - Correction to proper motion [mas]
; KEYWORD OUTPUT PARAMETERS:
; CONFIGURATION:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  Written by Marc W. Buie, Southwest Research Institute based on the
;     Python implmentation from Cantat-Gaudin and Brandt, A&A 649, A124 (2021)
;-
pro pmcorrdr3,pmra,pmdec,ra,dec,gmag,newpmra,newpmdec,pmracorr,pmdeccorr

   self='pmcorrdr3: '
   if badpar(pmra,[4,5],[0,1],caller=self+'(pmra) ') then return
   if badpar(pmdec,[4,5],[0,1],caller=self+'(pmdec) ') then return
   if badpar(ra,[4,5],[0,1],caller=self+'(ra) ') then return
   if badpar(dec,[4,5],[0,1],caller=self+'(dec) ') then return
   if badpar(gmag,[4,5],[0,1],caller=self+'(gmag) ') then return

   ctab=[[ 0.00,  9.00, 18.4, 33.8, -11.3], $
         [ 9.00,  9.50, 14.0, 30.7, -19.4], $
         [ 9.50, 10.00, 12.8, 31.4, -11.8], $
         [10.00, 10.50, 13.6, 35.7, -10.5], $
         [10.50, 11.00, 16.2, 50.0,   2.1], $
         [11.00, 11.50, 19.4, 59.9,   0.2], $
         [11.50, 11.75, 21.8, 64.2,   1.0], $
         [11.75, 12.00, 17.7, 65.6,  -1.9], $
         [12.00, 12.25, 21.3, 74.8,   2.1], $
         [12.25, 12.50, 25.7, 73.6,   1.0], $
         [12.50, 12.75, 27.3, 76.6,   0.5], $
         [12.75, 13.00, 34.9, 68.9,  -2.9]]
   sz=size(ctab,/dimen)
   nbins=sz[1]
   
   ra_r  = ra * !dpi/180.0d0
   dec_r = dec * !dpi/180.0d0

   pmracorr  = pmra
   pmdeccorr = pmdec

   for i=0,nbins-1 do begin
      z=where(gmag ge ctab[0,i] and gmag lt ctab[1,i],count)
      if count eq 0 then continue
      pmracorr[z] = -1*sin(dec_r[z])*cos(ra_r[z])*ctab[2,i] $
                    -sin(dec_r[z])*sin(ra_r[z])*ctab[3,i] $
                    +cos(dec_r[z])*ctab[4,i]
      pmdeccorr[z] = sin(ra_r[z])*ctab[2,i]-cos(ra_r[z])*ctab[3,i]
   endfor

   pmracorr=trimrank(pmracorr)
   pmdeccorr=trimrank(pmdeccorr)

   pmracorr = pmracorr/1000.0d0
   pmdeccorr = pmdeccorr/1000.0d0

   newpmra  = pmra  - pmracorr
   newpmdec = pmdec - pmdeccorr

   newpmra=trimrank(newpmra)
   newpmdec=trimrank(newpmdec)

end
