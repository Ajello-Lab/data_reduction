;+
; NAME:
;  gtrans
; PURPOSE:   (one line only)
;  Transform from Gaia (G,GB,GR) magnitudes to another standard bandpass
; DESCRIPTION:
;  Transformation is handled with a simple formula:
;    mag-g = c[1]*(gb-gr) + c[0]
;  where c is the coefficient array returned.  The input arrays should all
;  be the same length and be correlated arrays for the same source.  This
;  routine assumes the uncertainty on the Gaia photometry is negligible
;  compared to the standard magnitude.
;  This tool needs lots of stars to do any good.
; CATEGORY:
;  Photometry
; CALLING SEQUENCE:
;  gtrans,mag,err,g,gb,gr,thresh,c,csig
; INPUTS:
;  mag - Standard magnitudes in another system (eg., B or V)
;  err - Uncertainty in the standard magnitude
;  g   - Gaia G magnitude
;  gb  - Gaia GB magnitude
;  gr  - Gaia GR magnitude
;  thresh - sigma cutoff for bad point filtering
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;  BAD - array of bad flags (modified and returned), default is all points
;        are good
; OUTPUTS:
;  c   - Fitted coefficients
;  csig - uncertainties on the coefficients
; KEYWORD OUTPUT PARAMETERS:
; CONFIGURATION:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  Written by Marc W. Buie, Southwest Research Institute, 2024/07/24
;-
pro gtrans,mag,err,g,gb,gr,thresh,c,csig,BAD=bad

   self='gtrans: '
   if badpar(mag,[4,5],1,caller=self+'(mag) ',NPTS=npts) then return
   if badpar(err,[4,5],1,caller=self+'(err) ') then return
   if badpar(g,[4,5],1,caller=self+'(g) ') then return
   if badpar(gb,[4,5],1,caller=self+'(gb) ') then return
   if badpar(gr,[4,5],1,caller=self+'(gr) ') then return
   if badpar(thresh,[4,5],0,caller=self+'(thresh) ') then return
   if badpar(bad,[0,1,2,3],1,caller=self+'(BAD) ', $
                             default=bytarr(npts)) then return

   i=0
   repeat begin
      z=where(bad eq 0,ng)

      y = double(mag[z] - g[z])
      w = 1.0/err[z]
      ind = dblarr(2,ng)
      ind[0,*] = 1.0d0
      ind[1,*] = gb[z]-gr[z]
;print,'T',minmax(y),minmax(err),minmax(w),minmax(gb[z]-gr[z])

      yfit=1.
      chisq=1.
      covar=1.
      c=mysvdfit(ind,y,2,weight=w,yfit=yfit,chisq=chisq,covar=covar)
      csig = sqrt([covar[0,0],covar[1,1]])

      zz=where(abs(y-yfit) gt thresh*err[z],count)
;print,i,ng,count
      if count ne 0 then bad[z[zz]] = 1B
      i++
   endrep until count eq 0

end
