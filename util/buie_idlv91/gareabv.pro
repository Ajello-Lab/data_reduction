;+
; NAME:
;  gareabv
; PURPOSE:   (one line only)
;  Determine B,V transformation from Gaia in a region 
; DESCRIPTION:
; CATEGORY:
;  Photometry
; CALLING SEQUENCE:
;  gareabv,ra,dec,bc,bcsig,vc,vcsig
; INPUTS:
;  ra  - RA of area (radians)
;  dec - Dec of area (radians)
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;  hw - half-width of region in arsec, default = 1200
; OUTPUTS:
;  bc - transformation for B
;  bcsig - uncertainties on bc
;  vc - transformation for V
;  vcsig - uncertainties on vc
; KEYWORD OUTPUT PARAMETERS:
; CONFIGURATION:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  Written by Marc W. Buie, Southwest Research Insitute, 2024/07/24
;-
pro gareabv,racen,deccen,bc,bcsig,vc,vcsig,HW=hw

   self='gareabv: '
   if badpar(racen,[4,5],0,caller=self+'(ra) ') then return
   if badpar(deccen,[4,5],0,caller=self+'(dec) ') then return
   if badpar(hw,[0,4,5],0,caller=self+'(HW) ',default=1200.0) then return

   hw_r = hw/3600.0d0 * !dpi/180.0d0 ; radians
   ramin = racen - hw_r/cos(deccen)
   ramax = racen + hw_r/cos(deccen)
   decmin = deccen - hw_r
   decmax = deccen + hw_r

   cmd=['select idx,ra,decl,Bmag,Vmag,Berr,Verr', $
        'from apass10', $
        'where ra > '+strn(ramin), $
        'and ra < '+strn(ramax), $
        'and decl > '+strn(decmin), $
        'and decl < '+strn(decmax), $
        'and Bmag < 20', $
        'and Vmag < 20', $
        ';']

   openmysql,dblun,'phot'
   mysqlquery,dblun,cmd,idx,ra,dec,b,v,berr,verr, $
      format='l,d,d,f,f,f,f',ngood=nstars
   free_lun,dblun

   print,strn(nstars),' APASS stars found on field'
   berr = berr>0.01
   verr = verr>0.01

   refnet,racen,deccen,hw,hw,30.0,30.0,'star.cat',gaia=2024.0
   rdstarc,'star.cat.gcat',/silent,/noconvert,info=ginfo
   rtod = 180.0d0/!dpi

   srcor,ra*rtod/15.0d0,dec*rtod,ginfo.ra*rtod/15.0d0,ginfo.dec*rtod,4.0, $
      ind1,ind2,spherical=1,option=1

   sep=angsep(ra[ind1],dec[ind1],ginfo.ra[ind2],ginfo.dec[ind2])*rtod*3600.0

   bad=bytarr(n_elements(ind1))
;   print,'A',total(bad)
   robomean,sep,3.0,0.5,bad=bad
;   print,'B',total(bad)

   z=where(berr[ind1] gt 0.20 and bad eq 0,count)
   if count ne 0 then bad[z]=1B
;   print,'C',total(bad)
   z=where(verr[ind1] gt 0.10 and bad eq 0,count)
   if count ne 0 then bad[z]=1B
;   print,'D',total(bad)
   z=where(ginfo.bmag[ind2] gt 22.0 and bad eq 0,count)
   if count ne 0 then bad[z]=1b
;   print,'E',total(bad)
   z=where(ginfo.rmag[ind2] gt 22.0 and bad eq 0,count)
   if count ne 0 then bad[z]=1b
;   print,'F',total(bad)
   robomean,b[ind1]-ginfo.gmag[ind2],3.0,0.5,bad=bad
;   print,'G',total(bad)
   robomean,v[ind1]-ginfo.gmag[ind2],3.0,0.5,bad=bad
;   print,'H',total(bad)

   gtrans,b[ind1],berr[ind1],ginfo.gmag[ind2],ginfo.bmag[ind2], $
      ginfo.rmag[ind2],3.0,bc,bcsig,bad=bad
;   print,'G',total(bad)
   gtrans,v[ind1],verr[ind1],ginfo.gmag[ind2],ginfo.bmag[ind2], $
      ginfo.rmag[ind2],3.0,vc,vcsig,bad=bad

   z=where(b[ind1]-v[ind1] lt 0.5 or b[ind1]-v[ind1] gt 1.2 and bad eq 0,count)
   if count ne 0 then bad[z]=1b

;   print,'H',total(bad)
   gtrans,b[ind1],berr[ind1],ginfo.gmag[ind2],ginfo.bmag[ind2], $
      ginfo.rmag[ind2],3.0,bc,bcsig,bad=bad
   gtrans,v[ind1],verr[ind1],ginfo.gmag[ind2],ginfo.bmag[ind2], $
      ginfo.rmag[ind2],3.0,vc,vcsig,bad=bad

end
