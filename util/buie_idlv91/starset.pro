;+
; NAME:
;  starset
; PURPOSE:   (one line only)
;  Generate a list of nearby stars to help find a particular location
; DESCRIPTION:
; CATEGORY:
;  Astronomy
; CALLING SEQUENCE:
;  starset,jd,in_ra,in_dec,mag,objectid,listing
; INPUTS:
;  jd       - Julian date at time of intended observation
;  ra       - J2000 right ascension of target
;  dec      - J2000 declination of target
;  mag      - Apparent brightness of target
;  objectid - Name of the object at the supplied coordinates
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;  J2000    - Flag, if set, causes output positions to be J2000.  If not
;               set the positions are output at the equinox of jd.
;  FNOUT    - If provided, the listing will be printed to this file name.
; OUTPUTS:
;  listing  - String array for the listing
; KEYWORD OUTPUT PARAMETERS:
; CONFIGURATION:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
;  Needs access to phot.hygcat database table for star names
; PROCEDURE:
; MODIFICATION HISTORY:
;  Written by Marc W. Buie, Southwest Research Institute, 2015/09/13
;  2022/01/11, MWB, incorporated into library with cosmetic changes
;-
pro starset,in_jd,in_ra,in_dec,mag,objectid,listing,J2000=j2000,FNOUT=fnout

   listing=''

   self='starset: '
   if badpar(in_jd,[5,7],0,caller=self+'(jd) ',type=jdtype) then return
   if badpar(in_ra,[5,7],0,caller=self+'(ra) ',type=ratype) then return
   if badpar(in_dec,[5,7],0,caller=self+'(dec) ',type=dectype) then return
   if badpar(mag,[2,3,4,5],0,caller=self+'(mag) ') then return
   if badpar(objectid,7,0,caller=self+'(objectid) ') then return
   if badpar(j2000,[0,1,2,3],0,caller=self+'(J2000) ') then return
   if badpar(fnout,[0,7],0,caller=self+'(FNOUT) ',default='') then return

   if jdtype eq 7 then begin
      jd=jdparse(in_jd)
   endif else begin
      jd=in_jd
   endelse

   if ratype eq 7 then begin
      ra=raparse(in_ra)
   endif else begin
      ra=in_ra
   endelse

   if dectype eq 7 then begin
      dec=decparse(in_dec)
   endif else begin
      dec=in_dec
   endelse

   mags=strn(float(mag),format='(f10.1)')

   jd2year,jd,year
   moonpos,jd,mra,mdec,/radian
   mphase,jd,mph

   sc_pstar,ra,dec,35,id1,ra1,dec1,dra1,ddec1,mag1,odra1,oddec1, $
      mag_min=1.5,/silent
   sc_pstar,ra,dec,25,id2,ra2,dec2,dra2,ddec2,mag2,odra2,oddec2, $
      mag_min=2.8,mag_max=1,/silent
   sc_pstar,ra,dec, 4,id3,ra3,dec3,dra3,ddec3,mag3,odra3,oddec3, $
      mag_min=6.5,mag_max=3,/silent
   sc_pstar,ra,dec, 2,id4,ra4,dec4,dra4,ddec4,mag4,odra4,oddec4, $
      mag_min=8.5,mag_max=6,/silent
   sc_pstar,ra,dec,0.7,id5,ra5,dec5,dra5,ddec5,mag5,odra5,oddec5, $
      mag_min=10,/silent
   sc_pstar,ra,dec,0.5,id6,ra6,dec6,dra6,ddec6,mag6,odra6,oddec6,/silent

   hrac=[ra1,ra2,ra3,ra4,ra5,ra6]
   hdecc=[dec1,dec2,dec3,dec4,dec5,dec6]
   hid=[id1,id2,id3,id4,id5,id6]
   hra=[odra1,odra2,odra3,odra4,odra5,odra6]
   hdec=[oddec1,oddec2,oddec3,oddec4,oddec5,oddec6]
   hmag=[mag1,mag2,mag3,mag4,mag5,mag6]

   z=where(hid ne '',count,/null)
   if count ne 6 then begin
      hrac=hrac[z]
      hdecc=hdecc[z]
      hid=hid[z]
      hra=hra[z]
      hdec=hdec[z]
      hmag=hmag[z]
   endif

   msep=angsep(hra,hdec,mra,mdec)*!radeg
   sep=angsep(hra,hdec,ra,dec)*!radeg

   if not keyword_set(j2000) then begin
      precess,hra,hdec,2000.0,year,/radian
      precess,ra,dec,2000.0,year,/radian
   endif

   hids=string(hid,format='(i6.6)')
   rastr,hra,1,hras
   decstr,hdec,0,hdecs
   hmags=string(hmag,format='(f4.1)')
   seps=string(sep,format='(f5.2)')
   mseps=string(round(msep),format='(i3)')

   nstars=n_elements(hid)

   openmysql,dblun,'phot'

   laststarname=''
   fmt='(a-14,1x,a10,1x,a9,1x,a4,1x,a5,1x,a3,1x,f4.2)'

   jdstr,jd,-2,info
   info='('+info+'UT)'

   listing=['Star training set for '+objectid+', '+info, $
            string('Object','RA     ','Dec   ','mag','sep ','mel',format=fmt)]

   for i=0,nstars-1 do begin
      if hid[i] gt 0 then begin
         cmd='select name,bfdes,ra,decl,mag from hygcat'+ $
             ' where abs(ra-'+string(hrac[i])+') < 0.0001'+ $
             ' and abs(decl-'+string(hdecc[i])+') < 0.0001'+ $
             ';'
         mysqlquery,dblun,cmd,cname,cdes,cra,cdec,cmag, $
            format='a,a,d,d,a',ngood=nfound
         starname='PPM '+hids[i]
         if nfound gt 0 then begin
            dis=angsep(hrac[i],hdecc[i],cra,cdec)*!radeg*3600.0
            if min(dis) lt 20 then begin
               z=trimrank(where(dis eq min(dis)))
               if cname[z] ne 'NULL' then begin
                  starname=cname[z]
               endif else if cdes[z] ne 'NULL' then begin
                  starname=strcompresS(cdes[z])
               endif
            endif
         endif
         if starname ne laststarname then begin
            listing=[listing, $
                     string(starname,hras[i],hdecs[i],hmags[i], $
                            seps[i],mseps[i],format=fmt)]
         endif
         laststarname=starname
      endif
   endfor

   msep=angsep(ra,dec,mra,mdec)*!radeg
   seps='     '
   mseps=string(round(msep),format='(i3)')
   rastr,ra,1,ras
   decstr,dec,0,decs
   listing=[listing, $
            string(objectid,ras,decs,mags,seps,mseps,format=fmt)]
   if keyword_set(j2000) then $
      listing=[listing,'Positions are for J2000'] $
   else $
      listing=[listing,'Positions are for equinox of date']

   free_lun,dblun

   if fnout ne '' then wrstrarr,listing,fnout

end
