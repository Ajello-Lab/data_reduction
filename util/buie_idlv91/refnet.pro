;+
; NAME:
;  refnet
; PURPOSE:
;  Support routine for calling ``REFNET'' to get stars from master catalogs.
; DESCRIPTION:
;
; CATEGORY:
;  Astrometry
;
; CALLING SEQUENCE:
;  refnet,ra,dec,width,height,bmaglim,rmaglim,starfile
;
; INPUTS:
;  ra      - Right ascension of center of field for extraction (J2000)
;              input can be in radians (double,float) or
;              a string HH:MM:SS.S  (see RAPARSE for valid syntax).
;  dec     - Declination of center of field for extraction (J2000, radians).
;              input can be in radians (double,float) or
;              a string +DD:MM:SS.S  (see DECPARSE for valid syntax).
;  width   - Half-width of field to extract (arcsec).
;  height  - Half-height of field to extract (arcsec).
;  bmaglim - Limiting Blue magnitude to extract. (Ignored for Gaia)
;  rmaglim - Limiting Red magnitude to extract   (G mag for Gaia).
;  starfile- File name for the results of the catalog extraction.
;
; OPTIONAL INPUT PARAMETERS:
;
; KEYWORD INPUT PARAMETERS:
;
;  GAIA     - Set this keyword to some value in decimal years to get
;                stars from the Gaia catalog.  Doing so will ensure that
;                the final output file (starfile) is computed for the
;                epoch you provide with this keyword.  You also get another
;                file named starfile+'.gcat' which is an ASCII table with
;                all of the Gaia information.
;  TWOMASS  - String.  Set to 'J', 'H', or 'K' to request 2MASS point-source
;                   catalog data in that filter.  Ignored if GAIA is used.
;  DEBUG - Flag, if set prints debugging information
;  NOCONVERT - Flag, if set suppresses natural conversion of GAIA file,
;                 not used for other types.
;
; OUTPUTS:
;  The output is all to the file and contains a list of stars from the
;    USNO catalog according to the input constraints, unless GAIA is set
;    in which case you get Gaia catalog stars.
;
; KEYWORD OUTPUT PARAMETERS:
;
; COMMON BLOCKS:
;
; SIDE EFFECTS:
;
; RESTRICTIONS:
;  You must define an evironment variable, USNO_CAT, that points to the
;  directory where the data live.  Default is the current directory.
;  To use 2MASS you need to set TWOMASS_CAT.  To use Gaia you need to set
;  GAIA_CAT.  You must also have write permission in the current working
;  directory.
;
; PROCEDURE:
;   calls refnet for USNO stars, refnetx for 2MASS, or grefnet for Gaia.
;
; MODIFICATION HISTORY:
;  1997/05/08, Written by Marc W. Buie, Lowell Observatory
;  1997/11/24, MWB, Added CATPATH keyword
;  1999/06/22, MWB, changed CATPATH default
;  2002/02/06, MWB, rewrite for new refnet version
;  2005/06/26, MWB, added TWOMASS (2MASS catalog) support
;  2018/05/08, MWB, added GAIA option
;  2018/11/14, MWB, added DEBUG keyword
;  2021/01/28, MWB, added NOCONVERT keyword
;-
pro refnet,ra,dec,width,height,bmaglim,rmaglim,starfile, $
           TWOMASS=twomass,GAIA=epoch,DEBUG=debug,NOCONVERT=noconvert

   self='REFNET: '
   if badpar(ra,[4,5,7],0,CALLER=self+'(ra) ',TYPE=ratype) then return
   if badpar(dec,[4,5,7],0,CALLER=self+'(dec) ',TYPE=dectype) then return
   if badpar(twomass,[0,7],0,CALLER=self+'(TWOMASS) ',default='') then return
   if badpar(epoch,[0,4,5],0,CALLER=self+'(GAIA) ',type=epoch_type) then return
   if badpar(debug,[0,1,2,3],0,CALLER=self+'(DEBUG) ',default=0) then return
   if badpar(noconvert,[0,1,2,3],0,CALLER=self+'(NOCONVERT) ',default=0) then return

   if ratype  ne 7 then rastr,ra,1,ras else ras = ra
   if dectype ne 7 then decstr,dec,0,decs else decs = dec

   if epoch_type ne 0 then begin
      if debug then print,'Searching Gaia catalog'
      tmpstarfile=starfile+'.gcat'
      cmd='grefnet -ra '+ras+' -dec '+decs+' -w '+string(width)+ $
                    ' -h '+string(height)+' -g '+string(rmaglim)+ $
                    ' > '+tmpstarfile
      if debug then print,cmd
      spawn,cmd
      if not noconvert then $
         gaiafcat,tmpstarfile,epoch,starfile
   endif else begin

      if twomass eq '' then begin

         if debug then print,'Searching UCAC catalog'
         spawn,'refnet -ra '+ras+' -dec '+decs+' -w '+string(width)+ $
                       ' -h '+string(height)+' -b '+string(bmaglim)+ $
                       ' -r '+string(rmaglim)+' > '+starfile

      endif else begin

         if debug then print,'Searching TWOMASS catalog'
         spawn,'refnet2 -ra '+ras+' -dec '+decs+' -w '+string(width)+ $
                       ' -h '+string(height)+' -m '+string(bmaglim)+ $
                       ' -f '+twomass+' -c > '+starfile

      endelse

   endelse

end
