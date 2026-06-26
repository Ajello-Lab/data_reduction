;+
; NAME:
;  rdstarc
; PURPOSE:
;  Read refnet based star catalog files.
; DESCRIPTION:
;  Read star catalog file - there are a number of options and consequences
;    of running this.  The input file can be either in ASCII tabular format
;    or in binary format.  If the file is ASCII you MUST use /NOCONVERT if
;    you do not want the input file to be modified.  Otherwise, it will be
;    converted to the most appropriate binary format file using starcprmt
;    before it is read.
;
;  There are two types of ASCII files.  The old version dating back from the
;    USNO catalog days, does not contain any proper motion information.  This
;    form of file will naturally map into the v1.0 format file with no loss
;    of information.  This file has 10 columns of data of which only the first
;    8 are ever examined.  The other two columns were for backward compatibility
;    for some long-forgotten software.  These columns are optional and are
;    ignored if present.  The 7 columns that are read are:
;      col     Contents
;      0-2  -- Right ascension (H, M, S)
;      3-5  -- Declination (D, M, S).
;       6   -- Blue magnitude
;       7   -- Red magnitude
;
;  The second type of ASCII file is from Gaia DR1.  It has 11 columns of
;    information and is a newly designed format.  During conversion to binary
;    form, one of two things can happen.  If you provide EPOCH, the proper
;    motion and parallax is applied to the catalog positions and the resulting
;    coordinates are saved in the v1.0 binary format.  If you do not provide
;    EPOCH, all of the input information is kept and sent back to the user.
;    In this latter case, the information comes back in the INFO return
;    structure and the formal arguments are populated with the catalog
;    information that maps to these arguments but they really aren't of
;    much use.  The columns in the file are:
;      col     Contents
;       0      Right ascension (encoded as a single sexigesimal string, H:M:S)
;       1      Declination (encoded as a single sexigemsimal string, D:M:S)
;       2      RA proper motion (arcsec/year)
;       3      Dec proper motion (arcsec/year)
;       4      Parallax (arcsec)
;       5      RA uncertainty (arcsec)
;       6      Dec uncertainty (arcsec)
;       7      RA proper motion uncertainty (arcsec/year)
;       8      Dec proper motion uncertainty (arcsec/year)
;       9      Parallax uncertainty (arcsec)
;      10      G magnitude
;    If you use NOCONVERT, you must use the full info structure on return
;
; CATEGORY:
;  Astrometry
; CALLING SEQUENCE:
;  rdstarc,starfile,ra,dec,bmag,rmag,nstars
; INPUTS:
;  starfile - Name of file to read.
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;  SILENT - Flag, if set suppresses all non-error output.
;  NOCONVERT - Flag, if set, suppresses converting to binary format.
;                Only works if the file is not already binary.  If it's
;                binary the program dies most unpleasantly.
; OUTPUTS:
;  ra   - right ascension in radians (J2000)
;  dec  - declination in radians (J2000)
;  bmag - Blue magnitude
;  rmag - Red magnitude  (see the USNO A2.0 catalog docs to see what this means)
;  nstars - Number of stars found (can be zero- in this case other 
;          outputs not defined).
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
;   There is no version control or fancy validation provided if you bypass
;      conversion.  You really should use this only for tiny files where you
;      really need to maintain the ASCII version.  This is NOT a normal usage.
; PROCEDURE:
; MODIFICATION HISTORY:
;  Written by Marc W. Buie, Lowell Observatory, 2002/01/07
;  02/03/10 - MWB, added nstars return value
;  2007/11/23- Peter L. Collins, Lowell Observatory, 
;    convert to v1.0 binary fmt and add promote call.
;  2008/10/21, MWB, added SILENT keyword
;  2014/03/18, MWB, added NOCONVERT keyword
;  2016/11/05, MWB, added support for proper motion and parallax
;  2018/07/16, MWB, added reading of Gaia star "id"
;  2020/07/08, MWB, fixed case where info structure was accidentally
;                     overwritten
;  2021/01/28, MWB, fixes a case reading an empty catalog file
;  2022/02/25, MWB, used EPOCH to add proper motion corrected positions
;                     to the output structure for the GAIA DR2/3 case.
;-
pro rdstarc,starfile,ra,dec,bmag,rmag,nstars, $
       SILENT=silent,NOCONVERT=noconvert, $
       EPOCH=epoch,INFO=info

   supported='STARC v1.0' ; length is all that matters
   gsupported='STARC v2.0' ; length is all that matters
   self='RDSTARC: '
   nstars=0L

   if badpar(starfile,7,0,caller=self+'(starfile) ') then return
   if badpar(silent,[0,1,2,3],0,caller=self+'(SILENT) ',default=0) then return
   if badpar(noconvert,[0,1,2,3],0,caller=self+'(NOCONVERT) ', $
                                   default=0) then return
   if badpar(epoch,[0,4,5],0,caller=self+'(EPOCH) ', $
                                   default=0) then return

   if noconvert then begin

      nlines=file_lines(starfile)
      if nlines eq 0 then begin
         info={nstars: 0}
         nstars=0
         return
      endif
      ; Need to read the first line of the file to count the number of fields
      ;   and decide its format.
      openr,lun,starfile,/get_lun
      line=''
      readf,lun,line,format='(a)'
      free_lun,lun
      words=strsplit(strcompress(line),' ',/extract)
      nwords=n_elements(words)

      if nwords eq 11 then begin ; Gaia DR1
         readcol,starfile,ras,decs,rapm,decpm,par, $
                 raerr,decerr,rapmerr,decpmerr,parerr,gmag, $
                 format='a,a,d,d,d,d,d,d,d,d,f',/silent,COUNT=ngood
         if ngood eq 0 then begin
            info={nstars: 0}
            nstars=0
            return
         endif
         ra=raparse(ras)
         dec=decparse(decs)
         info = { $
            nstars:   ngood, $
            ra:       ra, $
            dec:      dec, $
            rapm:     rapm/1000.0d0/3600.0d0/180.0d0*!dpi/cos(dec), $
            decpm:    decpm/1000.0d0/3600.0d0/180.0d0*!dpi, $
            par:      par/1000.0d0/3600.0d0/180.0d0*!dpi, $
            raerr:    raerr/1000.0d0/3600.0d0/180.0d0*!dpi/cos(dec), $
            decerr:   decerr/1000.0d0/3600.0d0/180.0d0*!dpi, $
            rapmerr:  rapmerr/1000.0d0/3600.0d0/180.0d0*!dpi/cos(dec), $
            decpmerr: decpmerr/1000.0d0/3600.0d0/180.0d0*!dpi, $
            parerr:   parerr/1000.0d0/3600.0d0/180.0d0*!dpi, $
            gmag:     gmag $
            }
      endif else if nwords eq 18 then begin ; Gaia DR2/3
         readcol,starfile,ras,decs,cepoch,par,rapm,decpm, $
                 raerr,decerr,parerr,rapmerr,decpmerr, $
                 gmag,gmagerr,bmag,bmagerr,rmag,rmagerr,gstarid, $
                 format='a,a,d,d,d,d,d,d,d,d,d,f,f,f,f,f,f,a', $
                 /silent,COUNT=ngood
         if ngood eq 0 then begin
            info={nstars: 0}
            nstars=0
            return
         endif
         ra=raparse(ras)
         dec=decparse(decs)
         
         info = { $
            nstars:   ngood, $
            ra:       ra, $
            dec:      dec, $
            epoch:    cepoch, $
            par:      par/1000.0d0/3600.0d0/180.0d0*!dpi, $
            rapm:     rapm/1000.0d0/3600.0d0/180.0d0*!dpi/cos(dec), $
            decpm:    decpm/1000.0d0/3600.0d0/180.0d0*!dpi, $
            raerr:    raerr/1000.0d0/3600.0d0/180.0d0*!dpi/cos(dec), $
            decerr:   decerr/1000.0d0/3600.0d0/180.0d0*!dpi, $
            parerr:   parerr/1000.0d0/3600.0d0/180.0d0*!dpi, $
            rapmerr:  rapmerr/1000.0d0/3600.0d0/180.0d0*!dpi/cos(dec), $
            decpmerr: decpmerr/1000.0d0/3600.0d0/180.0d0*!dpi, $
            gmag:     gmag, $
            gmagerr:  gmagerr, $
            bmag:     bmag, $
            bmagerr:  bmagerr, $
            rmag:     rmag, $
            rmagerr:  rmagerr, $
            starid:   gstarid $
            }
         if epoch ne 0 then begin
            ara =info.ra + info.rapm*(epoch-info.epoch)
            adec=info.dec+ info.decpm*(epoch-info.epoch)
            info = { $
               nstars:   ngood, $
               ra:       ra, $
               dec:      dec, $
               epoch:    cepoch, $
               par:      par/1000.0d0/3600.0d0/180.0d0*!dpi, $
               rapm:     rapm/1000.0d0/3600.0d0/180.0d0*!dpi/cos(dec), $
               decpm:    decpm/1000.0d0/3600.0d0/180.0d0*!dpi, $
               raerr:    raerr/1000.0d0/3600.0d0/180.0d0*!dpi/cos(dec), $
               decerr:   decerr/1000.0d0/3600.0d0/180.0d0*!dpi, $
               parerr:   parerr/1000.0d0/3600.0d0/180.0d0*!dpi, $
               rapmerr:  rapmerr/1000.0d0/3600.0d0/180.0d0*!dpi/cos(dec), $
               decpmerr: decpmerr/1000.0d0/3600.0d0/180.0d0*!dpi, $
               ara:      ara, $
               adec:     adec, $
               aepoch:   epoch, $
               gmag:     gmag, $
               gmagerr:  gmagerr, $
               bmag:     bmag, $
               bmagerr:  bmagerr, $
               rmag:     rmag, $
               rmagerr:  rmagerr, $
               starid:   gstarid $
               }
         endif
      endif else if nwords ge 8 and nwords le 10 then begin
         readcol,starfile,hr,m1,s1,dgas,m2,s2,rmag,bmag, $
                 format='d,d,d,a,d,d,f,f',/silent,/NAN,NLINES=nlines,COUNT=ngood
         if ngood eq 0 then begin
            info={nstars: 0}
            nstars=0
            return
         endif
         nstars=long(n_elements(hr))
         signas = strmid(dgas,0,1)
         dg = fix(strmid(dgas,1,2))
         hmstorad,hr,m1,s1,ra
         sign = replicate(1.0,nstars)
         z=where(signas eq '-',count)
         if count ne 0 then sign[z] = -1.0
         dmstorad,sign,abs(dg),m2,s2,dec
         return
      endif else begin
         print,starfile,' does not match a recognized format.'
         info={nstars: 0}
         nstars=0
         return
      endelse

   endif

   version = supported ; just needs to be a string of the right length.
   if not noconvert then begin
      starcprmt, starfile, SILENT=silent
      if exists(starfile) then begin
         finfo=file_info(starfile)
         if finfo.size le strlen(version)  then begin
            if not silent then print,'Star catalog file ',starfile,' is too short.'
            return
         endif
         openr, slun,starfile,/GET_LUN
         version_act = strlowcase(version)
         readu, slun, version_act  ; already promoted, we hope.
         if version ne  version_act then begin
            ; This means promote failed.
            print,'Star catalog file ',starfile,' has invalid contents.'
            free_lun, slun
            return
         endif
         readu, slun, nstars
         swap_endian_inplace,nstars,/SWAP_IF_LITTLE_ENDIAN
         if nstars gt 0 then begin
            ra = dblarr(nstars)
            dec = dblarr(nstars)
            bmag = fltarr(nstars)
            rmag = fltarr(nstars)
            ; ra vector, double of length nstars.
            readu, slun,ra
            swap_endian_inplace, ra, /SWAP_IF_LITTLE_ENDIAN
            ; dec vector, double of length nstars.
            readu, slun,dec
            swap_endian_inplace, dec, /SWAP_IF_LITTLE_ENDIAN
            ; bmag vector, double of length nstars.
            readu, slun,bmag
            swap_endian_inplace, bmag, /SWAP_IF_LITTLE_ENDIAN
            ; rmag vector, double of length nstars.
            readu, slun,rmag
            swap_endian_inplace, rmag, /SWAP_IF_LITTLE_ENDIAN
         endif
         free_lun, slun
      endif else begin
         print,'Star catalog file ',starfile,' cannot be found.'
      endelse
   endif
end
