;+
; NAME:
;  fluxref
; PURPOSE:   (one line only)
;  Provide standard fluxes for standard filters
; DESCRIPTION:
; CATEGORY:
;  Astronomy
; CALLING SEQUENCE:
;  fluxref,filter,wave,flux,sun
; INPUTS:
;  filter - String, name of a filter to look up
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;  REFFILE - Name of file to load.  Default is support_files/luxref.dat that
;             must exist in your IDL_PATH.  This will be satisfied by this
;             library for the default.  The first one found will be loaded.  If
;             you want to load an explicit file not in your path you need
;             to provide a full qualified path (starts with /) and the path
;             search is avoided.
;  RELOAD - Flag, if set forces a re-read of the flux file.
; OUTPUTS:
;  wave - pivot wavelength (microns)
;  flux - absolute flux for the filter, in ergs/cm^2/s/A (Vega mag system)
;  sun  - absolute flux for the sun through the filter, same units as flux
; KEYWORD OUTPUT PARAMETERS:
; CONFIGURATION:
; COMMON BLOCKS:
;  fluxref_com - used to cache the data from the reference file, not intended
;                  to be used by any other routines.
; SIDE EFFECTS:
;  If there are useful standard values not present in the file that you
;    would like, send a request to have it added.
; RESTRICTIONS:
; PROCEDURE:
;  There is a file, fluxref.dat, that lives with the library.  On the first
;    call to this routine, this file is read and saved in a local common
;    block to cache the data.  All subsequent calls will use the data in 
;    memory.  The /RELOAD flag is provided to force it to re-read the file
;    but this flag is not intended for programmatic use since this situation
;    is likely to come up only during development.
; MODIFICATION HISTORY:
;  Written by Marc W. Buie, Southwest Research Institute, 2021/08/24 and based
;    on prototype code written by Brian Keeney.
;  2021/09/02 - MWB, added sun flux on output
;-
pro fluxref,filter,wave,flux,sun,REFFILE=in_reffile,RELOAD=reload

   common fluxref_com, info

   self='fluxref: '
   if badpar(filter,7,0,caller=self+'(filter) ') then return
   if badpar(reload,[0,1,2,3],0,caller=self+'(RELOAD) ',default=0) then return
   if badpar(in_reffile,[0,7],0,caller=self+'(REFFILE) ', $
                             default='support_files/fluxref.dat') then return

   sz_info=size(info)

   ; If no structure yet, the data must be loaded fresh
   if sz_info[n_elements(sz_info)-2] ne 8 then begin
      reload=1
   endif

   if reload then begin
      if strmid(in_reffile,0,1) ne '/' then $
         reffile=find_with_def(in_reffile,!path) $
      else $
         reffile=in_reffile
      if reffile eq '' then begin
         print,self,'File '+in_file+' does not exist, cannot continue.'
         stop
      endif
      print,'Loading ',reffile
      readcol,reffile,in_filter,in_wave,in_flux,in_sun, $
         format='a,f,f,f',count=nvals
      info={ $
         filter: in_filter, $
         wave:   in_wave, $
         flux:   in_flux, $
         sun:    in_sun, $
         nvals:  nvals }
   endif

   z=where(filter eq info.filter,count)
   if count eq 0 then begin
      wave=0.
      flux=0.
      sun=0.
      print,self,'Filter [',filter,'] not found in flux table.'
      return
   endif

   wave = info.wave[z[0]]
   flux = info.flux[z[0]]
   sun  = info.sun[z[0]]

end
