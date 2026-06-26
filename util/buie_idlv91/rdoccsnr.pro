;+
; NAME:
;  rdoccsnr
; PURPOSE:   (one line only)
;  Read an occultation SNR definition file and return a structure
; DESCRIPTION:
; CATEGORY:
;  Occultations
; CALLING SEQUENCE:
;  rdoccsnr,snr
; INPUTS:
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;  FNCONFIG - name of configuration file to read,
;                default='+support_files/snr.ini' which needs to be in your
;                IDL_PATH (is including in my library).
;                If you override, you can also put a + at the front of the
;                string to also search your path for the file.  If you leave
;                the '+' off, the file must be found as specfied.
; OUTPUTS:
;  snr - anonymous structure with the SNR parameter information
; KEYWORD OUTPUT PARAMETERS:
; CONFIGURATION:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  Written by Marc W. Buie, Southwest Research Institute, 2023/04/05
;-
pro rdoccsnr,snr,FNCONFIG=in_fnconfig

   snr={error:1}

   self='rdoccsnr: '
   default_value='+support_files/snr.ini'
   if badpar(in_fnconfig,[0,7],0,caller=self+'(FNCONFIG) ', $
                          default=default_value) then return
   if in_fnconfig eq '' then in_fnconfig=default_value

   auto_locate = strmid(in_fnconfig,0,1) eq '+'

   if auto_locate then begin
      fnconfig=find_with_def(strmid(in_fnconfig,1),!path)
      if fnconfig eq '' then begin
         print,self,in_fnconfig,' not found in path'
         return
      endif
   endif else begin
      fnconfig=in_fnconfig
   endelse

   if nofile(fnconfig,'ROI configuration file') then return

   loadini,file=fnconfig,info

   getvalue,info,'global','jy0mag',jy0mag,type=5
   getvalue,info,'global','lambda',lambda,type=5
   getvalue,info,'global','topt',topt,type=5
   getvalue,info,'global','emis',emis,type=5
   getvalue,info,'global','rdnoise',rdnoise,type=5
   getvalue,info,'global','gain',gain,type=5
   getvalue,info,'global','bandpass',bandpass,type=5
   getvalue,info,'global','pixel',pixel,type=5
   getvalue,info,'global','sky',sky,type=5
   getvalue,info,'global','dark',dark,type=5
   getvalue,info,'global','cy',cy,type=5
   getvalue,info,'global','zenith',zenith,type=5
   getvalue,info,'global','alt',alt,type=5
   getvalue,info,'global','hscale',hscale,type=5
   getvalue,info,'global','objrad',objrad,type=5
   getvalue,info,'global','seeing',seeing,type=5

   section=info.section
   z=where(section ne 'id' and section ne 'global',count,/null)
   section=section[z]
   systems=section[uniq(section,sort(section))]
   nsystems=n_elements(systems)
   name=strarr(nsystems)
   aper=fltarr(nsystems)
   fnum=fltarr(nsystems)
   tput=fltarr(nsystems)
   snrscale=fltarr(nsystems)

   for i=0,nsystems-1 do begin
      getvalue,info,systems[i],'name',name0
      getvalue,info,systems[i],'aper',aper0,type=5
      getvalue,info,systems[i],'fnum',fnum0,type=5
      getvalue,info,systems[i],'tput',tput0,type=5
      getvalue,info,systems[i],'snrscale',snrscale0,default=1.0d0,type=5
      name[i]=name0
      aper[i]=aper0
      fnum[i]=fnum0
      tput[i]=tput0
      snrscale[i]=snrscale0
   endfor

   snr={ $
      info: info, $
      jy0mag: jy0mag, $
      lambda: lambda, $
      topt: topt, $
      emis: emis, $
      rdnoise: rdnoise, $
      gain: gain, $
      bandpass: bandpass, $
      pixel: pixel, $
      sky: sky, $
      dark: dark, $
      cy: cy, $
      zenith: zenith, $
      alt: alt, $
      hscale: hscale, $
      objrad: objrad, $
      seeing: seeing, $
      nsystems: nsystems, $
      systems: systems, $
      name: name, $
      aper: aper, $
      fnum: fnum, $
      tput: tput, $
      snrscale: snrscale, $
      error: 0 $
      }

end
