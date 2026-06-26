;+
; NAME:
;  occsnrtab
; PURPOSE:   (one line only)
;  Create occultation SNR tables for various systems
; DESCRIPTION:
; CATEGORY:
;  Occultations
; CALLING SEQUENCE:
;  occsnrtab,set
; INPUTS:
;  set - Occultation set(s)
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
; OUTPUTS:
;  For each set in the input list and system and region in the set.ini,
;  a file called [set].[region].[system].tab is created, to summarize the SNR
;  predicted for all occultations in that set observed by that system.
; KEYWORD OUTPUT PARAMETERS:
; CONFIGURATION:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  Written by Brian Keeney, Southwest Research Institute, 2022/02/24
;  2023/07/31, MWB, added support for P objects
;  2024/11/14, MWB, modified for new snrpred calling sequence
;-
pro occsnrtab,set

   self='occsnrtab: '
   if badpar(set,7,[0,1],caller=self+'(set) ') then return

   spawn,'geteph',unit=pipe
   spawn,'getinfo',unit=pipe2

   for i=0,n_elements(set)-1 do begin

      fnini = set[i]+'.ini'

      if nofile(fnini,'Configuration') then return
      loadini,info,file=fnini
  
      getvalue,info,'global','maxexptime',tmax,type=4,default=99.9
      getvalue,info,'global','minexptime',tmin,type=4,default=0.125
      getvalue,info,'global','restscale',restscale,type=4,default=10.0
      getvalue,info,'global','minsnr',minsnr,type=4,default=10.0
      getvalue,info,'global','snrlimit',snrlimit,type=4,default=99.9
      getvalue,info,'global','VmG',targcol,type=4,default=2.2
  
      ; Read in list of systems
      getvalue,info,'global','systems',systems
      if (systems eq '') then begin
         print,'No systems specified in ',fnini
         return
      endif
      systems=strsplit(systems,',',/extract)
      nsystems=n_elements(systems)

      ; Read in list of regions
      getvalue,info,'global','regions',region
      if (region eq '') then begin
         print, 'No regions specified in ',fnini
         return
      endif
      region=strsplit(region,',',/extract)
      nregions=n_elements(region)

      getvalue,info,'global','snrconfig',fnsnr
      rdoccsnr,snrinfo,fnconfig=fnsnr
      if snrinfo.error then begin
         print,'Unable to read the SNR configuration'
         return
      endif
  
      ; Time vector for fitting
      tvec = findgen(11)/10*(tmax-tmin) + tmin

      ; Setup for Larry's programs
      obname=''
      eph=dblarr(8)
  
      ; Header for table files
      header=['Event                 Object Name                      '+ $
              'Objid             JD             RA              Dec        '+$
              'Gmag   Gtarg  Hv   diameter  speed '+ $
              'texp    snr  scl '+$
              'Sel Mel Mph   etrack etime Nst MdDR',$
              '                                           '+ $
              '                                        '+ $
              '                                               '+$
              '    30%   5% [km/s] [sec]       [km]']
      fmt='(a-21,1x,a-32,1x,'+ $
          'a-10,1x,f16.8,2(1x,a),1x,'+ $
          'f5.2,2x,f5.2,1x,f4.1,2(1x,i4),1x,f6.1,1x,'+ $
          'f5.3,1x,f5.1,1x,f4.1,1x,'+ $
          'i3,1x,i3,1x,f4.2,1x,f7.1,1x,f5.1,1x,i3,1x,4(a))'

      if nofile(set[i]+'.tab','Occultation summary') then return
      readcol,set[i]+'.tab', event,jdgeo,ras,decs,gmag,speed,gstar, $
         moonelon,moonphase,etrack,etime,regions,f='a,d,a,a,f,f,f,f,f,f,f,a', $
         count=nevents,delimiter=' '
      objid=strarr(nevents)
      for k=0,nevents-1 do begin
         objid[k] = (strsplit(event[k],'_',/extract))[0]
         if strmid(objid[k],0,1) eq 'P' then begin
            objid[k]=objid[k]
         endif else begin
            objid[k]='A'+objid[k]
         endelse
      endfor
  
      lastobjid = ''

      for j=0,nsystems-1 do begin

         for jj=0,nregions-1 do begin

            snrtab = set[i]+'.'+systems[j]+'.'+region[jj]+'.tab'        

            print,'Writing ',snrtab
;            print,header

            openw,lun,snrtab,/get_lun
            printf,lun,header

            for k=0,n_elements(event)-1 do begin

               if strpos(regions[k],region[jj]) lt 0 then continue
               if gmag[k] lt 0.01 then continue

               if objid[k] ne lastobjid then begin
                  getvalue,info,objid[k],'name',name,type=7,default=''
                  if name ne '' then begin
                     getvalue,info,objid[k],'diameter',diameter,type=4,default=10.0
                     getvalue,info,objid[k],'albedo',albedo,type=4,default=0.05
                     getvalue,info,objid[k],'etrack',setrack,type=4,default=10.0
                     getvalue,info,objid[k],'etime',setime,type=4,default=1.0
                     hv=diamptoh(diameter,albedo)
                     supp={good: 1, name:name, diameter:diameter, hv:hv, $
                           albedo:albedo, etrack: setrack, etime: setime}
                  endif else begin
                     supp={good: 0}
                  endelse
                  lastobjid=objid[k]
               endif

               if supp.good then begin
                  obname = supp.name
                  h = supp.hv
                  g = 0.2
                  diam30 = supp.diameter
                  diam5  = supp.diameter
               endif else begin
                  printf,pipe2,objid[k]
                  readf,pipe2,obname,h,g,format='(a32,1x,f6.2,1x,f5.2)'
                  diam30 = round(hptodiam(h,0.30))
                  diam5 = round(hptodiam(h,0.05))
               endelse

               printf,pipe,jdgeo[k],'500',22+50,objid[k],$
                      format='(f16.8,1x,a3,1x,i2,1x,a)'
               readf,pipe,eph
               ssgeom,eph,sun,earth,phang,elong
               disphase,0.,sun,earth,phang,g,hmag
     
               V_mag = h-hmag
               targmag = V_mag - targcol
           
               occexpt,systems[j],gmag[k],targmag,speed[k],diam5,exptime,snr
               occexpt,systems[j],gmag[k],targmag,speed[k],diam30,exptime30,snr30
               if exptime le 0 then continue

               snrscale = speed[k]*exptime

               nminsta = ceil((diam5+6.0*etrack[k])/(diam5*0.4))

               moontag='.'
               if moonelon[k] ge 30 or moonphase[k] lt 0.3 then moontag='*'
               d5tag='.'
               if snr gt 5 then d5tag='*'
               d30tag='.'
               if snr30 ge 5 then d30tag='*'
               ringtag='.'
               if (snr[0] gt 10 and speed[k]*tmin le 2) or $
                  (snr[0] gt 20) then ringtag='*'
               obname=strtrim(obname,2)
               obname=repchar(obname,' ','_')
           
               line = string(f=fmt,$
                             event[k],obname,objid[k],jdgeo[k], $
                             ras[k],decs[k],gmag[k],targmag, $
                             h,diam30,diam5,speed[k],$
                             exptime,snr,snrscale,round(elong), $
                             moonelon[k],moonphase[k],etrack[k],etime[k], $
                             nminsta,moontag,d30tag,d5tag,ringtag)
               printf,lun,line
;               print,line
            endfor
            free_lun,lun
         endfor
      endfor
   endfor

bailout:
   free_lun,pipe,pipe2
  
end
