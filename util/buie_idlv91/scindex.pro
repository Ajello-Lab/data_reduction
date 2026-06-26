;+
; NAME:
;  scindex
; PURPOSE:   (one line only)
;  Build an index into the data produced from one night and site of QHY data
; DESCRIPTION:
; CATEGORY:
;  Occultations
; CALLING SEQUENCE:
;  scindex,date
; INPUTS:
;  date - string with the date of observations to scan (YYYY-MM-DD)
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;  DDIR - optional directory to look for date in, default is the current
;           directory
;  SILENT - Flag, if set will suppress all non-error output.
; OUTPUTS:
;  info - anonymous structure with the information pulled from the directory
;           nobs is always defined but if zero, none of other other tags are.
; KEYWORD OUTPUT PARAMETERS:
; CONFIGURATION:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  Written by Marc W. Buie, Southwest Research Institute, 2021/02/02
;  2021/11/09, MWB, added last file number to the TOC and cleaned up the
;                   the TOC header
;-
pro scindex,date,info,DDIR=in_ddir,SILENT=silent

   self='scindex: '
   if badpar(date,7,0,caller=self+'(date) ') then return
   if badpar(in_ddir,[0,7],0,caller=self+'(date) ',default='') then return
   if badpar(silent,[0,1,2,3],0,caller=self+'(SILENT) ',default=0) then return

   if in_ddir eq '' then ddir='' else ddir=addslash(in_ddir)

   ddir=ddir+date+'/'

   if not exists(ddir) then begin
      print,ddir
      print,'Directory not found'
      return
   endif

   dirlist=file_search(ddir+'??_??_??',/test_directory,count=ndirs)
   timelist=strmid(dirlist,strlen(ddir))

   if not silent then begin
      print,'Scanning ',ddir
      print,strn(ndirs),' items found'
   endif

   if ndirs eq 0 then begin
      info={nobs: 0}
      return
   endif

   tab_folder=strarr(ndirs)
   tab_object=strarr(ndirs)
   tab_nexp  =lonarr(ndirs)
   tab_exptm =fltarr(ndirs)
   tab_gain  =intarr(ndirs)
   tab_offset=intarr(ndirs)
   tab_lat   =dblarr(ndirs)
   tab_lon   =dblarr(ndirs)
   tab_alt   =dblarr(ndirs)
   tab_info  =strarr(ndirs)

   if not silent then begin
      print,'UTfolder Object                Nexp Lastf Exptm Gain Ofst    Lat         Lon      Alt    SC Ver    Tdet'
      print,'-------------------------------------------------------------------------------------------------------'
   endif

   for i=0,ndirs-1 do begin
      fnsettings=file_search(ddir+timelist[i]+'/*CameraSettings.txt',count=nset)
      if nset ne 1 then begin
         print,ddir+timelist[i]+'/*CameraSettings.txt'
         print,nset
         print,fnsettings
         print,'*.CameraSettings.txt file not found, skipping.'
         continue
      endif
      loadini,info,file=fnsettings[0]
      ; this tool only works on QHY174M data, skip if no match
      z=where(info.section eq 'QHY174M',count)
      if count eq 0 then begin
         print,ddir+timelist[i]+'/*CameraSettings.txt'
         print,nset
         print,fnsettings
         print,'Data from an unsupported camera'
         print,info.section[0:5]
         continue
      endif
      getvalue,info,'QHY174M','Gain',gain
      getvalue,info,'QHY174M','Offset',offset
      getvalue,info,'QHY174M','Exposure',exposure,type=5
      getvalue,info,'QHY174M','SharpCapVersion',scversion
      getvalue,info,'QHY174M','Temperature',tdet,type=4
      exposure=round(exposure)
      fnimages=file_search(ddir+timelist[i]+'/*.fits',count=nimages)
      idx=sort(fnimages)
      lastfn=fnimages[idx[-1]]
      words=strsplit(lastfn,'_',/extract)
      words=strsplit(words[-1],'.',/extract)
      lastfn=long(words[0])
      if nimages eq 0 then begin
         print,ddir+timelist[i]+'/*.fits'
         print,'no images found'
         continue
      endif
      hdr=headfits(fnimages[0])
      for j=0,n_elements(hdr)-1 do $
         hdr[j] = strupcase(strmid(hdr[j],0,8))+strmid(hdr[j],8)
      object=sxpar(hdr,'OBJECT')
      if object eq '0' then object=''
      gps_lat=sxpar(hdr,'GPS_LAT')
      gps_lon=sxpar(hdr,'GPS_LONG')
      gps_alt_s=sxpar(hdr,'GPS_ALT')
      gps_alt=float(gps_alt_s)
      str=string(timelist[i],object,nimages,lastfn,exposure,gain,offset, $
            gps_lat,gps_lon,gps_alt,scversion,tdet, $
         format='(a,1x,a-20,3(1x,i5),1x,a4,1x,a3,1x,f10.6,1x,f11.6,1x,f6.1,1x,a,1x,f5.1)')
      if not silent then print,str

      tab_folder[i] = timelist[i]
      tab_object[i] = object
      tab_nexp[i]   = nimages
      tab_exptm[i]  = exposure
      tab_gain[i]   = gain
      tab_offset[i] = offset
      tab_lat[i]    = gps_lat
      tab_lon[i]    = gps_lon
      tab_alt[i]    = gps_alt
      tab_info[i]   = str

   endfor

   z=where(tab_folder ne '',count)
   if count eq 0 then begin
      info={nobs: 0}
   endif else begin
      info={nobs: count, $
            folder:  tab_folder[z], $
            object:  tab_object[z], $
            nexp:    tab_nexp[z], $
            exptime: tab_exptm[z], $
            gain:    tab_gain[z], $
            offset:  tab_offset[z], $
            lat:     tab_lat[z], $
            lon:     tab_lon[z], $
            alt:     tab_alt[z], $
            str:     tab_info[z] $
            }
   endelse

end
