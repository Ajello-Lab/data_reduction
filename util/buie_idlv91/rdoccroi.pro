;+
; NAME:
;  rdoccroi
; PURPOSE:   (one line only)
;  Read an occultation region of interest definition file and return a structure
; DESCRIPTION:
; CATEGORY:
;  Occultations
; CALLING SEQUENCE:
;  rdoccroi,roi
; INPUTS:
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;  FNCONFIG - name of configuration file to read,
;                default='support_files/roi.ini' which needs to be in your
;                IDL_PATH (is including in my library).
; OUTPUTS:
;  roi - anonymous structure with the ROI information
; KEYWORD OUTPUT PARAMETERS:
; CONFIGURATION:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  Written by Marc W. Buie, Southwest Research Institute, 2023/04/05
;-
pro rdoccroi,roi,FNCONFIG=in_fnconfig

   roi={error:1}

   self='rdoccroi: '
   default_value='+support_files/roi.ini'
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

   getvalue,info,'search','',regions

   roi={ $
      info: info, $
      regions: regions, $
      nregions: n_elements(regions), $
      error: 0 $
      }

end
