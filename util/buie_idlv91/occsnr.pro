;+
; NAME:
;  occsnr
; PURPOSE:   (one line only)
;  Compute estimated SNR for an occultation with a specified system
; DESCRIPTION:
;  This is mostly intend for occasional interactive calculations.
; CATEGORY:
;  Occultations
; CALLING SEQUENCE:
;  occsnr,system,exptime,gmag,gtarg,snr
; INPUTS:
;  system - name of a standard system
;  exptime - exposure time in seconds
;  gmag   - G magnitude of occultaiton star
;  gtarg  - G magnitude of the oculting body
; OPTIONAL INPUT PARAMETERS:
;  SILENt - flag, if set will suppress all printed output
; KEYWORD INPUT PARAMETERS:
; OUTPUTS:
;  snr - Estimated signal to noise ratio
; KEYWORD OUTPUT PARAMETERS:
; CONFIGURATION:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  Written by Marc W. Buie, Southwest Research Institute, 2024/11/09
;-
pro occsnr,system,exptime,gmag,gtarg,snr,SILENT=silent

   self='occsnr: '
   if badpar(system,7,0,caller=self+'(system) ') then return
   if badpar(exptime,[2,3,4,5],0,caller=self+'(exptime) ') then return
   if badpar(gmag,[2,3,4,5],0,caller=self+'(gmag) ') then return
   if badpar(gtarg,[2,3,4,5],0,caller=self+'(gtarg) ') then return
   if badpar(silent,[0,1,2,3],0,caller=self+'(SILENT) ',default=0) then return

   verbose = silent eq 0

   initsnrmod,'C11',s1
   if s1.error eq 1 then return

   snrpred,s1,exptime,gmag,gtarg,snr,verbose=verbose

end
