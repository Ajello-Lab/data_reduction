;+
; NAME:
; AJELLO_LAB_ROUND_NUMBER
; 
; PURPOSE:
; This routine will determine if the IUVS dataset from the specified path is from round 1, 2, 3, or 4
; 
; INPUTS:
; path_: Path to an IUVS dataset.
; 
; OUTPUTS:
; round_number: 
;-
pro ajello_lab_round_number, path_, round_number

if n_params() eq 0 then begin
  ;path_ = '/Volumes/projects/Phase_Development/MAVEN/IUVS_Data/IUVS_Breadboard/_BIG_e-gun/CO2/CO2_30eV_5/FUV_CDSImage_2C7AEADB__MCP910V_Exp60000ms_000.fits'
  path_ = '/Users/holsclaw/MAVEN/Ajello_lab/_Big_e-gun_RoundII/data_reduction/CO_30eV_MUV_test_4_image1.idl'
endif
path = strupcase(path_)

round_number = -1

pos2 = strpos(path,'ROUNDII')
if pos2 ne -1 then round2_flag = 1 else round2_flag = 0

pos3 = strpos(path,'ROUNDIII')
if pos3 ne -1 then round3_flag = 1 else round3_flag = 0

pos4 = strpos(path,'ROUNDIV')
if pos4 ne -1 then round4_flag = 1 else round4_flag = 0

flag_sum = round2_flag + round3_flag + round4_flag

if flag_sum gt 1 then begin
  print, 'Error: No more than one observation can be identified."
  stop
endif

round1_flag = 0
if flag_sum eq 0 then begin
  pos1 = strpos(path,"_BIG_E-GUN")
  if pos1 ne -1 then round1_flag = 1
endif

if round1_flag then round_number = 1
if round2_flag then round_number = 2
if round3_flag then round_number = 3
if round4_flag then round_number = 4

if n_params() eq 0 then begin
  print, round1_flag, round2_flag, round3_flag, round4_flag
  print, round_number 
  stop  
endif

end