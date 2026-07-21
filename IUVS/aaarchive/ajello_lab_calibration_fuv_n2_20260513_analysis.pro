

pro ajello_lab_calibration_fuv_n2_20260513_analysis

  ajello_lab_set_paths, path_base, path_repo

  path_save = '/users/holsclaw/Documents/'

file = '/Users/holsclaw/Documents/Round_12 - N2_30EV_FUV_TEST15_IMAGE1 - sensitivity.txt'
data = (read_ascii(file)).(0)
wave_sens_r12 = reform(data[0,*])
spec_sens_r12 = reform(data[1,*])

p = plot( wave_sens_r12, spec_sens_r12 )

file_sens = '/Users/holsclaw/Ajello-Lab/data_reduction/IUVS/IUVSbreadboardSensitivity_FUV.sav'
restore,file_sens,/ver

p1 = plot( final_wave/10., final_Sens )
p2 = plot( wave_sens_r12, spec_sens_r12, /over, color='red' )

  stop
  
end