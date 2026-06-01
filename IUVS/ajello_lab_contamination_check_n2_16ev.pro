;+
; PURPOSE
;  This routine will overplot one spectrum of N2 30eV image1 from round 10, 
;   and two spectra of the same from round 12.  The objective is to 
;   determine any change in sensitivity caused by a contamination
;   event that exposed the instrument to "Galden".
;   
;-
pro ajello_lab_contamination_check_n2_16ev

@'qualcolors'
loadcv, 78, rgb_table=rgb_table, /noqual

y1 = 140
y2 = 920

;file_r10 = '/Volumes/projects/Phase_Development/MAVEN/IUVS_Data/IUVS_Breadboard/NeweGun_round10_after_energy_correction/data_reduction/N2_30EV_FUV_TEST9_IMAGE1_HIPRESS_WITHH20.idl'
file_r10 = '/Volumes/projects/Phase_Development/MAVEN/IUVS_Data/IUVS_Breadboard/NeweGun_round10_after_energy_correction/data_reduction/N2_16EV_FUV_TEST17_IMAGE1_HIPRESS.idl'
restore,file_r10,/ver
int_time10 = int_time
wave10 = wl - 141.7 + 135.3
spec10 = total(arr[*,y1:y2],2) / int_time
arr10 = arr / int_time

; beam current: 21.8 microamps
;file_r12 = '/Volumes/projects/Phase_Development/MAVEN/IUVS_Data/IUVS_Breadboard/Round_12/data_reduction/N2_30EV_FUV_TEST4_IMAGE1.idl'
file_r12 = '/Volumes/projects/Phase_Development/MAVEN/IUVS_Data/IUVS_Breadboard/Round_12/data_reduction/N2_16EV_FUV_TEST18_IMAGE1.idl'
restore,file_r12,/ver
int_time12 = int_time
wave12 = wl - 141.0 + 135.3
spec12 = total(arr[*,y1:y2],2) / int_time
arr12 = arr / int_time

;file_r12b = '/Volumes/projects/Phase_Development/MAVEN/IUVS_Data/IUVS_Breadboard/Round_12/data_reduction/N2_30EV_FUV_TEST15_IMAGE1.idl'
;restore,file_r12b,/ver
;int_time12b = int_time
;wave12b = wl - 125.6 + 120.
;spec12b = total(arr[*,y1:y2],2) / int_time
;arr12b = arr / int_time

win = window(dim=[600*2,600])
max_value=10
im1 = image( arr10, current=win, min_value=0, max_value=max_value, layout=[3,1,1],title=file_Basename(file_r10), rgb_table=rgb_table )
im2 = image( arr12, current=win, min_value=0, max_value=max_value, layout=[3,1,2],title=file_Basename(file_r12), rgb_table=rgb_table )
;im3 = image( arr12b, current=win, min_value=0, max_value=max_value, layout=[3,1,3],title=file_Basename(file_r12b), rgb_table=rgb_table )

path_save = '/users/holsclaw/Documents/'

win = window(dim=[1600,800])
xr=[110,180]
p1 = plot( wave10, spec10, xtitle='wavelength', ytitle='DN/sec', yr=[0,900], current=win, name='R10 - '+file_basename(file_r10), xr=xr, font_size=16 )
p2 = plot( wave12, spec12, /over, color='red', name='R12 - '+file_basename(file_r12) )
;p3 = plot( wave12b, spec12b, /over, color='blue', name='R12 - '+file_basename(file_r12b) )
leg = legend(target=[p1,p2], font_size=16)
win.save, path_save + 'N2_16eV_R10_vs_R12.png'

;win = window(dim=[1600,800])
;xr=[110,180]
;p1 = plot( wave10, spec10, xtitle='wavelength', ytitle='DN/sec', yr=[0,800], current=win, name='R10 - '+file_basename(file_r10), xr=xr, font_size=16 )
;p2 = plot( wave12, spec12, /over, color='red', name='R12 - '+file_basename(file_r12) )
;;p3 = plot( wave12b, spec12b, /over, color='blue', name='R12 DIV 2 - '+file_basename(file_r12b) )
;leg = legend(target=[p1,p2,p3], font_size=16)
;;win.save, path_save + 'N2_30eV_R10_vs_R12_DIV2.png'


stop
end