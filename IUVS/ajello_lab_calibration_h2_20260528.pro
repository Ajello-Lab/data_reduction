
;+
; PURPOSE
;  This routine will 
;
; INPUTS
;  none
;
;-
pro ajello_lab_calibration_h2_20260528

  ajello_lab_set_paths, path_base, path_repo

  path_ref = path_repo + '/data/H2/'

  file_data = '/Volumes/projects/Phase_Development/MAVEN/IUVS_Data/IUVS_Breadboard/_Big_e-gun_RoundVII/data_reduction/H2_100EV_FUV_TEST12_H2_100EV_ROT_+7_IMAGE1_MED_PRESS_2E-5.idl'

  file_data = '/Volumes/projects/Phase_Development/MAVEN/IUVS_Data/IUVS_Breadboard/_Big_e-gun_RoundVII/data_reduction/H2_100EV_FUV_TEST11_H2_100EV_ROT_+7_IMAGE1_LOW_PRESS_4E-6.idl'
  ;restore,file,/ver

  sObj = OBJ_NEW('IDL_Savefile', file_data)
  sObj->Restore, 'arr'
  OBJ_DESTROY, sObj

  spat = total( arr, 1 )
  y0 = where( spat eq max(spat) )
  yw = 100
  y1 = y0-yw
  y2 = y0+yw
  spec = total( arr[*,y1:y2], 2 )

  ;
  ; retrieve wavelength scale for the rotated image
  ;
  ajello_lab_pixel_scale_rot, wlfuv, wlmuv, yfuv, ymuv

  ; assume peak emission at Lyman alpha
  wl_spec = wlfuv - wlfuv[ findndx(spec,max(spec)) ] + 121.6

  win = window(dim=[800,600])
  p1 = plot( wlfuv, spec, name='orignal data', current=win )
  p2 = plot( wl_spec, spec, /over, color='red', name='shifted' )
  leg = legend(target=[p1,p2])
  markerp,p1,x=121.6,linestyle=2

  file_ref = '/Users/holsclaw/Ajello-Lab/data_reduction//data/H2/h2euv100.txt'

  ajello_lab_h2_reference_h2euv100, file_ref, wave_ref, spec_ref
  
  win = window(dim=[1200,600])
  xr=[110,170]
  p1 = plot( wave_ref/10., spec_ref-min(spec_ref), yr=[0,5.2e4], name='reference', xtitle='wavelength (nm)', current=win, xr=xr, font_size=14 )
  p2 = plot( wl_spec, spec, /over, color='red', name='data' )
  leg = legend(target=[p1,p2],font_size=14)
  
  stop

end