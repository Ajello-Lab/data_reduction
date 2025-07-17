pro joes_idl_withticks

  Compile_opt idl2
  loadct,10,/silent
  ;*****************************************************************************;calibrated lab data
  ;added in the MAVEN hifi files to same directory
 
  print,' comparison of three files from Round10, file 1 info'

  date1='May 14,2025 '
  ;
  file1='N2_20EV_FUV_TEST13_IMAGE1_HIPRESS.idl'
  print, 'file name1',file1
  gas=' N2'

  en=' 20 eV'

  ;***********************************
  print,' comparison of three files, file 2 and file 3 are image 2 and 3 info'

  ;******************************************************************************

  ;restore,filename='Z:\IBM\IBM\MAVEN\data_flight\fuv_sonal\fuv_hifi_data.sav';flight data
  ; restore,filename='K:\SSD_I-drive\MAVEN\data_flight\fuv_sonal\fuv_hifi_data.sav';flight data
  restore,filename="D:\SSD_I-drive\MAVEN\data_flight\fuv_sonal\fuv_hifi_data.sav";flight data
  help
  ;stop
  wavelength_fuv=wavenm
  calibrated_fuv=fuv_AIRGLOW_IMG[*,30]/dn_conversion;150 km 30*5=150 km
  calibrated_fuv=calibrated_fuv/max(calibrated_fuv[580:600]);-range of 154.7 to 156.3 where !c is 598 and wavelenght [598] is 156.2nm
  n2_lbh = fitvec[3,*]
  ni1494=fitvec[6,*]
  n2_lbh_BB=[fltarr(124),n2_lbh[0:195],fltarr(240)]

  ;stop
  ;****************************

  ;*************************************************
  ;lets read in uncalibrated lab data
  ;read in one  20 eV lab data by pickfile
  print,' uncal data file info:'
  print, 'MAVEN egun was done in folder in October2024'
  print,'which large egun data file do you want to read from large egun studies

  fil1= "Z:\round10\NeweGun_round10_after_energy_correction\data_reduction\N2_20EV_FUV_TEST13_IMAGE1_HIPRESS.idl"

  ; Determine which detector channel
  ;
  fbase = file_basename(fil1)
  pos_fuv = strpos(fbase,'FUV')
  pos_muv = strpos(fbase,'MUV')
  if pos_fuv gt -1 then channel='FUV'
  if pos_muv gt -1 then channel='MUV'


  restore,fil1
  print,'integration time  ',int_time
  ;stop
  ;
  ; rotate image
  ;
  case channel of
    'FUV': begin
      arr_orig = arr
      ; arr_rot = rotate( arr_orig, 6 )
      ; arr = arr_rot
      arr1=arr*60/int_time
    end
    'MUV': begin
      arr_orig = arr
      arrr=arr
    end
  Endcase

  y1 = 130;outside key hole
  y2 = 940


  spec_mean = mean( arr[*,y1:y2], dimension=2, /nan )  ; for rotated image
  spat_mean = mean( arr         , dimension=1, /nan )  ; for rotated image


  ;stop

  delta_shift=69.;print,69*0.08315 =5.73735 nm shift but see below oct 2024 changed from 69 to 79
  waveuncal=shift(wl,delta_shift); ;
  ;stop
  waveuncal=waveuncal[75:1023]
  ;119.484      119.567      119.650      119.733      119.817      119.900
  ;119.983      120.066      120.149      120.232      120.315      120.398
  ;120.482      120.565      120.648      120.731      120.814      120.897
  ;120.980      121.064      121.147
  ;waveuncal[1200] = 95

  siguncal=spec_mean
  ;IGUNCAL        FLOAT     = Array[949]
  siguncal=siguncal[75:1023];
  ;justification for shift= I find print,max(siguncal[50:150])=391.900 NI 120.0 nm and  print,!c+50=87 where print,waveuncal[87]= 120.066 nm so 69 works
  ;siguncal=shift(siguncal,-7);since print,waveuncal[95]=120.731 and waveuncal[88]=120.066
  waveuncal=shift(waveuncal,+9)
  ;print,siguncal[95]= 54.3584MTHEMAX OF 1200 A
  ;print,waveuncaL[95]=119.983

  ;stop
  yspa1=yspa;0:1023
  set_plot, 'win'
  ;window,3 & plot,waveuncal,siguncal,xrange=[110,130]
  ;stop
 ; s=plot(waveuncal,siguncal,dim=[948,948],xrange=[100,200]);peak of H Ly a=
 ; plot,waveuncal,siguncal,xrange=[100,200]
  ;stop
 ; window,5 & plot,yspa1,spat_mean,xrange=[-150,100]
  print,'max of yspatial  ',max(spat_mean[130:940]), ' counts'  ;!c+130=column 570 =948 counts
  ;stop
  print, 'offset from image 1 e-beam zero  ', yspa1[!c+130], ' mm'; answer = -5.8728mm
  print, 'pixel number peak  ', !c+130;=570
  yspa_new1=yspa1-yspa1[!c+130];shift of -5.872 mm
  yspa_new1_correct=yspa_new1[130:940]
  ;print,where (YSPA_NEW1 ge -.5 and yspa_new1 LE .5,count)580         581         582         583         584 ,yspa_new1[582]=0.000000
 ; window,6&plot,yspa_new1,spat_mean,xrange=[-150,200]

 ; s=plot(yspa,spat_mean,dim=[1024,1024],xrange=[-150,200])
  ;p=plot(yspa_new1,spat_mean,dim=[1024,1024],xrange=[-150,200])
  ;stop
  ;******************************************************declarations-uncalibrated spectral data readin

  ;stop
  ;need to subtract background again
  aa=total(siguncal[900:930])/31.
  bb=total(siguncal[0:20])/21.
  back=0.5*(aa+bb)
  back=aa
  ;back=min(smooth(siguncal[700:1000],7))
  siguncal=siguncal-back
  ;stop
  ;**************************

  ;restore,filename='C:\Users\rele2355\Desktop\MAVEN\data_flight\fuv_sonal\'+'fuv_22sept2017_calibration.save'
  ;restore,filename='K:\SSD_I-drive\MAVEN\data_flight\calibration\model\2017\'+'fuv_22sept2017_calibration.save'
  restore,filename="D:\SSD_I-drive\MAVEN\data_flight\calibration\model\2017\"+'fuv_22sept2017_calibration.save'
  print,'I am back'
  close,1
  ;*****************************************************************************;uncalibrated lab data

  ;before we do anything we need to calibrate data CO2rrectly using BB calibration
  ;stop
  ;*********************************************************************
  cal=fltarr(949)
  ;window,5 & plot, waveuncal,siguncal
  cal_un=INVSENS_BB*siguncal;un: means unnormalized
  ;cal=siguncal[0:1019]/sens_fuv_1024[0:1019];UNornrmalized
  cal=cal_un/max(smooth(cal_un[260:290],1));max is waveuncal[525=155.98nm];normlaized
 ; window,14 & plot, waveuncal,cal,xrange=[110.,130.]
  ;**************************

  ;lets make new array from 73:948 in lambda
  ;and 130:940 in spatial
  arr1_correct=arr1[75:1023,130:940]

  ;stop
  ;********************************************
  ;lets read in File 2
  ;******************************************
  ;
  ;fil2='Z:\IUVS_Breadboard\_Big_e-gun_RoundVII\data_reduction\N2_20eV_FUV_TEST39_20eV_ROT_+7_IMAGE2_HI_PRESS_4E-5.idl'
  fil2='Z:\round10\NeweGun_round10_after_energy_correction\data_reduction\N2_20EV_FUV_TEST14_IMAGE2_HIPRESS.idl'

  ; Determine which detector channel
  ;
  fbase2 = file_basename(fil2)
  pos_fuv = strpos(fbase2,'FUV')
  pos_muv = strpos(fbase2,'MUV')
  if pos_fuv gt -1 then channel='FUV'
  if pos_muv gt -1 then channel='MUV'


  restore,fil2


  print,'integration time  ',int_time
  ;stop

  ;yr_spec = [150,900] ; spatial range of narrow slit, that avoids the keyholes
  ;yr_bump = [300,450] ; spatial range of feature to interpolate across
  ;ajello_lab_interpolate_spatial_feature_may12_im2, arr, arri, yr_spec=yr_spec, yr_bump=yr_bump
  ;arr = arri
  ;

  ;
  ; rotate image
  ;
  case channel of
    'FUV': begin
      arr_orig = arr
      ; arr_rot = rotate( arr_orig, 6 )
      ; arr = arr_rot
      arr2=arr*60/int_time
    end
    'MUV': begin
      arr_orig = arr
      ;arr_rot = rotate( arr_orig, 1 )
      ;arr = arr_rot
      arrr=arr
    end
  Endcase

  y1 = 130
  y2 = 940


  spec_mean2 = mean( arr2[*,y1:y2], dimension=2, /nan )  ; for rotated image
  spat_mean2 = mean( arr2         , dimension=1, /nan )  ; for rotated image


  !linetype=0
  siguncal2=spec_mean2
  yspa2=yspa

  siguncal2=siguncal2[75:1023]
  siguncal2=shift(siguncal2,0)
 ; window,4&plot,waveuncal,siguncal2,xrange=[100,200]
  ;stop
  waveuncal2=waveuncal
  ; stop
  ;**************************************
  ; from N2_round4 concatenate

  ;yspa2=yspa
;  window,7&plot,waveuncal,siguncal2,xrange=[110,130]
;  window,8&plot,yspa2,spat_mean2,xrange=[-150,100]

  yspa_new2=yspa2+6*25.4+5.8728;image 1 peak at -5.8728mm so this is true zero
  yspa_new2_correct=yspa_new2[130:940]
 ; window,9&plot,yspa_new2,spat_mean2,xrange=[0,300]

  ;Nov 15-problem spat_mean2 110:118
  ;
  print,'here is max signal in FUV ',max(siguncal2[200:300])
  print,' here is wavelenght of max ',waveuncal[!c+200], '  nm'
  ;******************************************************declarations-uncalibrated spectral data readin

  ;stop
  ;need to subtract background again
  aa2=total(siguncal2[900:930])/31
  bb2=total(siguncal2[0:20])/21.
  back2=0.5*(aa2+bb2)
  back2=aa2
  ;back=aa
  ;back=min(smooth(siguncal[700:1000],7))
  siguncal2=siguncal2-back2
  ;stop



  ;before we do anything we need to calibrate data CO2rrectly using flight calibration
  ;stop
  ;*********************************************************************
  cal2=fltarr(949)
  ;window,5 & plot, waveuncal,siguncal
  cal2_un=INVSENS_BB*siguncal2
  ;cal=siguncal[0:1019]/sens_fuv_1024[0:1019]
  cal2=cal2_un/max(smooth(cal2_un[260:290],1))
 ; window,22 & plot, waveuncal,cal2,xrange=[110.,130.]
  ; stop
  ;**************************
  ; lets get arr2 correct
  arr2_correct=arr2[75:1023,130:940]
  ;******************************************************
  ;stop
  ;lets read in File 3
  ;******************************************
  ;fil3='Z:\IUVS_Breadboard\_Big_e-gun_RoundVII\data_reduction\N2_20eV_FUV_TEST38_20eV_ROT_+7_IMAGE3_HI_PRESS_4E-5.idl'
  fil3='Z:\round10\NeweGun_round10_after_energy_correction\data_reduction\N2_20EV_FUV_TEST15_IMAGE3_HIPRESS.idl'

  ; Determine which detector channel
  ;
  fbase3 = file_basename(fil3)
  pos_fuv = strpos(fbase3,'FUV')
  pos_muv = strpos(fbase3,'MUV')
  if pos_fuv gt -1 then channel='FUV'
  if pos_muv gt -1 then channel='MUV'


  restore,fil3
  print,'integration time  ',int_time
  ;stop
  ;
  ; rotate image
  ;
  case channel of
    'FUV': begin
      arr_orig = arr
      ; arr_rot = rotate( arr_orig, 6 )
      ; arr = arr_rot
      ;arr3=arr
      arr3=arr*60/int_time
    end
    'MUV': begin
      arr_orig = arr
      ;arr_rot = rotate( arr_orig, 1 )
      ;arr = arr_rot
      arrr=arr*60/int_time
    end
  Endcase
  ;delta_shift=73;print,73 forim 3 and 69*0.08315 =5.73735 nm shift but see below oct 2024 changed from 69 to 79
  ;waveuncal=shift(wl,delta_shift); ;
  ;stop
  waveuncal3=waveuncal
  waveuncal3=shift(waveuncal3,75)
  y1 = 130
  y2 = 940


  spec_mean3 = mean( arr3[*,y1:y2], dimension=2, /nan )  ; for rotated image
  spat_mean3 = mean( arr3         , dimension=1, /nan )  ; for rotated image


  ;from n2_round4 concatanenate
  siguncal3=spec_mean3
  yspa3=yspa

 ; window,10&plot,waveuncal3,siguncal3,xrange=[100,200]
  ;stop
  ;window,11&plot,yspa3,spat_mean3,xrange=[-150,100]

  yspa_new3=yspa3+12*25.4+5.872; mm;image 1 peak at -5.872 mm so this is true zero
  yspa_new3_correct=yspa_new3[130:940]
 ; window,12&plot,yspa_new3,spat_mean3,xrange=[150,450]
  ;stop
  ;******************************************************declarations-uncalibrated spectral data readin
  ;siguncal2=spec_mean2
  ;yspa2=yspa

  ;siguncal2=siguncal2[75:1023]
  ;siguncal2=shift(siguncal2,0)
  ;window,4&plot,waveuncal,siguncal2,xrange=[100,200]
  ;waveuncal2=waveuncal

  ;stop
  ;need to subtract background again
  ;aa2=total(siguncal2[900:930])/31
  ;;bb2=total(siguncal2[0:20])/21.
  ;back2=0.5*(aa2+bb2)
  ;back2=aa2
  ;back=aa
  ;back=min(smooth(siguncal[700:1000],7))
  ;siguncal2=siguncal2-back2
  ;****************
  ;stop
  ;need to subtract background again
  aa3=total(siguncal3[900:930])/31
  bb3=total(siguncal3[0:20])/21.
  ;back3=0.5*(aa3+bb3)
  back3=aa3
  ;back=aa
  ;back=min(smooth(siguncal[700:1000],7))
  siguncal3=siguncal3-back3
  ;stop
  ;****************************
  ;siguncal2=spec_mean2
  ;yspa2=yspa

  ;siguncal2=siguncal2[75:1023]
  ;siguncal2=shift(siguncal2,0)
  ;window,4&plot,waveuncal,siguncal2,xrange=[100,200]
  ;waveuncal2=waveuncal




  ;******************************************


  ;before we do anything we need to calibrate data CO2rrectly using flight calibration
  ;stop
  ;*********************************************************************
  cal3=fltarr(949)
  ;window,5 & plot, waveuncal,siguncal
  cal3_un=INVSENS_BB*siguncal3
  ;cal=siguncal[0:1019]/sens_fuv_1024[0:1019]
  ;cal3=cal3_un/max(smooth(cal3_un[260:290],1))
  cal3=cal3_un/max(smooth(cal3_un[330:390],1));major error as was 260:290
  ;window,20& plot, waveuncal3,cal3,xrange=[110.,130.]
  ;window,10 & plot, waveuncal3,siguncal3,xrange=[110.,130.]
  ;**************************
  ; lets get arr3 correct
  arr3_correct=arr3[75:1023,130:940]
  ;******************************************************
  ;before we do anything lets normalize to flight data image 1 at 206.47 nm
  ;IUVS max at 216.2 nm #64,  BB max at 219.02nm #265
  ;*********************************************************************
  ;total stuff
  sig_total=siguncal+siguncal2+siguncal3
  cal_total=fltarr(949)
  cal_total_un=INVSENS_BB*sig_total
  cal_total=cal_total_un/max(smooth(cal_total_un[260:290],1))
  ;stop
  ;*************************************************************

  wdir_plots="Z:\MOBI\MOBI_2025\Round10\N2\20eV\save\"
  ;wdir_plots='C:\Users\rele2355\Desktop\Concatenate\Round 8\N2\ps\'
  ; stop
  set_plot,'ps'
  filename='fuv_Round10_N2_20EV_FUV_TEST13_14_15_HIPRES_7july2025_vL1.ps'
  device,/landscape,/color,filename=wdir_plots+filename
  loadct,39,/silent
  ;************************************************************
  !p.multi=[0]
  !linetype=0
  ;#1
;  plot,wavelength_fuv,smooth(calibrated_fuv,1),title='P1-mars IUVS HiFi Orbit (150 km) Airglow',ystyle=1,psym=10,yrange=[-1,4],xminor=10,thick=3,xstyle=1,XRANGE=[110,190],xticks=8, xtitle='wavelength (nm)',charsize=1.5,charthick=3.0, ytitle='relative sig (kR/nm)';spec * 1000  to get Ray from kR and divide by 33 to get mean
  ;calibrated spacecraft raw-data


;  xx=[112,120]
;  yy=[6,6]
;  oplot,xx,yy,thick=3
;  xyouts,125,3,'IUVS FUV HiFi Orbit (150 km)',charthick=3,charsize=1.25
;  ; plot,[0,0],yr=[-.1,1.0],xr=[800,1150],title='Titan Disk for File'+ file,xstyle=1,ystyle=1, xtitle='wavelength ('+ang+')', ytitle='sig (kR)'
;  ;stop
;  ;*************************************************************************
;  ;#2
;  !p.multi=[0]
;  ;PLOT,WAVEUNCAL[20:940],cal[20:940],TITLE='P2-BREADBOARD R10 '+fbase+'!c+e('+en+', ) CALIBRATED & NORMALIZED 135nm',CHARSIZE=1.5,xminor=10,CHARTHICK=3.0,THICK=3,YSTYLE=1,PSYM=10,YRANGE=[-1,8],XSTYLE=1,xrange=[110,190],xticks=8, XTITLE='WAVELENGTH (nm)', YTITLE='relative SIG (kR/nm)';SPEC * 1000  TO GET RAY FROM KR AND DIVIDE BY 33 TO GET MEAN !c
;  ; ;UNCAL BB NORMALIZED TO SPACECRAFT AT 2080A
;  xyouts,125,7,'P2-mean spectrum image 1 of 1(T13)- Round 10 ' ,charthick=3,charsize=1.25
;  oplot,WAVEUNCAL[160:680],2.0+SMOOTH(cal[160:680],1)*4,thick=3
;  xyouts,160,4,'X 4', charthick=3,charsize=1.25
;  ;*******************3****************************************************
;  ;#3
; ; PLOT,WAVEUNCAL[20:940],SMOOTH(SIGUNCAL[20:940],1),TITLE='P3-BREADBOARD LAB R10 '+ fbase+ '!c  + e('+en+', )  SPECTRUM UNCALIBRATED & UNNORMALIZED',CHARSIZE=1.5,CHARTHICK=3.0,THICK=3,YSTYLE=1,PSYM=10,YRANGE=[-10,100],XSTYLE=1,xrange=[110,190],xticks=8,xminor=10, yticks=11,yminor=5,XTITLE='WAVELENGTH (NM)', YTITLE='SIG (dn/minute)';SPEC * 1000  TO GET RAY FROM KR AND DIVIDE BY 33 TO GET MEAN
;  ;oplot,WAVEUNCAL[150:640],20+SMOOTH(SIGUNCAL[150:640],1)*4,thick=3
;  xx1=[122,125]
;  yy1=[30,30]
;  ;oplot,xx1,yy1,thick=3
;  xyouts,150,60,' P3 N2_20eV_test13_FUV_image1  !c UNCALIBRATED & UNNORMALIZED)  ',charthick=3,charsize=1.25
;  !linetype=0
;  ;oplot,WAVEUNCAL[160:680],35.0+SMOOTH(siguncal [160:680],1)*2,thick=3
;  ;xyouts,130,45,'X 2', charthick=3,charsize=1.25
;  ; stop
;  ;*******************************************
;  ;#4
; ; PLOT,WAVEUNCAL2[20:940],cal2[20:940],TITLE='P4-BREADBOARD  '+fbase2+ '!c  + e('+en+', )  SPECTRUM CALIBRATED AND NORMALIZED to 135.6A',CHARSIZE=1.5,xminor=10,yminor=5,CHARTHICK=3.0,THICK=3,YSTYLE=1,PSYM=10,YRANGE=[-0.5,1.5],yticks=4,XSTYLE=1,xrange=[110,190],xticks=8, XTITLE='WAVELENGTH (NM)', YTITLE='SIG (Relative kR/nm)';SPEC * 1000  TO GET RAY FROM KR AND DIVIDE BY 33 TO GET MEAN
;  ;UNCAL BB NORMALIZED TO SPACECRAFT AT 2080A
;
;  xyouts,112,1.3,'P4 mean image 2(T13),(T14),3(T15),round 10 ' ,charthick=3,charsize=1.25
;
;  ;oplot,WAVEUNCAL2[150:640],4+SMOOTH(cal2[150:640],1)*4,thick=3
;  ;xx1=[122,125]
;  ;yy1=[4,4]4
;  ;oplot,xx1,yy1,thick=3
;  ;xyouts,145,0,'N2_20eV_test17_FUV_image1  !c UNCALIBRATED & UNNORMALIZED)  !c' + fBASE,charthick=3,charsize=1.25
;  !linetype=0
;  ;xyouts,130,8,'X 4', charthick=3,charsize=1.25
;
;  ;*********************************************
;  ;#5
; ; PLOT,WAVEUNCAL2[20:940],SMOOTH(SIGUNCAL2[20:940],1),TITLE=' P5-BREADBOARD LAB '+fbase2+ '!c  + e('+en+', )  SPECTRUM UNCALIBRATED & UNNORMALIZED',CHARSIZE=1.5,CHARTHICK=3.0,THICK=3,YSTYLE=1,PSYM=10,YRANGE=[-5,25],XSTYLE=1,xrange=[110,190],xticks=8,xminor=10, yminor=5,yticks=6, XTITLE='WAVELENGTH (nm)', YTITLE='SIG (dn/minute)';SPEC * 1000  TO GET RAY FROM KR AND DIVIDE BY 33 TO GET MEAN
;  xx1=[157,160]
;  yy1=[60,60]
;  oplot,xx1,yy1,thick=3
;  xyouts,142,20,'P5 N!d2!n_20eV_test14_FUV_image2  !c UNCALIBRATED & UNNORMALIZED)  ',charthick=3,charsize=1.25
;  ;oplot,WAVEUNCAL[160:680],7.0+SMOOTH(siguncal2 [160:680],1)*2,thick=3
;  ;xyouts,160,13,'X 2', charthick=3,charsize=1.25
;  !linetype=0
;
;
;
;  ;****************************************
;
;  ;#6
; ; PLOT,WAVEUNCAL3[20:948],cal3[20:948],ystyle=1,yminor=5,TITLE='P6-BREADBOARD R10 '+fbase3+' !c + e('+en+' eV ) SPECTRUM  CALIBRATED AND NORMALIZED to 1356A',CHARSIZE=1.5,xminor=10,CHARTHICK=3.0,THICK=3,PSYM=10,YRANGE=[-3,12],yticks=15,XSTYLE=1,xrange=[110,190],xticks=8, XTITLE='WAVELENGTH (nm)', YTITLE='Relative SIG (kR/nm)';SPEC * 1000  TO GET RAY FROM KR AND DIVIDE BY 33 TO GET MEAN
;  ;UNCAL BB NORMALIZED TO SPACECRAFT AT 2080A N
;  xyouts,120,18.0,'P6 mean spectrum of image 3 (T13),2(T14),3(T15) !c, round 10 ' ,charthick=3,charsize=1.25
;  oplot,WAVEUNCAL3[160:680],4.0+SMOOTH(cal3[160:680],1)*5,thick=3
;  xyouts,160,6,'X 5', charthick=3,charsize=1.25
;  ;*****************************************
;  ;#7
; ; PLOT,WAVEUNCAL3[20:948],SMOOTH(SIGUNCAL3[20:948],1),TITLE='P7-BREADBOARD LAB R10 '+fbase3+'!c +e('+en+'eV , '+') SPECTRUM UNCALIBRATED & UNNORMALIZED',CHARSIZE=1.5,CHARTHICK=3.0,THICK=3,YSTYLE=1,PSYM=10,YRANGE=[-5,30],XSTYLE=1,xrange=[110,190],xticks=8,xminor=10, yminor=5,yticks=7,XTITLE='WAVELENGTH (nm)', YTITLE='SIG (dn/minute)';SPEC * 1000  TO GET RAY FROM KR AND DIVIDE BY 33 TO GET MEAN
;  xx1=[121,125]
;  yy1=[17.0,17.0]
;  ;oplot,xx1,yy1,thick=3
;  xyouts,127.,27.0,'N2_20eV_test15_FUV_image3 UNCALIBRATED & UNNORMALIZED)  ',charthick=3,charsize=1.25
;  !linetype=0
;  oplot,WAVEUNCAL[160:680],5+SMOOTH(siguncal3 [160:680],1)*4,thick=3
;  ; oplot,WAVEUNCAL3[160:680],4.0+SMOOTH(SIGUNCAL3[160:680],1)*2,thick=3
;  xyouts,160,8,'X 4', charthick=3,charsize=1.25
;
;
;
;  ;****************************************
;  ;stop
;  ;*******************************
;  ;8
;  color=0
; ; plot,wavelength_fuv[20:940],smooth(calibrated_fuv[20:940],1),linestyle=1,color=0,title='P8-Mars IUVS fuv glow & Calibrated BreadBoard !C Image 1(T13),2(T14),3(T15), Round 10',psym=10,yrange=[-2,10],yticks=6,charsize=1.5,charthick=3.0,thick=5,xstyle=1,ystyle=1,xrange=[110,170],xminor=10, xticks=6,xtitle='wavelength (nm)', ytitle='sig (kR/nm)';spec * 1000  to get Ray from kR and divide by 33 to get mean
;  oplot,waveuncal2[20:940],cal2[20:940], thick=5,color=230,linestyle=3
;  oplot,waveuncal3[20:940],cal3[20:940], thick=5,color=130,linestyle=2
;  oplot,waveuncal[20:940],cal[20:940], thick=5,color=30,linestyle=0
;  !linetype=0
;  ;!linetype=1
;  ;oplot,wave_venus,ratio_spicav*sig_venus,thick=3,color=50
;  xx1=[126,129]
;  yy1=[9,9]
;  !linetype=1&;oplot,xx1,yy1, thick=5,color=230
;  !linetype=0
;  ;oplot,xx1,yy1+8, thick=5,color=0
;  oplot,xx1,yy1, thick=5,color=0,linestyle=1
;  oplot,xx1,yy1-2, thick=5,color=230,linestyle=3
;  oplot,xx1,yy1-1, thick=5,color=130,linestyle=2
;  oplot,xx1,yy1-3, thick=5,color=30,linestyle=0
;  !linetype=0;xyouts,140,2.4,'spicav mars Express',color=50,charthick=3, charsize=1.25
;  xyouts,130,7,'IUVS BB, Image 2, ' ,color=r,charthick=5, charsize=1.25
;  xyouts,130,9,'Mars IUVS HiFi Orbit 150 km',color=0,charthick=3, charsize=1.25
;  xyouts,130,8,'IUVS BB, Image 3',color=g,charthick=5, charsize=1.25
;  xyouts,130,6,'IUVS BB, Image 1 ',color=b,charthick=5, charsize=1.25
;
;  ;********************;*******************
;  ;=
;  ;********************;***********
;  ;9
;  ;plot,wavelength_fuv[20:940],smooth(calibrated_fuv[20:940],1),title='P9 Mars IUVS fuv glow & Calibrated BreadBoard !C Image 1(T13),2(T14),3(T15), Round 10',ystyle=1,psym=10,yminor=5,yrange=[-1,2.5],yticks=7,charsize=1.5,charthick=3.0,thick=5,xstyle=1,xrange=[125,170],xminor=10, xticks=9,xtitle='wavelength (nm)', ytitle='sig (kR/nm)';spec * 1000  to get Ray from kR and divide by 33 to get mean
;  !linetype=1
;  oplot,waveuncal[20:940],cal2[20:940], thick=5,color=230
;  !linetype=3
;  oplot,waveuncal3[20:940],cal3[20:940], thick=5,color=130
;  !linetype=1
;  oplot,waveuncal[20:940],cal[20:940], thick=5,color=30
;  !linetype=0
;  ;!linetype=1
;  ;oplot,wave_venus,ratio_spicav*sig_venus,thick=3,color=50
;  xx1=[126,129]
;  yy1=[2.2,2.2]
;  !linetype=1&oplot,xx1,yy1, thick=5,color=230
;  !linetype=0
;  oplot,xx1,yy1, thick=5,color=0
;  ;oplot,x3.6x1,yy1, thick=5,color=50
;  oplot,xx1,yy1-0.1, thick=5,color=130,linestyle=3
;  oplot,xx1,yy1-0.2, thick=5,color=230,linestyle=2
;  oplot,xx1,yy1-0.3, thick=5,color=30,linestyle=1
;  !linetype=0;xyouts,140,2.4,'spicav mars Express',color=50,charthick=3, charsize=1.25
;  xyouts,130,2.0,'IUVS BB, Image 2' ,color=230,charthick=5, charsize=1.25
;  xyouts,130,2.2,'Mars IUVS HiFi Orbit 150 km',color=0,charthick=3, charsize=1.25
;  xyouts,130,2.1,'IUVS BB, Image 3 ',color=130,charthick=5, charsize=1.25
;  xyouts,130,1.9,'IUVS BB, Image 1 ',color=30,charthick=5, charsize=1.25
;  ;***************************************
;  ;plot10
;  print,'i made to plot10'
;  ;*******************************
; ; plot,wavelength_fuv[20:940],smooth(calibrated_fuv[20:940],1),title='P10-Mars IUVS fuv glow & Calibrated BreadBoard !C Image 1(T13),2(T14),3(T15), Round 10',ystyle=1,psym=10,yrange=[-1,4],yticks=5, yminor=10,charsize=1.5,charthick=3.0,thick=5,xstyle=1,xrange=[115,170],xminor=10, xticks=11,xtitle='wavelength (nm)', ytitle='sig (kR/nm)';spec * 1000  to get Ray from kR and divide by 33 to get mean
;  !linetype=2
;  oplot,waveuncal2[20:940],cal2[20:940], thick=5,color=230
;  !linetype=3
;  oplot,waveuncal3[20:940],cal3[20:940], thick=5,color=130
;  !linetype=1
;  oplot,waveuncal[20:940],cal[20:940], thick=5,color=30
;  !linetype=0
;  ;!linetype=1
;  ;oplot,wave_venus,ratio_spicav*sig_venus,thick=3,color=50
;  xx1=[130,135]
;  yy1=[3.7,3.7]
;  !linetype=1&oplot,xx1,yy1, thick=5,color=230
;  !linetype=0
;  oplot,xx1,yy1, thick=5,color=0
;  ;oplot,xx1,yy1, thick=5,color=50
;  oplot,xx1,yy1-.4, thick=5,color=130,linestyle=3
;  oplot,xx1,yy1-0.8, thick=5,color=230,linestyle=2
;  oplot,xx1,yy1-1.2, thick=5,color=30,linestyle=1
;  !linetype=0;xyouts,140,2.4,'spicav mars Express',color=50,charthick=3, charsize=1.25
;  xyouts,136,2.9,'IUVS BB, Image 2, ' ,color=230,charthick=5, charsize=1.25
;  xyouts,136,3.7,'Mars IUVS HiFi Orbit 150 km',color=0,charthick=3, charsize=1.25
;  xyouts,136,3.3,'IUVS BB, Image 3 ',color=130,charthick=5, charsize=1.25
;  xyouts,136,2.5,'IUVS BB, Image 1 ',color=30,charthick=5, charsize=1.25
;  ;**************************************
;  ;plot 11
;  ;uncalibrated and unnormalizedimages and total
;  ;************************************************
;  print,'i made to plot11'
;  PLOT,WAVEUNCAL[20:940],SMOOTH(SIGUNCAL[20:940],1),linestyle=0,TITLE='P11-BREADBOARD LAB '+ fbase+ '!c  + e('+en+', )  SPECTRUMUNCALIBRATED & UNNORMALIZED',CHARSIZE=1.5,CHARTHICK=3.0,THICK=6,YSTYLE=1,PSYM=10,YRANGE=[-10,120],XSTYLE=1,xrange=[110,190],xticks=8,xminor=10, XTITLE='WAVELENGTH (NM)', YTITLE='SIG (dn/minute)';SPEC * 1000  TO GET RAY FROM KR AND DIVIDE BY 33 TO GET MEAN
;  !linetype=16
;  oPLOT,WAVEUNCAL2[20:940],SMOOTH(SIGUNCAL2[20:940],1),color=230,thick=5,linestyle=2
;  !linetype=2
;  oPLOT,WAVEUNCAL3[20:940],SMOOTH(SIGUNCAL3[20:940],1),color=130,thick=5,linestyle=3
;  !linetype=3
;  oPLOT,WAVEUNCAL[20:940],SMOOTH(SIG_total[20:940],1),color=70,thick=5,linestyle=1
;  !linetype=0
;  xx1=[140,148]
;
;  !linetype=0
;  yy1=[90,90]
;  xyouts,150,yy1,'IUVS BB, image 1, ' ,color=30,charthick=3,charsize=1.25
;  xyouts,150,yy1-5,'IUVS BB, Image 2 ' ,color=230,charthick=5, charsize=1.25
;  ;xyouts,148,2.75,'Mars IUVS HiFi Orbit 150 km',color=0,charthick=3, charsize=1.25
;  xyouts,150,yy1-10,'IUVS BB, Image 3  ',color=130,charthick=5, charsize=1.25
;  xyouts,150,yy1-15,'IUVS BB, Image total ',color=70,charthick=5, charsize=1.25
;  !linetype=0
;  xyouts,170,32,'N!D2!N + e(20eV) ',color=0,charthick=5, charsize=1.75
;  oplot,xx1,yy1,thick=5,color=30
;  oplot,xx1,yy1-10, thick=5,color=130,linestyle=3
;  oplot,xx1,yy1-5, thick=5,color=230,linestyle=2
;  oplot,xx1,yy1-15, thick=5,color=70,linestyle=1
;  ;*******************************************
;  print,'i made to plot12'
;  ;plot12-calibrated
;
;  PLOT,WAVEUNCAL[20:940],SMOOTH(CAL_un[20:940],1),color=30,linestyle=0,TITLE='P12-BREADBOARD LAB R10 '+ fbase+ '!c  + e('+en+', )  SPECTRUM CALIBRATED & UNNORMALIZED',CHARSIZE=1.5,CHARTHICK=3.0,THICK=6,YSTYLE=1,PSYM=10,YRANGE=[-10,130],XSTYLE=1,xrange=[110,170],xticks=6,xminor=10, XTITLE='Wavelength (nm)', YTITLE='Relative Calibrated Intensity [arb units]';SPEC * 1000  TO GET RAY FROM KR AND DIVIDE BY 33 TO GET MEAN
;  !linetype=17
;  oPLOT,WAVEUNCAL2[20:940],SMOOTH(CAL2_un[20:940],1),color=230,thick=5 ,linestyle=3
;  !linetype=2
;  oPLOT,WAVEUNCAL3[20:940],SMOOTH(CAL3_un[20:940],1),color=130,thick=5,linestyle=2
;  !linetype=3
;  oPLOT,WAVEUNCAL[20:940],SMOOTH(CAL_total_un[20:940],1),color=70,thick=5,linestyle=1
;  !linetype=0
;  xx1=[141,144.5]
;  yy1=[120,120]
;  !linetype=0
;
;  xyouts,145,120,'IUVS BB Image 1 ',charthick=3,charsize=1.25,color=30
;  xyouts,145,120-10,'IUVS BB, Image 2 ',color=230,charthick=5, charsize=1.25
;  xyouts,145,120-20,'IUVS BB, Image3 ',color=130,charthick=5, charsize=1.25
;  ;xyouts,148,2.75,'Mars IUVS HiFi Orbit 150 km',color=0,charthick=3, charsize=1.25
;  xyouts,145,120-30,'IUVS BB, Image total ',color=70,charthick=5, charsize=1.25
;  xyouts,152,60,'N!D2!N + e(20eV) ',color=0,charthick=5, charsize=1.75
;  !linetype=0
;
;  oplot,xx1,yy1,thick=5,color=30
;  oplot,xx1,yy1-10, thick=5,color=230,linestyle=3
;  oplot,xx1,yy1-20, thick=5,color=130,linestyle=2
;  oplot,xx1,yy1-30, thick=5,color=70,linestyle=1
;
;
;  ;***************************************
;  ;plot13 140-170nm
;
;  ;uncalibrated and unnormalizedimages and total
  ;************************************************
;  PLOT,WAVEUNCAL[20:940],SMOOTH(SIGUNCAL[20:940],1),color=30,TITLE='P13-BREADBOARD LAB '+ fbase+ '!c  + e('+en+', )  SPECTRUM UNCALIBRATED & UNNORMALIZED',CHARSIZE=1.5,CHARTHICK=3.0,THICK=6,YSTYLE=1,PSYM=10,YRANGE=[-10,160],XSTYLE=1,xrange=[125,170],xticks=9,xminor=10, XTITLE='WAVELENGTH (NM)', YTITLE='SIG (dn/minute)';SPEC * 1000  TO GET RAY FROM KR AND DIVIDE BY 33 TO GET MEAN
;  !linetype=1
;  oPLOT,WAVEUNCAL2[20:940],SMOOTH(SIGUNCAL2[20:940],1),color=230,thick=5
;  !linetype=2
;  oPLOT,WAVEUNCAL3[20:940],SMOOTH(SIGUNCAL3[20:940],1),color=130,thick=5
;  !linetype=3
;  oPLOT,WAVEUNCAL[20:940],SMOOTH(SIG_total[20:940],1),color=30,thick=5
;  !linetype=0
;  xx1=[130,135]
;  yy1=[140,140]
;  !linetype=0
;
;  xyouts,136,140,'IUVS BB, Image 1 UNCAL & UNNORM) !c ' ,color=30,charthick=3,charsize=1.25
;  ;xyouts,148,2.75,'Mars IUVS HiFi Orbit 150 km',color=0,charthick=3, charsize=1.25
;  xyouts,136,140-6,'IUVS BB, Image 2 ,',color=230,charthick=5, charsize=1.25
;  xyouts,136,140-12,'IUVS BB, Image 3 ,',color=130,charthick=5, charsize=1.25
;  xyouts,136,140-18,'IUVS BB, Image total ',color=70,charthick=5, charsize=1.25
;  xyouts,150,130,'N!D2!N + e(20eV) ',color=0,charthick=5, charsize=1.75
;  !linetype=0
;  oplot,xx1,yy1,thick=5,color=30
;  oplot,xx1,yy1-12, thick=5,color=130,linestyle=3
;  oplot,xx1,yy1-6, thick=5,color=230,linestyle=2
;  oplot,xx1,yy1-18, thick=5,color=70,linestyle=1
;  ;*******************************************
;  ;plot14-calibrated
;
;  PLOT,WAVEUNCAL[20:940],SMOOTH(CAL_un[20:940],1),color=30,TITLE='P14-BREADBOARD LAB '+ fbase+ '!c  + e('+en+', )  SPECTRUM CALIBRATED & NORMALIZED 135.6 nm',CHARSIZE=1.5,CHARTHICK=3.0,THICK=6,YSTYLE=1,PSYM=10,YRANGE=[-10,120],XSTYLE=1,xrange=[125,170],xticks=9,xminor=10, XTITLE='WAVELENGTH (NM)', YTITLE='Calibrated Relative Intensity [arb units]';SPEC * 1000  TO GET RAY FROM KR AND DIVIDE BY 33 TO GET MEAN
;  !linetype=1
;  oPLOT,WAVEUNCAL2[20:940],SMOOTH(CAL2_un[20:940],1),color=230,thick=5
;  !linetype=2
;  oPLOT,WAVEUNCAL3[20:940],SMOOTH(CAL3_un[20:940],1),color=130,thick=5,linestyle=2
;  !linetype=3
;  oPLOT,WAVEUNCAL[20:940],SMOOTH(CAL_total_un[20:940],1),color=70,thick=5,linestyle=3
;  !linetype=0
;  xx1=[148,154]
;  yy1=[95,95]
;  !linetype=0
;
;  xyouts,155,95,'IUVS BB Image 1 ' ,charthick=3,charsize=1.25,color=30
;  xyouts,155,89,'IUVS BB, Image 2 ' ,color=230,charthick=5, charsize=1.25
;  ;xyouts,148,2.75,'Mars IUVS HiFi Orbit 150 km',color=0,charthick=3, charsize=1.25
;  xyouts,155,83,'IUVS BB, Image 3 ',color=130,charthick=5, charsize=1.25
;  xyouts,155,77,'IUVS BB, Image total ',color=70,charthick=5, charsize=1.25
;  !linetype=0
;  oplot,xx1,yy1,thick=5,color=30
;  oplot,xx1,yy1-6, thick=5,color=130,linestyle=2
;  oplot,xx1,yy1-12, thick=5,color=230,linestyle=1
;  oplot,xx1,yy1-18, thick=5,color=70,linestyle=3
;  xyouts,155,45,'N!D2!N + e(20eV) ',color=0,charthick=5, charsize=1.75
  ;**************************************************************
  ;plot 14-clibrated-fixed
  ;;plot14-calibrated

  PLOT,WAVEUNCAL[20:940],SMOOTH(CAL_un[20:940],1),color=30,TITLE='P14-fixed BREADBOARD LAB '+ fbase+ '!c  + e('+en+', )  SPECTRUM CALIBRATED & NORMALIZED 135.6 nm',CHARSIZE=1.5,CHARTHICK=3.0,THICK=6,YSTYLE=1,PSYM=10,YRANGE=[-10, 230], YTICKS=5, YMINOR=5,XSTYLE=1,XRANGE=[123, 170], XTICKV=[125, 130, 135, 140, 145, 150, 155, 160, 165, 170], XTICKS=9, XMINOR=5, XTITLE='WAVELENGTH (NM)', YTITLE='Calibrated Relative Intensity [arb units]';SPEC * 1000  TO GET RAY FROM KR AND DIVIDE BY 33 TO GET MEAN
  !linetype=1
  oPLOT,WAVEUNCAL2[20:940],SMOOTH(CAL2_un[20:940],1),color=230,thick=5
  !linetype=2
  oPLOT,WAVEUNCAL3[20:940],SMOOTH(CAL3_un[20:940],1),color=130,thick=5,linestyle=2
  !linetype=3
  oPLOT,WAVEUNCAL[20:940],SMOOTH(CAL_total_un[20:940],1),color=70,thick=5,linestyle=3
  !linetype=0
  xx1=[148,154]
  yy1=[95,95]
  !linetype=0

  xyouts,155,95,'IUVS BB Image 1 ' ,charthick=3,charsize=1.25,color=30
  xyouts,155,89,'IUVS BB, Image 2 ' ,color=230,charthick=5, charsize=1.25
  ;xyouts,148,2.75,'Mars IUVS HiFi Orbit 150 km',color=0,charthick=3, charsize=1.25
  xyouts,155,83,'IUVS BB, Image 3 ',color=130,charthick=5, charsize=1.25
  xyouts,155,77,'IUVS BB, Image total ',color=70,charthick=5, charsize=1.25
  !linetype=0
  oplot,xx1,yy1,thick=5,color=30
  oplot,xx1,yy1-6, thick=5,color=130,linestyle=2
  oplot,xx1,yy1-12, thick=5,color=230,linestyle=1
  oplot,xx1,yy1-18, thick=5,color=70,linestyle=3
  xyouts,155,45,'N!D2!N + e(20eV) ',color=0,charthick=5, charsize=1.75
  ;****************************
  ;fixed
  ;************************************************************
  
  ;
   
 
; TICS   =====================================================================================================================

; top black line
xyouts, 140, 215, 'N!d2!n LBH a-X', color=0, charthick=5, charsize=1.25
oplot, [168,168], [205,210], thick=4, color=0
oplot, [128,128], [205,210], thick=4, color=0
oplot, [128,168], [210,210], thick=2, color=0

; tic spacing
wave_line0 = [145.0, 150.1, 155.5,161.2,167.2]  ; v'lines v''ticks
wave_brt0 = [0.391, 0.389, 0.170,0.043]   ;v'lines v''ticks
xdim0     = n_elements(wave_line0)

wave_line1 = [141.6, 146.4, 151.5, 157.0,162.7,168.8]
wave_brt1 = [0.524, 0.016, 0.136, 0.203,0.094]
xdim1     = n_elements(wave_line1)

wave_line2 = [138.4,143.0,147.9,153.0,158.5,164.2]
xdim2     = n_elements(wave_line2)

wave_line3 = [135.4,139.8,144.4,149.3,154.5,160.0,165.8]
xdim3     = n_elements(wave_line3)

wave_line4 = [132.5,136.8,141.2,145.9,150.8,156.0,161.6,167.4]
xdim4     = n_elements(wave_line4)

wave_line5 = [129.9,133.9,138.2,142.7,147.4,152.3,157.6,163.1,169.0]
xdim5     = n_elements(wave_line5)

wave_line6 = [127.3,131.2,135.3,139.6,144.1,148.9,153.9,159.2,164.8]
xdim6     = n_elements(wave_line6)


; horizontal v' lines
xline0=[145.0,167.2]
yline0=[130,130]

xline1=[141.6,168.8]
yline1=[140,140]

xline2=[138.4,164.2]
yline2=[150,150]

xline3=[135.4,165.8]
yline3=[160,160]

xline4=[132.5,167.4]
yline4=[170,170]

xline5=[129.9,169.0]
yline5=[180,180]

xline6=[127.3,164.8]
yline6=[190,190]

; tic heights
y0tik0 = 125
y0tik1 = 130

y1tik0 = 135
y1tik1 = 140

y2tik0 = 145
y2tik1 = 150

y3tik0 = 155
y3tik1 = 160

y4tik0 = 165
y4tik1 = 170

y5tik0 = 175
y5tik1 = 180

y6tik0 = 185
y6tik1 = 190

;****
wmin = 125.
wmax = 169.
oplot,xline0,yline0,color=220,thick=3
oplot,xline1,yline1,color=220,thick=3
oplot,xline2,yline2,color=220,thick=3
oplot,xline3,yline3,color=220,thick=3
oplot,xline4,yline4,color=220,thick=3
oplot,xline5,yline5,color=220,thick=3
oplot,xline6,yline6,color=220,thick=3

; loops
for ii=1,xdim0 do begin
  if (wave_line0[ii-1] gt wmin and wave_line0[ii-1] lt wmax) then begin
    oplot,[wave_line0[ii-1],wave_line0[ii-1]],[y0tik0,y0tik1],thick=3,color=220
  endif
endfor

for ii=1,xdim1 do begin
if (wave_line1[ii-1] gt wmin and wave_line1[ii-1] lt wmax) then begin
    oplot,[wave_line1[ii-1],wave_line1[ii-1]],[y1tik0,y1tik1],thick=3,color=220
endif
endfor

for ii=1,xdim2 do begin
 if (wave_line2[ii-1] gt wmin and wave_line2[ii-1] lt wmax) then begin
   oplot,[wave_line2[ii-1],wave_line2[ii-1]],[y2tik0,y2tik1],thick=3,color=220
  endif
endfor

for ii=1,xdim3 do begin
  if (wave_line3[ii-1] gt wmin and wave_line3[ii-1] lt wmax) then begin
    oplot,[wave_line3[ii-1],wave_line3[ii-1]],[y3tik0,y3tik1],thick=3,color=220
  endif
endfor

for ii=1,xdim4 do begin
  if (wave_line4[ii-1] gt wmin and wave_line4[ii-1] lt wmax) then begin
    oplot,[wave_line4[ii-1],wave_line4[ii-1]],[y4tik0,y4tik1],thick=3,color=220
  endif
endfor

for ii=1,xdim5 do begin
  if (wave_line5[ii-1] gt wmin and wave_line5[ii-1] lt wmax) then begin
    oplot,[wave_line5[ii-1],wave_line5[ii-1]],[y5tik0,y5tik1],thick=3,color=220
  endif
endfor

for ii=1,xdim6 do begin
  if (wave_line6[ii-1] gt wmin and wave_line6[ii-1] lt wmax) then begin
    oplot,[wave_line6[ii-1],wave_line6[ii-1]],[y6tik0,y6tik1],thick=3,color=220
  endif
endfor


; labels
; v"
xyouts,145,120,'0',color=220,charthick=3,charsize=1.1
xyouts,150.1,120,'1',color=220,charthick=3,charsize=1.1
xyouts,155.5,120,'2',color=220,charthick=3,charsize=1.1
xyouts,161.2,120,'3',color=220,charthick=3,charsize=1.1
xyouts,167.2,120,'4=v"',color=220,charthick=3,charsize=1.1
; v'
xyouts,142.0,125," v'=0",color=220,charthick=3,charsize=1.1
xyouts,138.6,135," v'=1",color=220,charthick=3,charsize=1.1
xyouts,135.4,145," v'=2",color=220,charthick=3,charsize=1.1
xyouts,132.4,155," v'=3",color=220,charthick=3,charsize=1.1
xyouts,129.5,165," v'=4",color=220,charthick=3,charsize=1.1
xyouts,126.9,175," v'=5",color=220,charthick=3,charsize=1.1
xyouts,124.3,185," v'=6",color=220,charthick=3,charsize=1.1

; END TICS   ========================================================================================================================



  ;*********************************************************
  ;plot15-calibrated

  PLOT,waveuncal[20:940],cal[20:940],color=30,TITLE='P15-BB LAB N2 20eV Hi Press Image 1 !c(T17),2(T18),3(T19)  SPECTRUM CALIBRATED & NORMALIZED 135.6 nm',CHARSIZE=1.5,CHARTHICK=3.0,THICK=6,YSTYLE=1,yticks=4,yminor=10,PSYM=10,YRANGE=[-.5,1.5],XSTYLE=1,xrange=[125,170],xticks=9,xminor=10, XTITLE='WAVELENGTH (NM)', YTITLE='Calibrated Relative Intensity [arb units]';SPEC * 1000  TO GET RAY FROM KR AND DIVIDE BY 33 TO GET MEAN
  !linetype=1
  oPLOT,waveuncal2[20:940],cal2[20:940],color=230,thick=5
  !linetype=2
  oplot,waveuncal3[20:940],cal3[20:940],color=130,thick=5
  ;!linetype=3
  ;oPLOT,WAVEUNCAL[20:940],SMOOTH(CAL_total_un[20:940],1),color=230,thick=5
  !linetype=0
  xx1=[150.4,153]
  yy1=[1.1,1.1]
  ;!linetype=1&oplot,xx1,yy1, thick=5,color=230
  !linetype=0
  oplot,xx1,yy1-0.2, thick=5,color=130,linestyle=2
  ;oplot,x3.6x1,yy1, thick=5,color=50
  oplot,xx1,yy1, thick=5,color=230,linestyle=3
  ;!linetype=3
  oplot,xx1,yy1+0.2, thick=5,color=30,linestyle=1
  !linetype=0;xyouts,140,2.4,'spicav mars Express',color=50,charthick=3, charsize=1.25
  xyouts,153,1.1,'IUVS BB, Image 2' ,color=230,charthick=5, charsize=1.25
  ;xyouts,130,2.2,'Mars IUVS HiFi Orbit 150 km',color=0,charthick=3, charsize=1.25
  xyouts,153,0.9,'IUVS BB, Image 3 ',color=130,charthick=5, charsize=1.25
  xyouts,153,1.3,'IUVS BB, Image 1 ',color=30,charthick=5, charsize=1.25
  ;*******************************************
  ;multi plot16-calibrate
  ;!p.multi[0]
  !linetype=0
  !p.multi=[0,1,3]
  PLOT,WAVEUNCAL[20:700],SMOOTH(CAL_un[20:700],1),color=30,TITLE='P16-BREADBOARD LAB '+ fbase+'!c +e('+en+'eV, ) SPECTRUM CALIBRATED & UNNORMALIZED',CHARSIZE=1.5,CHARTHICK=3.0,THICK=6,YSTYLE=1,PSYM=10,YRANGE=[-5,100],XSTYLE=1,xrange=[125,170],xticks=9,xminor=10, XTITLE='WAVELENGTH (NM)', YTITLE='Calibrated Relative Intensity [arb units]';SPEC * 1000  TO GET RAY FROM KR AND DIVIDE BY 33 TO GET MEAN
  xx1=[131,133]
  yy1=[45,45]
  ;oplot,xx1,yy1,thick=5,color=0
  xyouts,140,70,'IUVS BB Image 1 ' ,color=30,charthick=3,charsize=1.25
  !linetype=0
  title=' '
  PLOT,WAVEUNCAL[20:700],SMOOTH(CAL2_un[20:700],1),color=230,THICK=6,YSTYLE=1,PSYM=10,YRANGE=[-5.0,40],XSTYLE=1,xrange=[125,170],xticks=9,xminor=10, XTITLE='WAVELENGTH (NM)', YTITLE='Calibrated Relative Intensity [arb units]'
  yy2=[39.9,39.9]
  ;oplot,xx1,yy2, thick=5,color=30,linestyle=3
  xyouts,160,15,'IUVS BB, Image 2 ' ,color=230,charthick=5, charsize=1.25
  !linetype=0
  PLOT,WAVEUNCAL3[20:700],SMOOTH(CAL3_un[20:700],1),color=130,THICK=6,YSTYLE=1,PSYM=10,YRANGE=[-3.0,10],XSTYLE=1,xrange=[125,170],xticks=9,xminor=10, XTITLE='WAVELENGTH (NM)', YTITLE='Calibrated Relative Intensity [arb units]'
  !linetype=2
  oplot,WAVEUNCAL3[20:700],SMOOTH(CAL3_un[20:700],5),color=160,THICK=6
  yy3=[4.5,4.5]
  ;oplot,xx1,yy3, thick=5,color=130,
  !linetype=0
  xyouts,150,8,'IUVS BB, Image 3 ' ,color=160,charthick=5, charsize=1.25
  ;PLOT,WAVEUNCAL[20:700],SMOOTH(CAL_total_un[20:700],1),color=230,THICK=6,YSTYLE=1,PSYM=10,YRANGE=[-.10,170],XSTYLE=1,xrange=[125,170],xticks=9,xminor=10, XTITLE='WAVELENGTH (NM)', YTITLE='Calibrated Relative Intensity [arb units]'
  yy4=[155.,155.]
  ;oplot,xx1,yy4, thick=5,color=230,linestyle=1
  ;xyouts,130,155,'IUVS BB, Image total ',color=230,charthick=5, charsize=1.25


  !linetype=0



  ;xyouts,148,2.75,'Mars IUVS HiFi Orbit 150 km',color=0,charthick=3, charsize=1.25


  !linetype=0

  ;****************************
  ;multi plot17-calibrated
  ;!p.multi[0]
  !linetype=0
  !p.multi=[0,1,3]
  PLOT,WAVEUNCAL[20:700],SMOOTH(CAL_un[20:700],1),color=30,TITLE='P17-BREADBOARD LAB '+ fbase+'!c +e('+en+'eV, )  SPECTRUM CALIBRATED & UNNORMALIZED',CHARSIZE=1.5,CHARTHICK=3.0,THICK=6,YSTYLE=1,PSYM=10,YRANGE=[-1,110],XSTYLE=1,xrange=[125,170], xticks=9,xminor=10, XTITLE='WAVELENGTH (NM)', YTITLE='Calibrated Relative Intensity [arb units]';SPEC * 1000  TO GET RAY FROM KR AND DIVIDE BY 33 TO GET MEAN
  xx1=[131,133]
  yy1=[5,5]
  ;oplot,xx1,yy1,thick=5,color=0
  xyouts,160,60,'IUVS BB Image 1 ' ,charthick=3,charsize=1.25
  !linetype=0
  title=' '
  PLOT,WAVEUNCAL2[20:700],SMOOTH(CAL2_un[20:700],1),color=230,THICK=6,YSTYLE=1,PSYM=10,YRANGE=[-5.0,20],XSTYLE=1,xrange=[125,170],xticks=9,xminor=10, XTITLE='WAVELENGTH (NM)', YTITLE='Calibrated Relative Intensity [arb units]'
  yy2=[9.9,9.9]
  ;oplot,xx1,yy2, thick=5,color=30,linestyle=3
  xyouts,160,14,'IUVS BB, Image 2 ' ,color=230,charthick=5, charsize=1.25
  !linetype=0
  PLOT,WAVEUNCAL3[20:700],SMOOTH(CAL3_un[20:700],1),color=130,THICK=6,YSTYLE=1,PSYM=10,YRANGE=[-1.0,10],XSTYLE=1,xrange=[125,170],xticks=9,xminor=10, XTITLE='WAVELENGTH (NM)', YTITLE='Calibrated Relative Intensity [arb units]'
  !linetype=2
  oplot,WAVEUNCAL3[20:700],SMOOTH(CAL3_un[20:700],5),color=160,THICK=6
  yy3=[8,8]
  ;oplot,xx1,yy3, thick=5,color=130,linestyle=2
  !linetype=0
  xyouts,155,9.0,'IUVS BB, Image 3 ',color=130,charthick=5, charsize=1.25
  ;PLOT,WAVEUNCAL[20:700],SMOOTH(CAL_total_un[20:700],1),color=230,THICK=6,YSTYLE=1,PSYM=10,YRANGE=[-.10,170],XSTYLE=1,xrange=[125,170],xticks=9,xminor=10, XTITLE='WAVELENGTH (NM)', YTITLE='Calibrated Relative Intensity [arb units]'
  ;yy4=[9,9]
  ;oplot,xx1,yy4, thick=5,color=230,linestyle=1
  ;xyouts,130,155,'IUVS BB, Image total ',color=230,charthick=5, charsize=1.25


  !linetype=0



  ;xyouts,148,2.75,'Mars IUVS HiFi Orbit 150 km',color=0,charthick=3, charsize=1.25


  !linetype=0
  ;*********************
  !p.multi=[0,0,0]
  ;************************************************************
  device,/close
  set_plot,'win'
  print,'i made to to the end'
  
  
  
  
  
  
  
  
  stop
  
  
  
  
  
  
  
  
  
  !p.multi=[0,0,0]
  ;*****************************************************************************
  ;lets compare 1356 and 1464 ratios
  print, 'ratio image 1 =1464/1356 =    ',max(cal_un[375:420])/max(cal_un[220:260])
  print, 'ratio image 2 =1464/1356 =    ',max(cal2_un[375:420])/max(cal2_un[220:260])
  print, 'ratio image 3 =1464/1356 =    ',max(cal3_un[375:420])/max(cal3_un[220:260])
  ;***************************************************************************************
  ;nrl='\\lasp-store\home\jajello\Documents\IBM\MOBI_2017\Big_e-gun_round-6\CO2\30eV\'
  ;oplot low presure
  ;save 7  20 eVimage 1,2,3 variables
  cal_hi_20eV_N2_R10=cal
  cal2_hi_20eV_N2_R10=cal2
  cal3_hi_20eV_N2_R10=cal3
  cal_total_hi_20eV_N2_R10=cal_total
  cal_un_hi_20eV_N2_R10=cal_un
  cal2_un_hi_20eV_N2_R10=cal2_un
  cal3_un_hi_20eV_N2_R10=cal3_un
  cal_total_un_hi_20eV_N2_R10=cal_total_un
  waveuncal_hi=waveuncal
  ;waveuncal_hi2=waveuncal2
  nrl="Z:\MOBI\MOBI_2025\Round10\N2\20eV\save\"
  ; nrl='C:\Users\rele2355\Desktop\Concatenate\Round 8\N2\saves\'
  ;save,SIG_total,SIGUNCAL,SIGUNCAL2,SIGUNCAL3,CAL_total_un,CAL_un,CAL2_un,CAL3_un,wavelength_fuv,calibrated_fuv,waveuncal,ratio_cal_total2*cal_total,CO2_model_thin_norm2,CO2_model_thick_norm2,filename=nrl+'CO2_30eV_round6_test1_2_3_all_IUVS_model_thick_thin_4nov2017'

  save,yspa_new1_correct,yspa_new3_correct,yspa_new2_correct,SIG_total,SIGUNCAL,SIGUNCAL2,SIGUNCAL3,CAL_total_un_hi_20eV_N2_R10,CAL_un_hi_20eV_N2_R10,CAL2_un_hi_20eV_N2_R10,CAL3_un_hi_20eV_N2_R10,cal_total_hi_20eV_N2_R10,CAL_hi_20eV_N2_R10,CAL2_hi_20eV_N2_R10,CAL3_hi_20eV_N2_R10,wavelength_fuv,calibrated_fuv,waveuncal,wl,filename=nrl+'N2_round_10_20eV_FUV_6-8_IMAGE1-3_hi_press_fixcal3_integration_time_corect_may13.idl'
  ;save,CAL_total_un_hi,CAL_un_hi,CAL2_un_hi,CAL3_un_hi,waveuncal, filename=nrl+'N2_20eV_august2019_FUV_TEST_19-20-21_IMAGE1_2_3_hi_press.idl'
  ;*******************************************************************************************

  ;stop

  close,1 & openw,1,nrl+'fuv_round10_test6_7_8_calibrated_N2_20eV_image1_2_3_hi_press_may2025_50us.txt'
  printf,1,'data from N2_20eV_6  taken on 05-13/round10/2025'


  printf,1,'  num Wavlen (nm)  cal_total_Data ( norm to max on data),calibrated data (~dn), Uncalibrated lab total signal(dn)';   , IUVS     , spicav,  Model


  for n=1,940 do begin

    printf,1,'$(i5,3x,(4f15.3))',n,waveuncal[n-1],cal_total[n-1],cal_total_un[n-1], SIG_total[n-1];,spicaver[n-1], IUVS[n-1]
  endfor

  close,1
  ;stop

  !p.multi=[0,0,0]


  ;CROSS SECTION CALCULATIONS R. LEE SEPT 2019 - NEED  20 eV Q's from Ajello 1985 w/ Malone 2008 correction
  ;*******************************************************************************************
  ;lets get LBH  20 eV cross section vs 120.0nm  20 eV cross section from Ajello 1985 and Ajello 2017
  Q120nm=4.48e-18*0.826; Malone correction
  Qlbh_partial=q120nm*total(cal_total_un_hi_20eV_N2_R10[248:327])/total(cal_total_un_hi_20eV_N2_R10[75:100])
  qlbh_total=qlbh_partial/0.135
  print,'the partial  20 eV Qlbh from 135-140.0 nm =  ', Qlbh_partial
  print,'the total  20 eV cross section  Qlbh from 126-250 nm =  ', Qlbh_total
  qdirect=q120nm*total(cal_un_hi_20eV_n2_R10[248:327])/total(cal_total_un_hi_20eV_n2_R10[75:100]);1335 to 1400A image1 and 1200 is 11901211
  qcascade=q120nm*total(cal3_un_hi_20eV_N2_R10[248:327])/total(cal_total_un_hi_20eV_N2_R10[75:100]);1335 to 1400A image3
  print,'the total Qlbh_direct  20 eV cross section from 126-250 nm =  ', Qdirect/.135
  print,'the total Qlbh cascade  20 eV cross section from 126-250 nm =  ', qcascade/.135
  ;*********************88
  ;second guess
  Q120nm=4.48e-18*0.826; Malone correction
  Qlbh_partial=q120nm*total(cal_total_un_hi_20eV_N2_R10[248:327])/total(cal_un_hi_20eV_N2_R10[75:100])
  qlbh_total=qlbh_partial/0.135
  print,'the partial  20 eV Qlbh from 135-140.0 nm =  ', Qlbh_partial
  print,'the total  20 eV cross section  Qlbh from 126-250 nm =  ', Qlbh_total
  qdirect=q120nm*total(cal_un_hi_20eV_n2_R10[248:327])/total(cal_un_hi_20eV_N2_R10[75:100]);1335 to 1400A image1 and 1200 is 11901211
  qcascade=q120nm*total(cal3_un_hi_20eV_N2_R10[248:327])/total(cal_un_hi_20eV_N2_R10);1335 to 1400A image3
  print,'the total Qlbh_direct  20 eV cross section from 126-250 nm =  ', Qdirect/.135
  print,'the total Qlbh cascade  20 eV cross section from 126-250 nm =  ', qcascade/.135

  ;******************************************************************
  ;stop
  ;**********************************************************88
  ;concatenate-Italy
  ;*****************************************************8

  ;***********************************************
  ;lets concantenate image 1,2 and 3 into one image_grand
  glow1=total(arr1_correct,1);(1024,1024) total over lambda and a function of yspa1
  glow2_unnorm=total(arr2_correct,1);(940,810)
  glow3_unnorm=total(arr3_correct,1)


  glow2=glow2_unnorm*mean(glow1[870-130:900-130])/mean(glow2_unnorm[150-130:180-130]);normalize glow2_unorm to glow 1 and call new result glow2
  glow3=glow3_unnorm*mean(glow2[870-130:900-130])/mean(glow3_unnorm[150-130:180-130]);normalize glow3_unnorm to glow2

  image_grand=fltarr(949,751*3);working with spatial 150:900
  image_grand[*,0:750]=arr1_correct[*,150-130:900-130]
  image_grand[*,751:751+750]=arr2_correct[*,150-130:900-130]*mean(glow1[870-130:900-130])/mean(glow2_unnorm[150-130:180-130]);normalize glow2_unorm to glow 1 and call new result glow2
  image_grand[*,751+751:751+751+750]=arr3_correct[*,150-130:900-130]*mean(glow2[870-130:900-130])/mean(glow3_unnorm[150-130:180-130]);normalize glow3_unnorm to glow2
  ;image_grand=arr[0:1023,150:900] +arr2[0:1023,150:900] + arr3[0:1023,150:900]
  yspa_grand=[yspa_new1[150:900],yspa_new2[150:900],yspa_new3[150:900]]
  yy=sort(yspa_grand);array indices of yspa_grand in ascending order outside of key hole
  ygrand= yspa_grand[yy]
  igrand=image_grand[*,yy]
  iglow=total(igrand,1)
  ;***********************************
  ;image LBH
  ;******************************
  ;glow1_lbh=fltarr(83)
  ;glow2_lbh=fltarr(83)
  ;glow3_lbh=fltarr(83)
  arr1_temp=arr1_correct[255:335,*];133.53-140.18 nm for LBH
  arr2_temp=arr2_correct[255:335,*]
  arr3_temp=arr3_correct[255:335,*]
  glow1_1356=total(arr1_temp,1)
  glow2_1356_unnorm=total(arr2_temp,1)
  glow3_1356_unnorm=total(arr3_temp,1)
  ;print,max(glow1_1356)=125159.

  glow2_1356=glow2_1356_unnorm*mean(glow1_1356[870-130:900-130])/mean(glow2_1356_unnorm[150-130:180-130]);normalize glow2_lbh_unorm to glow 1_lbh and call new result glow2_lbh
  glow3_1356=glow3_1356_unnorm*mean(glow2_1356[870-130:900-130])/mean(glow3_1356_unnorm[150-130:180-130]);normalize glow3_unnorm to glow2

  image_1356=fltarr(81,751*3);1347.8-1364.4
  image_1356[*,0:750]=arr1_temp[*,150-130:900-130]
  image_1356[*,751:751+750]=arr2_temp[*,150-130:900-130]*mean(glow1_1356[870-130:900-130])/mean(glow2_1356_unnorm[150-130:180-130])
  image_1356[*,751+751:751+751+750]=arr3_temp[*,150-130:900-130]*mean(glow2_1356[870-130:900-130])/mean(glow3_1356_unnorm[150-130:180-130]);normalize glow3_unnorm to glow2
  ;image_grand=arr[0:1023,110:940] +arr2[0:1023,110+940:940+940] + arr3[0:1023,110+940+940:940+940] ]
  yspa_grand=[yspa_new1[150:900],yspa_new2[150:900],yspa_new3[150:900]]
  yy=sort(yspa_grand);array indices of yspa_grand in ascending order outside of key hole
  ygrand= yspa_grand[yy]
  igrand_1356=image_1356[*,yy]
  iglow_1356=total(igrand_1356,1)
  ;******************************
  ;***********************************
  ;image 120nm
  ;******************************
  ;glow1_120=fltarr(22)
  ;glow2_120=fltarr(22)
  ;glow3_120=fltarr(22)


  arr1_temp=arr1_correct[80:100,*];]119.5--121.1A (100) vs 120.7(95)
  arr2_temp=arr2_correct[80:100,*]
  arr3_temp=arr3_correct[80:100,*]
  glow1_130=total(arr1_temp,1)
  glow2_130_unnorm=total(arr2_temp,1)
  glow3_130_unnorm=total(arr3_temp,1);*1.6;*glow2_120[850]/glow3_120[200]
  ;print,max(glow1_130)=65867.8

  glow2_130=glow2_130_unnorm*mean(glow1_130[870-130:900-130])/mean(glow2_130_unnorm[150-130:180-130]);normalize glow2_lbh_unorm to glow 1_lbh and call new result glow2_lbh
  glow3_130=glow3_130_unnorm*mean(glow2_130[870-130:900-130])/mean(glow3_130_unnorm[150-130:180-130]);normalize glow3_unnorm to glow2

  ;glow3_120=glow3-120*glow2_120[900]/glow3_120[150]
  image_130=fltarr(21,751*3);129.0-131.45 with 100
  ;image_130=fltarr(16,751*3);129.0-131.45 with 95
  image_130[*,0:750]=arr1_temp[*,150-130:900-130]
  image_130[*,751:751+750]=arr2_temp[*,150-130:900-130]*mean(glow1_130[870-130:900-130])/mean(glow2_130_unnorm[150-130:180-130]);normalize glow2_lbh_unorm to glow 1_lbh and call new result glow2_lbh
  image_130[*,751+751:751+751+750]=arr3_temp[*,150-130:900-130]*mean(glow2_130[870-130:900-130])/mean(glow3_130_unnorm[150-130:180-130]);normalize glow3_unnorm to glow2
  ;image_grand=arr[0:1023,110:940] +arr2[0:1023,110+940:940+940] + arr3[0:1023,110+940+940:940+940] ]
  ;yspa_grand=[yspa_new1[150:900],yspa_new2[150:900],yspa_new3[150:900]]
  ;yy=sort(yspa_grand);array indices of yspa_grand in ascending order outside of key hole
  ;ygrand= yspa_grand[yy]
  igrand_130=image_130[*,yy]
  iglow_130=total(igrand_130,1)
  ;******************************

  ;Y10MM
  Y10MM=10*FINDGEN(50)-100;-96.7 TO 374.3MM
  ;indices=fltarr(50)
  Iglow10mm=fltarr(50)
  Iglow10mm_1356=fltarr(50)
  FOR I=0,48 DO BEGIN
    ;FOR J=0,2431 DO BEGIN
    ;print,'ygrand is',ygrand[j]
    print,'y10mm is ',y10mm[i]
    INDICES= WHERE (YGRAND GT Y10MM[I] AND YGRAND LT Y10MM[I+1])

    ;ENDFOR
    ;Y10MM[I]=MEAN(YGRAND[INDICES[i]])
    Iglow10MM[I]=Mean(iglow[INDICES])
    ;print,'indices[i] is',indices
    ; print,iglow10mm[i]
    ;print,'mean ygrand is  ',MEAN(YGRAND[INDICES])
    ; print,'mean of iglow is   ',Mean(iglow[INDICES])
  ENDFOR
  ;*********************
  window,13&plot,ygrand,iglow,xrange=[-100,400],/ylog,yrange=[100,40000.]
  ;****************************
  ;lbh
  ;************************************
  Iglow10mm_lbh=fltarr(50)
  FOR I=0,48 DO BEGIN
    ;FOR J=0,2431 DO BEGIN
    ;print,'ygrand is',ygrand[j]
    print,'y10mm is ',y10mm[i]
    INDICES_1356= WHERE (YGRAND GT Y10MM[I] AND YGRAND LT Y10MM[I+1])

    ;ENDFOR
    ;Y10MM[I]=MEAN(YGRAND[INDICES[i]])
    Iglow10MM_1356[I]=Mean(iglow_1356[INDICES_1356])
    ;print,'indices[i] is',indices
    ; print,iglow10mm[i]
    ;print,'mean ygrand is  ',MEAN(YGRAND[INDICES])
    ; print,'mean of iglow is   ',Mean(iglow[INDICES])
  ENDFOR
  ;*********************
  ;130
  ;************************************
  Iglow10mm_130=fltarr(50)
  FOR I=0,48 DO BEGIN
    ;FOR J=0,2431 DO BEGIN
    ;print,'ygrand is',ygrand[j]
    print,'y10mm is ',y10mm[i]
    INDICES_130= WHERE (YGRAND GT Y10MM[I] AND YGRAND LT Y10MM[I+1])

    ;ENDFOR
    ;Y10MM[I]=MEAN(YGRAND[INDICES[i]])
    Iglow10MM_130[I]=Mean(iglow_130[INDICES_130])
    ;print,'indices[i] is',indices
    ; print,iglow10mm[i]
    ;print,'mean ygrand is  ',MEAN(YGRAND[INDICES])
    ; print,'mean of iglow is   ',Mean(iglow[INDICES])
  ENDFOR
  ;*********************
  window,14&plot,ygrand,iglow,xrange=[-100,400],/ylog,yrange=[100,40000.]
  window,15&plot,ygrand,iglow_1356,xrange=[-100,400],/ylog,yrange=[1,10000.]
  window,16&plot,ygrand,iglow_130,xrange=[-100,400],/ylog,yrange=[1,10000.]
  ;****************************
  ;stop
  ;******************************************
  ;sept2019
  ;lets bring in info fromLBH paper
  ;restore,filename='\\lasp-store\home\jajello\Documents\IBM\MOBI_2017\Big_e-gun_round-4\N2\lbh_paper_variables.save'

  ;help
  ;
  ;***********************************
  filename='fuv_IUVS_glow_12may2025_concatenate_hi_press_round10_N2_20eV_50us_no_bump_corrected.ps'
  ;wdir_plots='W:\Documents\IBM\MOBI_2017\Big_e-gun_round-6\CO2\40eV\ps\'
  wdir_plots= "Z:\MOBI\MOBI_2025\Round10\N2\20eV\ps"
  ;wdir_plots='C:\Users\rele2355\Desktop\Concatenate\Round 8\N2\ps\Glow\'
  set_plot,'ps'
  device,/landscape,/color,filename=wdir_plots+filename
  loadct,39,/silent

  ; stop
  ;************************************************************
  !p.multi=[0,0,0]

  ;*******************************************
  !linetype=0
  ;#1
  PLOT,ygrand,iglow,/ylog,xrange=[-100,400],yrange=[100,1.e5],xstyle=1,ystyle=1,xticks=10,color=0,xminor=5,thick=3,charsize=1.5,charthick=3.0,xtitle='radius (mm)', ytitle='Intensity all fuv (counts)',title='#1 N!d2!n  20 eV Glow Pattern-round 8 T17, 18,19(October 2024)';s
  oplot,yspa_new1[130:940],glow1, thick=5,color=230,linestyle=2;was 150:900
  oplot,yspa_new2[130:940],glow2, thick=5,color=130,linestyle=2
  oplot,yspa_new3[130:940],glow3, thick=5,color=30,linestyle=3
  oplot,y10mm[0:48],iglow10mm[0:48], thick=5,color=80,linestyle=0,psym=10
  xyouts,100,75000,'Image 1, ',color=230,charthick=5, charsize=1.25
  xyouts,100,63000,'image 2',color=130,charthick=3, charsize=1.25
  xyouts,100,42000,'Image 3, ',color=30,charthick=5, charsize=1.25
  xyouts,100,29000,'Image 1+2+3, N!d2!n :113-192nm ',color=0,charthick=5, charsize=1.25;133.12-140.60 nm
  xyouts,100,19000,'10mm average, FUV Image 1+2+3 ',color=80,charthick=5, charsize=1.25
  ;********************************
  ;#2
  color=0
  !linetype=0
  PLOT,ygrand,iglow,/ylog,xrange=[-100,400],yrange=[100,110000],xstyle=1,ystyle=1,xticks=10,color=0,xminor=5,thick=3,charsize=1.5,charthick=3.0,xtitle='radius (mm)', ytitle='Intensity LBH (counts)',title='#2 N!d2!n  20 eV Glow Pattern-Round 10 T13,T14,T15(May 2025)';s
  oplot,yspa_new1[130:940],glow1, thick=5,color=230,linestyle=1
  oplot,yspa_new2[130:940],glow2, thick=5,color=130,linestyle=2
  oplot,yspa_new3[130:940],glow3, thick=5,color=30,linestyle=3
  oplot,y10mm[0:48],iglow10mm[0:48], thick=5,color=80,linestyle=0,psym=10
  xyouts,100,80000,'Image 1 ',color=230,charthick=5, charsize=1.25
  xyouts,100,55000,'image 2',color=130,charthick=3, charsize=1.25
  xyouts,100,38000,'Image 3 ',color=30,charthick=5, charsize=1.25
  xyouts,100,26000,'Image  1+2+3',color=0,charthick=5, charsize=1.25
  xyouts,100,17000,'10mm average, FUV Image 1+2+3 ',color=80,charthick=5, charsize=1.25

  xyouts,60,10000,'e(20eV) + N!d2!n --> N!d2!n 113-192 nm fuv glow pattern ',color=0,charthick=5, charsize=1.25
  ;*******************************************
  ;#3
  !linetype=0
  PLOT,ygrand,iglow_1356,/ylog,xrange=[-100,400],yrange=[10,10000],xstyle=1,ystyle=1,xticks=10,color=0,xminor=5,thick=3,charsize=1.5,charthick=3.0,xtitle='radius (mm)', ytitle='Intensity (counts)',title='#3 N!d2!n  20 eV Glow Pattern-Round 10 T13,14,15,(May2025)';s
  oplot,yspa_new1[130:940],glow1_1356, thick=5,color=230,linestyle=1;was 150:900
  oplot,yspa_new2[130:940],glow2_1356, thick=5,color=130,linestyle=2
  oplot,yspa_new3[130:940],glow3_1356, thick=5,color=30,linestyle=3
  oplot,y10mm[0:48],iglow10mm_1356[0:48], thick=5,color=80,linestyle=0,psym=10
  xyouts,-20,230,'Image 1 ',color=230,charthick=5, charsize=1.25
  xyouts,-20,150,'image 2',color=130,charthick=3, charsize=1.25
  xyouts,-20,99,'Image 3 ',color=30,charthick=5, charsize=1.25
  xyouts,-90,68,'Image 1+2+3, LBH 133.12-140.60 nm',color=0,charthick=5, charsize=1.25
  xyouts,-90,42,'10mm average, LBH Image 1+2+3 ',color=80,charthick=5, charsize=1.25
  xyouts,80,7050,'e(20eV) + N!d2!n -->LBH  glow pattern ',color=0,charthick=5, charsize=1.25

  ;*******************************************
  ;#4
  !linetype=0
  PLOT,ygrand,iglow_130,/ylog,xrange=[-100,400],yrange=[1,4000],xstyle=1,ystyle=1,xticks=10,color=0,xminor=5,thick=3,charsize=1.5,charthick=3.0,xtitle='radius (mm)', ytitle='Intensity (counts)',title=' #4 N!d2!n  20 eV Glow Pattern-Round 10 T13,14,15 (May2025)';s
  oplot,yspa_new1[130:940],glow1_130, thick=5,color=230,linestyle=1; was 150:900
  oplot,yspa_new2[130:940],glow2_130, thick=5,color=130,linestyle=2
  oplot,yspa_new3[130:940],glow3_130, thick=5,color=30,linestyle=3
  oplot,y10mm[0:48],iglow10mm_130[0:48], thick=5,color=80,linestyle=0,psym=10
  xyouts,70,2900,'Image 1 ',color=230,charthick=5, charsize=1.25
  xyouts,70,2000,'image 2',color=130,charthick=3, charsize=1.25
  xyouts,70,1500,'Image 3 ',color=30,charthick=5, charsize=1.25
  xyouts,70,1100,'Image 1+2+3 119.0-121.1 nm',color=0,charthick=5, charsize=1.25
  xyouts,70,700,'10mm average, 120.0 nm Image 1+2+3 ',color=80,charthick=5, charsize=1.25

  xyouts,50,400,'e(20eV) + N!d2!n --> 120.0 nm glow pattern ',color=0,charthick=5, charsize=1.25
  ;*******************************************
  ;#5
  PLOT,ygrand,iglow_130,/ylog,xrange=[-100,400],yrange=[1,160000],xstyle=1,ystyle=1,xticks=10,color=0,xminor=5,thick=3,charsize=1.5,charthick=3.0,xtitle='radius (mm)', ytitle='Intensity (counts)',title=' #5 N!d2!n  20 eV Glow Pattern-Round 10  T13,14,15 (Mat2024)';s
  oplot,ygrand,iglow_1356,color=230,thick=4
  ;oplot,yspa_new1[150:900],glow1_120[150:900], thick=5,color=230,linestyle=2
  ;oplot,yspa_new2[150:900],glow2_120[150:900], thick=5,color=130,linestyle=2
  ;oplot,yspa_new3[150:900],glow3_120[150:900], thick=5,color=30,linestyle=3
  oplot,y10mm[0:48],iglow10mm_130[0:48], thick=5,color=80,linestyle=0,psym=10
  oplot,y10mm[0:48],iglow10mm_1356[0:48], thick=5,color=140,linestyle=0,psym=10
  ;xyouts,50,8000,'Image 1, ',color=230,charthick=5, charsize=1.25
  ;xyouts,50,6500,'image 2',color=130,charthick=3, charsize=1.25
  ;xyouts,50,5000,'Image 3, ',color=30,charthick=5, charsize=1.25
  xyouts,150,56000,'Image 120 nm 1+2+3 ',color=0,charthick=5, charsize=1.25
  xyouts,150,30000,'Image LBH 1+2+3 ',color=230,charthick=5, charsize=1.25
  xyouts,150,18000,'10mm average, 120.0nm, Image 1+2+3 ',color=80,charthick=5, charsize=1.25
  xyouts,50,10000,'10mm average, LBH 133.53-140.18 nm, Image 1+2+3 ',color=140,charthick=5, charsize=1.25

  ;*******************************************
  ;*******************************************
  ;#6
  PLOT,ygrand,iglow_130,/ylog,xrange=[-100,400],yrange=[1,160000],xstyle=1,ystyle=1,xticks=10,color=0,xminor=5,thick=3,charsize=1.5,charthick=3.0,xtitle='radius (mm)', ytitle='Intensity (counts)',title='#6 N!d2!n  20 eV Glow Pattern-Round 10 T13,14,15(May 2025)';s
  oplot,ygrand,iglow_1356,color=240,thick=4
  ;oplot,yspa_new1[150:900],glow1_120[150:900], thick=5,color=230,linestyle=2
  ;oplot,yspa_new2[150:900],glow2_120[150:900], thick=5,color=130,linestyle=2
  ;oplot,yspa_new3[150:900],glow3_120[150:900], thick=5,color=30,linestyle=3
  oplot,y10mm[0:48],iglow10mm_130[0:48], thick=5,color=80,linestyle=0,psym=10
  oplot,y10mm[0:48],iglow10mm_1356[0:48], thick=5,color=140,linestyle=0,psym=10
  ;xyouts,50,8000,'Image 1, ',color=230,charthick=5, charsize=1.25
  ;xyouts,50,6500,'image 2',color=130,charthick=3, charsize=1.25
  ;xyouts,50,5000,'Image 3, ',color=30,charthick=5, charsize=1.25

  xyouts,50,45000,'Image LBH 1+2+3 ',color=240,charthick=5, charsize=1.25
  xyouts,50,31000,'10mm average, LBH 133.53-140.18nm Image 1+2+3 ',color=140,charthick=5, charsize=1.25;133.53-140.18 nm for LBH;
  xyouts,50,20000,'Image 120.0 nm 1+2+3 ',color=0,charthick=5, charsize=1.25
  xyouts,50,12000,'10mm average, 120.0nm Image 1+2+3 ',color=80,charthick=5, charsize=1.25;119.0-120.65 nm for LBH

  ;******************************************************************************************************************
  ;*******************************************
  ;#7
  PLOT,ygrand,iglow_130*max(glow1_1356)/max(glow1_130),/ylog,xrange=[-100,400],yrange=[1,160000],xstyle=1,ystyle=1,xticks=10,color=0,xminor=5,thick=3,charsize=1.5,charthick=3.0,xtitle='radius (mm)', ytitle='Intensity (counts)',title=' #7 N!d2!n  20 eV Glow Pattern-Round 10 T13,14,15 (May2025)';s
  oplot,ygrand,iglow_1356,color=240,thick=4
  ;oplot,yspa_new1[150:900],glow1_120[150:900], thick=5,color=230,linestyle=2
  ;oplot,yspa_new2[150:900],glow2_120[150:900], thick=5,color=130,linestyle=2
  ;oplot,yspa_new3[150:900],glow3_120[150:900], thick=5,color=30,linestyle=3
  oplot,y10mm[0:48],iglow10mm_130[0:48]*max(glow1_1356)/max(glow1_130), thick=5,color=80,linestyle=0,psym=10
  oplot,y10mm[0:48],iglow10mm_1356[0:48], thick=5,color=140,linestyle=0,psym=10
  ;xyouts,50,8000,'Image 1, ',color=230,charthick=5, charsize=1.25
  ;xyouts,50,6500,'image 2',color=130,charthick=3, charsize=1.25
  ;xyouts,50,5000,'Image 3, ',color=30,charthick=5, charsize=1.25
  xyouts,50,18000,'Image 120.0 nm 1+2+3 normalized LBH[2,0]',color=0,charthick=5, charsize=1.25
  xyouts,50,49000,'Image LBH 1+2+3 ',color=240,charthick=5, charsize=1.25
  xyouts,50,9000,'10mm average, 120.0nm, Image 1+2+3 !cnormalized LBH[2,0]',color=80,charthick=5, charsize=1.25;119.0-120.65 nm for LBH
  xyouts,50,31000,'10mm average, LBH 133.53-140.18nm, Image 1+2+3 ',color=140,charthick=5, charsize=1.25;133.53-140.18 nm for LBH


  ;********************************************************************************************************
  ;;device,/close
  ;set_plot,'win'

  ;***************************************
  ;lets store best concenated glow for 135.6, 130.0 and full glow
  glow1_N2_R10_20eV=glow1
  glow1_1356_N2_R10_20eV=glow1_1356
  glow1_130_N2_R10_20eV=glow1_130
  glow2_N2_R10_20eV=glow2
  glow2_1356_N2_R10_20eV=glow2_1356
  glow2_130_N2_R10_20eV=glow2_130
  glow3_N2_R10_20eV=glow3
  glow3_1356_N2_R10_20eV=glow3_1356
  glow3_130_N2_R10_20eV=glow3_130
  yspa_new1_correct_N2_R10_20eV=yspa_new1_correct
  yspa_new2_correct_N2_R10_20eV=yspa_new2_correct
  yspa_new3_correct_N2_R10_20eV=yspa_new3_correct
  y10mm_N2_R10_20eV=y10mm
  iglow_N2_R10_20eV=iglow
  iglow_1356_N2_R10_20eV=iglow_1356
  iglow10mm_1356_N2_R10_20eV=iglow10mm_1356
  iglow_130_N2_R10_20eV=iglow_130
  ygrand_N2_R10_20eV=ygrand
  iglow10mm_130_N2_R10_20eV=iglow10mm

  SAVE, glow1_N2_R10_20eV, glow1_1356_N2_R10_20eV,glow1_130_N2_R10_20eV,glow2_N2_R10_20eV,glow2_1356_N2_R10_20eV,glow2_130_N2_R10_20eV,glow3_N2_R10_20eV,glow3_1356_N2_R10_20eV,glow3_130_N2_R10_20eV,yspa_new1_correct_N2_R10_20eV,yspa_new2_correct_N2_R10_20eV,yspa_new3_correct_N2_R10_20eV,y10mm_N2_R10_20eV,iglow_N2_R10_20eV,iglow_1356_N2_R10_20eV,iglow10mm_1356_N2_R10_20eV,iglow_130_N2_R10_20eV,ygrand_N2_R10_20eV,iglow10mm_130_N2_R10_20eV,filename=nrl+'N2_20eV_round10_concatenate_130_1356Image1_2_3_hi_press_25May2025.idl'
  ;where glow1 is total all wavelength glow for 811 y-pixels. glow1_1356 is 1356 glow etc
  ;yspa_new1_correct 811 sptial pixels from -96.81 to 82.2 mm in counts
  ;yspa_new2_correct 811 sptial pixels from 60.0 to 239 mm
  ;*******************************************
  ;**************************************
  ;lets calculate LBH band system at 100 eV from N2 with cal_total_un'
  q120_100eV=3.7e-18;100eV value from Malone comapred to 4.48e-18 from Ajello and Shemansky 1985 give correction of 0.826; over 1289.9-1318.7A Avakyan 5.78 and 5.65 but averge of ajello and Mumma is 6.03 at 40 eV; ajello= 7.56e-19/1.3 =5.82 and Mumma gets 10.4 x 0.6= 6.24. I get 6 channels =0.498 nm or 0.0831 nm/channel with peak at ch217=130.374 nm

  ;work
  back_1200=(cal_total_un[80]+cal_total_un[100])/2./21.;20eV
  sig_1200=total(cal_total_un[80:100])-back_1200;20 eV
  sig_1200_no_bkgd=sig_1200;-back_1200 @ 20eV

  ;back_1200=(cal_total_un[203]+cal_total_un[235])*33/2.
  print,'NI 120.0 nm ',4.48e-18*0.826; Malone correction at 100eV



  back_LBH=(cal_total_un[250]+cal_total_un[340])/2./91.;average back per channel x 21 channnels

  signal_LBH=total(cal_total_un[255:335]);20 eV 133.5-`40.0 nm
  sig_LBH_no_bkgd=signal_LBH;- back_LBH
  sig_LBH=signal_LBH- back_LBH; 133.5-140.0 nm

  ;lets get 20 eV 1200 cross section comparing sig 1200 at 100eV
  q_1200_20eV= sig_1200*3.7e-18/890.91; /vsb16.8

  QLBH_NO_BKGD=(Sig_LBH_no_bkgd*q_1200_20eV)/sig_1200_no_bkgd;20eV ; Malone correction for 1335-140.0 nm
  qLBH=(sig_LBH*q_1200_20eV)/sig_1200;20eV ; Malone correction to Ajello 1985

  print,' q_LBH = ',qlbh, ' Q 1331-1406A ', qlbh/.135,' Q total LBH with bkgd'
  print, qlbh/.135,' Q total LBH with bkgd'
  print,' q_LBH_no_bkgd= ',qlbh_no_bkgd, ' Q 1331-1406A no_bkgd', qlbh_no_bkgd/.135,' Q total LBH nobkgd'

  print,' ratio with bkgd(sig_LBH)/sig_1200)=  ',sig_LBH/sig_1200;133.5-140.0 nm
  print,' ratio with no bkgd(sig_LBH_no_bkgd)/sig1200_no_bkgd)=  ',sig_LBH/sig_1200;133.5-140.0 nm\

  ;last way
  print,'final way'
  q_lbh_20eV=sig_lbh*3.7e-18/330.6
  print,' q_LBH _20eV = ',q_lbh_20eV, ' Q 1331-1406A ', q_lbh_20eV/.135,' Q total LBH with bkgd'
  print, q_lbh_20eV/.135,' Q total LBH with bkgd'

  stop
  q156=5.47e-19;avakyan
  q165=13.5e-19;avakyan
  q146=1.34e-19; avakyan
  q148=0.17e-19
  q136=0.38e-19
  q133=5.05e-19
  q132=1.62e-19
  ;q165=13.15e-19*2./5.
  ; q146=(1.93e-19+.24e-19);ajello
  ;print,'qtotal = ',qtot
  ;A=qtot-(q135+q156+q165+q146+q148+q132+q133+q136)
  ;print,'qastate =',qA
  ;print,'qastate 1320-170nm as only v= 0-3 observed'
  ; print,'CI, II lines 1320-1700A  =  ',q156+q165+q146+q148+q132+q133+q136

  ;***********************************
  ;device,/close
  ;set_plot,'win'
  ; stop
  ;*****************************************************************
  ;;sept2019
  ;**************************************************************
  ;lets do 2017 LBH publication fit
  ;*************************************************************
  ;inputs=first step
  ;*****************************************************
  ;**********************
  ang=string("305b)
  tau=fltarr(6)
  tau[0]=1e-6; 2000 value in seconds
  tau[1]=10e-6; 2000 value in seconds
  tau[3]=100.e-6; 2000 value in seconds
  tau[4]=1000.e-6;
  tau[2]=50.e-6
  tau[5]=5000e-6
  ;tau[4]=100e-3
  ;k=8.6171e-5;eV/deg
  ;Temp=300
  ;en=k*Temp;eV mean value of may 2000
  ;en=1.20;eV mean value of may 2000
  ;lets use 1eV for en
  een=.026 ;kT
  en=0.0388;3/2kT
  en_erg=en*1.602e-12;eV to ergs
  kbolt=1.38e-16
  en_boltz=1.5*kbolt*300/1.602e-12
  m=28.*1.67e-24; in grams mass of N2
  vel=sqrt(2.*en_erg/m);cm/s
  travel=fltarr(5)
  travel=vel*35.e-6
  travel_cascade=vel*5000.e-6
  print,'en(eV),vel(cm/s),travel, mean en, travel_cascade',en,vel,travel,en_boltz,travel_cascade
  ;stop
  ;end input section
  los=dblarr(801,6)
  radius=findgen(801);minimum ray height distance to los in cm,max 400mm = 40cm=;this is'a';each point is 0.5 mm
  distancer=fltarr(801,1601);=+/- 40cm = +/- 400 mm =800 points at 0.5 mm per point distance along LOS =p 2nd variable from min radius to los ist var
  glow=dblarr(801,1601,6);volume emission rate  at point distfor each lifetime
  distance=fltarr(801,1601)
  newradius=radius/2.;all DISTANCEs IN 0.5 mm
  ;end declarations
  ;**********************************************************
  ;second step is los integrated intenesitees
  ;************************************8**
  ;;*********************************************************
  del_pee=0.5;integration increment in mm
  ;distance to los goes from 0mm at beam to 500mm at lens but lets go to 200 mm
  pee=findgen(1601)
  pee=pee/2.; define distance grid along los in mm, steps of 0.5 mm
  par=fltarr(6)
  par=tau*vel*10;units of cm x 10mm/cm= units of mm

  ;********************************************************8
  ;********************************************************8
  ;switch to mm
  ;*************************************88
  ; for large chamber
  for k=0,5 do begin
    for i=1,800 do begin;start stepping in radius from 0.5mm to 400 mm so 86 mm is 86*2=i=172
      for j=0,1600 do begin;integrate along pee LOS from -40 to + 40 cm;really j=0 is pee=0to
        distance[i,j]=sqrt( newradius[i]^2+(pee[j]-400)^2)
        ;glow(i,j,k)=del_pee*exp(-distance(i,j)/par(k))/(par(k)*distance(i,j))
        glow[i,j,k]=(del_pee/distance[i,j])*(exp(-distance[i,j]/par[k]))
      endfor
      ;los(i)=glow(i,j)+los(i-1)
      los[i,k]=total(glow[i,*,k]);integrated intesnisty for each value of a from 0.5mm ectc to 40 cm =400mm
    endfor

  endfor
  ;************************************
  ;lets normalize model to data at 25 mm or sig5_muv_bin[80]
  ;stop
  ;***********************************************
  for j=0,5 do begin
    los[*,j]= los[*,j]*iglow_1356_N2_R10_20eV[500]/los[28,j];try 53 as it is +14.5 4/26
    ;print,ygrand[500]=15.0300
  endfor

  ;**************************************************

  los[*,0]=los[*,0]*los[28,2]/los[28,0];normalize to los[5,1]
  los[*,1]=los[*,1]*los[28,2]/los[28,1]
  los[*,2]=los[*,2]*los[28,2]/los[28,2]
  los[*,3]=los[*,3]*los[28,2]/los[28,3]
  los[*,4]=los[*,4]*los[28,2]/los[28,4]
  los[*,5]=los[*,5]*los[28,2]/los[28,5]
  ;for j=0,3 do los[*,j]=los[*,j]*sig3_bin[32]/los[41,j]
  ;stop
  ;****************************************
  ;;step 3
  ;the fit of 1200 line with no cascade just fast allowed
  ;********************************************

  ;we need to fit the file of NI(1200A) with
  ;gaussfit
  ; organized image 1 and image 2 data in distance from e-beam espcially where  they overlap
  ;yy3=y3_1200[0:80]
  ;sig3_yy3=sig3_bin_1200[0:80]
  sig1200=iglow_130_N2_R10_20eV



  yy_1200=ygrand_N2_R10_20eV
  ;

  xxx=yy_1200[250:600];also xxx;yy_1200[34:59]

  xxy=XXX
  yyx=sig1200[250:600];34:59=-27.4 to 27.9mm =sig_1200[34:59]

  result1=Gaussfit(xxy,yyx,aa,CHISQ=chisq,nterms=3,SIGMA=sigma)
  ;lets plot result
  zxa=(xxy-aa[1])/aa[2]
  func1=aa[0]*exp(-zxa^2/2);+Aa[3]+Aa[4]*X+aa[5]*X^2
  psym=-4
  ;stop
  ;******************
  ;plot4 ****1200 fit aand 1200 data
  ;
  ;*******************************************************
  ;wdir_plots='H:\IBM\IBM\MOBI_2019\Round7\N2\20eV\png\'
  win5 = window(window_title='round10 Spectra Hi Pressure 120.0 nm 20eV PSF september 2019',background_color='white',dim=[1400,1200])
  ss1=PLOT(xxy,func1/max(func1),thick=2,xrange=[-20,20],yrange=[-1,1.5],font_size='24',color='black',current=win5,ytitle='Intensity 120.0 nm (arb units)', xtitle='radial distance (mm)', title='NI 120.0nm  20 eV glow pattern for 0.04 eV N!d2!n thermal Energy')
  ss2=plot(/over,yy_1200,sig1200/max(sig1200),   color='red',thick=3)
  ;oplot,-(yy_sort_1200),sig_sort_1200,psym=10,   color=225,thick=3
  bb=string(aa[0])
  cc=string(aa[1])
  dd=string(aa[2])
  tf1 = text(-18,1.3,'A(0)='+string(bb), /data, target=p1,font_size='28')
  tf2 = text(-18,1.1,'A(1)='+string(cc), /data, target=p1,font_size='28')
  tf3 = text(-18,0.9,'A(2)='+string(dd),/data, target=p1,font_size='28')
  win5.save,wdir_plots+'2025_UVIS BB_spectra_N2_ 20 eV_120.0 nm PSF at hi press_12may2025_50us.png'
  ;*********************************************

  ;step5
  ;*********************************************
  ;;;;;;;;;;;;;;;;;;;
  ;lets try con[!c]print,yy_1200[600]voltion of LOS[*,0-5] array with func1=kernel

  grand_func=func1

  grand_func=grand_func/max(grand_func)
  resulter=dblarr(801,6)
  for i=0,5 do begin
    resulter[*,i]=convol(los[*,i],grand_func/total(grand_func),/edge_truncate);,/nan,/center);convoluted model from -400 to 400
    resulter[*,i]=resulter[*,i]*los[28,I]/resulter[28,i]
  endfor
  ;lets normalize result to corresponding LOS at 14 mm n=28

  ;lets opot he results like Greg  did
  ;*************************************************
  ;*************************************
  ;fifth step is LBH lifetimes at 200 mm
  ;*******************************************
  tau1=fltarr(401)
  tau2=fltarr(401)
  ;lbhdat=[sig3_bin[47:80],sig2_bin[8:64]];64 is 199.8333 mm
  lbhdat=iglow_1356_N2_R10_20eV

  reg_radius=ygrand
  ;stop
  tau1=resulter[0:400,2];200 mm
  tau2=resulter[0:400,5]
  ;reg_radius=[y3[47:80],y2[8:64]]

  lbh_reg=congrid(lbhdat[432:1400],401,/interp)
  lbh_rad=congrid(ygrand_n2_R10_20eV[432:1400],401,/interp)
  ;********************************************************************************************************
  ;********************************
  ;plot 10 using y1
  ;*******************************************

  lbhdat1=lbhdat

  reg_radius1=ygrand;80 was 64?
  tau1=resulter[0:400,2];use till 350 mm to get same results as plot7
  tau2=resulter[0:400,5]

  lbh_reg1=congrid(lbhdat1[432:1401],601,/interp)
  lbh_rad1=congrid(ygrand_n2_R10_20eV[432:1401],601,/interp)
  ;stop
  ;********************************


  ;***************************************
  ;start regression stuff
  ;...set the radius region of interest in the fitting procedure
  ;wmin =  900.
  ;wmax = 1100.
  ;wmin =  850.
  rmin =  0.
  rmax = 300.
  ;stop
  ;*************************************************************

  ;*************************************************************
  tau1=resulter[0:600,2];200 mm
  tau2=resulter[0:600,5]
  ;lets create some vectors for jacques
  ibin    = where(lbh_rad1 ge rmin and lbh_rad1 le rmax,nbin)
  ;datafit = data(ibin)         ;...assemble data array;lamda index array data = fltarr(idim=512) & data(*) = datarr(1,*)
  rfit    = lbh_rad1[ibin]             ;...wavelengths
  lbhfit    = lbh_reg1[ibin]        ;...N I and N II lines red line
  tau1fit    = tau1[ibin]        ;...Laboratory spectrum  blue dashed
  tau2fit    = tau2[ibin]
  ;wdir_plots = 'w:\Documents\IBM\MAVEN\LBH bands\greg\v6\plots\'
  save,ibin,lbh_rad1,lbh_reg1,tau1fit,tau2fit,rfit,filename=wdir_plots+'regress_LBH_model_direct_cascade_0_350mm_12may2025_50us.sav'
  ;***********************************
  xtest   = fltarr(2,nbin)
  wgts    = fltarr(nbin) & wgts[*] = 1./(lbhfit[*])^2;1.

  xtest[0,*] = tau1fit
  xtest[1,*] = tau2fit

  ;...Here is the call to the regression
  bestfit    = regress(xtest,lbhfit,wgts,yfit,const,sigma,ftest,r,rmul,chisq,status); what do each of these keywords represent? and can they be changed to improve the fit?
  ;...Now plot the results
  ;****************************************
  ;*****************************************************
  ;***************************fourth step is to fit 120.0 nm line]

  sig_1200=sig1200

  xz=xxy

  yz=yyx
  ;stop
  ;********************************************
  ;need to fit data as point spread fuction
  ;**************************************

  result=Gaussfit(xz,yz,a,CHISQ=chisq,nterms=6,SIGMA=sigma)
  ;lets plot result
  zx=(xz-a[1])/a[2]
  func=a[0]*exp(-zx^2/2)+A[3]+A[4]*Xz+a[5]*Xz^2
  ;************************************************
  ;end 120.0 nm stuff
  ;********************************************************
  ;****************************************

  ;PLOT8  *****1200 data and fit with LBH

  ;***********************************
  PLOT,newradius[1:799],los[1:799,0],thick=2,/ylog,xrange=[-50,200],yrange=[100,200000.],xstyle=1,ystyle=1,xticks=5,xminor=5,charsize=1.5,charthick=3.0,ytitle='counts',xtitle='radial distance mm)',title='LBH bands glow pattern for 0.04 eV thermal Energy'
  oplot,newradius[1:799],resulter[1:799,0],thick=3,linestyle=2,color=0
  xx=[150,190]
  yy=[6500,6500]
  oplot,xx,yy,thick=3,linesty=2
  !linetype=2
  oPLOT,newradius[1:799],los[1:799,1],thick=3,color=50,linesty=0

  oplot,newradius[1:799],resulter[1:799,1],thick=3,color=50,linesty=2
  yy=[5000,5000]
  oplot,xx,yy,color=50,thick=3,linesty=2
  !linetype=1
  ;oplot,newradius(1:110),los(1:110,2)
  oplot,newradius[1:799],los[6:799,2],color=90,thick=3,linesty=0
  oplot,newradius[1:799],resulter[1:799,2],thick=3,color=90,linesty=2
  yy=[4000,4000]
  oplot,xx,yy,thick=3,linesty=2

  !linetype=1
  oplot,newradius[6:799],los[6:799,3], color=150,thick=3,linesty=0
  oplot,newradius[1:799],resulter[1:799,3],thick=3,color=150,linesty=2
  yy=[3200,3200]
  oplot,xx,yy, color=150,thick=3,linesty=2

  !linetype=5
  oplot,newradius[6:799],los[6:799,4], color=215,thick=3,linesty=0
  oplot,newradius[1:799],resulter[1:799,4],thick=3,color=215,linesty=2
  yy=[2400,2400]
  oplot,xx,yy, color=215,thick=3,linesty=2
  !linetype=0
  XYOUTS,80.,6500," LBH Model (1.0 !4l!3s)",charsize=1.25,charthick=3,color=0;-fraction observed is "+strcompress(string( ratio_areas180us )) ,CHARSIZE=1.2,CHARTHICK=3
  XYOUTS,80.,5000," LBH Model (10 !4l!3s)",charsize=1.25,charthick=3,color=50;-fraction observed is "+strcompress(string( ratio_areas120us )),CHARSIZE=1.2,CHARTHICK=3
  XYOUTS,80.,4000," LBH Model (50 !4l!3s)",charsize=1.25,charthick=3, color=90; -fraction observed is "+strcompress(string( ratio_areas60us )),CHARSIZE=1.2,CHARTHICK=3
  XYOUTS,80.,3200," LBH Model (100. !4l!3s)",charsize=1.25,charthick=3,color=150
  XYOUTS,80.,2400," LBH Model (1000. !4l!3s)",charsize=1.25,charthick=3,color=215
  ;********************************************************************************************************
  ;plot9
  xx=[-42,-35]
  !linetype=2
  PLOT,rfit,lbhfit,thick=4,/ylog,xrange=[-50,200],yrange=[20,20000.],psym=10,xstyle=1,ystyle=1,color=0,xticks=5,xminor=5,charsize=1.5,charthick=3.0,ytitle='Intensity (dn)', title='LBH Bands Round 8 High Press. Glow Pattern at 20eV !c Electron Impact Energy and Regression Model for N!d2!n 300K Thermal Energy'
  ;oplot,newradius[1:799],resulter[1:799,0],thick=3,linestyle=0
  yy=[125,125]
  oplot,xx,yy, color=0,thick=5,linesty=2
  !linetype=0
  XYOUTS,-32.,125," LBH Round 8 Hi Press Glow Data",charsize=1.25,charthick=4,color=0
  !linetype=2
  ;oplot,xr,r2[*,2],color=85,thick=4,linesty=2
  oplot,rfit,bestfit[0]*tau1fit,thick=4,color=70,linesty=2
  yy=[82,82]
  oplot,xx,yy,thick=4,linesty=2,color=70
  !linetype=0
  XYOUTS,-32.,82," LBH Direct Model is 62% (35 !4l!3s)",charsize=1.25,charthick=3, color=70
  ;tau2fit=tau2fit+const
  oplot,rfit,bestfit[1]*tau2fit+const, color=130,thick=4,linesty=2
  ;oplot,newradius[1:799],resulter[1:799,4],thick=3,color=175,linesty=0
  yy=[60,60]
  oplot,xx,yy, color=130,thick=5,linesty=2
  !linetype=0
  XYOUTS,-32.,60," LBH Cascade Model is 38% (5000. !4l!3s)",charsize=1.25,charthick=4,color=130

  oplot,rfit,yfit,color=200,thick=9;final fit
  yy=[40,40]
  oplot,xx,yy, color=200,thick=5,linesty=0
  XYOUTS,-32.,40," LBH Best Fit Regression Model ",charsize=1.25,charthick=4,color=200
  xyouts,50,5000, 'LBH 20eV total cross section 1.41x10!u-17!ncm!u2',charsize=1.25,charthick=4,color=200
  xyouts,50,3000, 'LBH 20eV direct cross section 8.74x10!u-18!n cm!u2',charsize=1.25,charthick=4,color=70
  xyouts,50,2000, 'LBH 20eV cascade cross section 5.36x10!u-18!ncm!u2',charsize=1.25,charthick=4,color=130
  ;********************************************************************************************************



  ;******************************************************************************

  set_plot,'win'
  ;stop
  ;***************************************
  ;********************************
  ;lets get areas three ways
  ;*******************************************
  ;method 1-model
  ;****************************
  ;********************************
  ;lets get areas three ways
  ;*******************************************
  ;method 1-model
  ;****************************
  ;stop
  direct_model=total(bestfit[0]*tau1fit[0:*])
  cascade_model=total(bestfit[1]*tau2fit[141:*]+const)
  per_dir=direct_model/(direct_model+cascade_model)
  per_casc=1.-per_dir
  print,'plot 9 direct inflection y2 cascade results from 70 mm of '
  print,'% direct model y2= ',per_dir,   '% cascade model y2 =',per_casc
  ;stop
  ;****************************************************
  ;method 2-data
  ;******************************
  direct_data=total( lbhfit[0:80])
  cascade_data=total( lbhfit[81:*])
  per_dir_data=direct_data/(direct_data+cascade_data)
  per_casc_data=1.-per_dir_data
  print,'% direct y1 inflection 70 mm data= ',per_dir_data,   '% cascade y1 inflection 70mm data =',per_casc_data
  ;****************************************************
  ;stop
  ;
  ;********************************
  ;plot 10 using y1
  ;*******************************************
  ;tau1=fltarr(801)
  ;tau2=fltarr(801)
  lbhdat1=lbhdat
  ;bbb=where (lbhdat1 lt 50,count)
  ;stop
  ; lbhdat1[bbb]=lbhdat1[bbb+3]
  ;stop
  ;lbhrad=[y3,y2]
  reg_radius1=ygrand;80 was 64?
  tau1=resulter[0:700,2];use till 350 mm to get same results as plot7
  tau2=resulter[0:700,5]
  ;result_rad=congrid(reg_radius,401,/interp)

  ;lbh_reg=congrid(lbhdat[432:1400],401,/interp)
  ;lbh_rad=congrid(ygrand_n2_R10_20eV[432:1400],401,/interp)

  lbh_reg1=congrid(lbhdat1[432:2135],701,/interp);1901 is 300 mm
  lbh_rad1=congrid(ygrand_n2_R10_20eV[432:2135],701,/interp)
  ;
  tau1=los[0:700,2];350 mm
  tau2=los[0:700,5]
  ;=====================================================================================================================
  ;start regression stuff
  ;...set the radius region of interest in the fitting procedure
  ;wmin =  900.
  ;wmax = 1100.
  ;wmin =  850.
  rmin =  0.
  rmax = 350.
  ;stop
  ;*************************************************************
  ;lets create some vectors for jacques
  ibin    = where(newradius ge rmin and newradius le rmax,nbin)
  ;datafit = data(ibin)         ;...assemble data array;lamda index array data = fltarr(idim=512) & data(*) = datarr(1,*)
  rfit1   = lbh_rad1[ibin];newradius[ibin]             ;...wavelengths 10nov2019
  lbhfit1    = lbh_reg1[ibin]        ;...N I and N II lines red line
  ;bfit    = betaspect(ibin)        ;...Lyman alpha
  tau1fit    = tau1[ibin]        ;...Laboratory spectrum  blue dashed
  tau2fit    = tau2[ibin]
  ;wdir_plots = 'w:\Documents\IBM\MAVEN\LBH bands\greg\v6\plots\'

  ;***********************************
  xtest   = fltarr(2,nbin)
  ;wgts    = fltarr(nbin) & wgts[*] = 1./(lbhfit1[*])^2;1./(10+lbhfit[*]);
  ;wgts    = fltarr(nbin) & wgts[*] = 1./(lbhfit1[*])^1;1./(10+lbhfit[*]);nov5
  ;measure_errors   = fltarr(nbin) & measure_errors = stddev(lbhfit1[*]);1./(lbhfit1[*])^2
  wgts    = fltarr(nbin) & wgts[*] = 1./(lbhfit1[*])^2;nov10
  xtest[0,*] = tau1fit
  xtest[1,*] = tau2fit

  ;...Here is the call to the regression
  bestfit    = regress(xtest,lbhfit1,wgts,yfit,const,sigma,ftest,r,rmul,chisq,status); what do each of these keywords represent? and can they be changed to improve the fit?
  save,bestfit,ibin,lbh_rad,lbh_reg,tau1fit,tau2fit,rfit,filename=wdir_plots+'regress_LBH_model_direct_cascade_29sept2019_50us_350mm.sav'
  ;********************************************************
  ;*****************************************************
  ;...Now plot the results
  ;****************************************
  ;plot 10
  !mtitle=' '

  set_plot,'ps'
  ;;********************************
  PLOT,rfit1,lbhfit1,thick=3,/ylog,xrange=[-50,350],yrange=[20,20000.],psym=10,xstyle=1,ystyle=1,color=0,xticks=8,xminor=5,charsize=1.5,charthick=3.0,ytitle='Intensity (counts/minute)', xtitle='Radius (mm)',title='#1 LBH Glow Pattern 20eV High press. !c & Regression Model N!d2!n 300K Thermal Energy'
  ;oplot,newradius[1:799],resulter[1:799,0],thick=3,linestyle=0
  yy=[12500,12500]
  xx=[115,130]
  !linetype=0
  oplot,xx,yy, color=0,thick=5
  XYOUTS,132.,12500," LBH Glow Data",charsize=1.25,charthick=4,color=0
  !linetype=2
  ;oplot,xr,r2[*,2],color=85,thick=4,linesty=2
  oplot,rfit1,bestfit[0]*tau1fit,thick=4,color=70,linesty=2
  yy=[9350,9350]
  oplot,xx,yy,thick=4,linesty=2,color=70
  !linetype=0
  XYOUTS,132.,9350," LBH Direct Model  69% (50 !4l!3s)",charsize=1.25,charthick=3, color=70
  ;tau2fit=tau2fit+const
  oplot,rfit1,bestfit[1]*tau2fit+const, color=130,thick=4,linesty=2
  ;oplot,newradius[1:799],resulter[1:799,4],thick=3,color=175,linesty=0
  yy=[7250,7250]
  oplot,xx,yy, color=130,thick=5,linesty=2
  !linetype=0
  print,'plot 10 direct cascade imflection results'
  XYOUTS,132.,7250," LBH Cascade Model 31% (5000. !4l!3s)",charsize=1.25,charthick=4,color=130

  oplot,rfit1,yfit,color=200,thick=9;final fit
  yy=[5500,5500]
  oplot,xx,yy, color=200,thick=5,linesty=0
  XYOUTS,132.,5500," LBH Best Fit Regression Model ",charsize=1.25,charthick=4,color=200
  ;xyouts,110,1000, 'LBH 20eV total cross section 1.41x10!u-17!n cm!u2',charsize=1.25,charthick=4,color=200
  ;xyouts,110,720, 'LBH 20eV direct cross section 8.74x10!u-18!n cm!u2',charsize=1.25,charthick=3, color=20
  ;xyouts,110,500, 'LBH 20eV cascade cross section 5.36x10!u-18!n cm!u2',charsize=1.25,charthick=4,color=130
  ;***********************************
  ;plot 11
  ang=string("305b)
  ;rmin = -0.1
  ;rmax =5.90
  !mtitle=' '
  ;plot11
  !linetype=2
  PLOT,rfit1,lbhfit1,thick=4,/ylog,xrange=[-50,300],yrange=[20,20000.],psym=10,xstyle=1,ystyle=1,color=0,xticks=7,xminor=5,charsize=1.5,charthick=3.0,ytitle='Intensity (dn)', title='#2 LBH Bands Glow Pattern 20eV Electron Impact energy !c and Regression Model N!d2!n 300K Thermal Energy'
  ;oplot,newradius[1:799],resulter[1:799,0],thick=3,linestyle=0
  yy=[2500,2500]
  xx=[260,285]
  oplot,xx,yy, color=0,thick=5,linesty=2
  !linetype=0
  ;XYOUTS,100.,2500," LBH Glow Data",charsize=1.25,charthick=4,color=0
  !linetype=2
  ;oplot,xr,r2[*,2],color=85,thick=4,linesty=2
  oplot,rfit1,bestfit[0]*tau1fit,thick=4,color=20,linesty=2
  yy=[7000,7000]
  oplot,xx,yy,thick=4,linesty=2,color=20
  !linetype=0
  XYOUTS,100.,7000," LBH Direct Model  62% (50 !4l!3s)",charsize=1.25,charthick=3, color=20
  ;tau2fit=tau2fit+const
  oplot,rfit1,bestfit[1]*tau2fit+const, color=130,thick=4,linesty=2
  ;oplot,newradius[1:799],resulter[1:799,4],thick=3,color=175,linesty=0
  yy=[5000,5000]
  oplot,xx,yy, color=130,thick=5,linesty=2
  !linetype=0
  print,'plot 10 direct cascade imflection results'
  XYOUTS,100.,5000," LBH Cascade Model 38% (5000. !4l!3s)",charsize=1.25,charthick=4,color=130

  oplot,rfit1,yfit,color=200,thick=9;final fit
  ;yy=[3500,3500]
  ;oplot,xx,yy, color=200,thick=5,linesty=0
  ; XYOUTS,100.,3500," LBH Best Fit Regression Model ",charsize=1.25,charthick=4,color=200
  yy=[2500,2500]
  xx=[260,285]
  oplot,xx,yy, color=0,thick=5,linesty=2
  !linetype=0
  XYOUTS,100.,2500," LBH Glow Data",charsize=1.25,charthick=4,color=0
  !linetype=2
  ;oplot,xr,r2[*,2],color=85,thick=4,linesty=2
  oplot,rfit1,bestfit[0]*tau1fit,thick=4,color=20,linesty=2
  yy=[7000,7000]
  oplot,xx,yy,thick=4,linesty=2,color=20
  !linetype=0
  XYOUTS,100.,7000," LBH Direct Model  62% (50 !4l!3s)",charsize=1.25,charthick=3, color=20
  ;tau2fit=tau2fit+const
  oplot,rfit1,bestfit[1]*tau2fit+const, color=130,thick=4,linesty=2
  ;oplot,newradius[1:799],resulter[1:799,4],thick=3,color=175,linesty=0
  yy=[5000,5000]
  oplot,xx,yy, color=130,thick=5,linesty=2
  !linetype=0
  print,'plot 10 direct cascade imflection results'
  XYOUTS,100.,5000," LBH Cascade Model 38% (5000. !4l!3s)",charsize=1.25,charthick=4,color=130

  oplot,rfit1,yfit,color=200,thick=9;final fit
  yy=[3500,3500]
  oplot,xx,yy, color=200,thick=5,linesty=0
  XYOUTS,100.,3500," LBH Best Fit Regression Model ",charsize=1.25,charthick=4,color=200
  xyouts,85,1000, 'LBH 20eV total cross section 1.41x10!u-17!n cm!u2',charsize=1.25,charthick=4,color=200
  xyouts,85,720, 'LBH 20eV direct  cross section 8.74x10!u-18!ncm!u2',charsize=1.25,charthick=3, color=20
  xyouts,85,500, 'LBH 20eV cascade cross section 5.36x10!u-18!n cm!u2',charsize=1.25,charthick=4,color=130

  ;***********************************

  ;********************************************
  device,/close
  set_plot,'win'
  ;**************************

  ;********************************
  ;lets get areas three ways
  ;*******************************************
  ;method 1-model
  ;****************************
  ;stop
  direct_model=total(bestfit[0]*tau1fit[0:140]);to 60 mm
  cascade_model=total(bestfit[1]*tau2fit[141:*]+const)
  per_dir=direct_model/(direct_model+cascade_model)
  per_casc=1.-per_dir
  print,'plot 10-11 direct model cascade inflection results at 70 mm'
  print,'% direct= ',per_dir,   '%cascade=',per_casc
  ;**************************************
  ;MAIN result of glow data is this fit of glow data using the regression percentage below labeled ALL
  direct_model_all=total(bestfit[0]*tau1fit[0:*])
  cascade_model_all=total(bestfit[1]*tau2fit[0:*]+const)
  per_dir_all=direct_model_all/(direct_model_all+cascade_model_all)
  per_casc_all=1.-per_dir_all
  print,'plot 10-11 direct model cascade ALL no inflection results'
  print,'% All direct= ',per_dir_all,   '% ALL cascade=',per_casc_all
  ;stop
  ;****************************************************
  ;method 2-data
  ;******************************
  direct_data=total( lbhdat[432:800]);now 70 mm and #432=0
  cascade_data=total( lbhdat[801:*]);was 90 in 2015
  per_dir_data=direct_data/(direct_data+cascade_data)
  per_casc_data=1.-per_dir_data
  print,'plot 10-11 direct cascade data inflection results'
  print,'% direct data= ',per_dir_data,   '% cascade data =',per_casc_data
  ;****************************************************
  ;method 2-best model
  ;******************************
  direct_best=total(lbhfit1[0:140])
  cascade_best=total(lbhfit1[141:*])
  per_dir_best=direct_best/(direct_best+cascade_best)
  per_casc_best=1.-per_dir_best
  print,'plot 10-11 direct cascade Best lbh fit model inflection results'
  print,'% direct lbh fit best= ',per_dir_best,   '% cascade lbh fit best=',per_casc_best
  ;stop
  ;




  ;;*******************************************************
  ;print, 'plot 11 model with y3, y2 and y1 to 400mm normalize at 40mm at inflection'
  answer=total(resulter[*,2])/(total(resulter[*,2])+total(los[*,5]*los[100,2]/los[100,5]))

  ;print,'fraction of y1 data that is inflection direct is',answer
  data_answ=total(total(lbhfit1[0:120]))/(total(lbhfit1))


  ;lets try regression
  ;lets use variables for fitting the LBH data of
  ;newradius 0:399.5 and model uses same scale
  ;newradius[1:799],resulter[1:799,2]
  ;other variable:oplot,xr[900:1400],los[80:400,5]*los[80,2]/los[80,5]
  ;convert old LBH radius to newradius
  ;********************************
  ;lets get areas three ways
  ;*******************************************
  ;method 1-model
  ;****************************
  ;stop
  direct_model=total(bestfit[0]*tau1fit[0:140])
  cascade_model=total(bestfit[1]*tau2fit[141:*]+const)
  per_dir=direct_model/(direct_model+cascade_model)
  per_casc=1.-per_dir
  print,'plot 11 direct cascade imflection results'
  print,'% direct before 70 mm inflection y1= ',per_dir,   '% cascade after 70 mm inflection y1 =',per_casc
  ;stop
  ;****************************************************
  ;method 2-data
  ;******************************
  direct_data=total(total(lbhfit1[0:140]))
  cascade_data=total(total(lbhfit1[141:*]))
  per_dir_data=direct_data/(direct_data+cascade_data)
  per_casc_data=1.-per_dir_data
  print,'% direct inflection y1 data before 70 mm = ',per_dir_data,   '% cascade y1 data after 70 mm =',per_casc_data
  ;****************************************************




  ;************************

  ;********************************************
  ;using the regression fit to glow data we find:
  Q120nm=4.48e-18*0.826; Malone correction
  Qlbh_partial=q120nm*total(cal_total_un_hi_20eV_N2_R10[258:327])/total(cal_total_un_hi_20eV_N2_R10[75:100])
  qlbh_total=qlbh_partial/0.135
  print,'the partial  20 eV Qlbh from 135-140.0 nm =  ', Qlbh_partial
  print,'the total Round 8  20 eV cross section  Qlbh from 126-250 nm 50 us model=  ', Qlbh_total

  print,'the total round10 2019 Qlbh_direct  20 eV cross section from 126-250 nm 50us model =  ', Qlbh_total*.62
  print,'the total  Round 8 2019 Qlbh cascade  20 eV cross section from 126-250 nm 50us model=  ', Qlbh_total*.38

  ;**********************************************
  stop
  ;******************************************************
end