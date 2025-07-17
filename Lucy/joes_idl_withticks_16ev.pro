pro joes_idl_withticks_16EV
  Compile_opt idl2
  loadct,10,/silent
  ;*****************************************************************************;calibrated lab data
  ;added in the MAVEN hifi files to same directory

  print,' comparison of three files from Round10, file 1 info'

  date1='May 14,2025 '
  ;
  file1='N2_16EV_FUV_TEST17_IMAGE1_HIPRESS.idl'
  print, 'file name1',file1
  gas=' N2'

  en=' 16 eV'

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

  fil1= "Z:\round10\NeweGun_round10_after_energy_correction\data_reduction\N2_16EV_FUV_TEST17_IMAGE1_HIPRESS.idl"

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
  ;fil2='Z:\IUVS_Breadboard\_Big_e-gun_RoundVII\data_reduction\N2_16ev_FUV_TEST39_16ev_ROT_+7_IMAGE2_HI_PRESS_4E-5.idl'
  fil2='Z:\round10\NeweGun_round10_after_energy_correction\data_reduction\N2_16EV_FUV_TEST18_IMAGE2_HIPRESS.idl'

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
  ;fil3='Z:\IUVS_Breadboard\_Big_e-gun_RoundVII\data_reduction\N2_16ev_FUV_TEST38_16ev_ROT_+7_IMAGE3_HI_PRESS_4E-5.idl'
  fil3='Z:\round10\NeweGun_round10_after_energy_correction\data_reduction\N2_16EV_FUV_TEST19_IMAGE3_HIPRESS.idl'

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
  ;total stuff
  sig_total=siguncal+siguncal2+siguncal3
  cal_total=fltarr(949)
  cal_total_un=INVSENS_BB*sig_total
  cal_total=cal_total_un/max(smooth(cal_total_un[260:290],1))
  ;stop
  ;plot 14-clibrated-fixed
  ;;plot14-calibrated
  wdir_plots="Z:\MOBI\MOBI_2025\Round10\N2\16EV\save\"
  
  set_plot,'ps'
  filename='fuv_Round10_N2_16EV_FUV_TEST17_18_19_HIPRES_10july2025_vL.ps'
  device,/landscape,/color,filename=wdir_plots+filename
  loadct,39,/silent
  ;************************************************************
  !p.multi=[0]
  !linetype=0
  
  
  
  

  PLOT,WAVEUNCAL[20:940],SMOOTH(CAL_un[20:940],1),color=30,TITLE='P14-fixed BREADBOARD LAB '+ fbase+ '!c  + e('+en+', )  SPECTRUM CALIBRATED & NORMALIZED 135.6 nm',CHARSIZE=1.5,CHARTHICK=3.0,THICK=6,YSTYLE=1,PSYM=10,YRANGE=[-10, 230], YTICKV=[0,50,100,150,200], YTICKS=5, YMINOR=5,XSTYLE=1,XRANGE=[123, 170], XTICKV=[125, 130, 135, 140, 145, 150, 155, 160, 165, 170], XTICKS=9, XMINOR=5, XTITLE='WAVELENGTH (NM)', YTITLE='Calibrated Relative Intensity [arb units]';SPEC * 1000  TO GET RAY FROM KR AND DIVIDE BY 33 TO GET MEAN
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
  xyouts,155,45,'N!D2!N + e(16ev) ',color=0,charthick=5, charsize=1.75
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

  PLOT,waveuncal[20:940],cal[20:940],color=30,TITLE='P15-BB LAB N2 16ev Hi Press Image 1 !c(T17),2(T18),3(T19)  SPECTRUM CALIBRATED & NORMALIZED 135.6 nm',CHARSIZE=1.5,CHARTHICK=3.0,THICK=6,YSTYLE=1,yticks=4,yminor=10,PSYM=10,YRANGE=[-.5,1.5],XSTYLE=1,xrange=[125,170],xticks=9,xminor=10, XTITLE='WAVELENGTH (NM)', YTITLE='Calibrated Relative Intensity [arb units]';SPEC * 1000  TO GET RAY FROM KR AND DIVIDE BY 33 TO GET MEAN
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
  print, 'current output device: ', !d.name
  device,/close
  set_plot,'win'
  print,'i made to to the end'


  ;lets compare 1356 and 1464 ratios
  print, 'ratio image 1 =1464/1356 =    ',max(cal_un[375:420])/max(cal_un[220:260])
  print, 'ratio image 2 =1464/1356 =    ',max(cal2_un[375:420])/max(cal2_un[220:260])
  print, 'ratio image 3 =1464/1356 =    ',max(cal3_un[375:420])/max(cal3_un[220:260])
end