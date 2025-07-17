;function resd, wl, pra
;  print,wl
;  ndx_138 = where( wl gt 1383.0-10.0 and wl lt 1388.0+10.0 )
;  aa=slbh2(pra[0],wl,population_in=pra[1:*])
;  model=total(aa,2)
;  modeln=model/max(model[ndx_138])
;  return, (modeln-data)/err
;end
pro sens1d, wave, sensl1c

  waveqt = [110.,115.,120., 125.,130.,135.4,138.3,149.3,155.5,161.2,180. ,200.]
  qtwave = [  0.,.002, .02, .035,.049,.0469,.0423,.0298,.0206,.0154,.0075,.001]
  qt     = interpol(qtwave,waveqt,wave)

  inttime = 7.2                                      ; integration time in seconds for L1C superpixel
  pixang = 0.00736828                                ; angular size of instrument pixel
  superpixang = 0.2                                  ; angular size of L1C superpixel
  etendue = 1.48e-6                                  ; A*Omega = pixheight*slitwith/f^2, per wem
  cspr = qt * etendue * 1.e6 / (4*!pi)               ; counts per second per pixel per Rayleigh
  sensl1c = cspr * inttime * superpixang / pixang    ; counts per Rayleigh for L1C superpixels
end
pro iuvs_psf_model,x,a,f
  f = A[0] * exp( -0.5 * (x-A[1])^2 / A[2]^2 )
  ;+ $
  ; A[3] / (1.+(x-A[1])^2 / A[4]^2) + $
  ;A[5]
end
FUNCTION MYFUNCT, p,dp, X=x, Y=y, ERR=err
;p010=[600,350,0.0701729,0.130241,0.163069,0.163164,0.163252,0.114495,0.0736068]
;pp=[p010[2:3],p[4],p010[5:6],p[7],p010[8,*]]
;p3040=[250,350,0.0696830,0.132016,0.165681,0.166555,0.161623,0.109873,0.0725692]
;p8090=[250,350,0.0679843,0.125710,0.159799,0.170227,0.161871,0.116585,0.0758244]
v=[0.0710558,     0.133515,     0.162337,     0.170232,     0.159273,     0.109679,    0.0719076]; vib pop of may 14-16 13-17 UT 60-70sza
  aa=slbh2(p[0],x,population_in=v)
 shp=size(aa)
  model=fltarr(shp[1])
  A=[1, 0, 0.80,       22.9622]
  ;apply psf and sum over different vibrational spectra
  for w=0, shp[1]-1 do begin;all wl
    for n=0,shp[2]-1 do begin
      a[0]=aa[w,n]
      a[1]=x[w]
      iuvs_psf_model,x+0,a,f
      ;print, size(f)
      model=model+f
    endfor
  endfor
  ;model=total(aa,2) 
  ;model=modl*gauss_smooth(reform(modl),50,/Edge_Truncate)
  iidx=where((x gt 1490.5) and (x lt 1498.0))
  model[iidx]=0.0D
  ndx_138 = where( x gt 1383.0-10.0 and x lt 1388.0+10.0 )
  sens1d,(x/10.0),sens
  modeln=model*sens*p[1]
  ;*(max(y[ndx_138])/max(model[ndx_138]))
  cgplot,x,modeln,color='red'
  cgplot,x,y,/over
;  model = F(x, p) ;; Model function
if n_params() GT 1 then begin
  ; Create derivative and compute derivative array
  requested = dp ; Save original value of DP
  dp = make_array(n_elements(x), n_elements(p), value=x[0]*0)
  ; Compute derivative if requested by caller
  for i = 0, n_elements(p)-1 do if requested(i) NE 0 then $
    dp(*,i) = FGRAD(x, p, i)
endif
  ;resid = (y - modeln)/(err);; Residual calculation (for example)
  ;print,resid
  return,modeln
end
;;;;;
FUNCTION MYFUNCT1, pp,dp, X=x, Y=y, ERR=err,T=t
  ;p010=[600,350,0.0701729,0.130241,0.163069,0.163164,0.163252,0.114495,0.0736068]
  ;pp=[p010[2:3],p[4],p010[5:6],p[7],p010[8,*]]
  ;p3040=[250,350,0.0696830,0.132016,0.165681,0.166555,0.161623,0.109873,0.0725692]
  ;p8090=[250,350,0.0679843,0.125710,0.159799,0.170227,0.161871,0.116585,0.0758244]
  aa=slbh2(t,x,population_in=pp[0:-2])
  ;,population_in=p010[2:*])
  shp=size(aa)
  model=fltarr(shp[1])
  A=[1, 0, 0.80,       22.9622]
  ;apply psf and sum over different vibrational spectra
  for w=0, shp[1]-1 do begin;all wl
    for n=0,shp[2]-1 do begin
      a[0]=aa[w,n]
      a[1]=x[w]
      iuvs_psf_model,x+0.57,a,f
      ;print, size(f)
      model=model+f
    endfor
  endfor
  ;model=total(aa,2)
  ;model=modl*gauss_smooth(reform(modl),50,/Edge_Truncate)
  iidx=where((x gt 1490.5) and (x lt 1498.0))
  model[iidx]=0.0D
  ndx_138 = where( x gt 1383.0-10.0 and x lt 1388.0+10.0 )
  sens1d,(x/10.0),sens
  modeln=model*sens*pp[-1]
  ;*(max(y[ndx_138])/max(model[ndx_138]))
  cgplot,x,modeln,color='red'
  cgplot,x,y,/over
  ;  model = F(x, p) ;; Model function
  if n_params() GT 1 then begin
    ; Create derivative and compute derivative array
    requested = dp ; Save original value of DP
    dp = make_array(n_elements(x), n_elements(p), value=x[0]*0)
    ; Compute derivative if requested by caller
    for i = 0, n_elements(p)-1 do if requested(i) NE 0 then $
      dp(*,i) = FGRAD(x, p, i)
  endif
  resid = (y - modeln)/(err);; Residual calculation (for example)
  ;print,resid
  return,resid
end
;;;;
FUNCTION MYFUNCTR, pp,dp, X=x, Y=y, ERR=err,Vr=vr,H=h
;p010=[600,350,0.0701729,0.130241,0.163069,0.163164,0.163252,0.114495,0.0736068]
;pp=[p010[2:3],p[4],p010[5:6],p[7],p010[8,*]]
;p3040=[250,350,0.0696830,0.132016,0.165681,0.166555,0.161623,0.109873,0.0725692]
;p8090=[250,350,0.0679843,0.125710,0.159799,0.170227,0.161871,0.116585,0.0758244]
aa=slbh2(pp[0],x+0.57,population_in=vr)
;,population_in=p010[2:*])
shp=size(aa)
model=fltarr(shp[1])
A=[1, 0, 0.80,       22.9622]
;apply psf and sum over different vibrational spectra
for w=0, shp[1]-1 do begin;all wl
  for n=0,shp[2]-1 do begin
    a[0]=aa[w,n]
    a[1]=x[w]
    iuvs_psf_model,x,a,f
    ;print, size(f)
    model=model+f
  endfor
endfor
;model=total(aa,2)
;model=modl*gauss_smooth(reform(modl),50,/Edge_Truncate)
iidx=where((x gt 1490.5) and (x lt 1498.0))
model[iidx]=0.0D
ndx_138 = where( x gt 1383.0-10.0 and x lt 1388.0+10.0 )
sens1d,(x/10.0),sens
modeln=model*sens*pp[1]
;*(max(y[ndx_138])/max(model[ndx_138]))
cgplot,x,modeln,color='red'
cgplot,x,y,/over
;  model = F(x, p) ;; Model function
if n_params() GT 1 then begin
  ; Create derivative and compute derivative array
  requested = dp ; Save original value of DP
  dp = make_array(n_elements(x), n_elements(p), value=x[0]*0)
  ; Compute derivative if requested by caller
  for i = 0, n_elements(p)-1 do if requested(i) NE 0 then $
    dp(*,i) = FGRAD(x, p, i)
endif
resid = (y - modeln)/(err);; Residual calculation (for example)
;print,resid
return,resid
end
;pro gold_fit_lm
;;;;;;
;pathh='C:\Users\saar3247\Documents\gold_spectra_diff_sza_w_err1_4days_10_12.txt'
;pathh='C:\Users\saar3247\Documents\gold_spectra_diff_sza_ju30ju2_mean_sm.txt'
;pathh='C:\Users\saar3247\Documents\gold_spectra_diff_sza_oct31_nov4_east_sqsm.txt'
;pathh='C:\Users\saar3247\Documents\gold_spectra_diff_sza_oct9_mean.txt'
;pathh='C:\Users\saar3247\Documents\gold_spectra_disk_lat60_3by3_day130_136.txt'
;pathh='C:\Users\saar3247\Documents\gold_spectra_disk_lat60_3by3_day130_136.txt'
pathh='C:\Users\saar3247\Documents\gold_spectra_may14may15_count_space_sqsm.txt'
;pathh='C:\Users\saar3247\Documents\gold_spectra_may14may15_count_space_sqsm_13_17.txt'
dat=read_ascii(pathh)
rdat=dat.FIELD001
;;; Read all data from GOLD averaged over time and latitudes at different tangent altitudes ;;;;;;;;;;;
ini=17
fin=55
wl1=rdat[*,0] ;wavelengths
;ndx_138 = where( wl1 gt 138.3-0.6 and wl1 lt 138.3+1.9 )
ndx_138 = where( wl1 gt 153.0-0.7 and wl1 lt 153.0+1.4)
wl=wl1[ndx_138] ;wavelengths
;;;
g01=rdat[*,1] ;; perhaps need to change names but this is earth 0-100 km
g01e=(rdat[*,2]);; uncertainity 0-100
g0_1=g01[ndx_138] ;; perhaps need to change names but this is earth 0-100 km
g0_1e=g01e[ndx_138];; uncertainity 0-100

;;;
;g12=rdat[*,2];; earth 100-200 km
;g12e=(rdat[*,6]) ;;  uncertainity 100-200
;g1_2=g12[ndx_138];; earth 100-200 km
;g1_2e=g12e[ndx_138] ;;  uncertainity 100-200
;
;;g1_2e=(rdat[1:*,6]) ;;  uncertainity 100-200
;g23=rdat[*,3];; earth  200-300 km
;g23e=(rdat[*,7]) ;; uncertainity 200-300
;g2_3=g23[ndx_138];; earth  200-300 km
;g2_3e=g23e[ndx_138] ;; uncertainity 200-300
;
;;g2_3e=rdat[1:*,7]
;g34=rdat[*,4];; earth 300-400 km
;g34e=(rdat[*,8]) ;; uncertainity 300-400
;g3_4=g34[ndx_138];; earth 300-400 km
;g3_4e=g34e[ndx_138] ;; uncertainity 300-400
;
;;g3_4e=rdat[1:*,8]
;;;;;
;g1n=g0_1/max(g0_1[ndx_138])
;g1ne=g0_1e/max(g0_1[ndx_138]) ;normalized to 138 peak
;;;;
;g2n=g1_2/max(g1_2[ndx_138])
;g2ne=g1_2e/max(g1_2[ndx_138])
;;;;
;g3n=g2_3/max(g2_3[ndx_138])
;g3ne=g2_3e/max(g2_3[ndx_138])
;;/max(g2_3[ndx_138]);
;;;;
;g4n=g3_4/max(g3_4[ndx_138])
;g4ne=g3_4e/max(g3_4[ndx_138])
;;/max(g3_4[ndx_138]);
iidx=where((wl1 gt 149.05) and (wl1 lt 149.8))
;;;;
g01[iidx]=0.0
;g12[iidx]=0.0
;g23[iidx]=0.0
;g34[iidx]=0.0
;g4ne[iidx]=0.0D
;;; for the band structures;;;
;nmax = 40
;nv = 7                ;a-state vibration levels
;nvv = 20              ;X-state vibration levels
;slbh_enlev,nmax,nv,nvv,at,ag_vj,af_vj,xt,xg_vj,xf_vj,bv,bvv
;;
;; Read in the Franck-Condon factors, compute the band orgins,
;; and generate the band transition probabilities for mag dipole
;; and elec quadr transitions
;;
;slbh_fc,at,ag_vj,xt,xg_vj,nu_vv,q_vv,ad_vv,aq_vv ;initial fack condon factors from theory
T=600.0 ; initial guess for temperature in K
p0=[T,1000.0]
;p0=[T,350,.043491185,0.011578861,0.0017042322,0.18256165,0.15942593,0.12141883,0.082964793]
wlp=wl*10
wlp1=wl1*10
fa1={X:wlp,Y:g0_1,ERR:g0_1e}
;R = AMOEBA(1.0e-5,Function_name='myfunct', SCALE=1.0e2, P0 = p0, FUNCTION_VALUE=fa1)
p1=mpfit('MYFUNCT', p0, functargs=fa1,XTOL=1e-30,GTOL=1e-30,FTOL=1e-30,STATUS=status,NFEV=10000,PERROR=peerror1,BESTNORM=bestnorm1,dof=dof1) 
perror1 = PeERROR1 * SQRT(BESTNORM1 / DOF1)   ; scaled uncertainties
p01=[.043491185,0.11578861,0.17042322,0.18256165,0.15942593,0.12141883,0.082964793,p1[1]]
fa11={X:wlp1,Y:g01,ERR:g01e,T:p1[0]}
p11=mpfit('MYFUNCT1', p01, functargs=fa11,XTOL=1e-20,GTOL=1e-20,FTOL=1e-20,STATUS=status,NFEV=1000,PERROR=peerror11,BESTNORM=bestnorm11,dof=dof11)
perror11 = PeERROR11 * SQRT(BESTNORM11 / DOF11)   ; scaled uncertainties
;;
;fa2={X:wlp,Y:g1_2,ERR:g1_2e}
;p2=mpfit('MYFUNCT', p0, functargs=fa2,XTOL=1e-10,GTOL=1e-10,FTOL=1e-10,STATUS=status,NFEV=500,PERROR=peerror2,BESTNORM=bestnorm2,dof=dof2)
;perror2 = PeERROR2 * SQRT(BESTNORM2 / DOF2)   ; scaled uncertainties
;;;
;p01=[.043491185,0.11578861,0.17042322,0.18256165,0.15942593,0.12141883,0.082964793,p2[1]]
;fa22={X:wlp1,Y:g12,ERR:g12e,T:p2[0]}
;p22=mpfit('MYFUNCT1', p01, functargs=fa22,XTOL=1e-20,GTOL=1e-20,FTOL=1e-20,STATUS=status,NFEV=1000,PERROR=peerror22,BESTNORM=bestnorm22,dof=dof22)
;perror22 = PeERROR22 * SQRT(BESTNORM22 / DOF22)   ; scaled uncertainties
;;;
;fa3={X:wlp,Y:g2_3,ERR:g2_3e}
;p3=mpfit('MYFUNCT', p0, functargs=fa3,XTOL=1e-10,GTOL=1e-10,FTOL=1e-10,STATUS=status,NFEV=500,PERROR=peerror3,BESTNORM=bestnorm3,dof=dof3)
;perror3 = PeERROR3 * SQRT(BESTNORM3 / DOF3)   ; scaled uncertainties
;;
;p01=[.043491185,0.11578861,0.17042322,0.18256165,0.15942593,0.12141883,0.082964793,p3[1]]
;fa33={X:wlp1,Y:g23,ERR:g23e,T:p3[0]}
;p33=mpfit('MYFUNCT1', p01, functargs=fa33,XTOL=1e-20,GTOL=1e-20,FTOL=1e-20,STATUS=status,NFEV=1000,PERROR=peerror33,BESTNORM=bestnorm33,dof=dof33)
;perror33 = PeERROR33 * SQRT(BESTNORM33 / DOF33)   ; scaled uncertainties
;;;
;fa4={X:wlp,Y:g3_4,ERR:g3_4e}
;p4=mpfit('MYFUNCT', p0, functargs=fa4,XTOL=1e-10,GTOL=1e-10,FTOL=1e-10,STATUS=status,NFEV=500,PERROR=peerror4,BESTNORM=bestnorm4,dof=dof4)
;perror4 = PeERROR1 * SQRT(BESTNORM4 / DOF4)   ; scaled uncertainties
;;;
;p01=[.043491185,0.11578861,0.17042322,0.18256165,0.15942593,0.12141883,0.082964793,p4[1]]
;fa44={X:wlp1,Y:g34,ERR:g34e,T:p4[0],H:p4[1]}
;p44=mpfit('MYFUNCT1', p01, functargs=fa44,XTOL=1e-20,GTOL=1e-20,FTOL=1e-20,STATUS=status,NFEV=1000,PERROR=peerror44,BESTNORM=bestnorm44,dof=dof44)
;perror44 = PeERROR44 * SQRT(BESTNORM44 / DOF44)   ; scaled uncertainties
;;;;;;;;;;;;;;;;;;;;;;
;;           Plots    ;;;;;;;;;;;;;;;;;;;;;;;;;;;
cgplot,0.878*p11[0:-2]/total(p11[0:-2]),color='red',psym=-16,ERR_YHigh=0.878*perror11[0:-2]/total(p11[0:-2]),ERR_YLow=0.878*perror11/total(p11[0:-2]),XRANGE=[-1,7]
;cgplot,0.878*p22[0:-2]/total(p22[0:-2]),color='blue',psym=-16,ERR_YHigh=0.878*perror22[0:-2]/total(p22[0:-2]),ERR_YLow=0.878*perror22/total(p22[0:-2]),/over
;cgplot,0.878*p33[0:-2]/total(p33[0:-2]),color='green',psym=-16,ERR_YHigh=0.878*perror33[0:-2]/total(p33[0:-2]),ERR_YLow=0.878*peerror33/total(p33[0:-2]),/over
;cgplot,0.878*p44[0:-2]/total(p44[0:-2]),color='blu7',psym=-16,ERR_YHigh=0.878*peerror44[0:-2]/total(p44[0:-2]),ERR_YLow=0.878*peerror44/total(p44[0:-2]),/over
cgplot,p01[0:-2],/over
cglegend,SymColors=['red', 'blue','green','blu7','black'], PSyms=[-16,-16,-16,-16,-16], Symsize=1.5, Location=[0.725, 0.8],Titles=['SZA 0-10','SZA 30-40','SZA 60-70','SZA 80-90','Theory']
;;;coeff=LMFIT(wlp,g1n,pra,MEASURE_ERRORS=g1en, /DOUBLE, FITA = fitp,Function_name='modl')
;;params= MPFIT(
cgwindow,'cgplot',0.878*p11[0:-2]/total(p11[0:-2]),color='red',psym=-16,ERR_YHigh=0.878*perror11[0:-2]/total(p11[0:-2]),ERR_YLow=0.878*perror11[0:-2]/total(p11[0:-2]),Xtitle='a-state vibrational',ytitle="Observed Franck-Condon Factor qv'0",XRANGE=[-0.2,6.2],YRANGE=[0,0.2], WMULTI=[3]
;cgwindow,'cgplot',0.878*p22[0:-2]/total(p22[0:-2]),color='blue',psym=-16,ERR_YHigh=0.878*perror22[0:-2]/total(p22[0:-2]),ERR_YLow=0.878*perror22[0:-2]/total(p22[0:-2]),/over,/AddCmd
;cgwindow,'cgplot',0.878*p33[0:-2]/total(p33[0:-2]),color='green',psym=-16,ERR_YHigh=0.878*perror33[0:-2]/total(p33[0:-2]),ERR_YLow=0.878*peerror33[0:-2]/total(p33[0:-2]),/over,/AddCmd
;cgwindow,'cgplot',0.878*p44[0:-2]/total(p44[0:-2]),color='blu7',psym=-16,ERR_YHigh=0.878*peerror44[0:-2]/total(p44[0:-2]),ERR_YLow=0.878*peerror44[0:-2]/total(p44[0:-2]),/over,/AddCmd
cgwindow,'cgplot',p01[0:-2],/over,/AddCmd
cgwindow,'cglegend',SymColors=['red','black'], PSyms=[-16,-16], Symsize=1.5, Location=[0.725, 0.9],Titles=['SZA 0-10','SZA 30-40','SZA 60-70','SZA 80-90','Theory'],/AddCmd
;cgControl, Output=pathh.substring(0,-5)+'_sza.png', IM_Width=600
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;print,'Synthetic',p00[0],0.878*p00[2:*]/total(p00[2:*])
;print,'            ',peerror00[0],0.878*peerror00[2:*]/total(p00[2:*])
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Now retreiving temperature by switching the vibrational population;;;;;

;;;;
;pr1=[p4[0],p4[1]]; initial guess for temperature
;;v=0.878*p11[0:-2]/total(p11[0:-2])
;;v=[0.0699723,0.131177,     0.162596,     0.169735,     0.159663,     0.111449,    0.0734068]; vib pop of may 14-16 13-17 UT 30-40sza
;v=[0.0703519,     0.131231,     0.160807,     0.170687,     0.159877,     0.111188,    0.0738580]; vib pop of may 14-16 13-17 UT 60-70sza
;;v=[0.0694045,     0.130196,     0.163736,     0.165979,     0.161777,     0.111457,    0.0754509]; vib pop of june 30-jul216 14-16 UT 30-40sza
;;v=p01[0:-2]
;far4={X:wlp1,Y:g34,ERR:g34e,vr:v,H:p4[1]}
;pr41=mpfit('MYFUNCTR', pr1, functargs=far4,XTOL=1e-20,GTOL=1e-20,FTOL=1e-20,STATUS=status,NFEV=1000,PERROR=peerror41,BESTNORM=bestnorm41,dof=dof41)
;perror41 = PeERROR41 * SQRT(BESTNORM41 / DOF41)   ; scaled uncertainties
;;;;;;;;;;
;;v=0.878*p11/total(p11)
;pr1=[p3[0],p3[1]]; initial guess for temperature
;far3={X:wlp1,Y:g23,ERR:g23e,vr:v,H:p3[1]}
;pr31=mpfit('MYFUNCTR', pr1, functargs=far3,XTOL=1e-20,GTOL=1e-20,FTOL=1e-20,STATUS=status,NFEV=1000,PERROR=peerror31,BESTNORM=bestnorm31,dof=dof31)
;perror31 = PeERROR31 * SQRT(BESTNORM31 / DOF31)   ; scaled uncertainties
;;;
;pr1=[p2[0],p2[1]]; initial guess for temperature
;far2={X:wlp1,Y:g12,ERR:g12e,vr:v,H:p2[1]}
;pr21=mpfit('MYFUNCTR', pr1, functargs=far2,XTOL=1e-20,GTOL=1e-20,FTOL=1e-20,STATUS=status,NFEV=1000,PERROR=peerror21,BESTNORM=bestnorm21,dof=dof21)
;perror21 = PeERROR21 * SQRT(BESTNORM21 / DOF21)   ; scaled uncertainties
;;;
;pr1=[p1[0],p1[1]]; initial guess for temperature
;far1={X:wlp1,Y:g01,ERR:g01e,vr:v,H:p1[1]}
;pr11=mpfit('MYFUNCTR', pr1, functargs=far1,XTOL=1e-20,GTOL=1e-20,FTOL=1e-20,STATUS=status,NFEV=1000,PERROR=peerror111,BESTNORM=bestnorm111,dof=dof111)
;perror111 = PeERROR111 * SQRT(BESTNORM111 / DOF111)   ; scaled uncertainties
;;;;;;;;;;;;;;;;;;;;;;
print,'For 0-10 SZA',p1[0],0.878*p11[0:-2]/total(p11[0:-2])
print,'            ',perror1[0],0.878*perror11[0:-2]/total(p11[0:-2])
;;
;print,'For 30-40 SZA',p2[0],pr21[0],0.878*p22[0:-2]/total(p22[0:-2])
;print,'             ',perror2,perror21[0],0.878*perror22[0:-2]/total(p22[0:-2])
;;;
;print,'For 60-70 SZA',p3[0],pr31[0],0.878*p33[0:-2]/total(p33[0:-2])
;print,'             ',perror3[0],perror31[0],0.878*perror33[0:-2]/total(p33[0:-2])
;;;;
;print,'For 80-90 SZA',p4[0],pr41[0],0.878*p44[0:-2]/total(p44[0:-2])
;print,'             ',perror4[0],perror41[0],0.878*perror44[0:-2]/total(p44[0:-2])
end