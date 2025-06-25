
pro ajello_lab_wavelength_def, round_number, wlfuv, wlmuv

wlmuv_flight = 0.1636 * findgen(1024) + 175.6636  ; coefficients were determined by the above linfit
case round_number of
  1: begin
    wlmuv = wlmuv_flight
  end
  2: begin
    ;wlmuv_shift = shift(wlmuv,14)  ; Joe says shifting the data by -14 pixels results in a good wavelength scale for round2
    wlmuv = wlmuv_flight - 14.*0.1636
  end
  3: begin
    wlmuv = wlmuv_flight
  end
  4: begin
    wlmuv = wlmuv_flight
  end
  else: begin
    print, 'ajello_lab_wavelength_def: no round_number defined'
    wlmuv = wlmuv_flight
  end
endcase


return

stop

; I received code from Joe on June 5, 2017
; The following is based on this code.
; He basically reads in the wavelength scale from an old data file created by Alan from Round1
; Joe then shifts this by 14 pixels to adjust for round2

  ;fil2='F:\IBM\IBM\MOBI\MAVEN\_BIG_e-gun\CO2\CO2_30eV_5\MUVSpectralData.dat'
  fil2 = '/Volumes/projects/Phase_Development/MAVEN/IUVS_Data/IUVS_Breadboard/_BIG_e-gun/CO2/CO2_30eV_5/MUVSpectralData.dat'
  nnum=1024
  numbers=intarr(nnum)
  WAVEuncal=dblARR(nnum)
  SIGuncal=dblARR(nnum)
  ;sigcal20=dblarr(NNUM)
  ;stop
  ;******************************************************declarations-uncalibrated spectral data readin
  OpenR, lun, fil2, /Get_Lun
  ;readf,lun,header1
  FOR I=0,nnum-1 DO BEGIN
    READF,lun,As,Bs,cs
    numbers[I]=aS
    waveuncal[I]=bs;for nov 2006 offset based on 30nov 1200A multiplet
    siguncal[I]=cS

  ENDFOR
  siguncal=shift(siguncal,0);958.7A is at 959.7,delta is 0.15A/channel shift -4
  waveuncal=reverse(waveuncal)/10.
  siguncal=reverse(siguncal)

  x = findgen(1024)
  lin = linfit(x,waveuncal)


stop

end