
pro ajello_lab_calibrate_examples

file = '/Users/holsclaw/MAVEN/Ajello_lab/calibration/2017fuv_calibration.sav'
restore,file, /ver
;% RESTORE: Restored variable: LAMBDA_FLIGHT.
;% RESTORE: Restored variable: SENS_BB.
;% RESTORE: Restored variable: INVSENS_BB.
;% RESTORE: Restored variable: SENS_IUVS_1024_MARS_FLIGHT.
;% RESTORE: Restored variable: FINAL_WAVE_2015.
;% RESTORE: Restored variable: FINAL_SENS_2015.

;LAMBDA_FLIGHT               FLOAT     = Array[949]
;SENS_BB                     DOUBLE    = Array[949]
;INVSENS_BB                  DOUBLE    = Array[949]
;SENS_IUVS_1024_MARS_FLIGHT  DOUBLE    = Array[949]
;FINAL_WAVE_2015             DOUBLE    = Array[1024]
;FINAL_SENS_2015             DOUBLE    = Array[1024]

win = window(dim=[800,600])
p1 = plot( LAMBDA_FLIGHT, SENS_BB, name='SENS_BB', current=win, thick=2, $
  xtitle='wavelength (nm)', font_size=16, title=file_basename(file) )
p2 = plot( LAMBDA_FLIGHT, SENS_IUVS_1024_MARS_FLIGHT, /over, color='red', name='SENS_IUVS_1024_MARS_FLIGHT', thick=2 )
ndx = where( FINAL_WAVE_2015 ne 0. )
p3 = plot( FINAL_WAVE_2015[ndx], FINAL_SENS_2015[ndx], /over, color='blue', name='FINAL_SENS_2015', thick=2 )
leg = legend(target=[p1,p2,p3])


file = '/Users/holsclaw/MAVEN/Ajello_lab/calibration/fuv_hifi_data.sav'
restore,file,/ver
;% RESTORE: Restored variable: WFIT.
;% RESTORE: Restored variable: FITVEC.
;% RESTORE: Restored variable: PARAMSALL.
;% RESTORE: Restored variable: SIGMALL.
;% RESTORE: Restored variable: BASELINE.
;% RESTORE: Restored variable: CHISQALL.
;% RESTORE: Restored variable: WAVENM.
;% RESTORE: Restored variable: FUV_AIRGLOW_IMG.
;% RESTORE: Restored variable: FUV_AIRGLOW_IMG_ERR.
;% RESTORE: Restored variable: DN_CONVERSION.
;% RESTORE: Restored variable: ALT_BIN.
;help, wfit, fitvec, paramsall, sigmall, baseline, chisqall, wavenm, FUV_AIRGLOW_IMG, FUV_AIRGLOW_IMG_ERR, DN_CONVERSION, ALT_BIN
;WFIT            DOUBLE    = Array[196]
;FITVEC          FLOAT     = Array[14, 196]
;PARAMSALL       FLOAT     = Array[14, 80]
;SIGMALL         FLOAT     = Array[14, 80]
;BASELINE        FLOAT     = Array[80]
;CHISQALL        FLOAT     = Array[80]
;WAVENM          DOUBLE    = Array[1024]
;FUV_AIRGLOW_IMG DOUBLE    = Array[1024, 80]
;FUV_AIRGLOW_IMG_ERR
;DOUBLE    = Array[1024, 80]
;DN_CONVERSION   FLOAT     = Array[1024]
;ALT_BIN         FLOAT     = Array[80]

p1 = plot( wavenm, DN_CONVERSION )

p1 = plot( wfit, fitvec[0,*] )
p2 = plot( wfit, fitvec[1,*], /over )
 



file = '/Users/holsclaw/Ajello-Lab/data_reduction/IUVS/IUVSbreadboardSensitivity_FUV.sav'
restore,file,/ver
;% RESTORE: Restored variable: FINAL_WAVE.
;% RESTORE: Restored variable: FINAL_INVSENS.
;% RESTORE: Restored variable: FINAL_SENS.
;help, FINAL_WAVE, FINAL_INVSENS, FINAL_SENS
;FINAL_WAVE      DOUBLE    = Array[1020]
;FINAL_INVSENS   DOUBLE    = Array[1020]
;FINAL_SENS      DOUBLE    = Array[1020]
p1 = plot( FINAL_WAVE, FINAL_SENS, title=file_basename(file) )

file = '/Users/holsclaw/Ajello-Lab/data_reduction/IUVS/IUVSFlightSensitivity.sav'
restore,file,/ver
;% RESTORE: Restored variable: DESCRIPTION.
;% RESTORE: Restored variable: MUV_WAVE.
;% RESTORE: Restored variable: FUV_WAVE.
;% RESTORE: Restored variable: MUV_SENS.
;% RESTORE: Restored variable: FUV_SENS.
;% RESTORE: Restored variable: DESCRIPTION.
;% RESTORE: Restored variable: MODEL.
;help, DESCRIPTION, MUV_WAVE, FUV_WAVE, MUV_SENS, FUV_SENS, DESCRIPTION, MODEL
;DESCRIPTION     STRING    = 'muv_wave is wavelength scale in angstroms for the MUV scale. muv_s'...
;MUV_WAVE        FLOAT     = Array[101]
;FUV_WAVE        FLOAT     = Array[101]
;MUV_SENS        DOUBLE    = Array[101]
;FUV_SENS        DOUBLE    = Array[101]
;DESCRIPTION     STRING    = 'muv_wave is wavelength scale in angstroms for the MUV scale. muv_s'...
;MODEL           INT       =        2
;IDL> print, DESCRIPTION, format='(A)'
;muv_wave is wavelength scale in angstroms for the MUV scale. muv_sens is the 
;sensitivity at each wavelength in arbitrary units, after dark is subtracted off. 
;Same thing for fuv_wave and fuv_sens. Model is IUVS calibration version number 
;(2) indicating that this is based on in-flight data, including 7nm peak shift 
;found in October 2016
p1 = plot( FUV_WAVE, FUV_SENS )
p2 = plot( MUV_WAVE, MUV_SENS, /over )


restore,'/Users/holsclaw/Downloads/2025-07-17_joe/fuv_22sept2017_calibration.save',/ver
;% RESTORE: Portable (XDR) SAVE/RESTORE file.
;% RESTORE: Save file written by jajello@WINL3276, Sun Sep 24 08:11:25 2017.
;% RESTORE: IDL version 8.5 (Win32, x86_64).
;% RESTORE: Restored variable: LAMBDA_FLIGHT.
;% RESTORE: Restored variable: SENS_BB.
;% RESTORE: Restored variable: INVSENS_BB.
;% RESTORE: Restored variable: SENS_IUVS_1024_MARS_FLIGHT.
;% RESTORE: Restored variable: FINAL_WAVE_2015.
;% RESTORE: Restored variable: FINAL_SENS_2015.

p1 = plot( LAMBDA_FLIGHT, INVSENS_BB )
p2 = plot( LAMBDA_FLIGHT, 1./SENS_BB, /over, color='red' )
stop

end