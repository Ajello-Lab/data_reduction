;+
; NAME:
;    qhyprop
; PURPOSE:   (one line only)
;    Return QHY camera properties
; DESCRIPTION:
;    This tool provides a way to connect header and setting information
;      to more useful fundamental properties of the image data.
; CATEGORY:
;  Miscellaneous
; CALLING SEQUENCE:
;    qhyprop, setting, gain, rdnoise, dynrng, fullwell
; INPUTS:
;    setting : GAIN setting from SharpCap, valid range is 0-400 but is
;                validated.
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;    BIT12 = Return 12-bit gain values instead of scaling to 16-bit
;              both refer to 12-bit data conversions but the default
;              is to bit shift to the 12-bit data by 4 bits making it
;              appear to "fill" a 16-bit value.  This introduces an
;              apparent change in gain due to the bit shift (16x).
;              Set this keyword to avoid the extra factor of 16.
; OUTPUTS:
;    gain     : Camera gain [e-/ADU]
;    rdnoise  : Camera read noise [e-]
;    dynrng   : Camera dynamic range [QHY units]
;    fullwell : Camera full well capacity [e-]
; KEYWORD OUTPUT PARAMETERS:
; CONFIGURATION:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
;    Only the QHY174M-GPS camera is supported at the moment and this will
;      remain the default into the future with other cameras added through
;      the use of keywords as needed.
; PROCEDURE:
;    Camera properties are inferred from plots at:
;    https://www.qhyccd.com/index.php?m=content&c=index&a=show&catid=94&id=46&cut=1.
;    Outputs are linearly interpolated from the camera properties,
;    which are currently read by eye, except for the gain. The full
;    well capacity in particular is not trustworthy when SETTING>250. 
;    If SETTING is input as a scalar then the outputs are scalars; 
;    otherwise, they're vectors with the same length as SETTING.
;
; MODIFICATION HISTORY:
;    Written by Brian Keeney, SwRI, 2021/01/15
;    2022/03/08, BK, Modified to remove dependence on external files
;-
pro qhyprop,setting,gain,rdnoise,dynrng,fullwell,BIT12=bit12

  self='qhyprop: '
  if badpar(setting,[2,3,4,5],[0,1],caller=self+'(setting) ') then return
  if badpar(bit12,[0,1,2,3],0,caller=self+'(BIT12) ',default=0) then return

  qset  = [0.,20.,40.,60.,80.,100.,150.,200.,250.,300.,350.,400.]
  qgain = [7.344,5.9488,4.6768,3.7136,2.9616,2.3536,1.3248,0.744,0.416,$
            0.232,0.128,0.0672]
  qrn   = [5.5,5.1,4.7,4.4,4.2,4.0,3.7,3.4,3.1,2.8,2.5,2.2]
  qdr   = [12.4,12.2,12.0,11.8,11.4,11.3,10.5,9.8,9.1,8.3,7.6,7.0]
  qfw   = [30000.,24500.,19200.,15500.,12200.,10000.,5300.,3400.,1800.,$
           900.,400.,200.]

  gain     = interpol(qgain, qset, setting)
  rdnoise  = interpol(qrn,   qset, setting)
  dynrng   = interpol(qdr,   qset, setting)
  fullwell = interpol(qfw,   qset, setting)

  if bit12 eq 0 then gain /= 16.0
   
end
