;+
; PURPOSE
;  This routine will provide a notional wavelength scale for the GOLD EM
;  
; INPUTS
;  None
;  
; OUTPUTS
;  wl_full: vector of 4096 elements representing the notional wavelength
;   scale in angstroms for the detector with no binning or windowing
;-
pro ajello_lab_gold_wavelength_scale, wl_full

  ;
  ; Wavelength scale from Alan (see forwarded email from Joe Jan 24, 2020)
  ; Lambda (in Angstroms) = 0.1943 * Detector Column + 1187
  ;
  wl_full = 0.1943 * findgen(4096) + 1187
  ;wl = wl_full[ wind[0] : wind[2] ] / 10.
  
end