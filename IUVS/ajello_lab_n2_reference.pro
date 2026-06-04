;+
; PURPOSE
;  This routine will return a reference N2 LBH emission spectrum that has
;   been convolved with the IUVS FUV line-spread function
;
; INPUTS
;  none
;
; OUTPUTS
;  wave_lbh: wavelength, nm
;  lbh_tot: spectrum, arbitrary units
;
; KEYWORDS
;  show_plots: set to 1 to show some plots
;  file_n2_model: if not executing this file from the GitHub repo,
;   the full path to the LBH model file must be provided
;-
pro ajello_lab_n2_reference, wave_lbh, lbh_tot, $
  show_plots = show_plots, $
  file_n2_model = file_n2_model
  compile_opt idl2

  ;
  ; If no model file is provided, use the one in the GitHub repo
  ;
  ; ****** DO NOT EDIT THE PATH BELOW *********
  ;
  if keyword_set(file_n2_model) eq 0 then begin
    ajello_lab_set_paths, path_base, path_repo
    file_n2_model = path_repo + '/data/N2/n2_lbh_rot_293K.sav'
    ; % RESTORE: Recovering incompatible definition of structure LIST using relaxed structure assignment rules.
    ; % RESTORE: Recovering incompatible definition of structure HASH using relaxed structure assignment rules.
    ; % RESTORE: Restored variable: TROT.
    ; % RESTORE: Restored variable: WAVE.
    ; % RESTORE: Restored variable: LBH.
    ; % RESTORE: Restored variable: LIST.
  endif

  if file_test(file_n2_model) eq 0 then begin
    print, ' '
    print, 'the LBH model file: '
    print, file_n2_model
    print, 'cannot be found'
    print, ' '
    stop
  endif

  ;
  ; restore the N2 model
  ;
  restore, file_n2_model, /relax

  if keyword_set(show_plots) then begin
    ; PLOT SEPERATE v' ***
    win = window(dim = [800, 600])
    p0 = plot(wave[*] / 10.0, lbh[0 : 1799, 0] / lbh[385, 3], xr = [120, 180], name = 'v''=0', $
      yr = [0, 1.5], color = 'black', layout = [1, 1, 1], xtitle = 'Wavelength (nm)', $
      ytitle = ' Relative Intensity of v''  [arb units]', $
      title = ' LBH relative intensity with v''', current = win)
    p1 = plot(wave[*] / 10.0, lbh[0 : 1799, 1] / lbh[385, 3], /over, color = 'red', name = 'v1')
    p2 = plot(wave[*] / 10.0, lbh[0 : 1799, 2] / lbh[385, 3], /over, color = 'blue', name = 'v2')
    p3 = plot(wave[*] / 10.0, lbh[0 : 1799, 3] / lbh[385, 3], /over, color = 'coral', name = 'v3')
    p4 = plot(wave[*] / 10.0, lbh[0 : 1799, 4] / lbh[385, 3], /over, color = 'green', name = 'v4')
    p5 = plot(wave[*] / 10.0, lbh[0 : 1799, 5] / lbh[385, 3], /over, color = 'violet', name = 'v5')
    p6 = plot(wave[*] / 10.0, lbh[0 : 1799, 6] / lbh[385, 3], /over, color = 'orange', name = 'v6')
    leg = legend(target = [p0, p1, p2, p3, p4, p5, p6], position = [177, 1.1], $
      /data, /auto_text_color)
  endif

  ;
  ; retreive IUVS PSF
  ;
  scaled_iuvs_psf_model, wave[0 : 301], psf
  wave_lbh = wave / 10.

  ;
  ; convolve each rotational band by the PSF
  ;
  ; TODO: multiply the high-resolution model by the instrument sensitivity
  ; prior to convolving by the PSF
  ;
  lbh_orig = lbh
  for i = 0, ((size(lbh))[2] - 1) do $
    lbh[*, i] = convol(lbh[*, i], psf)

  lbh_orig_tot = total(lbh_orig, 2)
  lbh_tot = total(lbh, 2) / total(psf)

  if keyword_set(show_plots) then begin
    win = window(dim = [800, 600])
    p1 = plot(wave, lbh_orig_tot, current = win, title = 'LBH model', $
      name = 'original', thick = 2, font_size = 16)
    p2 = plot(wave, lbh_tot, /over, color = 'red', name = 'convolved with PSF', thick = 2)
    leg = legend(target = [p1, p2])

    ; x = wave[0:301]
    ; v = psf
    ; fwhm, x, v, x1out, x2out, fwhm_val, x1in=x1in, x2in=x2in, $
    ; interpval=interpval, fac=fac, method=method
  endif
end
