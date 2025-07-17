; shortened graph with tics
 pro maven_N2_graph_tics
  compile_opt idl2

  ;============== PostScript Output ==============
  set_plot, 'ps'
  device, /landscape, /color, filename='Z:\MOBI\MOBI_2025\Round10\N2\20eV\save\plot_spectrum_with_ticks.ps'
  loadct, 0, /silent

  ;============== Restore Data ==============
  restore, 'D:\SSD_I-drive\MAVEN\data_flight\fuv_sonal\fuv_hifi_data.sav'
  restore, 'Z:\round10\NeweGun_round10_after_energy_correction\data_reduction\N2_20EV_FUV_TEST13_IMAGE1_HIPRESS.idl'
  restore, 'Z:\round10\NeweGun_round10_after_energy_correction\data_reduction\N2_20EV_FUV_TEST14_IMAGE2_HIPRESS.idl'
  restore, 'Z:\round10\NeweGun_round10_after_energy_correction\data_reduction\N2_20EV_FUV_TEST15_IMAGE3_HIPRESS.idl'
  restore, 'D:\SSD_I-drive\MAVEN\data_flight\calibration\model\2017\fuv_22sept2017_calibration.save'

  ;============== Build Calibrated Spectra ==============
  y1 = 130 & y2 = 940
  spec_mean1 = mean(arr[*, y1:y2], dimension=2, /nan)
  waveuncal = shift(wl, 69)
  waveuncal = shift(waveuncal[75:1023], +9)
  siguncal = spec_mean1[75:1023]
  back1 = total(siguncal[900:930])/31
  siguncal = siguncal - back1
  cal_un = INVSENS_BB * siguncal
  cal = cal_un / max(smooth(cal_un[260:290], 1))

  spec_mean2 = mean(arr2[*, y1:y2], dimension=2, /nan)
  siguncal2 = spec_mean2[75:1023]
  back2 = total(siguncal2[900:930])/31
  siguncal2 = siguncal2 - back2
  cal2_un = INVSENS_BB * siguncal2
  cal2 = cal2_un / max(smooth(cal2_un[260:290], 1))

  spec_mean3 = mean(arr3[*, y1:y2], dimension=2, /nan)
  siguncal3 = spec_mean3[75:1023]
  back3 = total(siguncal3[900:930])/31
  siguncal3 = siguncal3 - back3
  cal3_un = INVSENS_BB * siguncal3
  cal3 = cal3_un / max(smooth(cal3_un[330:390], 1))

  sig_total = siguncal + siguncal2 + siguncal3
  cal_total_un = INVSENS_BB * sig_total
  cal_total = cal_total_un / max(smooth(cal_total_un[260:290], 1))

  ;============== Plot P14-Fixed ==============
  PLOT, waveuncal[20:940], smooth(cal_un[20:940], 1), color=30, $
        TITLE='P14-fixed BREADBOARD LAB N2 20eV CALIBRATED & NORMALIZED 135.6 nm', $
        CHARSIZE=1.5, CHARTHICK=3.0, THICK=6, YSTYLE=1, PSYM=10, $
        YRANGE=[-10,180], XSTYLE=1, xrange=[125,190], $
        xticks=9, xminor=10, XTITLE='WAVELENGTH (NM)', $
        YTITLE='Calibrated Relative Intensity [arb units]'

  oplot, waveuncal[20:940], smooth(cal2_un[20:940], 1), color=230, thick=5, linestyle=1
  oplot, waveuncal[20:940], smooth(cal3_un[20:940], 1), color=130, thick=5, linestyle=2
  oplot, waveuncal[20:940], smooth(cal_total_un[20:940], 1), color=70, thick=5, linestyle=3

  ;============== Legend ==============
  xx = [148, 154]
  yy = [95, 95]
  oplot, xx, yy, thick=5, color=30
  oplot, xx, yy-6, thick=5, color=130, linestyle=2
  oplot, xx, yy-12, thick=5, color=230, linestyle=1
  oplot, xx, yy-18, thick=5, color=70, linestyle=3

  xyouts, 155, 95,    'IUVS BB Image 1',     color=30, charthick=3, charsize=1.25
  xyouts, 155, 89,    'IUVS BB Image 2',     color=230, charthick=5, charsize=1.25
  xyouts, 155, 83,    'IUVS BB Image 3',     color=130, charthick=5, charsize=1.25
  xyouts, 155, 77,    'IUVS BB Image total', color=70, charthick=5, charsize=1.25
  xyouts, 155, 45,    'N!D2!N + e(20eV)',     color=0, charthick=5, charsize=1.75

  ;============== Vibrational Ladder ==============
  tick_color = 250
  label_x_offset = 1.5

  ; Define helper for ruler
  for v=0,6 do begin
  case v of
    0: begin
      wave_line = [145.0, 150.1, 155.5, 161.2, 167.2]
      yval = 115
    end
    1: begin
      wave_line = [141.6, 146.4, 151.5, 157.0, 162.7, 168.8]
      yval = 130
    end
    2: begin
      wave_line = [138.4, 143.0, 147.9, 153.0, 158.5, 164.2]
      yval = 145
    end
    3: begin
      wave_line = [135.4, 136.9, 138.3, 139.7, 141.0, 142.3, 143.6, 144.9, 146.2, 147.4]
      yval = 160
    end
    4: begin
      wave_line = [132.5, 134.1, 135.6, 137.0, 138.4, 139.8, 141.2, 142.5, 143.8, 145.1, 146.4]
      yval = 175
    end
    5: begin
      wave_line = [129.9, 131.5, 133.0, 134.4, 135.8, 137.2, 138.5, 139.9, 141.2, 142.5, 143.8, 145.1]
      yval = 190
    end
    6: begin
      wave_line = [127.6, 129.3, 130.8, 132.3, 133.7, 135.1, 136.5, 137.8, 139.2, 140.5, 141.8]
      yval = 205
    end
  endcase

  xline = [wave_line[0], wave_line[n_elements(wave_line)-1]]
  plots, xline, [yval, yval], color=tick_color, thick=2
  for i=0, n_elements(wave_line)-1 do plots, [wave_line[i], wave_line[i]], [yval-5, yval], color=tick_color
  xyouts, xline[1]+label_x_offset, yval, "v'=" + strtrim(v, 2), charsize=1.1, color=tick_color
endfor


  ;============== Finalize ==============
  device, /close
  set_plot, 'win'
  print, 'Saved: plot_spectrum_with_ticks.ps'

end
