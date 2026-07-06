;
; PURPOSE
; fits a poly curve to set of csv data
;
pro fit_curve
  compile_opt idl2

  data_path = '/Users/benjamincondit/Desktop/Plot_combiner_Outputs/Final Curve_Averaged_Values.csv'
  save_path = '/Users/benjamincondit/Desktop/'

  readcol, data_path, x_out, y_out, err_out, delimiter = ',', format = '(F10.2, F10.6, F10.8)', /silent

  ; co = poly_fit(double(x_out), double(y_out), 5, measure_errors = err_out, /double)
  ; fit_curve = co[0] + co[1] * x_out + co[2] * x_out ^ 2 $
  ;   + co[3] * x_out ^ 3 + co[4] * x_out ^ 4 + co[5] * x_out ^ 5 ;+ co[6] * x_out ^ 6

  weights = 1.0d / err_out ^ 2.0d
  A = [0.03d, -0.12d, -100.0d]
  exp_p = curvefit(double(x_out), double(y_out), weights, A, Sigma, function_name = 'exp_poly', /double)
  xvals = double(indgen(70)) + abs(A[2])
  fit_curve = A[0] * (xvals + A[2]) ^ 2.0 * exp(A[1] * (xvals + A[2]))
  print, 'Form a(x + c)^2 * e^ b(x + c): '
  print, 'a = ' + string(A[0]) + '  b = ' + string(A[1]) + '  c = ' + string(A[2])

  win = window(dim = [1440, 600], buffer = 0)
  xr = [115, 185]
  yr = [0, 1.5]
  p1 = errorplot(x_out, y_out, err_out, current = win, color = 'red', font_size = 16, xrange = xr, yrange = yr, $
    name = 'Combined Sensitivities', symbol = 24, sym_filled = 1, sym_size = 0.5, xtitle = 'Wavelength (nm)', font_name = 'times', title = 'IUVS Sensititivity')
  p2 = plot(xvals, fit_curve, symbol = 24, sym_filled = 1, sym_size = 0.5, /over, color = 'blue', name = 'Sensitivities Fit Line')
  win.save, save_path + 'Combined_Sensitivities_Fit.png'
  csv_name = save_path + 'Combined_Sensitivities_Fit.csv'
  fit_struct = {set1: xvals, set2: fit_curve}
  write_csv, csv_name, fit_struct.set1, fit_struct.set2, header = ['fit x', 'fit y']
end
