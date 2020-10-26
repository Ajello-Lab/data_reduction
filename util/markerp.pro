pro markerp, plot_handle,x=x,y=y,_extra=_extra
  if keyword_set(y) then begin
    xval = [plot_handle.xrange[0],plot_handle.xrange[1]]
    yval = [y,y]
  endif
  if keyword_set(x) then begin
    xval = [x,x]
    yval = [plot_handle.yrange[0],plot_handle.yrange[1]]
  endif
  plot_marker = plot(xval,yval,/overplot,_extra=_extra)
end
