pro markerp, plot_handle,x=x,y=y,_extra=_extra
  if isa(y) then begin
    xval = [plot_handle.xrange[0],plot_handle.xrange[1]]
    yval = [y,y]    
  endif
  if isa(x) then begin
    xval = [x,x]
    yval = [plot_handle.yrange[0],plot_handle.yrange[1]]
  endif
  if isa(xval) and isa(yval) then $
    plot_marker = plot(xval,yval,/overplot,_extra=_extra)
end
