;+
; PURPOSE
;  This function plots a shaded region in the x-dimension of a 2D plot
;
; INPUTS
;  plot_handle_in: the IDL plot handle containing the data over which
;   the shaded region will be plottted
;  x1: starting position of shaded region
;  x2: ending position of shaded region
; 
; RETURNS
;  plot handle of shade object
;  
; KEYWORDS
;  All keywords to the plot() function are allowed.
;  
; EXAMPLE
;  x = findgen(100)/100*2.*!pi
;  p1 = plot( x, sin(x) )
;  p2 = plot_shade( p1, !pi/2.-!pi/4, !pi/2.+!pi/4, fill_transparency=70, fill_color='blue' )
;-
function plot_shade, plot_handle_in, x1, x2, _extra=_extra
  ;plot_handle_shade = plot( [x1,x2], [1,1]*p1.yrange[0], /over, fill_level=p1.yrange[1], /fill_background, linestyle=0, fill_transparency=70, fill_color='blue' )
  plot_handle_shade = plot( [x1,x2], [1,1]*plot_handle_in.yrange[0], /over, fill_level=plot_handle_in.yrange[1], /fill_background, _extra=_extra )
  return, plot_handle_shade
end