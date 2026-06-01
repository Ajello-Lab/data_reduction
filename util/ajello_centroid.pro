
function ajello_centroid, x, y, help=help

if (n_elements(x) eq 0) or (n_elements(y) eq 0) or (keyword_set(help)) then begin
  print,'usage: xpos = centroid(x,y)'
  print,'returns: total( float(x) * float(y) ) / total( float(y) )'
  return,-1
endif

;return, total( float(x) * float(y) ) / total( float(y) )

return, total( double(x) * double(y) ) / total( double(y) )

end