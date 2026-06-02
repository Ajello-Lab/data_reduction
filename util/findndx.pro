; finds index of nearest value inside an array
function findndx, arr, val
  temp = min( abs(arr-val) )
  ndx = !c
  return, ndx
end

