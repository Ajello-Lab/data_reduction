; PURPOSE
; take in a set of data and normalize it
; INPUT
; to_norm: structure containing the data to be normalized
; norm_basis: structure containing the set of data to be normalized to
; OUTPUT
; to_norm: structure modified to contain the normalized data
pro normalize_to, to_norm, norm_basis
  compile_opt idl2

  ; Normalizes via mean
  ;
  ndx_basis = where(norm_basis.avg_x gt 127.5 and norm_basis.avg_x lt 161, basis_count)
  ndx_norm = where(to_norm.avg_x gt 127.5 and to_norm.avg_x lt 161, norm_count)
  norm_factor = mean(norm_basis.avg_y[ndx_basis]) / mean(to_norm.avg_y[ndx_norm])
  to_norm.avg_y *= norm_factor
  to_norm.comb_err *= norm_factor

  ; Normalizes via interpolation
  ;
  ; ndx_norm = where(to_norm.avg_x gt 145 and to_norm.avg_x lt 155)
  ; norm_xs = norm_basis.avg_x[ndx_norm]
  ; interp_basis = interpol(norm_basis.avg_y, norm_basis.avg_x, norm_xs)

  ; to_norm.avg_y *= mean(interp_basis / to_norm.avg_y[ndx_norm])
end
