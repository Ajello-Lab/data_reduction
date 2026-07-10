;
; PURPOSE
; Takes in the raw data and scale and aligns it to the nm scale
;
; INPUTS
; arr_list: list of data tables to work through
;
; OUTPUTS
; structs_out: list of structures of aligned data
;
pro scale_and_align_n2, arr_list, structs_out, scale_factor = scale_factor
  compile_opt idl2

  ; retrieve wavelength scale for the rotated image
  ajello_lab_pixel_scale_rot, wlfuv, wlmuv, yfuv, ymuv

  structs_out = list()
  for i = 0, n_elements(arr_list) - 1 do begin
    cur_arr = arr_list[i]

    spat = total(cur_arr, 1)
    y0 = where(spat eq max(spat))
    dimensions = size(cur_arr, /dimensions)
    yw = 100
    y1 = y0 - yw
    y2 = y0 + yw
    y1 = y1 > 0
    y2 = y2 < (dimensions[1] - 1)
    spec = total(cur_arr[*, y1 : y2], 2)
    ; spec -= min(spec)
    if not keyword_set(scale_factor) then $
      scale_factor = 1 / max(spec)
    spec *= scale_factor

    wl_spec = wlfuv - wlfuv[findndx(spec, max(spec))] + 135.4

    wl_ndx = where(wl_spec ge 115 and wl_spec le 190)
    wl_spec = wl_spec[wl_ndx]
    spec = spec[wl_ndx]

    ; Remove floor values
    sorted = spec[sort(spec)]
    ; base_est = sorted[long(0.4 * (n_elements(spec)) - 1)]
    base_est = median(spec, 300)
    spec -= base_est

    ; Apply filter
    ; spec = gauss_smooth(spec, 1)

    temp_struct = {wl_spec: wl_spec, spec: spec, scale_factor: scale_factor}
    structs_out.add, temp_struct
  endfor
end
