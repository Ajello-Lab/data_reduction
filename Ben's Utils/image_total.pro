;
; PURPOSE
; Takes in images 1, 2, and 3, and combines then together
;
; INPUTS
; struct_in: Structure containing the data from the three images
; - scale1: x-axis for image 1
; - image1: spectrum for image 1
; - scale2: x-axis for image 2
; - image2: spectrum for image 2
; - scale3: x-axis for image 3
; - image3: spectrum for image 3
;
; OUTPUTS
; struct_out: Structure containing the summation of the three images
; - scale: x-axis scale
; - imagetot: sum of all image spectrums together
;
pro image_total, struct_in, struct_out ; SET UP FOR 2 IMAGES, COMMAS IN PLACE TO UPDATE TO THREE
  compile_opt idl2

  if n_tags(struct_in) eq 6 then begin
    range_start = max([struct_in.scale1[0], struct_in.scale2[0], struct_in.scale3[0]]) ; Add Scale 3
    range_end = min([struct_in.scale1[-1], struct_in.scale2[-1], struct_in.scale3[-1]]) ; Add scale 3

    scale_ndx = where(struct_in.scale1 ge range_start and struct_in.scale1 le range_end)
    scale = struct_in.scale1[scale_ndx]

    imagetot = fltarr(n_elements(scale))
    for i = 0, n_elements(scale) - 1 do begin
      ndx1 = where(struct_in.scale1 eq scale[i])
      ndx2 = where(struct_in.scale2 eq scale[i])
      ndx3 = where(struct_in.scale3 eq scale[i]) ; UNCOMMENT for img3

      if (ndx1 eq -1) then $
        message, 'point not found... (ndx1) ' + string(ndx1)

      if (ndx2 eq -1) then begin ; Add ndx 3 here
        ndx2_expanded = where(struct_in.scale2 lt scale[i] + 0.005 and struct_in.scale2 gt scale[i] - 0.005, count)
        if count ne 1 then $
          message, 'Broader search returned ' + string(count) + ' values ||' + string(ndx1) + string(ndx2) + string(ndx3) + string(i)
        ndx2 = ndx2_expanded
      endif

      if (ndx3 eq -1) then begin ; Add ndx 3 here
        ndx3_expanded = where(struct_in.scale2 lt scale[i] + 0.005 and struct_in.scale2 gt scale[i] - 0.005, count)
        if count ne 1 then $
          message, 'Broader search returned ' + string(count) + ' values ||' + string(ndx1) + string(ndx2) + string(ndx3) + string(i)
        ndx3 = ndx3_expanded
      endif
      imagetot[i] = struct_in.image1[ndx1] + struct_in.image2[ndx2] + struct_in.image3[ndx3] ; UNCOMMENT for img3
    endfor
  endif else begin
    range_start = max([struct_in.scale1[0], struct_in.scale2[0]])
    range_end = min([struct_in.scale1[-1], struct_in.scale2[-1]])

    scale_ndx = where(struct_in.scale1 ge range_start and struct_in.scale1 le range_end)
    scale = struct_in.scale1[scale_ndx]

    imagetot = fltarr(n_elements(scale))
    for i = 0, n_elements(scale) - 1 do begin
      ndx1 = where(struct_in.scale1 eq scale[i])
      ndx2 = where(struct_in.scale2 eq scale[i])

      if (ndx1 eq -1) then $
        message, 'point not found... (ndx1) ' + string(ndx1)

      if (ndx2 eq -1) then begin ; Add ndx 3 here
        ndx2_expanded = where(struct_in.scale2 lt scale[i] + 0.005 and struct_in.scale2 gt scale[i] - 0.005, count)
        if count ne 1 then $
          message, 'Broader search returned ' + string(count) + ' values ||' + string(ndx1) + string(ndx2) + string(ndx3) + string(i)
        ndx2 = ndx2_expanded
      endif

      imagetot[i] = struct_in.image1[ndx1] + struct_in.image2[ndx2]
    endfor
  endelse

  struct_out = {scale: scale, imagetot: imagetot}
end
