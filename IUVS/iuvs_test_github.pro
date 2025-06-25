
pro iuvs_test_github

  file = "\\lasp-store\home\lufa5942\Documents\IUVS\N2_100EV_FUV_TEST20_IMAGE1_HIPRESS.idl"
  file = "\\lasp-store\home\lufa5942\Documents\IUVS\N2_100EV_FUV_TEST21_IMAGE2_HIPRESS.idl"
  restore, file, /ver

  im = image( arr, min_value=0, max_value=100 )


  y1 = 150
  y2 = 920
  spec = total( arr[*,y1:y2], 2 )

  p = plot( wl, spec )

  stop


end