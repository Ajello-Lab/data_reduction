; ADD in this section of code for tic marks on existing graph (copy+paste)
; Note: may not need all 7 

pro ignore

  ;  START TICS  *******************************
  
  ; top black line
  xyouts, 140, 215, 'N!d2!n LBH a-X', color=0, charthick=5, charsize=1.25
  oplot, [168,168], [205,210], thick=4, color=0
  oplot, [128,128], [205,210], thick=4, color=0
  oplot, [128,168], [210,210], thick=2, color=0

  ; tic spacing
  wave_line0 = [145.0, 150.1, 155.5,161.2,167.2]  ; v'lines v''ticks
  wave_brt0 = [0.391, 0.389, 0.170,0.043]   ;v'lines v''ticks
  xdim0     = n_elements(wave_line0)

  wave_line1 = [141.6, 146.4, 151.5, 157.0,162.7,168.8]
  wave_brt1 = [0.524, 0.016, 0.136, 0.203,0.094]
  xdim1     = n_elements(wave_line1)

  wave_line2 = [138.4,143.0,147.9,153.0,158.5,164.2]
  xdim2     = n_elements(wave_line2)

  wave_line3 = [135.4,139.8,144.4,149.3,154.5,160.0,165.8]
  xdim3     = n_elements(wave_line3)

  wave_line4 = [132.5,136.8,141.2,145.9,150.8,156.0,161.6,167.4]
  xdim4     = n_elements(wave_line4)

  wave_line5 = [129.9,133.9,138.2,142.7,147.4,152.3,157.6,163.1,169.0]
  xdim5     = n_elements(wave_line5)

  wave_line6 = [127.3,131.2,135.3,139.6,144.1,148.9,153.9,159.2,164.8]
  xdim6     = n_elements(wave_line6)

  ; horizontal v' lines
  xline0=[145.0,167.2]
  yline0=[130,130]

  xline1=[141.6,168.8]
  yline1=[140,140]

  xline2=[138.4,164.2]
  yline2=[150,150]

  xline3=[135.4,165.8]
  yline3=[160,160]

  xline4=[132.5,167.4]
  yline4=[170,170]

  xline5=[129.9,169.0]
  yline5=[180,180]

  xline6=[127.3,164.8]
  yline6=[190,190]

  ; tic heights
  y0tik0 = 125
  y0tik1 = 130

  y1tik0 = 135
  y1tik1 = 140

  y2tik0 = 145
  y2tik1 = 150

  y3tik0 = 155
  y3tik1 = 160

  y4tik0 = 165
  y4tik1 = 170

  y5tik0 = 175
  y5tik1 = 180

  y6tik0 = 185
  y6tik1 = 190

  ;***
  wmin = 125.
  wmax = 169.
  oplot,xline0,yline0,color=220,thick=3
  oplot,xline1,yline1,color=220,thick=3
  oplot,xline2,yline2,color=220,thick=3
  oplot,xline3,yline3,color=220,thick=3
  oplot,xline4,yline4,color=220,thick=3
  oplot,xline5,yline5,color=220,thick=3
  oplot,xline6,yline6,color=220,thick=3

  ; loops
  for ii=1,xdim0 do begin
    if (wave_line0[ii-1] gt wmin and wave_line0[ii-1] lt wmax) then begin
      oplot,[wave_line0[ii-1],wave_line0[ii-1]],[y0tik0,y0tik1],thick=3,color=220
    endif
  endfor

  for ii=1,xdim1 do begin
    if (wave_line1[ii-1] gt wmin and wave_line1[ii-1] lt wmax) then begin
      oplot,[wave_line1[ii-1],wave_line1[ii-1]],[y1tik0,y1tik1],thick=3,color=220
    endif
  endfor

  for ii=1,xdim2 do begin
    if (wave_line2[ii-1] gt wmin and wave_line2[ii-1] lt wmax) then begin
      oplot,[wave_line2[ii-1],wave_line2[ii-1]],[y2tik0,y2tik1],thick=3,color=220
    endif
  endfor

  for ii=1,xdim3 do begin
    if (wave_line3[ii-1] gt wmin and wave_line3[ii-1] lt wmax) then begin
      oplot,[wave_line3[ii-1],wave_line3[ii-1]],[y3tik0,y3tik1],thick=3,color=220
    endif
  endfor

  for ii=1,xdim4 do begin
    if (wave_line4[ii-1] gt wmin and wave_line4[ii-1] lt wmax) then begin
      oplot,[wave_line4[ii-1],wave_line4[ii-1]],[y4tik0,y4tik1],thick=3,color=220
    endif
  endfor

  for ii=1,xdim5 do begin
    if (wave_line5[ii-1] gt wmin and wave_line5[ii-1] lt wmax) then begin
      oplot,[wave_line5[ii-1],wave_line5[ii-1]],[y5tik0,y5tik1],thick=3,color=220
    endif
  endfor

  for ii=1,xdim6 do begin
    if (wave_line6[ii-1] gt wmin and wave_line6[ii-1] lt wmax) then begin
      oplot,[wave_line6[ii-1],wave_line6[ii-1]],[y6tik0,y6tik1],thick=3,color=220
    endif
  endfor


  ; labels
  ; v"
  xyouts,145,120,'0',color=220,charthick=3,charsize=1.1
  xyouts,150.1,120,'1',color=220,charthick=3,charsize=1.1
  xyouts,155.5,120,'2',color=220,charthick=3,charsize=1.1
  xyouts,161.2,120,'3',color=220,charthick=3,charsize=1.1
  xyouts,167.2,120,'4=v"',color=220,charthick=3,charsize=1.1
  ; v'
  xyouts,142.0,125," v'=0",color=220,charthick=3,charsize=1.1
  xyouts,138.6,135," v'=1",color=220,charthick=3,charsize=1.1
  xyouts,135.4,145," v'=2",color=220,charthick=3,charsize=1.1
  xyouts,132.4,155," v'=3",color=220,charthick=3,charsize=1.1
  xyouts,129.5,165," v'=4",color=220,charthick=3,charsize=1.1
  xyouts,126.9,175," v'=5",color=220,charthick=3,charsize=1.1
  xyouts,124.3,185," v'=6",color=220,charthick=3,charsize=1.1

  ; END TICS  ***************************