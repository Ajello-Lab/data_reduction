;+
; NAME:
;  upsample
; PURPOSE:   (one line only)
;  Upsample an image given a dithered set of undersampled data
; DESCRIPTION:
; CATEGORY:
;  Image Processing
; CALLING SEQUENCE:
;  upsample,cube,xoff,yoff,type,image
; INPUTS:
;  cube - 3-D array of undersampled images [N,M,F] where the images are
;             all NxM and there are F images.  These images should already
;             have any mean background removed.
;  xoff - X offset of each image.  This probably works best when the offsets
;             are referred to a mean of zero for all offsets but this doesn't
;             have to be strictly true
;  yoff - Y offset of each image.
;  type - Type of upsampling:
;           0 = sqrt(2) upsampling, two dithers required, output image will
;                  end up rotated by 45 degrees (CW) from the input images.
;           1 = 2x2 upsampling, four dithers required
;           2 = 3x3 upsampling, nine dithers required
;           You can provide more dithers than the minimum.
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;  QUIET - Flag, if set suppressses printed output
; OUTPUTS:
;  image - Output image up-sampled by the factor implied by type, always
;             returned as a float.
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  2016/09/13, Written by Marc W. Buie, Southwest Research Institute
;  2017/01/13, MWB, seems to be working now (still lots of debugging output)
;  2018/09/27, MWB, added QUIET keyword
;-
pro upsample,cube,xoff,yoff,type,image, $
   transform=transform,ZOOMFAC=zf,QUIET=quiet

   self='upsample: '
   if badpar(cube,[1,2,3,4,5],3,caller=self+'(cube) ',dimen=sz) then return
   if badpar(xoff,[2,3,4,5],1,caller=self+'(xoff) ') then return
   if badpar(yoff,[2,3,4,5],1,caller=self+'(yoff) ') then return
   if badpar(type,[2,3],0,caller=self+'(type) ') then return
   if badpar(zf,[0,2,3],0,caller=self+'(ZOOMFAC) ',default=8) then return
   if badpar(quiet,[0,1,2,3],0,caller=self+'(QUIET) ',default=0) then return

   if type eq 0 then begin
      if not quiet then print,'SQRT(2) upsample'
      testim=spexpand(cube[*,*,0],/root2)
      szt=size(testim,/dimen)
      sz2=[szt[0],szt[1],sz[2]]
      if not quiet then print,'Win 5, transform after phase shift applied'
      setwin,5,xsize=sz2[0]*zf,ysize=sz2[1]*zf*sz[2]
      if not quiet then print,'Win 8, shifted expanded image'
      setwin,8,xsize=sz2[0]*zf,ysize=sz2[1]*zf*sz[2]
      if not quiet then print,'Win 9, sparse expanded image'
      setwin,9,xsize=sz2[0]*zf,ysize=sz2[1]*zf*sz[2]
      mkxyarr,sz2[0],sz2[1],uarr,varr, $
         x0=sz2[0]/2.0,y0=sz2[1]/2.0,scale=1.0/max(sz2[0:1])
      fcube=complexarr(sz2[0],sz2[1],sz2[2])
      tcube=fltarr(sz2[0],sz2[1],sz2[2])
      scube=fltarr(sz2[0],sz2[1],sz2[2])
      cang=cos(45./!radeg)
      sang=sin(45./!radeg)
      dxp = (xoff*cang-yoff*sang)*sqrt(2.)
      dyp = (xoff*sang+yoff*cang)*sqrt(2.)
      phi1=complexarr(2,sz[2])
      phi2=complexarr(2,sz[2])
      accum=fltarr(sz2[0],sz2[1])
      if not quiet then $
         print,'               xoff', $
               '         yoff', $
               '       xoff(r)', $
               '       yoff(r)'
      for i=0,sz[2]-1 do begin
         if not quiet then print,i,xoff[i],yoff[i],dxp[i],dyp[i]
         t = -2.0*!pi*(uarr*dxp[i]+varr*dyp[i])
         scube[*,*,i] = spexpand(cube[*,*,i],/root2)
         fcube[*,*,i]= $
            fft(spexpand(cube[*,*,i],/root2),/center)*complex(cos(t),sin(t))
         tcube[*,*,i]=real_part(fft(fcube[*,*,i],/inverse,/center))
         accum+=tcube[*,*,i]
setwin,5
tvscl,rebin(abs(fcube[*,*,i]),sz2[0]*zf,sz2[1]*zf,/sample),i
setwin,8
tvscl,rebin(tcube[*,*,i],sz2[0]*zf,sz2[1]*zf,/sample),i
setwin,9
tvscl,rebin(scube[*,*,i],sz2[0]*zf,sz2[1]*zf,/sample),i
         vx = -!pi*dxp[i]
         vy = -!pi*dyp[i]
         phi1[0,i]=complex(1.0,0.0)
         phi1[1,i]=complex(cos(vx+vy),sin(vx+vy))
         vx = -!pi*dxp[i]
         vy =  !pi*dyp[i]
         phi2[0,i]=complex(1.0,0.0)
         phi2[1,i]=complex(cos(vx+vy),sin(vx+vy))
      endfor

;itool,real_part(tcube)
;test=real_part(tcube[*,*,0])+real_part(tcube[*,*,1])
;itool,test

;print,'--phi--'
;print,phi
;print,'-----'
      phi1=0.5*phi1
      phi2=0.5*phi2
      ; upper right-hand quadrant
      hp = transpose(conj(phi1))
      hpt = la_invert(hp ## phi1)
      c_a = hpt ## hp
;print,'--c_a--'
;print,c_a
;print,'-------'
      ; lower right-hand quadrant
      hp = transpose(conj(phi2))
      hpt = la_invert(hp ## phi2)
      c_b = hpt ## hp
setwin,6,xsize=sz2[0]*zf*sz2[2],ysize=sz2[1]*zf
      sumim=complexarr(sz2[0],sz2[1])
;print,sz
;print,sz2
      for i=0,sz[2]-1 do begin
         ur = c_a[i,0]*fcube[sz2[0]/2:*,sz2[1]/2:*,i]
         lr = c_b[i,0]*fcube[sz2[0]/2:*,0:sz2[1]/2-1,i]
         sumim[sz2[0]/2:*,sz2[1]/2:*]     += ur
         sumim[sz2[0]/2:*,0:sz2[1]/2-1]   += lr
tvscl,rebin(abs(sumim),sz2[0]*zf,sz2[1]*zf,/sample),i
      endfor
;      sumim[0:sz2[0]/2-1,*] = $
;         shift(rotate(conj(sumim[sz2[0]/2:sz2[0]-1,*]),2),0,1)
      sumim[1:sz2[0]/2,*] =  $
         shift(rotate(conj(sumim[sz2[0]/2:sz2[0]-1,*]),2),0,1)
      ; lop off the outermost edge of values in transform
      sumim[*,0]=complex(0.,0.)
      sumim[*,-1]=complex(0.,0.)
      sumim[0,*]=complex(0.,0.)
      sumim[-1,*]=complex(0.,0.)
      if not quiet then print,'Win 7, summed transform (power spectrum)'
      setwin,7,xsize=sz2[0]*zf,ysize=sz2[1]*zf
      tvscl,rebin(abs(sumim),sz2[0]*zf,sz2[1]*zf,/sample)
      if not quiet then print,'transform ',minmax(abs(sumim))
      transform=sumim
      result=fft(sumim,/inverse,/center)
      image=real_part(result)

   ; 2x2 case
   endif else if type eq 1 then begin
      sz2=[2,2,1]*sz
setwin,5,xsize=sz[0]*zf,ysize=sz[1]*zf*sz[2]
setwin,8,xsize=sz[0]*zf,ysize=sz[1]*zf*sz[2]
setwin,9,xsize=sz[0]*zf,ysize=sz[1]*zf*sz[2]
      mkxyarr,sz2[0],sz2[1],uarr,varr, $
;         x0=sz2[0]/2.0,y0=sz2[1]/2.0,scale=1.0/max(sz2[0:1])
         x0=sz2[0]/2.0-0.5,y0=sz2[1]/2.0-0.5,scale=1.0/max(sz2[0:1])
      fcube=complexarr(sz2[0],sz2[1],sz2[2])
      tcube=fltarr(sz2[0],sz2[1],sz2[2])
      scube=fltarr(sz2[0],sz2[1],sz2[2])
      phi1=complexarr(4,sz[2])
      phi2=complexarr(4,sz[2])
      accum=fltarr(sz2[0],sz2[1])
      for i=0,sz[2]-1 do begin
         if not quiet then print,'Image',i,xoff[i],yoff[i]
         t = -2.0*!pi*2*(uarr*xoff[i]+varr*yoff[i])
         scube[*,*,i] = spexpand(cube[*,*,i])
         fcube[*,*,i]= $
            fft(spexpand(cube[*,*,i]),/center)*complex(cos(t),sin(t))
         tcube[*,*,i]=real_part(fft(fcube[*,*,i],/inverse,/center))
         accum+=tcube[*,*,i]
setwin,5
tvscl,rebin(abs(fcube[*,*,i]),sz[0]*zf,sz[1]*zf,/sample),i
setwin,8
tvscl,rebin(tcube[*,*,i],sz[0]*zf,sz[1]*zf,/sample),i
setwin,9
tvscl,rebin(scube[*,*,i],sz[0]*zf,sz[1]*zf,/sample),i
         ; multiply by the complex phase: exp[ -2pi K i (x-x0 + y-y0) ]
         ;   i in comment above is sqrt(-1), not the index
         ;   K = 2 for 2x2 up-sampling

         vx = -2.0*!pi*xoff[i] ; factor of 2 here is really K
         vy = -2.0*!pi*yoff[i]
         phi1[0,i]=complex(1.0,0.0)
         phi1[1,i]=complex(cos(vx),sin(vx))
         phi1[2,i]=complex(cos(vy),sin(vy))
         phi1[3,i]=complex(cos(vx+vy),sin(vx+vy))
         vx = -2.0*!pi*xoff[i]
         vy =  2.0*!pi*yoff[i]
         phi2[0,i]=complex(1.0,0.0)
         phi2[1,i]=complex(cos(vx),sin(vx))
         phi2[2,i]=complex(cos(vy),sin(vy))
         phi2[3,i]=complex(cos(vx+vy),sin(vx+vy))
;print,'phi',phi[*,i]
      endfor
;print,'--phi--'
;print,phi
;print,'-----'
;print,conj(phi)

;itool,[[[scube[*,*,3]]],[[float(tcube[*,*,3])]]]

;itool,float(scube)
;itool,real_part(tcube)
;itool,accum
;itool,uarr
;itool,varr

      phi1=0.25*phi1
      phi2=0.25*phi2

      ; upper right-hand quadrant
      hp = transpose(conj(phi1))
;aaa=hp ## phi1
;print,'hp ## phi'
;print,(hp # phi)
;print,aaa
      ;hpt = la_invert((hp # phi),status=status)
      hpt = la_invert(hp ## phi1,status=status)
;print,'hpt'
;print,hpt
;print,'check'
;print,hpt # (hp # phi)
;print,hpt ## aaa
;      hpt = invert((hp # phi),status)
;print,'status c_a',status
      c_a = hpt ## hp

      ; lower right-hand quadrant
      hp = transpose(conj(phi2))
      hpt = la_invert(hp ## phi2,status=status)
;      hpt = invert((hp # phi),status)
;print,'status c_b',status
;print, hpt ## (hp ## phi2)
      c_b = hpt ## hp
;help,hpt,hp,phi,c_b
;print,'--c_a--','--c_b--',format='(22x,a,22x,a)'
;print,c_a
;for i=0,sz[2]-1 do $
;   print,'Image ',strn(i),c_a[i,0],c_b[i,0]
;print,'-------'

setwin,6,xsize=sz2[0]*zf*sz2[2],ysize=sz2[1]*zf

      sumim=complexarr(sz2[0],sz2[1])
;print,'sz ',sz
;print,'sz2',sz2
      for i=0,sz[2]-1 do begin
;         ri = c_a[i,0]*fcube[sz2[0]/2:*,*,i]
;         sumim[sz2[0]/2:*,*] += ri

;         ur = c_a[i,0]*fcube[sz2[0]/2:*,sz2[1]/2:*,i]
;         lr = rotate(ur,7)
;         sumim[sz2[0]/2:*,sz2[1]/2:*]     += ur
;         sumim[sz2[0]/2:*,0:sz2[1]/2-1]   += lr

         ur = c_a[i,0]*fcube[sz2[0]/2:*,sz2[1]/2:*,i]
         lr = c_b[i,0]*fcube[sz2[0]/2:*,0:sz2[1]/2-1,i]
         sumim[sz2[0]/2:*,sz2[1]/2:*]     += ur
         sumim[sz2[0]/2:*,0:sz2[1]/2-1]   += lr

tvscl,rebin(abs(sumim),sz2[0]*zf,sz2[1]*zf,/sample),i
      endfor
;      sumim[1:sz2[0]/2,*] = reverse(conj(sumim[sz2[0]/2:sz2[0]-1,*]),1)
      sumim[1:sz2[0]/2,*] = shift(rotate(conj(sumim[sz2[0]/2:sz2[0]-1,*]),2),0,1)
      ; lop off the outermost edge of values in transform
      sumim[*,0]=complex(0.,0.)
      sumim[*,-1]=complex(0.,0.)
      sumim[0,*]=complex(0.,0.)
      sumim[-1,*]=complex(0.,0.)
      setwin,7,xsize=sz2[0]*zf,ysize=sz2[1]*zf
      tvscl,rebin(abs(sumim),sz2[0]*zf,sz2[1]*zf,/sample)
      if not quiet then print,'transform ',minmax(abs(sumim))
      transform=sumim
      result=fft(sumim,/inverse,/center)
      image=real_part(result)
;help,phi,hp,hpt,c_a,c_b,sumim,result,fcube,image
   endif else begin
      print,self,'Type code ',strn(type),' is not supported.'
   endelse

end
