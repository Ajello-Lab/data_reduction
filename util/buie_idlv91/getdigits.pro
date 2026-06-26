;+
; NAME:
;   getdigits
; PURPOSE:   (one line only)
;   Decode a single image with an IOTA-VTI time code
; DESCRIPTION:
;   Scan image for IOTA-VTI time code information and decode into a
;     a machine-readable form.  This can handle IOTA-VTI output only and
;     only in either 720x480 or 640x480 pixel formats.
; CATEGORY:
;  Video data processing
; CALLING SEQUENCE:
;  getdigits,image,odd,even,otime,onum,etime,enum
; INPUTS:
;  image - 3 x N x M byte image to decode
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;  DEBUG - Flag, if set will generate copious output and graphics.
; OUTPUTS:
;  odd   - String array with the one character decoded from each cell
;             from the odd field.
;  even  - String array with the one character decoded from each cell
;             from the even field.
;  otime - String with the odd field time, HH:MM:SS.ssss, empty if decode
;             was not successful.
;  onum  - Long integer with the odd field number
;  etime - String with the even field time, HH:MM:SS.ssss, empty if decode
;             was not successful.
;  enum  - Long integer with the even field number
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
;  Image must be 3x720x480 or 3x640x480.  Only plane 0 is read.
;
;  Requires access to two files:
;    digits_640_480.fits and digits_720_480.fits
;  These files must be somewhere in your IDL_PATH but usual reside in the
;    same directory as my IDL library.
; PROCEDURE:
;  A hueristic procedure is used to determine the row location of the
;   time code data.  The x location of the cells where characters are
;   found is hard coded.  A row-summed version of the image shows a
;   characteristic three-peak shape.  The middle peak is used as the center
;   of the digits written to the video.  The position is required to be
;   in a small range near the bottom of the image.  The results are not
;   validated in any way.  The return is an array of characters and if nothing
;   was found, the character is a empty string.
;
;  The time codes are read from the even and odd fields of the images as well
;   as the field counter.
;
; MODIFICATION HISTORY:
;  2016/03/08, Written by Marc W. Buie, Southwest Research Institute
;  2016/03/25, MWB, added DEBUG, XOFF, and YOFF keywords
;  2016/09/01, MWB, added support for 2-d images and improved reliability
;-
pro getdigits_onecell,im,x0,y0,xdw,ydw,digits,val, $
       YOFF=yoff,SUB=sub,METRIC=metric

   val=''
   metric=0

   self='getdigits_onecell: '
   if badpar(yoff,[0,2,3],0,caller=self+'(YOFF) ',default=0) then return

   ; extract from the image the cell of the digit to convert
   subarr,im,x0-xdw,x0+xdw,y0-ydw+yoff,y0+ydw+yoff,sub
;   sub=sub-min(sub)
   npixels=(xdw*2+1)*(ydw*2+1)

   ; Run the detection
   raw=total(sub)/float(npixels)
;print,total(sub),raw
   if raw gt 25 then begin
      diff=lonarr(10)
      dd1=fft(float(sub),1)
      for i=0,9 do begin
;         dd2=fft(float(trimrank(digits[*,*,i])),1)
;         dd3=dd1-dd2
;         dd4=fft(dd3,0)
;         dd4=abs(fft(dd3,0))
;         z=where(dd4/max(dd4) lt 0.5,count)
;         if count ne 0 then dd4[z]=0.0
;         diff[i]=total(abs(dd4))
         ;cdiff=long(sub)-long(digits[*,*,i]-min(digits[*,*,i]))
         cdiff=abs(long(sub)-long(trimrank(digits[*,*,i])))
         z=where(cdiff/255.0 lt 0.5,count)
         if count ne 0 then cdiff[z]=0
         diff[i]=total(abs(cdiff))
      endfor
      diff=float(diff)/max(diff)
;print,diff
      metric=min(diff)
      z=where(diff eq metric)
      val=strn(z[0])
   endif

end

pro getdigits,image,odd,even,otime,onum,etime,enum, $
       DEBUG=debug,XOFF=xoff,YOFF=yoff

   self='getdigits: '
   if badpar(image,[1,2,3,4,5],[2,3],caller=self+'(image) ',rank=image_rank) then return
   if badpar(debug,[0,1,2,3],0,caller=self+'(DEBUG) ',default=0) then return
   if badpar(xoff,[0,2,3],0,caller=self+'(XOFF) ',default=0) then return
   if badpar(yoff,[0,2,3],0,caller=self+'(YOFF) ',default=0) then return

   ; default (bad) values in case of bailout
   otime=''
   etime=''
   onum=0
   enum=0

   if image_rank eq 3 then begin
      sz=size(image,/dimen)
      nx=sz[1]
      ny=sz[2]
      im=trimrank(image[0,*,*])
   endif else begin
      sz=size(image,/dimen)
      nx=sz[0]
      ny=sz[1]
      im=image
   endelse
   if debug then print,'Image size is ',strn(nx),' by ',strn(ny)
   o=indgen(ny/2)*2
   e=indgen(ny/2)*2+1
   imo=im[*,o]
   ime=im[*,e]

   if debug then begin
      setwin,0,xsize=nx,ysize=ny
      tv,im
      setwin,1,xsize=nx,ysize=ny/2
      tv,imo
      setwin,2,xsize=nx,ysize=ny/2
      tv,ime
   endif

   levelcut=80

   z=where(imo lt levelcut,count)
   if count ne 0 then imo[z]=0
   z=where(ime lt levelcut,count)
   if count ne 0 then ime[z]=0

   if nx eq 720 then begin
      fnraw='digits_720_480.fits'
      if debug then print,'Loading templates from ',fnraw
      file=find_with_def(fnraw,!path)
      if file eq '' then begin
         print,'File ',fnraw,' not found in IDL_PATH'
         return
      endif
      digits=readfits(file)
      x0=[88,112, 160,184, 232,256, $
          304,328,352,376, $
          424,448,472,496, $
          544,568,592,616,640,664]
      y0=16
      xdw=10
      ydw=8
   endif else if nx eq 640 then begin
      fnraw='digits_640_480.fits'
      if debug then print,'Loading templates from ',fnraw
      file=find_with_def(fnraw,!path)
      if file eq '' then begin
         print,'File ',fnraw,' not found in IDL_PATH'
         return
      endif
      digits=readfits(file)
      x0=[73,95,139,160,204,226, $
          269,291,313,335, $
          378,400,422,444, $
          488,509,531,553,575,597]
      y0=16
      xdw=9
      ydw=8

      ; 0 -> 0 shift by definition
      ; 1 -> 0 shift
      digits[*,*,2] = shift(digits[*,*,2],-1,0)
      ; 3 -> 0 shift
      ; 4 -> 0 shift
      digits[*,*,5] = shift(digits[*,*,5],-1,0)
      digits[*,*,6] = shift(digits[*,*,6],-1,0)
      ; 7 -> 0 shift
      ; 8 -> 0 shift
      ; 9 -> 0 shift

   endif else begin
      if debug then print,'Illegal image size for decoding'
      return
   endelse

   z=where(digits lt levelcut,count)
   if count ne 0 then digits[z]=0

   x0=x0+xoff

   if debug then print,'template guides y0=',strn(y0),', xdw=',strn(xdw), $
                       ', ydw=',strn(ydw),'  offset ',strn(xoff),',',strn(yoff)

   ; Do a check to see where the timing data live on image, this works
   ;  over a limited range
   dy=indgen(36)-8
   ndy=n_elements(dy)
   metric=fltarr(ndy)
   for i=0,ndy-1 do begin
      ; look for strip with numbers
      strip=long(imo[*,y0+dy[i]])-long(imo[*,y0+dy[i]-ydw])
      z=where(strip gt 200,count)
      metric[i]=count
   endfor
   xidx=lclxtrem(metric,4,/maxima,/point_order)
   z=where(metric[xidx] gt 100,count)
   if count eq 3 then dyval=dy[xidx[z[1]]] else dyval=0
   if debug then print,'dyval set to ',strn(dyval)
   y0=y0+dyval+yoff

   if debug then begin
      setwin,3
      plot,dy,metric,psym=-4
      oplot,dy[xidx[z]],metric[xidx[z]],psym=4,color='0000ff'xl
   endif

   ; Check first two digits with small adjustment to y position
   dy=[-1,0,1]
   ndy=n_elements(dy)
   ckval0=fltarr(ndy)
   ckval1=fltarr(ndy)
   for i=0,ndy-1 do begin
      getdigits_onecell,imo,x0[0],y0+dy[i],xdw,ydw,digits,val,metric=check
      ckval0[i]=check
      getdigits_onecell,imo,x0[1],y0+dy[i],xdw,ydw,digits,val,metric=check
      ckval1[i]=check
   endfor

   if debug then begin
      print,'0 check',ckval0
      print,'1 check',ckval1
   endif

   z0=where(ckval0 eq min(ckval0))
   z1=where(ckval1 eq min(ckval1))
   if z0[0] eq z1[0] then begin
      if debug then print,'DY adjustment ',dy[z0[0]]
      y0=y0+dy[z0[0]]
   endif else begin
      if debug then print,'Decode failed on dy adjustment scan.'
      return
   endelse

   ; extract the sub-images on each cell that should contain a digit.
   ncells=n_elements(x0)
   cellodd=bytarr(xdw*2+1,ydw*2+1,ncells)
   celleven=bytarr(xdw*2+1,ydw*2+1,ncells)
   odd=strarr(ncells)
   even=strarr(ncells)
   ock=fltarr(ncells)
   eck=fltarr(ncells)
   for i=0,ncells-1 do begin
      getdigits_onecell,imo,x0[i],y0,xdw,ydw,digits,val,metric=ocheck,sub=sub
      sub=float(sub)-min(sub)
      if max(sub) gt 0 then begin
         sub=sub/max(sub)*255
         cellodd[*,*,i]=byte(sub)
         odd[i]=val
         ock[i]=ocheck
      endif
      getdigits_onecell,ime,x0[i],y0,xdw,ydw,digits,val,metric=echeck,sub=sub
      sub=float(sub)-min(sub)
      if max(sub) gt 0 then begin
         sub=sub/max(sub)*255
         celleven[*,*,i]=byte(sub)
         even[i]=val
         eck[i]=echeck
      endif
   endfor
;print,minmax(cellodd)
;print,minmax(celleven)
;print,minmax(digits)
;itool,imo
;itool,cellodd,/block

   if debug then begin
      setwin,4,xsize=2*(2*xdw+1)*ncells,ysize=2*(2*ydw+1)*5
      ipick=8
      for i=0,ncells-1 do begin
         ddiff=abs(long(trimrank(cellodd[*,*,i]))-long(trimrank(digits[*,*,ipick])))
         z=where(ddiff/255.0 lt 0.5,count)
         if count ne 0 then ddiff[z]=0
         tvscl,rebin(ddiff,2*(2*xdw+1),2*(2*ydw+1),/sample),i
         ;tvscl,rebin(fix(trimrank(cellodd[*,*,i]))-fix(digits[*,*,ipick]),2*(2*xdw+1),2*(2*ydw+1),/sample),i
         tv,rebin(trimrank(cellodd[*,*,i]),2*(2*xdw+1),2*(2*ydw+1),/sample),i+ncells
         tv,rebin(digits[*,*,ipick],2*(2*xdw+1),2*(2*ydw+1),/sample),i+ncells*2
         tv,rebin(trimrank(celleven[*,*,i]),2*(2*xdw+1),2*(2*ydw+1),/sample),i+ncells*3
;         dd1=fft(float(trimrank(celleven[*,*,i])),1)
;         dd2=fft(float(digits[*,*,ipick]),1)
;         dd3=dd1-dd2
;         dd4=abs(fft(dd3,0))
;         z=where(dd4/max(dd4) lt 0.5,count)
;         if count ne 0 then dd4[z]=0.0
;;print,i,total(abs(dd4))
;         tvscl,rebin(abs(dd4),2*(2*xdw+1),2*(2*ydw+1),/sample),i+ncells*4
         ddiff=abs(long(trimrank(celleven[*,*,i]))-long(trimrank(digits[*,*,ipick])))
         z=where(ddiff/255.0 lt 0.5,count)
         if count ne 0 then ddiff[z]=0
         tvscl,rebin(ddiff,2*(2*xdw+1),2*(2*ydw+1),/sample),i+ncells*4
      endfor
      print,'Odd check',ock
      print,'Even check',eck
   endif

   if debug then begin
      print,'   ',replicate('.',ncells)
      print,'O: ',odd,format='(a,20(1x,a1))'
      print,'E: ',even,format='(a,20(1x,a1))'
   endif

   otime=''
   etime=''
   onum=0
   enum=0

   otime=odd[0]+odd[1]+':'+odd[2]+odd[3]+':'+odd[4]+odd[5]+'.'
   if odd[6] eq '' then $
      otime=otime+odd[10]+odd[11]+odd[12]+odd[13] $
   else $
      otime=otime+odd[6]+odd[7]+odd[8]+odd[9]
   onum=long(strjoin(odd[14:*],''))

   etime=even[0]+even[1]+':'+even[2]+even[3]+':'+even[4]+even[5]+'.'
   if even[6] eq '' then $
      etime=etime+even[10]+even[11]+even[12]+even[13] $
   else $
      etime=etime+even[6]+even[7]+even[8]+even[9]
   enum=long(strjoin(even[14:*],''))

   otime=trimrank(otime)
   etime=trimrank(etime)
   onum=trimrank(onum)
   enum=trimrank(enum)

   fail=0
   if strlen(otime) ne 13 then fail=1
   if strlen(etime) ne 13 then fail=1

   if not fail then begin
      val1=raparse(otime,error=error1)
      rastr,val1,4,str
      if otime ne str then fail=1
      val2=raparse(etime,error=error2)
      rastr,val2,4,str
      if etime ne str then fail=1
      if error1 or error2 then fail=1
      if abs(val1-val2) gt 7.2e-5 then fail=1
      if debug then print,'Time difference is ',abs(val1-val2)
   endif

   if not fail then begin
      if abs(onum-enum) gt 1 then fail=1
      if debug then print,'Field count difference is ',abs(onum-enum)
   endif

   if debug then begin
      if fail then print,'decode FAILED' $
      else print,'Successful decode'
      print,'Odd  ',otime,onum
      print,'Even ',etime,enum
   endif

bailout:
   if fail then begin
      otime=''
      etime=''
      onum=0
      enum=0
   endif

end
