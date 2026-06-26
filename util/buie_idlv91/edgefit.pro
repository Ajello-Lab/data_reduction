;+
; NAME:
;  edgefit
; PURPOSE:
;  Fit an edge between two signal levels
; DESCRIPTION:
; CATEGORY:
;  Function fitting
; CALLING SEQUENCE:
;  edgefit,x,y,err,xedge,xedgesig
; INPUTS:
;  x - independent variable (vector)
;  y - dependent variable (vector)
;  err - uncertainties on y
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
; OUTPUTS:
;  Two plots are generated and the fitted values are printed to the screen.
;  xedge - 3 element vector:
;              0 - signal level on the left of the edge
;              1 - signal level on the right of the edge
;              2 - the x value of where the edge is
;  xedgesig - 1-sigma uncertainty on the values in xedge
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
;  Assumes data are strictly uniform in x (spacing does not vary).
; PROCEDURE:
; MODIFICATION HISTORY:
;  1996/12/23, Written by Marc W. Buie, Lowell Observatory
;  2017/01/19, MWB, many rewrites, may sort of work now but still a bit
;                 fragile or non-general.  Use with extreme caution.
;-
function edgefit_func, vals
   common edgefit_common,info
   left  = vals[0]
   right = vals[1]
   midpt = vals[2]
   info.model[*] = 0.0 ; fltarr(info.len)
   info.model[where(info.x lt midpt-info.dx/2.0,/null)]=left
   info.model[where(info.x gt midpt+info.dx/2.0,/null)]=right
   z=where(midpt-info.dx/2.0 le info.x and midpt+info.dx/2.0 ge info.x,count)
   if count ne 0 then begin
      if left lt right then $
         info.model[z] = (right*(midpt-(info.x[z]-info.dx/2.0)) + $
                          left*((info.x[z]-info.dx/2.0)-midpt))/info.dx $
      else $
         info.model[z] = (left*(midpt-(info.x[z]-info.dx/2.0)) + $
                          right*((info.x[z]-info.dx/2.0)-midpt))/info.dx
   endif
   ; this returns the total weighted chisq, not a reduced chisq
   return,total(((info.y-info.model)/info.ysig)^2)
end

pro edgefit,x,y,err,xedge,xedgesig

   common edgefit_common,info

   len=n_elements(x)

   ; Get the mean level at the ends (first and last 10%)
   left  = mean(y[0:0.1*len])
   right = mean(y[0.9*len:len-1])
   print,'Starting guess for top and bottom',left,right
   midpt = x[len/2]
   dx=x[1]-x[0]

   info={ $
      x: x, $
      y: y, $
      ;ysig: replicate(1.0,len), $
      ysig: err, $
      dx: dx, $
      len: len, $
      model: fltarr(len) $
      }

   vals=double([left,right,midpt])
   scale=[vals[0]*0.01,vals[1]*0.01,1.0]
   ftol=1.0e-4
   fitval=amoeba(ftol,function_name='edgefit_func', $
                 p0=vals,scale=scale,nmax=5000)
   chibest=edgefit_func(fitval)
   print,'original chisq',chibest,', npoints',len
   sigscale=sqrt(chibest/float(len))
   print,'sig scale fac',sigscale
   info.ysig=info.ysig*sigscale
   chibest=edgefit_func(fitval)

   print,'----left,right,edge----'
   print,fitval
   print,'-----'
   print,'chisq',chibest
   print,'dx',info.dx

   setwin,0
   plot,info.x,info.y
   oplot,info.x,info.model,psym=-5

   setwin,1
   plot,info.x,info.y-info.model

   print,'pre MC fit',fitval

   scale=[mean(info.ysig),mean(info.ysig),dx/10.0]
   region=scale*5.0
   mcmcsamp,fitval,scale,region,pdf,function_name='edgefit_func', $
      verbose=0,display=0,nsteps=10000

   xedge=[mean(pdf[0,*]),mean(pdf[1,*]),mean(pdf[2,*])]
   xedgesig=[stddev(pdf[0,*]),stddev(pdf[1,*]),stddev(pdf[2,*])]

   chibest=edgefit_func(xedge)
   print,'final chisq',chibest

   setwin,0
   plot,info.x,info.y
   oplot,info.x,info.model,psym=-5
   oplot,[1,1]*xedge[2],!y.crange,color='00ff00'xl

   setwin,1
   plot,info.x,info.y-info.model

   setwin,2
   yr=minmax(info.y)
   xr=xedge[2]+[-dx,dx]*10
   ploterror,info.x,info.y,info.ysig,xr=xr,yr=yr,psym=-8,symsize=1.0
   oplot,info.x,info.model,psym=-5,color='0000ff'xl
   oplot,[1,1]*xedge[2],!y.crange,color='00ff00'xl


end

