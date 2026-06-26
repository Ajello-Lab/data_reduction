;+
; NAME:
;  occmodel
; PURPOSE:   (one line only)
;  Compute a synthetic occultation profile
; DESCRIPTION:
; CATEGORY:
;  Occultations
; CALLING SEQUENCE:
;  occmodel,time,time1,time2,model,error
; INPUTS:
;  time  - vector of time for the samples, this value references the middle of
;           the sample
;  time1 - time of disappearance, same units as "time"
;  time2 - time of reappearance, same units as "time"
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
; OUTPUTS:
;  model - synthetic occultation profile, values are mostly either 0 or 1.  For
;             the transition points, the flux is between 0 and 1 depending on
;             where the transition is between the time interval of the point.
; KEYWORD OUTPUT PARAMETERS:
; CONFIGURATION:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  Written by Marc W. Buie, Southwest Research Institute, 2020/03/29
;-
pro occmodel,time,time1,time2,model,error

   error=1
   model=replicate(1.0,n_elements(time))

   if time1 le mean(time[0:1])   then return
   if time2 ge mean(time[-2:-1]) then return
   if time2 le time1 then return

   ; Sample that contains time1 
   dt1=abs(time-time1)
   z1 = where(dt1 eq min(dt1))
   z1=z1[0]

   ; Sample that contains time2 
   dt2=abs(time-time2)
   z2 = where(dt2 eq min(dt2))
   z2=z2[0]

   model[z1:z2]=0.0

   if z1 ne z2 then begin
      ; disappearance
      t0=(time[z1-1]+time[z1])/2.0
      t1=(time[z1]+time[z1+1])/2.0
      model[z1] = (time1-t0)/(t1-t0)

      ; reaappearance
      t0=((time[z2-1])+time[z2])/2.0
      t1=(time[z2]+time[z2+1])/2.0
      model[z2] = 1.0-(time2-t0)/(t1-t0)
   endif else begin
      t0=(time[z1-1]+time[z1])/2.0
      t1=(time[z1]+time[z1+1])/2.0
      model[z1] = 1.0-(time2-time1)/(t1-t0)
   endelse

   error=0

end
