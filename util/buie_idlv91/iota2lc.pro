;+
; NAME:
;  iota2lc
; PURPOSE:   (one line only)
;  Read an IOTA cvs lightcurve file and convert to a simple text table
; DESCRIPTION:
; CATEGORY:
;  Photometry
; CALLING SEQUENCE:
;  iota2lc,fnin,fnout
; INPUTS:
;  fnin - Name of the input file
;  fnout - Name of the output file, default is fnin with the trailing .csv
;             changed to .lc
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;  TCORR - camera and integration offset to add to timing [seconds]
;             t_final = t_file + tcorr
;  DCOL  - column(field) for the data, default=3 (the code is not general inside)
;  WIDE - flag, if set forces condition of 10 or more columns
;  BINFAC - measurement binning factor to compensative for video frame
;             integration, Default = 1 (no binning)
;  OFFSET - Trim this many points from the start of a lightcurve so that the
;             the binned points match the integration boundarries.  The
;             default is zero.  This value is constrained internally to be
;             between 0 and binfac-1.
;  N_HEADER - number of lines to skip before the column header
; OUTPUTS:
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  Written by Marc W. Buie, Southwest Research Institute, 2020/03/26
;  2021/05/14, MWB, Added DCOL keyword
;  2023/06/08, MWB, Added WIDE, BINFAC, and OFFSET keywords
;  2024/09/04, MWB, Added case for more than 100(!) columns in input file
;-
pro iota2lc,date,fnin,fnout, $
       TCORR=tcorr,LIMOVIE=limovie,N_HEADER=n_header,DCOL=dcol,WIDE=wide, $
       BINFAC=binfac,OFFSET=in_offset,UNISTELLAR=unistellar, $
       HMSTIME=hmstime

   self='iota2lc: '
   if badpar(date,7,0,caller=self+'(date) ') then return
   if badpar(fnin,7,0,caller=self+'(fnin) ') then return
   if badpar(fnout,[0,7],0,caller=self+'(fnin) ', $
                           default='[[default]]') then return
   if badpar(tcorr,[0,4,5],0,caller=self+'(TCORR) ',default=0.0) then return
   if badpar(limovie,[0,1,2,3],0,caller=self+'(LIMOVIE) ',default=0) then return
   if badpar(unistellar,[0,1,2,3],0,caller=self+'(UNISTELLAR) ',default=0) then return
   if limovie then def_n_header=3 else def_n_header=2
   if badpar(n_header,[0,1,2,3],0,caller=self+'(N_HEADER) ', $
                                  default=def_n_header) then return
   if badpar(dcol,[0,2,3],0,caller=self+'(DCOL) ',default=3) then return
   if badpar(wide,[0,1,2,3],0,caller=self+'(WIDE) ',default=0) then return
   if badpar(binfac,[0,2,3],0,caller=self+'(BINFAC) ',default=1) then return
   if badpar(in_offset,[0,2,3],0,caller=self+'(OFFSET) ',default=0) then return
   if badpar(hmstime,[0,1,2,3],0,caller=self+'(HMSTIME) ',default=0) then return

   offset=in_offset<(binfac-1)>0

   if nofile(fnin,'IOTA Lightcurve') then return

   if fnout eq '[[default]]' then begin
      fnout=repchar(fnin,'.csv','.lc')
   endif

   print,'Read ',fnin
   print,'Write ',fnout


   if limovie then begin
      data=read_csv(fnin,count=nobs,header=header,n_table_header=n_header)

      idx=lindgen(nobs)
      oframe=long(data.field01)
print,'not implemented yet'
return
   endif else if unistellar then begin
      data=read_csv(fnin,count=nobs,header=header,n_table_header=n_header)

      idx=lindgen(nobs)
      ncols=n_elements(header)
      oframe=idx
      time=data.field01
      if dcol eq 2 then begin
         rawflux=data.field02
      endif else if dcol eq 4 then begin
         rawflux=data.field04
      endif else begin
         print,'DCOL=',strn(dcol),' not supported'
         return
      endelse
      jd=jdparse(data.field01)+tcorr/86400.0d0
      robomean,rawflux,3.0,0.5,meanflux,dummy,dsig
      print,'Mean star signal',meanflux,', per-point noise is ',dsig
      print,'per-point SNR = ',meanflux/dsig
      print,'time correction applied is ',tcorr

      flux = rawflux/meanflux
   endif else begin

      data=read_csv(fnin,count=nobs,header=header,n_table_header=n_header)

      idx=lindgen(nobs)
      ncols=n_elements(header)
      if ncols lt 10 and wide eq 0 then begin
         oframe=long(data.field1)
         time=data.field2
         if dcol eq 2 then begin
            rawflux=data.field2
         endif else if dcol eq 3 then begin
            rawflux=data.field3
         endif else if dcol eq 4 then begin
            rawflux=data.field4
         endif else begin
            print,'DCOL=',strn(dcol),' not supported'
            return
         endelse
      endif else if ncols lt 100 then begin
         oframe=long(data.field01)
         time=data.field02
         if dcol eq 3 then begin
            rawflux=data.field03
         endif else if dcol eq 4 then begin
            rawflux=data.field04
         endif else begin
            print,'DCOL=',strn(dcol),' not supported'
            return
         endelse
      endif else begin
         oframe=long(data.field001)
         time=data.field002
         if dcol eq 3 then begin
            rawflux=data.field003
         endif else if dcol eq 8 then begin
            rawflux=data.field008
         endif else begin
            print,'DCOL=',strn(dcol),' not supported'
            return
         endelse
      endelse

      time=repchar(time,'[','')
      time=repchar(time,']','')

      jd=jdparse(date+' '+time)+tcorr/86400.0d0

      if binfac gt 1 then begin
         if offset gt 0 then begin
            print,'Trimming ',strn(offset), $
                  ' points from the beginning of the lightcurve'
            jd=jd[offset:*]
            rawflux=rawflux[offset:*]
            nobs=n_elements(jd)
         endif
         len = nobs/binfac
         tmp1=reform(rawflux[0:len*binfac-1],binfac,len)
help,tmp1
print,rawflux[0:3]
print,tmp1[*,0]
         tmp1=total(tmp1,1)/float(binfac)
help,tmp1
print,tmp1[0]/4.0

         tmp2=reform(jd[0:len*binfac-1],binfac,len)
         tmp2=total(tmp2,1)/double(binfac)

         plot,(jd-jd[0])*86400.0d0,rawflux,psym=8,xr=[0,6.]
         oplot,(tmp2-jd[0])*86400.0d0,tmp1,psym=8,color=cpalette(1)

         for i=1L,41,2 do $
            oplot,(jd[i*binfac:(i+1)*binfac-1]-jd[0])*86400.0d0, $
                  rawflux[i*binfac:(i+1)*binfac+1], $
                  psym=8,color=cpalette(60),symsize=1.5

         jd=tmp2
         rawflux = tmp1
         nobs=len
      endif

;      x=data.field09
;      y=data.field10

      robomean,rawflux,3.0,0.5,meanflux,dummy,dsig
      print,'Mean star signal',meanflux,', per-point noise is ',dsig
      print,'per-point SNR = ',meanflux/dsig
      print,'time correction applied is ',tcorr

      flux = rawflux/meanflux

   endelse

   ;print,header

   openw,lun,fnout,/get_lun

   printf,lun,'Timing correction applied ',tcorr,' seconds'
   for i=0,nobs-1 do begin
;      printf,lun,idx[i],jd[i],flux[i],oframe[i],rawflux[i],x[i],y[i], $
;         format='(i5,1x,f18.10,1x,f9.6,1x,i6,e12.4,2(1x,f6.2))'
      printf,lun,idx[i],jd[i],flux[i],oframe[i],rawflux[i], $
         format='(i5,1x,f18.10,1x,f9.6,1x,i6,e12.4)'
   endfor
   free_lun,lun

end
