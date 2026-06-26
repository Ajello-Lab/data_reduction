;+
; NAME:
;  timeinfo
; PURPOSE:   (one line only)
;  Collect IOTA-VTI time codes from one or more video frames
; DESCRIPTION:
;  Take one or more images extracted from a video file containing time
;    stamps from an IOTA_VTI timer and decode time and field number.
; CATEGORY:
;  Video data processing
; CALLING SEQUENCE:
;  timeinfo,fnlist,otime,onum,etime,enum
; INPUTS:
;  fnlist - String or string array with files names (fully qualified with
;             path if needed).  The input files are expected to be single
;             images extracted from a video file.  The file formats allowed
;             are those that are readable by IDL's read_image utility.
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;  DEBUG - Flag, if set will generate copious output and graphics.
;            Use with caution.  It's best not to use this flag when
;            processing a long list of files.
; OUTPUTS:
;  otime  - String with the UT time of the odd field HH:MM:SS.ssss
;  onum   - Long integer with the field number of the odd field
;  etime  - String with the UT time of the even field HH:MM:SS.ssss
;  enum   - Long integer with the field number of the even field
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
;  see getdigits.pro for any restrictions on decoding process
; PROCEDURE:
; MODIFICATION HISTORY:
;  2016/03/08, Written by Marc W. Buie, Southwest Research Institute
;  2016/03/25, MWB, added DEBUG, XOFF, and YOFF keywords
;-
pro timeinfo,fnlist,otime,onum,etime,enum, $
       DEBUG=debug,XOFF=xoff,YOFF=yoff

   self='timeinfo: '
   if badpar(fnlist,7,[0,1],caller=self+'(fnlist) ') then return
   if badpar(debug,[0,1,2,3],0,caller=self+'(DEBUG) ',default=0) then return
   if badpar(xoff,[0,2,3],0,caller=self+'(XOFF) ',default=0) then return
   if badpar(yoff,[0,2,3],0,caller=self+'(YOFF) ',default=0) then return

   nfiles=n_elements(fnlist)
   otime=strarr(nfiles)
   onum=lonarr(nfiles)
   etime=strarr(nfiles)
   enum=lonarr(nfiles)

   for i=0,nfiles-1 do begin
      if not exists(fnlist[i]) then begin
         print,fnlist[i]
         print,self,'File not found.  Aborting.'
         return
      endif
      if debug then print,'Reading ',fnlist[i]
      image=read_image(fnlist[i])
      getdigits,image,odd,even,otime0,onum0,etime0,enum0, $
         debug=debug,xoff=xoff,yoff=yoff
      otime[i]=otime0
      etime[i]=etime0
      onum[i]=onum0
      enum[i]=enum0
   endfor

   otime=trimrank(otime)
   etime=trimrank(etime)
   onum=trimrank(onum)
   enum=trimrank(enum)

end
