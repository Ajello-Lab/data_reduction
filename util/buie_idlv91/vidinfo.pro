;+
; NAME:
;  vidinfo
; PURPOSE:   (one line only)
;  Extract and return information about a video file
; DESCRIPTION:
; CATEGORY:
;  Video data processing
; CALLING SEQUENCE:
;  vidinfo,fn,info,error
; INPUTS:
;  fn - string that contains file name (with or without path)
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
; OUTPUTS:
;  info - anonymous structure containing information about the video
;            The following tags are provided:
;     dir      - directory where data are found (not including top root)
;     filename - Name of the file (no path)
;     codec    - Name of the coded used for video encoding/compression
;     pixfmt   - Name of the pixel encoding format
;     length   - Duration of the video in seconds
;     width    - pixel width of each video frame
;     height   - pixel height of each video frame
;     nframes  - Number of video frames in the file (provided by ffprobe)
;     cframes  - Calculated number of frames (length*29.97)
;     mode     - string, permissions on video file
;     fjd      - JD of time of last file modification (same as mtime)
;     datetime - mysql datetime string equivalent to fjd
;     ctime    - File creation time (seconds from Unix reference time)
;     mtime    - File last modification time (seconds from reference)
;     size     - Size of the file in bytes
;
;  error - Flag, when set indicates an error was encountered with the file
;
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  2016/02/19, Written by Marc W. Buie, Southwest Research Institute
;-
function vidinfo_fetch_float,info,key
   z=where(strpos(info,key) eq 0,count)
   value=0
   for i=0,count-1 do begin
      words=strsplit(info[z[i]],'=',/extract)
      if words[1] ne 'N/A' then begin
         value=float(words[1])
         break
      endif
   endfor
   return,value
end

function vidinfo_fetch_int,info,key
   z=where(strpos(info,key) eq 0,count)
   value=0
   for i=0,count-1 do begin
      words=strsplit(info[z[i]],'=',/extract)
      if words[1] ne 'N/A' then begin
         value=fix(words[1])
         break
      endif
   endfor
   return,value
end

function vidinfo_fetch_long,info,key
   z=where(strpos(info,key) eq 0,count)
   value=0L
   for i=0,count-1 do begin
      words=strsplit(info[z[i]],'=',/extract)
      if words[1] ne 'N/A' then begin
         value=long(words[1])
         break
      endif
   endfor
   return,value
end

function vidinfo_fetch_string,info,key
   z=where(strpos(info,key) eq 0,count)
   if count ge 1 then begin
      words=strsplit(info[z[0]],'=',/extract)
      value=words[1]
   endif else begin
      value=''
   endelse
   return,value
end

pro vidinfo,fn,info,error

   self='vidinfo: '
   if badpar(fn,7,0,caller=self+'(fn) ') then return

   finfo=file_info(fn,/noexpand_path)

   error=0

   if finfo.exists eq 0 then begin
      print,fn
      print,self,'Error! File not found.'
      error=1
      return
   endif
   if finfo.size eq 0 then begin
      print,fn
      print,self,'Error!  File is empty.'
      error=1
      return
   endif

   pos=strpos(fn,'/',/reverse_search)
   dir=strmid(fn,0,pos+1)
   file=strmid(fn,pos+1)

   jd = jdparse('1970/01/01')+double(finfo.mtime)/86400.0d0
   jdstr,jd,300,jds

   cmd='unset LD_LIBRARY_PATH ; ffprobe -v error -show_format -show_streams -show_error '+quote(fn)
   spawn,cmd,results,/sh

   width=vidinfo_fetch_int(results,'width=')
   height=vidinfo_fetch_int(results,'height=')
   nframes=vidinfo_fetch_long(results,'nb_frames=')
   length=vidinfo_fetch_float(results,'duration=')
   codec=vidinfo_fetch_string(results,'codec_name=')
   pixfmt=vidinfo_fetch_string(results,'pix_fmt=')

   cframes=round(length*29.97)

   info={ $
      dir:     dir, $
      filename: file, $
      codec:   codec, $
      pixfmt:  pixfmt, $
      length:  length, $
      width:   width, $
      height:  height, $
      nframes: nframes, $
      cframes: cframes, $
      mode:    string(finfo.mode,format='(o03.3)'), $
      fjd:     jd, $
      datetime: jds, $
      ctime:   finfo.ctime, $
      mtime:   finfo.mtime, $
      size:    finfo.size $
      }

end
