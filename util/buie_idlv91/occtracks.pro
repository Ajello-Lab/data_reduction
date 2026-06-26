;+
; NAME:
;  occtracks
; PURPOSE:   (one line only)
;  Make support files for occultation tracks
; DESCRIPTION:
; CATEGORY:
;  Occultations
; CALLING SEQUENCE:
;  occmap
; INPUTS:
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
; OUTPUTS:
;  Behavior is governed by keywords in config.ini. One KML file is
;  created in the directory where OCCTRACKS is run, as well as a
;  subdirectory called tracks/ containing files for individual
;  tracks. See occmap_howto.txt for details.
; KEYWORD OUTPUT PARAMETERS:
; CONFIGURATION:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  Written by Brian Keeney, Southwest Research Institute, 2023-04-28
;-
pro occtracks

  self='occtracks: '
  
  ; Load config file
  cfgfile = '../config.ini'
  cfgdir  = exists(cfgfile) ? '' : '../'
  cfgfile = cfgdir + cfgfile
  
  if nofile(cfgfile,'Configuration') then return
  loadini, file=cfgfile, info

  getvalue, info, 'id', 'type', type
  if (type ne 'occ') then begin
     print, 'The file '+cfgfile+' must have type = occ'
     return
  endif


  getvalue, info, 'global', 'objectid', globalid
  getvalue, info, 'occmap', 'tracks',   maptracks
  getvalue, info, 'occmap', 'prefix',   mapprefix
  getvalue, info, 'occmap', 'trackcol', maptrackcol, default='ffff80ff'
  
  cd, current=cwd
  fnroot = (strsplit(cwd,'/',/extract))[-1]
  objid  = stregex(fnroot, '^P20', /boolean) ? fnroot : globalid

  maptracks = cfgdir + maptracks
  getvalue, info, objid, 'tracks', tracks, default=maptracks
  getvalue, info, objid, 'prefix', prefix, default=mapprefix
  getvalue, info, objid, 'trackcol', trackcol, default=maptrackcol

  
  if (tracks eq '') then begin
     print, 'Please specify tracks keyword.'
     return
  endif

  if nofile(tracks, 'Tracks') then return
  readcol, tracks, f='a,f', trackname, trackpos

  trackcol = strsplit(trackcol,',',/extract)
  if (n_elements(trackcol) eq 1) then $
     trackcol = replicate(trackcol, n_elements(trackname))

  if (n_elements(trackcol) ne n_elements(trackname)) then begin
     print, 'Trackcol must be a scalar or have the same length as ' + $
            'the number of tracks to be generated.'
     return
  endif

  if (prefix ne '') then begin
     pos = stregex(trackname, '[0-9]')
     for j=0,n_elements(trackname)-1 do $
        trackname[j] = prefix + strmid(trackname[j], pos[j])
  endif
  
  
  ; Kernel check if objid begins with 'P20'
  getvalue, info, 'global', 'kernel', kernel, default='astorb.dat'
  ephtype = stregex(globalid, '^P20', /boolean) ? 'spice' : 'astorb'

  if (ephtype eq 'spice') then begin
     if (kernel eq 'astorb.dat') then begin
        print, 'Add kernel information to config.ini.'
        return
     endif
     
     naifdir = getenv('NAIF_DIR')
     if (naifdir eq '') then begin
        print, 'Cannot find $NAIF_DIR. Please add environment variable.'
        return
     endif

     kernels = strsplit(kernel, ',', /extract)
     for j=0,n_elements(kernels)-1 do begin
        if nofile(naifdir+kernels[j],'Kernel') then begin
           root  = (strsplit(kernels[j], '_', /extract))[0]
           guess = file_search(naifdir+root+'_*.bsp', count=nfiles)
           if (nfiles eq 1) then $
              print, 'Active kernel appears to be ' + guess
           return
        endif
     endfor
  endif else begin
     if (kernel ne 'astorb.dat') then begin
        print, 'Kernel specified is incompatible with object id.'
        return
     endif
  endelse
       

  ; All inputs good, call ocmapex and kmlgen
  spawn, 'rm occtrack*.* tracks/*'

  for j=0,n_elements(trackname)-1 do begin
     openr, rlun, fnroot+'.in', /get_lun
     openw, wlun, 'occtrack_'+trackname[j]+'.in', /get_lun

     ctr = 0
     line = ''

     while not eof(rlun) do begin
        readf, rlun, line
        
        if (ctr eq 1) then begin
           printf, wlun, f='(2f10.1,a)', abs(trackpos[j])>0.1, $
                   abs(trackpos[j])>0.1, strmid(line,20) 
        endif else begin
           printf, wlun, line
        endelse

        ctr++
     endwhile        
        
     free_lun, rlun, wlun

              
     ; Run ocmapex to produce tracks
     openw,  lun, 'mktrack_'+trackname[j], /get_lun
     printf, lun, '#!/bin/csh'
     printf, lun
     printf, lun
     printf, lun, 'ocmapex << EOI'
     printf, lun, 'o'
     printf, lun, objid
     printf, lun, 'occtrack_'+trackname[j]+'.in'
     printf, lun, 'EOI'
     printf, lun
     printf, lun, 'rm tempor.tmp'
     printf, lun, 'mv ocmap.out occtrack_'+trackname[j]+'.out'

     if (trackpos[j] eq 0) then begin
        printf, lun, 'if (-f center.out) then'
        printf, lun, '  mv center.out occtrack_'+trackname[j]+'_path.dat'
        printf, lun, 'endif'
        printf, lun, 'if (-f north.out) then'
        printf, lun, '  rm north.out'
        printf, lun, 'endif'
        printf, lun, 'if (-f south.out) then'
        printf, lun, '  rm south.out'
        printf, lun, 'endif'
     endif else if (trackpos[j] lt 0) then begin
        printf, lun, 'if (-f center.out) then'
        printf, lun, '  rm center.out'
        printf, lun, 'endif'
        printf, lun, 'if (-f north.out) then'
        printf, lun, '  rm north.out'
        printf, lun, 'endif'
        printf, lun, 'if (-f south.out) then'
        printf, lun, '  mv south.out occtrack_'+trackname[j]+'_path.dat'
        printf, lun, 'endif'
     endif else begin
        printf, lun, 'if (-f center.out) then'
        printf, lun, '  rm center.out'
        printf, lun, 'endif'
        printf, lun, 'if (-f north.out) then'
        printf, lun, '  mv north.out occtrack_'+trackname[j]+'_path.dat'
        printf, lun, 'endif'
        printf, lun, 'if (-f south.out) then'
        printf, lun, '  rm south.out'
        printf, lun, 'endif'
     endelse
           
     free_lun, lun

     file_chmod, 'mktrack_'+trackname[j], /a_execute
     spawn, 'mktrack_'+trackname[j], result
     file_delete, 'mktrack_'+trackname[j]
  endfor

  files = file_search('occtrack_*_path.dat')
  kmlgen, files, trackcol, 'occtracks.kml', /tracks
  
end
