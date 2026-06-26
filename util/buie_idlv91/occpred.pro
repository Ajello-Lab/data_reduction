;+
; NAME:
;  occpred
; PURPOSE:   (one line only)
;  Make occultation prediction
; DESCRIPTION:
;  Behavior is governed by keywords in config.ini. A subdirectory is
;  created with new prediction products. See occmap_howto.txt for details.
; CATEGORY:
;  Occultations
; CALLING SEQUENCE:
;  occpred
; INPUTS:
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;  objectid :String,  objectid to use (overwrites config.ini value)
; OUTPUTS:
;  version  :String,  name of the subdirectory created
; KEYWORD OUTPUT PARAMETERS:
; CONFIGURATION:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  Written by Brian Keeney, Southwest Research Institute, 2023-04-05
;-
pro occpred, version, objectid=objectid

  self='occpred: '
  if badpar(objectid, [0,7], 0, caller=self+'(objectid) ', default='') then return

  
  ; Load config file
  cfgfile = 'config.ini'
  cfgdir  = exists(cfgfile) ? '' : '../'
  cfgfile = cfgdir + cfgfile

  if nofile(cfgfile,'Configuration') then return
  loadini, file=cfgfile, info

  getvalue, info, 'id', 'type', type
  if (type ne 'occ') then begin
     print, 'The file '+cfgfile+' must have type = occ'
     return
  endif

  getvalue, info, 'global', 'event',    event
  getvalue, info, 'global', 'geomid',   geomid
  getvalue, info, 'global', 'objectid', objid

  if (objectid ne '') then objid = objectid

  getvalue, info, 'global', 'objname',  objname,  default=objid
  getvalue, info, 'global', 'kernel',   kernel,   default='astorb.dat'
  getvalue, info, 'global', 'catname',  starid
  getvalue, info, 'global', 'ora',      rastr
  getvalue, info, 'global', 'odec',     decstr
  getvalue, info, 'global', 'gmag',     gmag,     type=4


  ; Kernel check if objid begins with 'P20'
  ephtype = stregex(objid, '^P20', /boolean) ? 'spice' : 'astorb'
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

  
  ; Create new prediction
  mkocceph, objname, objid, geomid, starid, rastr, decstr, version
  
  
  ; Run ocmapex to produce shadow path
  cd, version

  openw,  lun, 'mkcenter', /get_lun
  printf, lun, '#!/bin/csh'
  printf, lun
  printf, lun, 'ocmapex << EOI'
  printf, lun, 'o'
  printf, lun, objid
  printf, lun, version+'.in'
  printf, lun, 'EOI'
  printf, lun
  printf, lun, 'mv tempor.tmp '+version+'.plt'
  printf, lun, 'mv ocmap.out '+version+'.out'
  printf, lun, 'if (-f center.out) then'
  printf, lun, '  mv center.out '+version+'_center.dat'
  printf, lun, 'endif'
  printf, lun, 'if (-f north.out) then'
  printf, lun, '  mv north.out '+version+'_north.dat'
  printf, lun, 'endif'
  printf, lun, 'if (-f south.out) then'
  printf, lun, '  mv south.out '+version+'_south.dat'
  printf, lun, 'endif'
  free_lun, lun

  file_chmod, 'mkcenter', /a_execute
  spawn, 'mkcenter', result
  file_delete, 'mkcenter'

  
  ; Create CSV file with event times
  timecsv, version+'_center.dat'
  cd, '..'
  
end
