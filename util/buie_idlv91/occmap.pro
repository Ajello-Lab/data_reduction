;+
; NAME:
;  occmap
; PURPOSE:   (one line only)
;  Make support files for occultation maps
; DESCRIPTION:
;  Behavior is governed by keywords in config.ini. See
;  occmap_howto.txt for details.
; CATEGORY:
;  Occultations
; CALLING SEQUENCE:
;  occmap
; INPUTS:
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
; OUTPUTS:
; KEYWORD OUTPUT PARAMETERS:
; CONFIGURATION:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  Written by Brian Keeney, Southwest Research Institute, 2023-04-28
;    2023-10-02, BAK: Fixed bug when roilims = [[[none]]]
;    2024-06-03, BAK: Added objname to line descriptions
;-
pro occmap

  self='occmap: '
  
  ; Load config file
  cfgfile = '../config.ini'
  if nofile(cfgfile,'Configuration') then return
  loadini, file=cfgfile, info

  getvalue, info, 'id', 'type', type
  if (type ne 'occ') then begin
     print, 'The file '+cfgfile+' must have type = occ'
     return
  endif


  ; Parse input from config.ini
  getvalue, info, 'global', 'objectid', globalid
  getvalue, info, 'global', 'objname', globalname, default=globalid

  getvalue, info, 'global', 'diameter', diameter, type=4, default=0.
  getvalue, info, 'global', 'xsig', xsig, type=4, default=0.

  getvalue, info, 'occmap', 'centercol', centercol, default='ff636363'
  getvalue, info, 'occmap', 'limbcol', limbcol, default='ff9c5108'

  getvalue, info, 'occmap', 'roilims', roilimsin
  if (roilimsin eq '') then begin
     roilims = [0.5*diameter+3*xsig,-0.5*diameter-3*xsig]
     roidesc = [globalname+' +3 sigma', globalname+' -3 sigma']

     getvalue, info, 'occmap', 'roicol', roicol, default='ffbd8231'

     roicol = strsplit(roicol,',',/extract)
     if (n_elements(roicol) eq 1) then begin
        roicol = replicate(roicol, n_elements(roilims))
     endif
     
     if (n_elements(roilims) ne n_elements(roicol)) then begin
        print, 'ROILIMS and ROICOL must have the same length.'
        return
     endif
  endif else if (roilimsin ne '[[[none]]]') then begin
     getvalue, info, 'occmap', 'roidesc', roidesc
     if (roidesc eq '') then begin
        print, 'If ROILIMS is set, then ROIDESC must be too.'
        return
     endif

     getvalue, info, 'occmap', 'roicol', roicol, default='ffbd8231'

     roilims = float(strsplit(roilimsin,',',/extract))
     roidesc = strsplit(roidesc,',',/extract)
     roicol  = strsplit(roicol,',',/extract)

     if (n_elements(roicol) eq 1) then begin
        roicol = replicate(roicol, n_elements(roilims))
     endif

     if (n_elements(roilims) ne n_elements(roidesc)) then begin
        print, 'ROILIMS and ROIDESC must have the same length.'
        return
     endif

     if (n_elements(roilims) ne n_elements(roicol)) then begin
        print, 'ROILIMS and ROICOL must have the same length.'
        return
     endif
  endif

  getvalue, info, 'occmap', 'sunalt', sunaltin, default='0,-8,-12,-18'
  if (sunaltin ne '[[[none]]]') then begin
     getvalue, info, 'occmap', 'suncol', suncol, $
               default='ff85befd,ff3c8dfd,ff0d55e6,ff0336a6'
        
     sunalt = strsplit(sunaltin,',',/extract)
     suncol = strsplit(suncol,',',/extract)
        
     if (n_elements(sunalt) ne n_elements(suncol)) then begin
        print, 'SUNALT and SUNCOL must have the same length.'
        return
     endif
  endif

  getvalue, info, 'occmap', 'staralt', staraltin, $
            default='10,15,20,25,30,89'
  if (staraltin ne '[[[none]]]') then begin
     getvalue, info, 'occmap', 'starcol', starcol, $
               default='ffdab9d4,ffc794c9,ffb065df,ff771cdd,ff430098,ff2b4bee'

     staralt = strsplit(staraltin,',',/extract)
     starcol = strsplit(starcol,',',/extract)

     if (n_elements(staralt) ne n_elements(starcol)) then begin
        print, 'STARALT and STARCOL must have the same length.'
        return
     endif
  endif

     
  ; Kernel check if objid begins with 'P20' and we need to run ocmapex
  getvalue, info, 'global', 'kernel', kernel, default='astorb.dat'
  ephtype = stregex(globalid, '^P20', /boolean) ? 'spice' : 'astorb'

  if ((diameter ne 0) or (roilimsin ne '[[[none]]]')) then begin
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
  endif
     

  ; All inputs good, call ocmapex and kmlgen as appropriate
  cd, current=cwd
  version = (strsplit(cwd,'/',/extract))[-1]
  
  ; Create mapdesc.txt
  getvalue, info, 'global', 'geomid',  geomid
  getvalue, info, 'global', 'ora',     ora
  getvalue, info, 'global', 'odec',    odec
  getvalue, info, 'global', 'gmag',    gmag, type=4

  openw, lun, 'mapdesc.txt', /get_lun
  printf, lun, f='(a,x,a," UT")', globalname, geomid
  printf, lun, f='(a,x,a)', ora, odec

  if (diameter eq 0) then begin
     printf, lun, f='("G = ", f5.2)', gmag
  endif else begin
     readcol, version+'.info', f="d,a", values, units
     speed = values[where(units eq 'km/sec')]
     gstar = gmag - 2.5d * alog10((diameter/40d)/(speed/20d))        
     printf, lun, f='("G = ", f5.2," G* = ",f4.1)', gmag, gstar
  endelse

  free_lun, lun


  
  getvalue, info, 'occmap', 'objectid', objectid
  objids = strsplit(objectid,',',/extract)

  for i=0,n_elements(objids)-1 do begin
     if (objids[i] ne globalid) then begin
        getvalue, info, objids[i], 'objname', objname, default=globalname
        getvalue, info, objids[i], 'diameter', objdiam, type=4, default=diameter
        getvalue, info, objids[i], 'xsig', objxsig,type=4, default=xsig
        getvalue, info, objids[i], 'centercol', objcencol, default=centercol
        getvalue, info, objids[i], 'limbcol', objcol, default=limbcol

        getvalue, info, objids[i], 'roilims', objroilimsin, default=roilimsin
        if (objroilimsin eq '') then begin
           objroilims = [0.5*objdiam+3*objxsig,-0.5*objdiam-3*objxsig]
           objroidesc = [objname+' +3 sigma', objname+' -3 sigma']

           getvalue, info, objids[i], 'roicol', objroicol
           if (objroicol eq '') then begin
              objroicol = roicol
           endif else begin
              objroicol = strsplit(objroicol,',',/extract)
              if (n_elements(objroicol) eq 1) then begin
                 objroicol = replicate(objroicol, n_elements(objroilims))
              endif
           endelse

           if (n_elements(objroilims) ne n_elements(objroicol)) then begin
              print, 'ROILIMS and ROICOL must have the same length.'
              return
           endif
        endif else if (objroilimsin ne '[[[none]]]') then begin
           getvalue, info, objids[i], 'roidesc', objroidesc
           if (objroidesc eq '') then begin
              print, 'If ROILIMS is set, then ROIDESC must be too.'
              return
           endif

           getvalue, info, objids[i], 'roicol', objroicol
           if (objroicol eq '') then begin
              print, 'If ROILIMS is set, then ROICOL must be too.'
              return
           endif

           objroilims = float(strsplit(objroilimsin,',',/extract))
           objroidesc = strsplit(objroidesc,',',/extract)
           objroicol  = strsplit(objroicol,',',/extract)

           if (n_elements(objroilims) ne n_elements(objroidesc)) then begin
              print, 'ROILIMS and ROIDESC must have the same length.'
              return
           endif

           if (n_elements(objroilims) ne n_elements(objroicol)) then begin
              print, 'ROILIMS and ROICOL must have the same length.'
              return
           endif
        endif

        
        fnroot = objids[i]
        if exists(fnroot+'/') then begin
           cd, fnroot
        endif else begin
           ; Create a prediction for this object if it doesn't exist yet
           occpred, objver, object=objids[i]
           file_move, objver, fnroot
        
           cd, fnroot

           file_move, objver+'.in',   fnroot+'.in'
           file_move, objver+'.info', fnroot+'.info'
           file_move, objver+'.out',  fnroot+'.out'
           file_move, objver+'.plt',  fnroot+'.plt'
        
           file_move, objver+'_center.dat', fnroot+'_center.dat'
           if exists(objver+'_north.dat') then $
              file_move, objver+'_north.dat', fnroot+'_north.dat'
           if exists(objver+'_south.dat') then $
              file_move, objver+'_south.dat', fnroot+'_south.dat'
        endelse
     endif else begin
        objname      = globalname
        objdiam      = diameter
        objxsig      = xsig
        objcencol    = centercol
        objcol       = limbcol
        objroilimsin = roilimsin
        objroilims   = (n_elements(roilims) gt 0) ? roilims : !null
        objroidesc   = (n_elements(roidesc) gt 0) ? roidesc : !null
        objroicol    = (n_elements(roicol) gt 0)  ? roicol  : !null
        fnroot       = version
     endelse

     
     ; Create shadow.kml
     print, 'Building shadow.kml...'

     openr, rlun, fnroot+'.in', /get_lun
     openw, wlun, 'occmap.in', /get_lun

     ctr = 0
     line = ''

     while not eof(rlun) do begin
        readf, rlun, line

        if (ctr eq 1) then begin
           printf, wlun, f='(2f10.1,a)', 0.5*objdiam, 0.5*objdiam, $
                   strmid(line,20) 
        endif else begin
           printf, wlun, line
        endelse

        ctr++
     endwhile        
        
     free_lun, rlun, wlun

     if (objdiam eq 0) then begin
        file_copy, fnroot+'_center.dat', 'occmap_center.dat', /overwrite
        file_copy, fnroot+'.plt', 'occmap.plt', /overwrite
        kmlgen,'occmap_center.dat', objcencol, 'shadow.kml', $
               objname+' center line', objname, geomid, version+'; '+kernel
     endif else begin           
        ; Run ocmapex to produce shadow path
        openw,  lun, 'mkcenter', /get_lun
        printf, lun, '#!/bin/csh'
        printf, lun
        printf, lun
        printf, lun, 'rm occmap*.dat'
        printf, lun
        printf, lun, 'ocmapex << EOI'
        printf, lun, 'o'
        printf, lun, objids[i]
        printf, lun, 'occmap.in'
        printf, lun, 'EOI'
        printf, lun
        printf, lun, 'mv tempor.tmp occmap.plt'
        printf, lun, 'mv ocmap.out occmap.out'
        printf, lun, 'if (-f center.out) then'
        printf, lun, '  mv center.out occmap_center.dat'
        printf, lun, 'endif'
        printf, lun, 'if (-f north.out) then'
        printf, lun, '  mv north.out occmap_north.dat'
        printf, lun, 'endif'
        printf, lun, 'if (-f south.out) then'
        printf, lun, '  mv south.out occmap_south.dat'
        printf, lun, 'endif'
        free_lun, lun

        file_chmod, 'mkcenter', /a_execute
        spawn, 'mkcenter', result
        file_delete, 'mkcenter'

        files  = ['occmap_north.dat','occmap_center.dat','occmap_south.dat']
        colors = [objcol, objcencol, objcol]
        desc   = [objname+' northern limb', objname+' center line', $
                  objname+' southern limb']
           
        kmlgen, files, colors, 'shadow.kml', desc, objname, geomid, $
                version+'; '+kernel
     endelse


     ; Create occroi.kml, if requested
     if (objroilimsin ne '[[[none]]]') then begin
        print, 'Building occroi.kml...'

        spawn, 'rm occroi*.*'

        for j=0,n_elements(objroilims)-1 do begin
           openr, rlun, fnroot+'.in', /get_lun
           openw, wlun, 'occroi'+strtrim(j,2)+'.in', /get_lun

           ctr = 0
           line = ''

           while not eof(rlun) do begin
              readf, rlun, line

              if (ctr eq 1) then begin
                 printf, wlun, f='(2f10.1,a)', abs(objroilims[j]), $
                         abs(objroilims[j]), strmid(line,20) 
              endif else begin
                 printf, wlun, line
              endelse

              ctr++
           endwhile

           free_lun, rlun, wlun


           ; Run ocmapex to produce OBJROIs
           openw,  lun, 'mkobjroi'+strtrim(j,2), /get_lun
           printf, lun, '#!/bin/csh'
           printf, lun
           printf, lun
           printf, lun, 'ocmapex << EOI'
           printf, lun, 'o'
           printf, lun, objids[i]
           printf, lun, 'occroi'+strtrim(j,2)+'.in'
           printf, lun, 'EOI'
           printf, lun
           printf, lun, 'rm tempor.tmp'
           printf, lun, 'mv ocmap.out occroi'+strtrim(j,2)+'.out'
           printf, lun, 'if (-f center.out) then'
           printf, lun, '  rm center.out'
           printf, lun, 'endif'

           if (objroilims[j] lt 0) then begin
              printf, lun, 'if (-f north.out) then'
              printf, lun, '  rm north.out'
              printf, lun, 'endif'
              printf, lun, 'if (-f south.out) then'
              printf, lun, '  mv south.out occroi'+strtrim(j,2)+'_path.dat'
              printf, lun, 'endif'
           endif else begin
              printf, lun, 'if (-f north.out) then'
              printf, lun, '  mv north.out occroi'+strtrim(j,2)+'_path.dat'
              printf, lun, 'endif'
              printf, lun, 'if (-f south.out) then'
              printf, lun, '  rm south.out'
              printf, lun, 'endif'
           endelse

           free_lun, lun

           file_chmod, 'mkobjroi'+strtrim(j,2), /a_execute
           spawn, 'mkobjroi'+strtrim(j,2), result
           file_delete, 'mkobjroi'+strtrim(j,2)
        endfor

        files = file_search('occroi*_path.dat')
        kmlgen, files, objroicol, 'occroi.kml', objroidesc, /roi
     endif

     if (objids[i] ne globalid) then begin
        cd, '..'
     endif
  endfor
  

  ; Don't run plotdat if we don't need to
  if ((sunaltin eq '[[[none]]]') and (staraltin eq '[[[none]]]')) then begin
     spawn, 'rm occmap.ps sunalt.dat sunalt.kml staralt.dat staralt.kml'
  endif else begin
     print, 'Building sunalt.kml and/or staralt.kml...'

     openw, lun, 'doplot', /get_lun
     printf, lun, '#!/bin/csh'
     printf, lun
     printf, lun, 'plotdat << EOI'
     printf, lun, 'o'
     printf, lun, 'x x'
     printf, lun, 'x'
     printf, lun, '38,-98'
     printf, lun, '5'
     printf, lun, '9.5'
     printf, lun, '6.8'
     printf, lun, 'occmap.plt'
     printf, lun, 'y'
     printf, lun

     datetime = strsplit(geomid, ' ', /extract)
     printf, lun, datetime[0]+' occultation by '+objname+' ['+version+']'
     printf, lun, 'n'
     printf, lun, 'n'

     if (sunaltin eq '[[[none]]]') then begin
        printf, lun, 'n'
     endif else begin
        printf, lun, 'y'

        date = strsplit(datetime[0],'-',/extract)
        time = strsplit(datetime[1],':',/extract)
        sec  = string(f='(i02)',round(float(time[2])))
        printf, lun, date[0] + ',' + date[1] + ',' + date[2] + ',' + $
                time[0] + ',' + time[1] + ',' + sec

        for j=0,n_elements(sunalt)-1 do $
           printf, lun, sunalt[j]

        printf, lun, '99'
     endelse
     
     if (staraltin eq '[[[none]]]') then begin
        printf, lun, 'n'
     endif else begin
        printf, lun, 'y'

        ra = strsplit(ora,':',/extract)
        sec  = string(f='(f04.1)',0.1*round(float(ra[2]*10)))
        printf, lun, ra[0] + ',' + ra[1] + ',' + sec

        dec = strsplit(odec,':',/extract)
        sec  = string(f='(i02)',round(float(dec[2])))
        printf, lun, dec[0] + ',' + dec[1] + ',' + sec

        for j=0,n_elements(staralt)-1 do $
           printf, lun, staralt[j]

        printf, lun, '99'
     endelse

     printf, lun, 'EOI'
     printf, lun
     printf, lun, 'set plotfile=`ls -rt *.ps | tail -1`'
     printf, lun
     printf, lun, 'mv $plotfile occmap.ps'

     free_lun, lun

     file_chmod, 'doplot', /a_execute
     spawn, 'doplot', result
     file_delete, 'doplot'
           
     if (sunaltin ne '[[[none]]]') then begin
        file_move, 'sunll.tmp', 'sunalt.dat', /overwrite
        kmlgen, 'sunalt.dat', suncol, 'sunalt.kml', /sun
     endif

     if (staraltin ne '[[[none]]]') then begin
        file_move, 'objll.tmp', 'staralt.dat', /overwrite
        kmlgen, 'staralt.dat', starcol, 'staralt.kml', /star
     endif
  endelse
  
end
