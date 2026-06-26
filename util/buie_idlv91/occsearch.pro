;+
; NAME:
;  occsearch
; PURPOSE:   (one line only)
;  Search one or more object for occultations
; DESCRIPTION:
; CATEGORY:
;  Occultations
; CALLING SEQUENCE:
;  occsearch,set
; INPUTS:
;  set - Name of a set.  There will be a file set+'.set' that must contain
;          a list of objects.  This can be the output of newlist.pro and
;          must have the geteph prefix for the object.
;        It is also required to have a configuration file, set+'.ini' with
;          control information.  The following must be in the [global]
;          section:
;            fnlist - name of a file that must contain a lits of objects.
;                     This can be the output of newlist.pro and must have
;                     the geteph prefix for the object.
;            date1  - Gregorian calendar date for the beginning of the
;                       search window
;            date2  - Gregorian calendar date for the end of the
;                       search window
;            geominsep - maximum geocentric minimum separation [arcsec]
;            minsel  - minimum solar elongation allowed [degrees]
;INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
; OUTPUTS:
;  Lots and lots of files in a direc.
; KEYWORD OUTPUT PARAMETERS:
; CONFIGURATION:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  Written by Marc W. Buie, Southwest Research Institute, 2023/03/25
;  2023/04/04, MWB, added a hack to the spawn command to run convert to
;                      bypass a problem with IDL.
;  2024/11/13, MWB, changed convert command to "magick".  Dropped geominsep
;                 from .ini file.  This is now dyanamically calculated
;                 for each appulse.
;-
pro occsearch,set

   self='occsearc: '
   if badpar(set,7,0,caller=self+'(set) ') then return

   fnini=set+'.ini'

   if nofile(fnini,'configuration') then return

   loadini,file=fnini,info

   getvalue,info,'global','fnlist',fnlist
   getvalue,info,'global','date1',date1s
   getvalue,info,'global','date2',date2s
;   getvalue,info,'global','geominsep',seplimit,type=4
   getvalue,info,'global','minsel',minsel,type=4

   if date1s eq '' then return
   if date2s eq '' then return

   if nofile(fnlist,'Occultating body list') then return

   print,'Occultating body list ',fnlist
   print,'Time range ',date1s,' to ',date2s
   jd1=jdparse(date1s)
   jd2=jdparse(date2s)

   readcol,fnlist,objid,format='a',count=nobjs
   print,strn(nobjs),' objects found in list'
   if nobjs eq 0 then return

   if not exists(set) then file_mkdir,set

   spawn,'getinfo',unit=plun

   for i=0,nobjs-1 do begin

      if strmid(objid[i],0,1) eq 'P' then begin
         cobjid=objid[i]
      endif else begin
         cobjid=strmid(objid[i],1)
      endelse

      getvalue,info,objid[i],'name',name,type=7,default=''
      if name ne '' then begin
         getvalue,info,objid[i],'diameter',diameter,type=4,default=10.0
         getvalue,info,objid[i],'albedo',albedo,type=4,default=0.05
         getvalue,info,objid[i],'etrack',etrack,type=4,default=10.0
         getvalue,info,objid[i],'etime',etime,type=4,default=1.0
         hv=diamptoh(diameter,albedo)
         hvfororbit=diamptoh(diameter,0.3)
         supp={name:name, diameter:diameter, hv:hv, albedo:albedo, $
               etrack: etrack, etime: etime}
      endif else begin
         supp=0
      endelse

      fnxyz=cobjid+'.xyz'
      fnout=cobjid+'.out'

      if not exists(set+'/'+fnout) then begin
         cmd1='orbint_r '+cobjid+string(jd1,jd2,format='(2(1x,f9.1))')
         if name ne '' then begin
            tmpname=nobname(name)
            tmpname=repchar(tmpname,'_','-')
            cmd1=cmd1+' '+tmpname+' '+strn(hvfororbit,format='(f10.2)')
         endif
         print,cmd1
         spawn,cmd1,result
         cmd2='catprd_r '+cobjid
         print,cmd2
         spawn,cmd2,result
         file_move,fnxyz,set,/overwrite
         file_move,fnout,set,/overwrite
      endif

      pushd,set

      readcol,fnout,starid,geominsep,etmid,ras,decs, $
         gmag,band,melong, $
         format='a,f,d,x,x,a,a,f,a,f',count=napp
      jdmid=etmid-etut(etmid)/86400.0d0

      ra=raparse(ras)
      dec=decparse(decs)
      jdstr,jdmid,300,jds
      jdstr,jdmid,0,jds_pretty

      ephem,jdmid,'500',22,objid[i],eph
      ssgeom,eph,sun,earth,phang,elong,kscale
      rearth = 6378./kscale

      line=''
      for j=0,napp-1 do begin
         if (geominsep[j] gt rearth[j]) then continue

         sunpos,jdmid[j],sunra,sundec,/radian
         selong=angsep(ra[j],dec[j],sunra,sundec)*!radeg
         if selong lt minsel then continue

         fnps=cobjid+'_'+jds[j]+'_W.ps'
         fnpng=cobjid+'_'+jds[j]+'_W.png'
         fntpng=cobjid+'_'+jds[j]+'_Wt.png'

         if not exists(fnps) then begin
            cmd3='ocmap_r '+cobjid+ $
                 ' '+string(jdmid[j],format='(f13.5)')+ $
                 ' '+string(ra[j],format='(f12.10)')+ $
                 ' '+string(dec[j],format='(f13.10)')+ $
                 ' '+starid[j]+' '+string(gmag[j],format='(f5.2)')+' '+band[j]+ $
                 ' '+fnps+' W'
            print,cmd3
            spawn,cmd3,result
         endif

         if not exists(fnpng) then begin
            if name eq '' then begin
               printf,plun,objid[i]
               readf,plun,line,format='(a)'
               line=strmid(line,0,34)
               line=strtrim(strcompress(line),2)
               title=line+' '+jds_pretty[j]
            endif else begin
               title=name+' '+jds_pretty[j]
            endelse
            mags=strn(gmag[j],format='(f8.2)')+' '+band[j]

            appuldis,objid[i],'500',jdparse(jds_pretty[j]),ra[j],dec[j],$
                     jdmin,sep,starerr=0.001,info=ginfo,supp=supp
            gstar=gmag[j]-2.5*alog10((ginfo.diam1/40)/(ginfo.speed/20))
            mstar=strn(gstar,format='(f8.2)')+' '+band[j]+'*'

            cmd5='unsetenv LD_LIBRARY_PATH ;'+ $
                 ' magick '+fnps+' -crop 320x320+140+310'+ $
                 ' -gravity south -pointsize 14 -fill '+quote('#100090')+ $
                 ' -annotate +0+1070 '+quote(title)+ $
                 ' -annotate +120+790 '+quote(mags)+ $
                 ' -annotate -120+790 '+quote(mstar)+ $
                 ' '+fntpng
            print,cmd5
            spawn,cmd5,result

            cmd5='unsetenv LD_LIBRARY_PATH ; magick '+fnps+' '+fnpng
            print,cmd5
            spawn,cmd5,result
         endif
      endfor
      popd

   endfor

bailout:
   free_lun,plun

end
