;+
; NAME:
;  srodcheck
; PURPOSE:   (one line only)
;  Widget tool for verification of SRO data reduction results
; DESCRIPTION:
; CATEGORY:
;  CCD data processing
; CALLING SEQUENCE:
;  srodcheck
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
;  Written by Marc W. Buie, Southwest Research Institute, 2021/02/05
;  2024/07/06, MWB, rework for obs tool upgrades
;-
pro srodcheck_cleanup, tlb

   ; get pointer to state structure
   widget_control, tlb, get_uvalue=state

   ; Free up any dynamically allocated space with the state structure
   ptr_free,(*state).stackidx
   ptr_free,(*state).sidfile
   ptr_free,(*state).objfile
   ptr_free,(*state).exptime
   ptr_free,(*state).jdstart
   ptr_free,(*state).jdmid
   ptr_free,(*state).jdend
   ptr_free,(*state).objid
   ptr_free,(*state).photidx
   ptr_free,(*state).fwhmstk
   ptr_free,(*state).grade

   ; Free up the state structure itself.
   ptr_free, state

end

pro srodcheck_getstacklist,state

   widget_control,(*state).objinputid,get_value=objectsel

   (*state).objectsel = trimrank(objectsel)
   (*state).curstack = -1
   (*state).object = ''

   openmysql,dblun,'swriobs'

   cmd=['select idx,sidfile,objfile,exptime,jdstart,jdmid,jdend,', $
        'objid,photidx,fwhmstk,grade', $
        'from stack', $
        'where grade is NULL', $
        'and photidx is not NULL']
   if (*state).objectsel ne '' then $
      cmd=[cmd,'and sidfile like '+quote((*state).objectsel+'%')]
   cmd=[cmd,'order by jdmid;']

   mysqlquery,dblun,cmd,idx,sidfile,objfile,exptime,jdstart,jdmid,jdend, $
                        objid,photidx,fwhmstk,grade, $
      format='l,a,a,f,d,d,d,a,l,f,a',ngood=nstacks

   free_lun,dblun

   str=strn(nstacks)+' stacks'

   widget_control,(*state).stackinfoid,set_value=str

   (*state).nstacks = nstacks
   if nstacks gt 0 then begin
      ptr_free,(*state).stackidx
      ptr_free,(*state).sidfile
      ptr_free,(*state).objfile
      ptr_free,(*state).exptime
      ptr_free,(*state).jdstart
      ptr_free,(*state).jdmid
      ptr_free,(*state).jdend
      ptr_free,(*state).photidx
      ptr_free,(*state).fwhmstk
      ptr_free,(*state).grade
      ptr_free,(*state).objid
      (*state).stackidx= ptr_new(idx)
      (*state).sidfile = ptr_new(sidfile)
      (*state).objfile = ptr_new(objfile)
      (*state).exptime = ptr_new(exptime)
      (*state).jdstart = ptr_new(jdstart)
      (*state).jdmid   = ptr_new(jdmid)
      (*state).jdend   = ptr_new(jdend)
      (*state).photidx = ptr_new(photidx)
      (*state).fwhmstk = ptr_new(fwhmstk)
      (*state).grade   = ptr_new(grade)
      (*state).objid   = ptr_new(objid)
   endif

end

pro srodcheck_loaddata,state

   if (*state).curstack eq -1 then return

   str=strn((*state).curstack+1)+' of '+strn((*state).nstacks)+' stacks'
   widget_control,(*state).stackinfoid,set_value=str

   dir='/net/mandor/raid/buie/sro/sciproc/'

   curstate = (*(*state).grade)[(*state).curstack]

   words=strsplit((*(*state).objfile)[(*state).curstack],'_',/extract)
   if words[0] ne (*state).object then update=1 else update=0
   (*state).object=words[0]
   (*state).rundate=words[1]
   (*state).telidx=words[2]

   if (*state).progcode eq '' or update then begin
      openmysql,dblun,'swriobs'
      cmd=['select stidx,code from image,program', $
           'where stackidx='+strn((*(*state).stackidx)[(*state).curstack]), $
           'and program.idx=propidx', $
           'limit 1;']
      mysqlquery,dblun,cmd,stidx,progcode,format='l,a'
      free_lun,dblun
      (*state).progcode=progcode
      (*state).stidx=stidx
   endif

   stubdir=(*state).progcode+'/'+strmid((*state).rundate,0,4)+'/'
   if not exists(dir+stubdir+(*(*state).sidfile)[(*state).curstack]) then return
   if not exists(dir+stubdir+(*(*state).objfile)[(*state).curstack]) then return

   sim=readfits(dir+stubdir+(*(*state).sidfile)[(*state).curstack],shdr)
   astinfo,shdr,ainfo,error
   tim=readfits(dir+stubdir+(*(*state).objfile)[(*state).curstack],ohdr)
   photzp=sxpar(ohdr,'PHOTZP')
   photzperr=sxpar(ohdr,'PHOTZPER')

   openmysql,dblun,'phot'
   cmd=['select ra,decl,raerr,decerr,mag,err,obscode', $
        'from data where idx='+strn((*(*state).photidx)[(*state).curstack])+';']
   mysqlquery,dblun,cmd,ra,dec,raerr,decerr,mag,err,obscode, $
      format='d,d,d,d,f,f,a',ngood=ncheck
   free_lun,dblun
;print,cmd
;help,obscode
;print,'sidfile ',(*(*state).sidfile)[(*state).curstack]
;print,'stackidx',(*(*state).stackidx)[(*state).curstack]
;print,'photidx ',(*(*state).photidx)[(*state).curstack]
;print,'grade   ',(*(*state).grade)[(*state).curstack]

   jdmid=(*(*state).jdmid)[(*state).curstack]

   ephem,jdmid,obscode,52, $
         (*(*state).objid)[(*state).curstack],eph
   ephra=eph[0]
   ephdec=eph[1]

   ephem,(*(*state).jdstart)[(*state).curstack],obscode,52, $
         (*(*state).objid)[(*state).curstack],eph1
   ephem,(*(*state).jdend)[(*state).curstack],obscode,52, $
         (*(*state).objid)[(*state).curstack],eph2

   astcvt,'rd',ephra,ephdec,ainfo,'xy',ephx,ephy
   astcvt,'rd',ra,dec,ainfo,'xy',x,y
   astcvt,'rd',eph1[0],eph1[1],ainfo,'xy',ephx1,ephy1
   astcvt,'rd',eph2[0],eph2[1],ainfo,'xy',ephx2,ephy2

   boxm,tim,x,y,2,2,xpeak,ypeak
   objpeak=tim[xpeak,ypeak]

   i0=round(ephx-(*state).dw)
   i1=round(ephx+(*state).dw)
   j0=round(ephy-(*state).dw)
   j1=round(ephy+(*state).dw)
   mkcircle,0.,0.,(*(*state).fwhmstk)[(*state).curstack],xc,yc

   skysclim,tim,loval,hival,meanval,sigma
   hival=meanval+8.0*sigma

   subarr,sim,i0,i1,j0,j1,sidsub,error,fill=meanval
   subarr,tim,i0,i1,j0,j1,objsub,error,fill=meanval

   bsidsub=bytscl(sidsub,min=loval,max=hival,top=255)
   bobjsub=bytscl(objsub,min=loval,max=hival,top=255)

   dns=strn(round(loval))+' to '+strn(round(hival))+')'
   widget_control,(*state).dnlab,set_value=dns

   jdstr,jdmid,0,jds
   widget_control,(*state).utlab,set_value=jds
   jd2year,jdmid,year

   ra_now=ra
   dec_now=dec
   precess,ra_now,dec_now,2000.0,year,/radian
   am=airmass(jdmid,ra_now,dec_now,(*state).lat,(*state).lon)

   sunpos,jdmid,sunra,sundec,/radian
   precess,sunra,sundec,2000.0,year,/radian
   sam=airmass(jdmid,sunra,sundec,(*state).lat,(*state).lon, $
               lst=lst,lha=slha,alt=salt,/hardie)
   sel=angsep(ra_now,dec_now,sunra,sundec)*!radeg

   moonpos,jdmid,moonra,moondec,/radian
   mphase,jdmid,moonphase
   precess,moonra,moondec,2000.0,year,/radian
   mam=airmass(jdmid,moonra,moondec,(*state).lat,(*state).lon, $
               lha=mlha,alt=malt,/hardie)

   airms='X='+strn(am,format='(f10.2)')
   widget_control,(*state).airmlab,set_value=airms

   sinfo='(S '+strn(salt*!radeg,format='(f10.1)')+ $
              ','+strn(round(sel))+')'
   widget_control,(*state).sunlab,set_value=sinfo

   if malt lt 0 then begin
      minfo='(M down)'
   endif else begin
      mel=angsep(ra_now,dec_now,moonra,moondec)*!radeg
      minfo='(M '+strn(malt*!radeg,format='(f10.1)')+ $
              ','+strn(moonphase,format='(f10.2)')+ $
              ','+strn(round(mel))+')'
   endelse
   widget_control,(*state).moonlab,set_value=minfo

   zpstr='ZP = '+strn(photzp,format='(f10.3)')+' +/- '+ $
               strn(photzperr,format='(f10.3)')
   widget_control,(*state).zplab,set_value=zpstr

   widget_control,(*state).sidwin,get_value=sidwin
   widget_control,(*state).objwin,get_value=objwin

   wset,sidwin
   tvscl,bsidsub
   plot,[0],[1],/nodata,xmargin=[0,0],ymargin=[0,0], $
      xr=[i0,i1],yr=[j0,j1],xstyle=5,ystyle=5,/noerase
   oplot,[ephx1,ephx2],[ephy1,ephy2],psym=-4,color=cpalette(1),symsize=2
   wcsarrows,ainfo,ainfo=oinfo,color=cpalette(1), $
      x=60,y=(*state).sz-60
   wset,objwin
   tvscl,bobjsub
   plot,[0],[1],/nodata,xmargin=[0,0],ymargin=[0,0], $
      xr=[i0,i1],yr=[j0,j1],xstyle=5,ystyle=5,/noerase
   oplot,xc+x,yc+y,color=cpalette(2)
   astmark,ephx,ephy,color=cpalette(1),/data

   snr=1.085736205/err

   fwhm=(*(*state).fwhmstk)[(*state).curstack]
   fwhm_as=fwhm*oinfo.pscale
   str1=string('fwhm=',fwhm,'pix,',fwhm_as,' as', $
               format='(a,1x,f10.2,1x,a,1x,f10.2,1x,a)')
   str1=strcompress(str1)

   str2=string('Eph x,y',ephx,',',ephy,format='(a,1x,f10.2,a,1x,f10.2)')
   str2=' '+strcompress(str2)

   str3=string('Meas x,y',x,',',y,format='(a,1x,f10.2,a,1x,f10.2)')
   str3=strcompress(str3)

   str4=string('diff x,y',x-ephx,',',y-ephy,'pixels', $
                format='(a,1x,f10.2,a,1x,f10.2,1x,a)')
   str4=strcompress(str4)

   if abs(x-ephx) gt 2 or abs(y-ephy) gt 2 then begin
      str5=string('diff x,y',(x-ephx)*oinfo.pscale,',', $
                             (y-ephy)*oinfo.pscale,'as', $
                   format='(a,1x,f10.2,a,1x,f10.2,1x,a)')
   endif else begin
      str5=string('diff x,y',(x-ephx)*oinfo.pscale*1000.0,',', $
                             (y-ephy)*oinfo.pscale*1000.0,'mas', $
                   format='(a,1x,f10.1,a,1x,f10.1,1x,a)')
   endelse
   str5=strcompress(str5)

   str6=string('Mag =',mag,'+/-',err,'SNR=',snr, $
                format='(a,1x,f10.4,1x,a,1x,f10.4,1x,a,1x,f10.1)')
   str6=strcompress(str6)

   str7=string('Sky =',meanval,', objpeak =',objpeak, $
               format='(a,1x,f7.1,a,1x,f7.1)')

   str=[str1,str7,str2,str3,str4,str5,str6]

   widget_control,(*state).fileinfoid,set_value=str

   z=trimrank(where(curstate eq (*state).gradelist))
   if z eq -1 then z=(*state).ngrades
   widget_control,(*state).gradeid,set_value=z[0]

end

pro srodcheck_refreshdisplay,state

;print,'refresh display'

   if (*state).curstack eq -1 then begin
      widget_control,(*state).filenameid,set_value=''
   endif else begin
      str=(*(*state).objfile)[(*state).curstack]
      widget_control,(*state).filenameid,set_value=str
   endelse

end

pro srodcheck_eve, event

   widget_control, event.top, GET_UVALUE=state

   if event.id eq (*state).mainbase then $
      event_name = 'Mainbase' $
   else $
      widget_control, event.id,  GET_UVALUE=event_name, /HOURGLASS

   exit = event_name eq 'THE_MENU'
   if exit then exit = event.value eq 'Exit'

   case event_name of

      'THE_MENU': begin
         case event.value of

;            : begin
;            end

            'Exit' : begin
               widget_control, event.top, /DESTROY
               return
            end

            else: begin
               message, 'Unknown menu event:', /INFO
               help, event, /STRUCTURE
            end

         endcase

      end ; THE_MENU

      'Mainbase': begin

         ; Use if you have other widgets on screen, need to take off their
         ;   size from the event x,y size.
;         info=widget_info((*state).colbaseid,/geometry)
;         widget_control,(*state).drawwin,xsize=event.x,ysize=event.y-info.ysize

         ; Use if draw window is only thing in the tool.
         widget_control,(*state).drawwin,xsize=event.x,ysize=event.y
;         widget_control,(*state).drawwin,xsize=event.x,ysize=event.y-info.ysize

;        refresh window here
      end

      'Grade Set': begin
         if (*state).nstacks eq 0 then return
         if event.value eq (*state).ngrades then return
         newgrade=(*state).gradelist[event.value]
         (*(*state).grade)[(*state).curstack]=newgrade
         openmysql,dblun,'swriobs'
         cmd=['update stack', $
              'set grade='+quote(newgrade), $
              'where idx='+strn((*(*state).stackidx)[(*state).curstack]), $
              ';']
         mysqlcmd,dblun,cmd
         if newgrade ne 'g' then begin
            cmd=['update phot.data', $
                 'set bad=1', $
                 'where idx='+strn((*(*state).photidx)[(*state).curstack]), $
                 ';']
            mysqlcmd,dblun,cmd
         endif
         free_lun,dblun
         (*state).curstack=((*state).curstack+1)<((*state).nstacks-1)
         srodcheck_loaddata,state
         srodcheck_refreshdisplay,state
      end

      'Next Stack': begin
         if (*state).nstacks eq 0 then return
         if (*state).curstack eq -1 then (*state).curstack=0 $
         else (*state).curstack=((*state).curstack+1)<((*state).nstacks-1)
         srodcheck_loaddata,state
         srodcheck_refreshdisplay,state
      end

      'Object Entry': begin
         srodcheck_getstacklist,state
         srodcheck_refreshdisplay,state
      end

      'Prev Stack': begin
         if (*state).nstacks eq 0 then return
         if (*state).curstack eq -1 then (*state).curstack=(*state).nstacks-1 $
         else (*state).curstack=((*state).curstack-1)>0
         srodcheck_loaddata,state
         srodcheck_refreshdisplay,state
      end

      else: begin
         print,'EVENT NAME: ',event_name
         message, 'Unknown event:', /INFO
         help, event, /STRUCTURE
      end

   endcase

end ; end of event handler

pro srodcheck

   ; optional
   if xregistered('srodcheck') then return

   if (!d.flags and 256) eq 0 then begin
      print, 'Error. No windowing device. SRODCHECK cannot be started.'
      return
   endif

   dw=100
   sz=2*dw+1
   pdw=10
   pzf=2
   psz=2*pdw+1

   ;Define the main base.
   mainbase = widget_base( TITLE='SRODCHECK: Data Check Tool', $
                           /COLUMN, UVALUE=0, MBAR=bar, /TLB_SIZE_EVENTS )

   menu = CW_PdMenu(bar, /RETURN_NAME, $
                    ['1\File',$
                     '0\action 1',$
                     '0\action 2',$
                     '2\Exit',$
                     '1\Tools',$
                     '0\tool 1', $
                     '0\tool 2', $
                     '2\tool 3'], UVALUE='THE_MENU', /MBAR)

   openmysql,dblun,'swriobs'
   cmd='select grade,sdesc,ldesc from gradeinfo'+ $
       ' where dorder>0 order by dorder,grade;'
   mysqlquery,dblun,cmd,grade,sdesc,ldesc,format='a,a,a',ngood=ngrades
   free_lun,dblun

   bt = widget_base(mainbase,/row)
   t1 = widget_label(bt,value='(DN')
   t_dns = widget_label(bt,value='',/dynamic_resize)
   t_ut = widget_label(bt,value='',/dynamic_resize)
   t_airm = widget_label(bt,value='',/dynamic_resize)
   t_salt = widget_label(bt,value='',/dynamic_resize)
   t_malt = widget_label(bt,value='',/dynamic_resize)
   t_zp = widget_label(bt,value='',/dynamic_resize)

   base = widget_base(mainbase,/row)

   win1 = widget_draw( base, XSIZE=sz, YSIZE=sz, RETAIN=2, $
                       UVALUE='objWindow')

   cbase=widget_base(base,/col)
   b2 = widget_base(cbase,row=1)
   t1 = widget_label(b2,value='Object',/align_right,/dynamic_resize)
   objid = widget_text( b2, value='', uvalue='Object Entry',/align_left,/editable )

   b2 = widget_base(cbase,row=1)
   t1 = widget_button(b2,value='Prev',uvalue='Prev Stack')
   t1 = widget_button(b2,value='Next',uvalue='Next Stack')
   stackinfoid=widget_label(b2,value='no stacks',/dynamic_resize)

;   t1=widget_label(cbase,value='Image information')
   filenameid =widget_label(cbase,value='',/dynamic_resize)
   fileinfoid =widget_text(cbase,value='',xsize=40,ysize=7)

;   win3 = widget_draw( cbase, XSIZE=pzf*psz, YSIZE=pzf*psz, RETAIN=2, $
;                       UVALUE='psfWindow' )

   win2 = widget_draw( base, XSIZE=sz, YSIZE=sz, RETAIN=2, $
                       UVALUE='sidWindow' )

   sdesclist=[sdesc,'no grade']
   gradeid = cw_bgroup(mainbase,sdesclist,/row,/exclusive, $
                         /return_index,set_value=ngrades,uvalue='Grade Set', $
                         /no_release)

   getobloc,'G80',obs

   state = ptr_new({ $

      ; Data and information in the widget
      dw: dw, $                 ; half width and height of ROI window
      sz: sz, $                 ; full width and height of ROI window
      pdw: dw, $                ; half width and height of psf window
      psz: sz, $                ; full width and height of psf window
      pzf: pzf, $               ; zoom factor on psf
      objectsel: '', $          ; name of object to select
      object: '', $             ; name of current object
      stidx: 0L, $
      progcode: '', $
      rundate: '', $
      telidx: '', $
      stackidx: ptr_new(), $
      objid: ptr_new(), $
      sidfile: ptr_new(), $
      objfile: ptr_new(), $
      exptime: ptr_new(), $
      jdstart: ptr_new(), $
      jdmid: ptr_new(), $
      jdend: ptr_new(), $
      photidx: ptr_new(), $
      fwhmstk: ptr_new(), $
      grade: ptr_new(), $
      nstacks:  0, $
      curstack: -1, $
      gradelist: grade, $
      sdesclist: sdesclist, $
      ldesclist: ldesc, $
      ngrades: ngrades, $
      lat:     obs.lat, $
      lon:     obs.wlon, $

      ; Widget ids
      objinputid: objid, $
      stackinfoid: stackinfoid, $
      filenameid: filenameid, $
      fileinfoid: fileinfoid, $
      gradeid: gradeid, $
      objwin: win1, $           ; ID of main draw window
      sidwin: win2, $           ; ID of main draw window
;      psfwin: win3, $           ; ID of main draw window
      dnlab:  t_dns, $
      utlab:  t_ut, $
      airmlab: t_airm, $
      sunlab: t_salt, $
      moonlab: t_malt, $
      zplab: t_zp, $

      mainbase: mainbase $       ; ID of top level base.

      })

   ;Stash the state structure pointer.
   widget_control, mainbase, SET_UVALUE=state

   ;Realize the main base.
   widget_control, mainbase, /REALIZE

   ; Give control to the XMANAGER.
   xmanager, 'srodcheck', mainbase, $
             EVENT_HANDLER='srodcheck_eve',/NO_BLOCK, $
             GROUP_LEADER=mainbase, CLEANUP='srodcheck_cleanup'

end
