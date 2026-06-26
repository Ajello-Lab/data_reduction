;+
; NAME:
;  wastrom
; PURPOSE:   (one line only)
;  Widget for determining an interactive astrometric solution
; DESCRIPTION:
;  This is a widget tool for interactively guiding an astrometric solution.
;     This is not a general program in the sense that a lot of work needs to
;     done to get ready to call this tool.  In addition to an image and some
;     rough guess on the WCS, you have to provide the source and catalog
;     lists.  The WCS guess can be pretty rough but the chirality of the image
;     must be correctly declared with the "flip" input variable.  You can
;     lock in on the real solution for all but the chirality.
;  This is a model widget and will block all other activities while it runs.
;     Upon return, the astrometric solution is supplied along with a flag
;     that indicates if the operation was successful.  This tool works equally
;     well if called from within a widget heirarchy or a normal IDL procedure.
;     It is not really intended to be called directory by the user.  It needs
;     a lot of setup and will usually be embedded in a larger program that
;     will call this tool as needed.
; CATEGORY:
;  Astrometry
; CALLING SEQUENCE:
;  wastrom,im,ainfo,flip,sra,sdec,mag,xsrc,ysrc,binfo,success
; INPUTS:
;  im - image to process (used for display only)
;  ainfo - initial astrometric information structure (see mkastinfo.pro)
;  flip  - Flag, if set means the image is inverted.
;  sra   - Catalog RA (radians) of sources
;  sdec  - Catalog Dec (radians) of sources
;  mag   - Catalog magnitude of sources
;  xsrc  - x pixel location of source in image
;  ysrc  - y pixel location of source in image
; OPTIONAL INPUT PARAMETERS:
;  GROUP - widget id of the group leader (providing this is entirely optional)
; KEYWORD INPUT PARAMETERS:
; OUTPUTS:
;  binfo   - fitted astrometric solution for image (useful if success=1)
;  success -  Flag, if set means the operation was a success and binfo
;               will contain a useful, if not final, astrometric solution
; KEYWORD OUTPUT PARAMETERS:
; CONFIGURATION:
; COMMON BLOCKS:
;  mwb_wastrom: used to return the output information
; SIDE EFFECTS:
; RESTRICTIONS:
;  Currently hardcoded for linear plate solutions.
; PROCEDURE:
;  The first two sources must be done entirely manually.  The first source
;    is used to get an offset to line up the catalog against one source in
;    the image.  Click left to select the source in the image (green X), click
;    middle to select the corresponding source in the catalog (red diamonds).
;    The order of providing these two items does not matter.  When both are
;    selected, the GUI will show 'src <-> cat' and other ancillary information
;    that indicates a valid pair has been selected.  To link this pair
;    together, click the "Link" button.  At this point the catalog will snap
;    to the selected source.  The rotation and scale could be completely 
;    wrong at this point.  At the end of this first step, you do not yet have
;    a valid solution and need to move on to specifying a second source.
;  The second source is selected in the same way as the first.  Note that
;    once a source is used from the source list and catalog it cannot be
;    selected again.  After linking the second source you now should have a
;    good scale and orientation but this solution is still not complete.  If
;    you exit at this point the caller will be told the action was unsuccessful.
;    You must have three or more linked stars to have a valid WCS solution.
;  Next, you can either continue adding sources one at a time, or, clicking
;    "Auto Match".  If you opt for the automatic tool, it will match all
;    sources it can.  However, sources too close to the edge are ignored.
;    Those sources that are being used to constrain the solution have different
;    markings on the image from those that are still candidates for linking.
;
;  As mentioned earlier, you cannot manually measure new sources to add to the
;    source list.  This is a firm design choice since there are many ways to
;    measure sources and this tool does not presume to know how to do this.
;    Mixing measurement techniques is bad for astrometry, hence the reason
;    you must provide positions for sources that are externally determined.
;    This also gives you full control over the reference catalog as well.G
; MODIFICATION HISTORY:
;  2021/11/24, Written by Marc W. Buie, Southwest Research Institute
;  2021/11/30, MWB, fixed a problem with flipped data.
;-
pro wastrom_cleanup, tlb

   common mwb_wastrom,com_binfo,com_success

   ; get pointer to state structure
   widget_control, tlb, get_uvalue=state

   com_binfo=(*(*state).binfo)
   com_success=(*state).success

   ; Free up any dynamically allocated space with the state structure
   ptr_free,(*state).ind1
   ptr_free,(*state).ind2
   ptr_free,(*state).binfo

   ; Free up the state structure itself.
   ptr_free, state

end

pro wastrom_addpair, state, NOUPDATE=noupdate

   if (*state).tind1 lt 0 or (*state).tind2 lt 0 then return

   if (*state).npairs eq 0 then begin
      ptr_free,(*state).ind1
      ptr_free,(*state).ind2
      ind1 = [(*state).tind1]
      ind2 = [(*state).tind2]
      (*state).npairs = 1
   endif else begin
      ind1 = (*(*state).ind1)
      ind2 = (*(*state).ind2)
      ptr_free,(*state).ind1
      ptr_free,(*state).ind2
      ind1 = [ind1,(*state).tind1]
      ind2 = [ind2,(*state).tind2]
      (*state).npairs++
   endelse

   (*state).ind1 = ptr_new(ind1)
   (*state).ind2 = ptr_new(ind2)

   (*state).sused[(*state).tind1] = 1B
   (*state).cused[(*state).tind2] = 1B

   (*state).tind1 = -1
   (*state).tind2 = -1

   if not keyword_set(noupdate) then $
      wastrom_updatewcs,state

end

pro wastrom_display, state

   widget_control,(*state).drawwin,get_value=dwin
   wset,dwin

   wastrom_updatestars,state

   if (*state).bvalid then begin
      astinfo=(*(*state).binfo)
      t3str='B new'
   endif else begin
      astinfo=(*state).ainfo
      t3str='A initial'
   endelse

   tvscl,(*state).bim
   plot,[0],[1],/nodata,xmargin=[0,0],ymargin=[0,0], $
      xr=[0,float((*state).nx)],yr=[0,float((*state).ny)], $
      xstyle=5,ystyle=5,/noerase

   wcsarrows,astinfo,color=cpalette(62)

   for i=0,(*state).nstars-1 do begin
      if (*state).cused[i] eq 1 then begin
         oplot,[(*state).sx[i]],[(*state).sy[i]],psym=4, $
            color=cpalette(21),symsize=3
      endif else begin
         oplot,[(*state).sx[i]],[(*state).sy[i]],psym=4, $
            color=cpalette(1),symsize=(*state).ssz[i]
      endelse
   endfor
   oplot,(*state).xsrc,(*state).ysrc,psym=7,color=cpalette(21),symsize=2

   fmt='(f10.1)'

   t7str=''
   if (*state).tind1 ge 0 then begin
      t4str='src <-> '
      oplot,[(*state).xsrc[(*state).tind1]],[(*state).ysrc[(*state).tind1]], $
         psym=4,color=cpalette(21),symsize=2
      t5str='s: '+ $
            strn((*state).xsrc[(*state).tind1],format=fmt)+', '+ $
            strn((*state).ysrc[(*state).tind1],format=fmt)
   endif else begin
      t4str='xxx <-> '
      t5str=''
   endelse

   if (*state).tind2 ge 0 then begin
      t4str=t4str+'cat'
      oplot,[(*state).sx[(*state).tind2]],[(*state).sy[(*state).tind2]], $
         psym=6,color=cpalette(21),symsize=4
      t6str='c: '+ $
            strn((*state).sx[(*state).tind2],format=fmt)+', '+ $
            strn((*state).sy[(*state).tind2],format=fmt)
      if (*state).tind1 ge 0 then begin
       psep = sqrt( $
          ((*state).sx[(*state).tind2]-(*state).xsrc[(*state).tind1])^2 + $
          ((*state).sy[(*state).tind2]-(*state).ysrc[(*state).tind1])^2 ) 
       t7str = 'sep: '+strn(psep,format='(f10.1)')
       if psep gt 10 then t7str=t7str+' ***'
      endif
   endif else begin
      t4str=t4str+'xxx'
      t6str=''
   endelse

   t8str='npairs: '+strn((*state).npairs)

   widget_control,(*state).t3win,set_value=t3str
   widget_control,(*state).t4win,set_value=t4str
   widget_control,(*state).t5win,set_value=t5str
   widget_control,(*state).t6win,set_value=t6str
   widget_control,(*state).t7win,set_value=t7str
   widget_control,(*state).t8win,set_value=t8str

end

pro wastrom_updatestars, state

   if (*state).bvalid then astinfo=(*(*state).binfo) else astinfo=(*state).ainfo

   astcvt,'rd',(*state).sra,(*state).sdec,astinfo,'xy',sx,sy

   (*state).sx = sx
   (*state).sy = sy

end

pro wastrom_updatewcs, state

   if (*state).npairs eq 0 then return

   if (*state).npairs eq 1 then begin
      binfo=(*state).ainfo
      dx = (*state).sx[(*(*state).ind2[0])]-(*state).xsrc[(*(*state).ind1[0])]
      dy = (*state).sy[(*(*state).ind2[0])]-(*state).ysrc[(*(*state).ind1[0])]
      binfo.xcref -= dx
      binfo.ycref -= dy
   endif else begin
      xsrc = (*state).xsrc[(*(*state).ind1)]
      ysrc = (*state).ysrc[(*(*state).ind1)]
      sra  = (*state).sra[(*(*state).ind2)]
      sdec = (*state).sdec[(*(*state).ind2)]
      xcen=(*state).nx/2.0
      ycen=(*state).ny/2.0
      binfo=(*(*state).binfo)
      astcvt,'xy',xcen,ycen,binfo,'rd',racen,deccen
      astrd2sn,sra,sdec,racen,deccen,xi,eta,/arcsec
      terms=binfo.terms
      if (*state).npairs eq 2 then begin
         dx = (xsrc[1]-xsrc[0])
         dy = (ysrc[1]-ysrc[0])
         pixsep = sqrt(dx^2+dy^2)
         angsep = sqrt((xi[1]-xi[0])^2+(eta[1]-eta[0])^2)
         pscale  = angsep/pixsep
         ; PA of star pair in pixel coords
         rang = atan(dy,dx)*180.0d0/!dpi
         if rang lt 0 then rang += 360.0d0
         xyrang=rang-90.0 ; reference to +y now, angle CCW
         ; PA of star pair on sky
         rang = atan(eta[1]-eta[0],xi[0]-xi[1])*180.0d0/!dpi
         if rang lt 0 then rang += 360.0d0
         if (*state).flip then $
            xerang=360-(rang+90.0) $
         else $
            xerang=rang-90.0 ; reference to N now, angle CCW
         ; combine to total PA
          rang = 360.0d0-(xyrang-xerang) ; original
         if rang lt 0 then rang += 360.0d0
         if rang ge 360 then rang -= 360.0d0
         mkastinfo,racen,deccen,xcen,ycen,pscale,binfo, $
            yflip=(*state).flip,posang=rang
      endif else begin
         wastrom_wcsfit,xcen,ycen,xsrc,ysrc,xi,eta,racen,deccen,terms,info,error
         if error eq 0 then begin
            astcvt,'xy',xcen,ycen,info,'rd',racen,deccen
            astrd2sn,sra,sdec,racen,deccen,xi,eta,/arcsec
            wastrom_wcsfit,xcen,ycen,xsrc,ysrc,xi,eta,racen,deccen,terms,info
            binfo=info
            (*state).success = 1
         endif else begin
            (*state).success = 0
         endelse
      endelse
   endelse

   ptr_free,(*state).binfo
   (*state).binfo = ptr_new(binfo)
   (*state).bvalid = 1

end

pro wastrom_wcsfit,xcen,ycen,xsrc,ysrc,xi,eta,racen,deccen,terms,info,error

   renormfac=sqrt(xcen^2+ycen^2)
   dx = (xsrc - xcen)/renormfac
   dy = (ysrc - ycen)/renormfac
   tbad=bytarr(n_elements(dx))
   astsolve,dx,dy,xi,eta,terms,renormfac,tbad,cxi,ceta,error=error
   info={renormfac:renormfac,cxi:cxi,ceta:ceta,terms:terms, $
         xcref:xcen,ycref:ycen,prot: 0.0d0, $
         raref:racen,decref:deccen}

end

pro wastrom_eve, event

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
;         widget_control,(*state).drawwin,xsize=event.x,ysize=event.y
;         widget_control,(*state).drawwin,xsize=event.x,ysize=event.y-info.ysize

;        refresh window here
      end

      'Auto Match': begin
         if (*state).npairs lt 2 then return
         pad=10
         dirty=0
         for i=0,(*state).nsrc-1 do begin
            if (*state).sused[i] then continue
            if (*state).xsrc[i] lt pad or $
               (*state).xsrc[i] gt (*state).nx-pad then continue
            if (*state).ysrc[i] lt pad or $
               (*state).ysrc[i] gt (*state).ny-pad then continue
            dist = sqrt(((*state).xsrc[i]-(*state).sx)^2+ $
                        ((*state).ysrc[i]-(*state).sy)^2)
            z = where(dist eq min(dist))
            z = z[0]
            if dist[z] lt 2 then begin
               (*state).tind1 = i
               (*state).tind2 = z
               wastrom_addpair,state,/noupdate
               dirty=1
            endif
         endfor
         if dirty then wastrom_updatewcs,state
      end

      'Link': begin
         if (*state).tind1 lt 0 or (*state).tind2 lt 0 then return
         wastrom_addpair,state
      end

      'Reset': begin
         (*state).npairs=  0
         (*state).tind1 = -1
         (*state).tind2 = -1
         (*state).cused[*] = 0
         (*state).sused[*] = 0
         (*state).bvalid=0
         (*state).success=0
      end

      'Window': begin
         if event.release ne 0 then return

         if event.press eq 1 then begin
            dist = sqrt((event.x-(*state).xsrc)^2+(event.y-(*state).ysrc)^2)
            z = where(dist eq min(dist))
            z = z[0]
            if (*state).sused[z] eq 0 then (*state).tind1 = z
         endif else if event.press eq 2 then begin
            dist = sqrt((event.x-(*state).sx)^2+(event.y-(*state).sy)^2)
            z = where(dist eq min(dist))
            z = z[0]
            if (*state).cused[z] eq 0 then (*state).tind2 = z
         endif else begin
         endelse
      end

      'Exit' : begin
         widget_control, event.top, /DESTROY
         return
      end

      else: begin
         print,'EVENT NAME: ',event_name
         message, 'Unknown event:', /INFO
         help, event, /STRUCTURE
      end

   endcase

   wastrom_display,state

end ; end of event handler

pro wastrom,im,ainfo,flip,sra,sdec,mag,xsrc,ysrc,binfo,success, $
       GROUP=group

   common mwb_wastrom,com_binfo,com_success

   ; optional
   if xregistered('wastrom') then return

   self='wastrom: '
   if badpar(im,[2,3,4,5],2,caller=self+'(im) ') then return
   if badpar(ainfo,8,1,caller=self+'(ainfo) ') then return
   if badpar(flip,[1,2,3],0,caller=self+'(flip) ') then return
   if badpar(sra,[4,5],1,caller=self+'(sra) ') then return
   if badpar(sdec,[4,5],1,caller=self+'(sdec) ') then return
   if badpar(mag,[4,5],1,caller=self+'(mag) ') then return
   if badpar(xsrc,[4,5],1,caller=self+'(xsrc) ') then return
   if badpar(ysrc,[4,5],1,caller=self+'(ysrc) ') then return

   windo=!d.window

   com_binfo=ainfo
   com_success=0

   if (!d.flags and 256) eq 0 then begin
      print, 'Error. No windowing device. wastrom cannot be started.'
      return
   endif

   ;Define the main base.
;   mainbase = widget_base( TITLE='wastrom: Manual Astrometry Tool', $
;                           /COLUMN, UVALUE=0, MBAR=bar, /TLB_SIZE_EVENTS )
   mainbase = widget_base( TITLE='wastrom: Manual Astrometry Tool', $
                           /COLUMN, UVALUE=0 )

;   menu = CW_PdMenu(bar, /RETURN_NAME, $
;                    ['1\File',$
;                     '0\action 1',$
;                     '0\action 2',$
;                     '2\Exit',$
;                     '1\Tools',$
;                     '0\tool 1', $
;                     '0\tool 2', $
;                     '2\tool 3'], UVALUE='THE_MENU', /MBAR)

   base = widget_base(mainbase)

   bt = widget_base(base,column=1)

   b1 = widget_base(bt,row=1)

   sz=size(im,/dimen)
   nx=sz[0]
   ny=sz[1]

   skysclim,im,lowval,hival,meanval,sigma
   hival = meanval + 8.0*sigma

   bim=bytscl(im,min=lowval,max=hival,top=255)

   ssz = (((15.0-mag+1)/1.1 - 0.5 ) < 4 ) > 0.5
   nstars=n_elements(sra)
   nsrc=n_elements(xsrc)

   xviewsize=nx<800
   yviewsize=ny<600
   win1 = widget_draw( b1, XSIZE=nx, YSIZE=ny, RETAIN=2, $
                       X_SCROLL_SIZE=xviewsize, Y_SCROLL_SIZE=yviewsize, $
                       KEYBOARD_EVENTS=0,/BUTTON_EVENTS, UVALUE='Window' )

   b2 = widget_base(b1,column=1)

   t1 = widget_label(b2,value='ncat = '+strn(nstars),/align_left)
   t2 = widget_label(b2,value='nsrc = '+strn(nsrc),/align_left)
   t3 = widget_label(b2,value='A',/align_left,/dynamic_resize)
   t4 = widget_label(b2,value='none',/align_left,/dynamic_resize)
   t5 = widget_label(b2,value='',/align_left,/dynamic_resize)
   t6 = widget_label(b2,value='',/align_left,/dynamic_resize)
   t7 = widget_label(b2,value='',/align_left,/dynamic_resize)
   t8 = widget_label(b2,value='npairs = 0',/align_left,/dynamic_resize)

   b3 = widget_base(bt,row=1)

   u1 = widget_button(b3,value='Link',uvalue='Link')
   u2 = widget_button(b3,value='Auto Match',uvalue='Auto Match')
   u3 = widget_button(b3,value='Reset',uvalue='Reset')
   u4 = widget_button(b3,value='Exit',uvalue='Exit')

   state = ptr_new({ $

      ; Data and information in the widget
      im: im, $
      nx: nx, $
      ny: ny, $
      lowval: lowval, $
      hival: hival, $
      sky: meanval, $
      skysig: sigma, $
      bim: bim, $
      ainfo: ainfo, $
      flip: flip, $
      sra: sra, $
      sdec: sdec, $
      mag: mag, $
      ssz: ssz, $
      sx: fltarr(n_elements(sra)), $
      sy: fltarr(n_elements(sra)), $
      cused: bytarr(n_elements(sra)), $
      nstars: nstars, $
      xsrc: xsrc, $
      ysrc: ysrc, $
      nsrc: nsrc, $
      sused: bytarr(n_elements(xsrc)), $
      tind1: -1L, $
      tind2: -1L, $
      ind1: ptr_new(), $
      ind2: ptr_new(), $
      npairs: 0, $
      binfo: ptr_new(ainfo), $
      bvalid: 0, $
      success: 0, $

      ; Widget ids
      drawwin: win1, $           ; ID of main draw window
      t3win: t3, $
      t4win: t4, $
      t5win: t5, $
      t6win: t6, $
      t7win: t7, $
      t8win: t8, $

      mainbase: mainbase $       ; ID of top level base.

      })

   ;Stash the state structure pointer.
   widget_control, mainbase, SET_UVALUE=state

   ;Realize the main base.
   widget_control, mainbase, /REALIZE

   wastrom_display, state

   ; Give control to the XMANAGER.
   xmanager, 'wastrom', mainbase, $
             EVENT_HANDLER='wastrom_eve',/MODAL, $
             GROUP_LEADER=group, CLEANUP='wastrom_cleanup'

   binfo=com_binfo
   success=com_success

   wset,windo

end
