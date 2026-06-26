;+
; NAME:
;  occwcsgen
; PURPOSE:   (one line only)
;  WCS solutions for occultation datasets
; DESCRIPTION:
; CATEGORY:
;  Occultations
; CALLING SEQUENCE:
; INPUTS:
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;  DISPLAY - flag, if set will generate graphics for each image processed
;  TEAM    - String, if set selects data from just one team to process
;  BINFAC  - Only process data with this binning factor
;  REDO    - Ignore prior astrometric solution for image and process anyway
;  INDEX   - Pick just one image to work on without saving
;  NOASK   - When batch processing, automatically flag images bad as needed
;  TEST    - Flag, if set means to process just one frame and quit without
;              saving.  This is only useful when setting either TEAM or INDEX
;  FORCESAVE - Flag, force updating WCS save to image header for cases where
;              there is already a solution.
;  MANUAL  - Flag, if set permits a manual intervention in the astrometry.
;              However, this option is applied only when everything else
;              fails.  If triggered, you see a widget pop up (wastrom.pro)
;              that lets you take over the plate solving process.
; OUTPUTS:
; KEYWORD OUTPUT PARAMETERS:
; CONFIGURATION:
;   File: config.ini
;   [global]
;     event   - string with short ID name of event (ex: OR20200914)
;     maglim  - limiting magnitude of reference catalog (default=0.)
;     matchthresh - Percent of matching sources to trust (default=80)
;   [TEAM] - name of team
;     hwfov - half-width of the field of view
;     maglim  - limiting magnitude of reference catalog (default=0.)
;     matchthresh - Percent of matching sources to trust (default=80)
;   [ddir]   - section to list potential root directories to find data
;     XXX - <value>
;       XXX is a unique string (eg., D01)
;       <value> is a directory path
;       for additional details see getddir.pro
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  Written by Marc W. Buie, Southwest Research Institute, 2020/09/24
;  2021/01/31, MWB, changed logic for the name of the star catalog file
;  2021/02/01, MWB, fixed logic for batch processing
;  2021/11/23, MWB, changed logic on dummy wcsinit line if data not found
;                     and added new output line of wcsinit info when
;                     solution is successful, added matchthresh config option,
;                     added MANUAL keyword
;  2021/11/29, MWB, integrated wastrom into this tool, enabled with /MANUAL
;-
pro occwcsgen,DISPLAY=display,TEAM=cteam,INDEX=index,NOASK=noask, $
           TEST=test,FORCESAVE=forcesave,BINFAC=in_binfac,REDO=redo, $
           MANUAL=manual

   self='occwcsgen: '
   if badpar(display,[0,1,2,3],0,caller=self+'(DISPLAY) ',default=0) then return
   if badpar(noask,[0,1,2,3],0,caller=self+'(NOASK) ',default=0) then return
   if badpar(test,[0,1,2,3],0,caller=self+'(TEST) ',default=0) then return
   if badpar(cteam,[0,7],0,caller=self+'(TEAM) ',default='') then return
   if badpar(index,[0,2,3],0,caller=self+'(INDEX) ',default=-1) then return
   if badpar(forcesave,[0,1,2,3],0,caller=self+'(FORCESAVE) ',default=0) then return
   if badpar(in_binfac,[0,1,2,3],0,caller=self+'(BINFAC) ',default=0) then return
   if badpar(redo,[0,1,2,3],0,caller=self+'(REDO) ',default=0) then return
   if badpar(manual,[0,1,2,3],0,caller=self+'(MANUAL) ',default=0) then return

   dirty=0
   autobad=0
   openmysql,dblun,'occlc'

;   getddir,cinfo,root
   loadini,cinfo
   getvalue,cinfo,'global','event',event
   
   cmd=['select info.idx,status,count(*)', $
              'from info,stars', $
              'where info.idx=stars.imidx', $
              'and wcs is NULL', $
              'and stars.event='+quote(event)]
   if index lt 0 then cmd=[cmd,'and (qual != '+quote('bad')+' or qual is NULL)']
   if cteam ne '' then cmd=[cmd,'and team='+quote(cteam)]
   if in_binfac gt 0 then cmd=[cmd,'and binfac='+strn(in_binfac)]
   if index ge 0 then cmd=[cmd,'and info.idx='+strn(index)]
   cmd=[cmd,'group by idx,status', $
            ';']

   print,cmd
   mysqlquery,dblun,cmd,idx,status,nstat,format='l,a,l',ngood=nfiles
   if nfiles eq 0 then begin
      print,'nothing found.'
      goto,bailout
   endif
   print,strn(nfiles),' images to process'

   cmd='select geomid,ora,odec from campaign where event='+quote(event)+';'
   mysqlquery,dblun,cmd,geomid,rastar,decstar,format='a,d,d',ngood=ncheck
   if ncheck ne 1 then begin
      print,cmd
      print,'campign db entry not found.'
      goto,bailout
   endif

   if not exists('wcsinit.dat') then begin
      rastr,rastar,1,rastars
      decstr,decstar,0,decstars
      info=' 1 000.0 '+rastars+' '+decstars+' 1.000'
      repwrite,'wcsinit.dat','T01','T01'+info
      print,'adding a dummy entry, edit and retry'
      goto,bailout
   endif

   readcol,'wcsinit.dat',iteam,xflip,posang,racens,deccens,iscale, $
      format='a,i,f,a,a,f'
   racen=raparse(racens)
   deccen=decparse(deccens)

   uidx=idx[uniq(idx)]
   nidx=n_elements(uidx)

   mkcircle,0.,0.,1.,xcirc,ycirc
   dcr=5.

   ndone=0
   lastteam=''
   lastbinfac=0
   lastposang=0.0
   lastracen=0.0
   lastdeccen=0.0
   lastscale=0.0
   for i=0,nidx-1 do begin
      z=where(idx eq uidx[i],count)
      if count ne 1 then begin
         print,'Count from idx/uidx check is ',strn(count)
         print,'index is ',strn(uidx[i])
         lastteam=''
         continue
      endif

      if status[z[0]] ne 'auto' then begin
         lastteam=''
         continue
      endif

      cmd=['select team,stemdir,filename,binfac', $
           'from info where idx='+strn(uidx[i])+';']
;      print,cmd
      mysqlquery,dblun,cmd,team,stemdir,filename,binfac,format='a,a,a,i'
      zt=trimrank(where(team eq iteam,count))
      if count ne 1 then begin
         print,'team ',team,' not found in wcsinit.dat file'
         rastr,rastar,1,rastars
         decstr,decstar,0,decstars
         xflipguess=fix(median(xflip))
         posangguess=median(posang)
         scaleguess=median(iscale)
         info=' '+strn(xflipguess)+' '+string(posangguess,format='(f5.1)')+ $
              ' '+rastars+' '+decstars+' '+string(scaleguess,format='(f5.3)')
         repwrite,'wcsinit.dat',team,team+info
         print,'adding a dummy entry, edit and retry'
         goto,bailout
      endif

      ; if the team changes we need to reload a lot of parameters and
      ;   the catalog
      if lastteam ne team then begin
         rothist=[]
         nrothist=0
         getddir,cinfo,root,team=team
         getvalue,cinfo,team,'hwfov',hwfov,type=4
         getvalue,cinfo,team,'maglim',maglim,type=4,default=0.
         getvalue,cinfo,team,'matchthresh',matchthresh,type=2,default=80
      endif

      ddir=root+stemdir
      if not exists(ddir+filename) then begin
         print,ddir+filename
         print,'File not found.'
         lastteam=''
         continue
      endif

      if binfac eq 1 then begin
         fnim=ddir+filename
      endif else begin
         fnim=ddir+'binned_'+strn(binfac)+'/'+filename
      endelse

      im=float(readfits(fnim,hdr))

      if not redo then begin
         astinfo,hdr,tinfo,error,/silent
         if error eq 0 then begin
            lastteam=''
            continue
         endif
      endif
      print,fnim
      print,uidx[i],' Team ',team,'  ',stemdir,filename,binfac

      if lastteam ne team then begin
         fncat=event+'_'+strn(round(hwfov))+'.cat'
         if not exists(fncat) then begin
            print,'build star catalog file ',fncat
            jdgeo=jdparse(geomid)
            jd2year,jdgeo,year
            refnet,rastar,decstar,hwfov,hwfov,0.,16.,fncat,gaia=year
         endif
         rdstarc,fncat,sra,sdec,bmag,rmag,nstars
         print,'Loaded ',strn(nstars),' reference stars from ',fncat

         if maglim gt 0 then begin
            z=where(rmag lt maglim,count)
            if count eq 0 then begin
               print,'No stars left after maglim'
               goto,bailout
            endif
            sra=sra[z]
            sdec=sdec[z]
            bmag=bmag[z]
            rmag=rmag[z]
            nstars=count
            print,strn(nstars),' stars left after maglim filtering',maglim
         endif
         print,'Faintest reference star is ',max(rmag)
         ssz = (((15.0-rmag+1)/1.1 - 0.5 ) < 4 ) > 0.5

         ; Mark brightest stars to help find the field
         sidx=sort(rmag)
         nm=20
         mra=sra[sidx[0:nm-1]]
         mdec=sdec[sidx[0:nm-1]]
      endif

      cmd=['select x,y from stars', $
           'where imidx='+strn(uidx[i]), $
           ';']
      mysqlquery,dblun,cmd,xsrc,ysrc,format='f,f',ngood=nsrcs
      print,strn(nsrcs),' total sources on image.'

      backsub,im,/row
      sz=size(im,/dimen)
      xcen=sz[0]/2.0
      ycen=sz[1]/2.0
      renormfac=sqrt(xcen^2+ycen^2)
      scale=iscale[zt]*float(binfac)

      if team ne lastteam or binfac ne lastbinfac then begin
         print,'wcsinit.dat guess for scale and orientation'
         posang_guess = posang[zt]
         racen_guess  = racen[zt]
         deccen_guess = deccen[zt]
         scale_guess  = scale
      endif else begin
         print,'Using last solution for next guess.'
         posang_guess = lastposang
         racen_guess  = lastracen
         deccen_guess = lastdeccen
;         scale_guess  = lastscale
         scale_guess  = scale
      endelse
      print,'Guess values ',racen_guess,deccen_guess,posang_guess, $
                            scale_guess,scale_guess/binfac

retry:
      mkastinfo,racen_guess,deccen_guess,xcen,ycen,scale_guess,info, $
         posang=posang_guess,xflip=xflip[zt]

      astcvt,'rd',sra,sdec,info,'xy',sx,sy
      astcvt,'rd',rastar,decstar,info,'xy',x,y
      astcvt,'rd',mra,mdec,info,'xy',mx,my

      if display then begin
         showsrc,im,window=0,hisig=22 ; 12
         for j=0,nstars-1 do $
            oplot,[sx[j]],[sy[j]],psym=4,color=cpalette(1),symsize=ssz[j]
         oplot,[x],[y],psym=5,color=cpalette(2),symsize=5
         oplot,mx,my,psym=6,color=cpalette(3),symsize=5
         wcsarrows,info,color=cpalette(5),ainfo=ainfo
         oplot,xsrc,ysrc,psym=7,color=cpalette(11),symsize=2
      endif

      srcor,sx,sy,xsrc,ysrc,dcr,sind1,sind2,option=1
      print,strn(n_elements(sind1)),' correlated sources out of ',strn(nsrcs)
      ixoff=sx[sind1]-xsrc[sind2]
      iyoff=sy[sind1]-ysrc[sind2]
      bad=replicate(0B,n_elements(sind1))
      robomean,ixoff,3.0,0.5,rmeanxoff,dummy,rmeanxsig,bad=bad
      robomean,iyoff,3.0,0.5,rmeanyoff,dummy,rmeanysig,bad=bad
      robomean,ixoff,3.0,0.5,rmeanxoff,dummy,rmeanxsig,bad=bad
      print,'xoffset=',rmeanxoff,' +/- ',rmeanxsig
      print,'yoffset=',rmeanyoff,' +/- ',rmeanysig

      ; percent of sources matched
      matched=round((n_elements(sind1)-total(bad))/float(nsrcs)*100)

      print,strn(matched),'% of ',strn(nsrcs),' were matched to catalog'
      xoff=rmeanxoff
      yoff=rmeanyoff

      if matched lt matchthresh then begin
         print,'Fewer than ',strn(matchthresh),'% of sources matched with simple match, try frmdxdy'
         ;frmdxdy,sx,sy,xsrc,ysrc,xoff,yoff,error,maxerr=5,fndrad=6 ; ,scalefac=0.5
         frmdxdy,sx,sy,xsrc,ysrc,xoff,yoff,error,scalefac=1.0,maxerr=6
         if error ne 0 then begin
            print,'Initial try failed with error code ',error
            frmdxyr,sx,sy,xsrc,ysrc,-0.75,0.76,0.05, $
               xoff,yoff,theta,error,scalefac=1.0
         endif
         print,xoff,yoff,error
         if error ne 0 then begin
            if test eq 0 and index lt 0 and not noask then begin
               if not autobad then begin
                  if manual then begin
                     wastrom,im,info,xflip[zt],sra,sdec,rmag,xsrc,ysrc, $
                        tinfo,success
                     if success then begin
                        astcvt,'rd',sra,sdec,info,'xy',sx,sy
                        xoff=0.
                        yoff=0.
                        srcor,sx,sy,xsrc,ysrc,dcr,ind1,ind2,option=1
                        astcvt,'rd',rastar,decstar,info,'xy',x,y
                        goto,manualsolved
                     endif
                  endif
                  ans='n'
                  read,prompt='mark frame bad? (def=n) ',ans
                  if ans eq 'Y' then begin
                     autobad=1
                  endif
               endif else begin
                  ans='y'
                  print,'Auto-marking frame bad due to user input.'
               endelse
               if strlowcase(strmid(ans,0,1)) eq 'y' then begin
                  cmd='update info set qual='+quote('bad')+ $
                      ' where idx='+strn(uidx[i])+';'
                  print,cmd
                  mysqlcmd,dblun,cmd
                  dirty=1
;                  lastteam=''
                  continue
               endif else goto,bailout
            endif else begin
               print,'astrometric error bailout'
               if error ne 0 then goto,bailout
            endelse
         endif
      endif

      srcor,sx+xoff,sy+yoff,xsrc,ysrc,dcr,ind1,ind2,option=1
      print,strn(n_elements(ind1)),' correlated sources.'

      if n_elements(ind1) lt 3 then begin
         if manual then begin
            wastrom,im,info,xflip[zt],sra,sdec,rmag,xsrc,ysrc, $
               tinfo,success
            if success then goto,manualsolved
         endif
         if test eq 0 then begin
            print,'Not enough sources with final srcor, marking frame bad.'
            cmd='update info set qual='+quote('bad')+ $
                ' where idx='+strn(uidx[i])+';'
            print,cmd
         endif else begin
            print,'Not enough sources with final srcor, quitting'
         endelse
         mysqlcmd,dblun,cmd
         dirty=1
;         lastteam=''
         continue
      endif

      ; first pass, using crude center
      dx = (xsrc[ind2] - xcen)/renormfac
      dy = (ysrc[ind2] - ycen)/renormfac
      astrd2sn,sra[ind1],sdec[ind1],racen_guess,deccen_guess,xi,eta,/arcsec
      tterms=['CONST','X','Y']
      tbad=bytarr(n_elements(ind1))
      astsolve,dx,dy,xi,eta,tterms,renormfac,tbad,cxi,ceta
      print,cxi
      print,ceta
      tinfo={renormfac:renormfac,cxi:cxi,ceta:ceta,terms:tterms, $
                         xcref:xcen,ycref:ycen,prot: 0.0d0, $
                         raref:racen[zt],decref:deccen[zt]}
      astcvt,'xy',xcen,ycen,tinfo,'rd',newracen,newdeccen

      astrd2sn,sra[ind1],sdec[ind1],newracen,newdeccen,xi,eta,/arcsec
      tbad=bytarr(n_elements(ind1))
      astsolve,dx,dy,xi,eta,tterms,renormfac,tbad,cxi,ceta
      tinfo={renormfac:renormfac,cxi:cxi,ceta:ceta,terms:tterms, $
                         xcref:xcen,ycref:ycen,prot: 0.0d0, $
                         raref:newracen,decref:newdeccen}

manualsolved:
      astcvt,'xy',xcen,ycen,tinfo,'rd',newracen,newdeccen
      rastr,newracen,1,newracens
      decstr,newdeccen,0,newdeccens

      astcvt,'xy',[xcen,xcen+10],[1,1]*ycen,tinfo,'SN',xtest,etest
      newpscale = sqrt((xtest[1]-xtest[0])^2+(etest[1]-etest[0])^2)/10.0

      print,'Solved center at ',newracens,' ',newdeccens
      print,'new plate scale ',newpscale,newpscale/binfac
      astcvt,'rd',rastar,decstar,tinfo,'xy',xstar,ystar
      if (test eq 0 and index lt 0) or forcesave then begin
         print,'update wcs in header'
         astinfo,hdr,tinfo,/toheader
         sxaddpar,hdr,'BZERO',32768
         sxdelpar,hdr,'O_BZERO'
         modfits,fnim,0,hdr
         cmd='update info set wcs=1'+ $
             ' where idx='+strn(uidx[i])+';'
         mysqlcmd,dblun,cmd
         dirty=1
      endif

      if display then begin
         showsrc,im,window=1,hisig=22
         for j=0,nstars-1 do $
            oplot,[sx[j]+xoff],[sy[j]+yoff],psym=4, $
               color=cpalette(1),symsize=ssz[j]
         oplot,[x],[y],psym=5,color=cpalette(2),symsize=5
         wcsarrows,tinfo,color=cpalette(5),ainfo=ainfo
         oplot,xsrc,ysrc,psym=6,color=cpalette(14),symsize=2,thick=2
         for j=0,n_elements(ind1)-1 do $
            oplot,xcirc*20+xsrc[ind2[j]],ycirc*20+ysrc[ind2[j]], $
               color=cpalette(13),thick=2
         oplot,[xstar],[ystar],psym=5,color=cpalette(2),symsize=4
         print,'small red diamonds, estimated star catalog positions'
         print,'green square, PSF-fitted sources'
         print,'big red circles, correlated sources for the astrometry fit'
      endif else begin
         wcsarrows,tinfo,ainfo=ainfo,/noplot
      endelse

      newpang=90-ainfo.pang
      if newpang lt 0 then newpang=newpang+360
      rothist=[rothist,newpang]
      nrothist++
      i1=nrothist-1
      i0=(i1-40)>0
      robomean,rothist[i0:i1],3.0,0.5,meanrot
      print,'Position angle ',ainfo.pang,'  [',newpang,meanrot,']'

      print,team,xflip[zt],newpang,newracens,newdeccens,newpscale/binfac, $
         format='(a,1x,i1,1x,f5.1,1x,a,1x,a,1x,f5.3)'

;x=[962.767,1048.849,1025.716,1118.152]
;y=[554.988,470.048,289.367,115.693]
;astcvt,'xy',x,y,tinfo,'rd',ra,dec
;rastr,ra,4,ras
;decstr,dec,3,decs
;for ii=0,n_elements(x)-1 do begin
;   print,ras[ii],' ',decs[ii]
;endfor

      ndone++
;if ndone ge 100 then break

      lastteam = team
      lastbinfac = binfac
      lastposang = newpang
      lastracen=newracen
      lastdeccen=newdeccen
      lastscale=newpscale

      if (test eq 1) or index ge 0 then goto,bailout
   endfor

   print,'Processed ',strn(ndone),' images.'

bailout:
   free_lun,dblun

end
