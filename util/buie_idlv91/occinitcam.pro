;+
; NAME:
;  occinitcam
; PURPOSE:   (one line only)
;  Initialize an occultation campaign in the occlc database.
; DESCRIPTION:
; CATEGORY:
;  Occultations
; CALLING SEQUENCE:
;  occinitcam
; INPUTS:
;  input information comes from local config.ini file
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;  RELOAD - Flag, if set causes the relevant information from the configuration
;              file to be reloaded into the database.  The existing record is
;              updated in place.  If not set and the record already exists for
;              this event, nothing is done.
; OUTPUTS:
;  all output goes to the occlc database
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
;  uses the occlc database
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  Written by Marc W. Buie, Southwest Research Institute, 2020/09/23
;-
pro occinitcam,RELOAD=reload

   self='occinitcam: '
   if badpar(reload,[0,1,2,3],0,caller=self+'(RELOAD) ',default=0) then return

   if not exists('config.ini') then begin
      print,'configuration file, config.ini, not found.'
      return
   endif

   getddir,cinfo,ddir

   getvalue,cinfo,'global','event',event
   if event eq '' then begin
      print,'configuration file does not contain the event name'
      return
   endif

   openmysql,dblun,'occlc'
   cmd='select idx from campaign where event='+quote(event)+';'
   print,cmd
   mysqlquery,dblun,cmd,idx,ngood=ncheck,format='l'

   if ncheck eq 0 then idx = -1

   if idx lt 0 then begin
      print,'New event to be added'
      cmd0 = 'insert into campaign set'
      cmd1 = ''
   endif else begin
      if reload then begin
         cmd0='update campaign set'
         cmd1='where idx='+strn(idx)
      endif else begin
         print,'Event already loaded.  Aborting'
         goto,bailout
      endelse
   endelse

   getvalue,cinfo,'global','geomid',geomid
   getvalue,cinfo,'global','ora',orax
   getvalue,cinfo,'global','odec',odecx
   getvalue,cinfo,'global','oepoch',oepoch
   getvalue,cinfo,'global','orsig',orsig
   getvalue,cinfo,'global','odsig',odsig
   getvalue,cinfo,'global','catalog',catalog
   getvalue,cinfo,'global','cra',crax
   getvalue,cinfo,'global','cdec',cdecx
   getvalue,cinfo,'global','cepoch',cepoch
   getvalue,cinfo,'global','parallax',parallax
   getvalue,cinfo,'global','pmra',pmra
   getvalue,cinfo,'global','pmdec',pmdec
   getvalue,cinfo,'global','crsig',crsig
   getvalue,cinfo,'global','cdsig',cdsig
   getvalue,cinfo,'global','parasig',parasig
   getvalue,cinfo,'global','pmrsig',pmrsig
   getvalue,cinfo,'global','pmdsig',pmdsig
   getvalue,cinfo,'global','gmag',gmag
   getvalue,cinfo,'global','gmagerr',gmagerr
   getvalue,cinfo,'global','bmag',bmag
   getvalue,cinfo,'global','bmagerr',bmagerr
   getvalue,cinfo,'global','rmag',rmag
   getvalue,cinfo,'global','rmagerr',rmagerr
   getvalue,cinfo,'global','catname',catname

   ora=raparse(orax)
   odec=decparse(odecx)
   cra=raparse(crax)
   cdec=decparse(cdecx)

   c=','

   cmd=[cmd0, $
        'event='+quote(event)+c, $
        'geomid='+quote(geomid)+c, $
        'ora='+strn(ora,format='(f20.14)')+c, $
        'odec='+strn(odec,format='(f20.14)')+c, $
        'oepoch='+oepoch+c, $
        'orsig='+orsig+c, $
        'odsig='+odsig+c, $
        'catalog='+quote(catalog)+c, $
        'cra='+strn(cra,format='(f20.14)')+c, $
        'cdec='+strn(cdec,format='(f20.14)')+c, $
        'cepoch='+cepoch+c, $
        'parallax='+parallax+c, $
        'pmra='+pmra+c, $
        'pmdec='+pmdec+c, $
        'crsig='+crsig+c, $
        'cdsig='+cdsig+c, $
        'parasig='+parasig+c, $
        'pmrsig='+pmrsig+c, $
        'pmdsig='+pmdsig+c, $
        'gmag='+gmag+c, $
        'gmagerr='+gmagerr+c, $
        'bmag='+bmag+c, $
        'bmagerr='+bmagerr+c, $
        'rmag='+rmag+c, $
        'rmagerr='+rmagerr+c, $
        'catname='+quote(catname), $
        cmd1, $
        ';']

   pos1=strpos(cmd,'=,')
   pos2=strpos(cmd,"''")
   if max([pos1,pos2]) ge 0 then begin
      print,cmd
      print,self,'One or more values needed are missing, check the above and'
      print,'update the config.ini file accordingly'
      print,''
      print,'save aborted'
      goto,bailout
   endif

   print,cmd
   mysqlcmd,dblun,cmd

bailout:
   free_lun,dblun

end
