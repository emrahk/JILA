pro coltimex, obsid, eafile=fileea
  
; This program collects information so that rxte shell program can run
; the correct routines
;
; INPUTS
;
; obsid:full obsid
;
; OPTIONAL INPUTS
;
; eafile: a file that holds the correct ea and channel range information
;
; OUTPUTS
;
; NONE (a file with relevant info for shell programs)
;
; USES
;
; NONE
;
; USED BY
;
; alldo (bash shell)
;
; created by Emrah Kalemci, november 2014
;
; November 12: EK, remove unnecessary gti files and background files so that combinelc
; and combinepha would work fine
;
; November 12: EK, write a small script to create a file with the mjd
; start time for further collection
;
; November 12: EK, fix the AO determination after AO9
; December 8: EK, behaving incorrect when all detectors were on, fixed
; January 6: EK: if more than one detector off, the filter was not
; working corectly. fixed.
; March 19: EK, check for exclusive keyword...

obspl=strsplit(obsid,'-',/extract)
IF NOT keyword_set(fileea) THEN fileea=obspl[0]+'_eainfo.txt'

; find the total good time and pcu combination
;

gtifils=file_search(obspl[2]+'.'+obspl[3]+'/filter','good*.gti',count=nfil)
detofft=strarr(nfil)

;determine if exclusive
chexcl=strmatch(gtifils[0],'*excl*')

IF nfil eq 1 THEN indx=0 ELSE BEGIN
      totexpo=fltarr(nfil)

      FOR i=0, nfil-1 DO BEGIN
         ogtfx=strsplit(gtifils[i],'/',/extract)
         ogtf=ogtfx[2]
         detoffx=strsplit(ogtf,'_',/extract)
         detsoff=strpos(detoffx[1],'off') ; number of pcus off
         IF detsoff eq -1 THEN BEGIN
            detsoff=0
            detofft[i]='' 
            ENDIF ELSE detofft[i]=strmid(detoffx[1],0,detsoff)
         starts=loadcol(gtifils[i],'START')
         stops=loadcol(gtifils[i],'STOP')
         expo=0.
         FOR j=0, N_ELEMENTS(starts)-1 DO expo=expo+stops[j]-starts[j]
         totexpo[i]=expo*(5-detsoff)
         print,totexpo[i]
      ENDFOR
      
      indx=where(totexpo eq max(totexpo))
      
      
      nind=where(indgen(nfil) NE indx[0]) ; index of detector combinations to be removed

      print,nind, nfil, indx

      FOR i=0, N_ELEMENTS(nind)-1 DO Begin
         IF detofft[nind[i]] eq '' THEN BEGIN
            IF chexcl THEN BEGIN
               spawn, 'rm -fr '+obspl[2]+'.'+obspl[3]+'/standard2f_excl'
               sepat='*'+'good_excl.*'
            ENDIF ELSE BEGIN
               spawn, 'rm -fr '+obspl[2]+'.'+obspl[3]+'/standard2f'
               sepat='*'+'good.*'
         ENDELSE
         ENDIF ELSE sepat='*_'+detofft[nind[i]]+'off*'
         remfiles=file_search(obspl[2]+'.'+obspl[3]+'/',sepat, count=nfilr)
         print,remfiles
         FOR j=0, nfilr-1 DO spawn, 'rm -fr '+remfiles[j]
         ENDFOR
ENDELSE

; remove all unnecessary files
   
ogtfx=strsplit(gtifils[indx],'/',/extract)
ogtf=ogtfx[2]
detoffx=strsplit(ogtf,'_',/extract)
detsoff=strpos(detoffx[1],'off') 
detflag=''

IF detsoff eq -1 THEN detflag='-all ' ELSE BEGIN
    FOR k=0, detsoff-1 DO detflag=detflag+'-'+strmid(detoffx[1],k,1)+'off '
 ENDELSE

; write down the mjd in a file for later collection

starts=loadcol(gtifils[indx],'START')
openw,1,'mjdstart.txt'
mjds=met2jd(starts[0],/mjd)
printf,1,mjds
close,1

; read ea file to create light curve extraction routines

readcol,'../'+fileea,ea,minc,maxc, FORMAT = 'A, A, A'

ao=strmid(obspl[0],0,1)

IF ao EQ '9' THEN ao=strtrim(string(9+long(strmid(obspl[0],1,1))),1)


openw,1,'allextiming'

printf,1,'#!/bin/ksh'
printf,1,'eaextract=eaextract'
printf,1,'obspath=/Users/emka1155/RXTE/DATA_AN/RAW/AO'+ao+'/P'+obspl[0]+'/'
IF chexcl THEN BEGIN
   FOR i=0, n_elements(ea)-1 DO BEGIN
   printf,1, '${eaextract} '+obsid+' ${obspath} '+obspl[2]+'.'+obspl[3]+'t -dt=8 -exclusive -ea='+ea+' '+detflag+minc+' '+maxc+' > '+obspl[2]+'.'+obspl[3]+'t_ea'+ea+'.log'
  ENDFOR
ENDIF ELSE BEGIN
   FOR i=0, n_elements(ea)-1 DO BEGIN
   printf,1, '${eaextract} '+obsid+' ${obspath} '+obspl[2]+'.'+obspl[3]+'t -dt=8  -ea='+ea+' '+detflag+minc+' '+maxc+' > '+obspl[2]+'.'+obspl[3]+'t_ea'+ea+'.log'
  ENDFOR  
ENDELSE

close,1

spawn,'chmod u+x allextiming'

END

