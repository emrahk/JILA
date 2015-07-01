pro bolcor_diskbbv2, tin, norm, dbflux, erange=rene
  
;If only one tin and normalization is given, this program calculates
;0.1-50 keV flux. If a two dimensional array of tin and norm is given,
;than the program calculates dbflux and its error

;
;INPUT
;
; tin : inner disk temperature from XSPEC diskbb fits, a single vaue
;or an array of [2,X]
;
; norm: normalization from XSPEC fits, a single vaue
;or an array of [2,X]
;
; OUTPUTS
;
; dbflux : diskbb flux at 0.1 - 50 keV range assuming that flux goes
;                               as 2.1374*T^4. Either a single result
;or an array, first element flux, second element its error 
;
; OPTIONS
;
; erange=rene : energy range can be given to calculate flux with
;error. In this case the program writes a tcl script and runs xspec
;and reads the flux with the given energy range
;
; NONE
;
; USES
;
; NONE
;
; USED BY
;
; plotallrise_XXX
;
; created by EK on October 27, 2014
;
; LOGS
;
; DEC 17 2014
; now handles upper limits
; 
; JAN 12 2015
; A bug in indexing is fixed to handle upper limits correctly
;

sz=size(tin)

;multiple tins with errors
mer=0
;multiple tins without errors
mner=0
;single tin no errors
sner=0
;single tin, with errors
ser=0

;it is difficult to differentiate single tin with errors and multiple
;tins without errors, assume if only 2 parameters given it is single
;tin with errors

CASE n_elements(sz) OF
   5: mer=1
   4: IF sz[1] EQ 2 THEN ser=1 ELSE mner=1
   3: sner=1
   ELSE: print, "Unexpected input parameter structure"
ENDCASE

IF (sner or mner) THEN BEGIN
   IF keyword_set(rene) THEN dbflux=getxspecdbb(tin, norm, rene) ELSE dbflux=norm*2.1374e-11*tin^4. 
ENDIF ELSE BEGIN
   dbflux=norm ; it will have the same number of elements
   dbflux1=fltarr(n_elements(norm)/2)
   dbflux2=dbflux1
   xx=where(norm[1,*] NE -1)
   IF xx[0] NE -1 THEN BEGIN
      tinlow=tin[0,xx]-tin[1,xx]
      normlow=norm[0,xx]+norm[1,xx]
      tinhi=tin[0,xx]+tin[1,xx]
      normhi=norm[0,xx]-norm[1,xx]
      FOR i=0, N_ELEMENTS(xx)-1 DO BEGIN
         IF keyword_set(rene) THEN BEGIN
            dbflux1[xx[i]]=getxspecdbb(tinhi[i],normhi[i], rene)
            dbflux2[xx[i]]=getxspecdbb(tinlow[i], normlow[i], rene)
         ENDIF ELSE BEGIN
            dbflux1[xx[i]]=normhi[i]*2.1374e-11*tinhi[i]^4.
            dbflux2[xx[i]]=normlow[i]*2.1374e-11*tinlow[i]^4.
         ENDELSE
         dbflux[0,xx[i]]=(dbflux1[xx[i]]+dbflux2[xx[i]])/2.
         dbflux[1,xx[i]]=abs(dbflux1[xx[i]]-dbflux2[xx[i]])/2.
      ENDFOR
   ENDIF 
   xx=where(norm[1,*] EQ -1)
   IF xx[0] NE -1 THEN BEGIN
      tinlow=tin[0,xx]
      normup=norm[0,xx]
      FOR i=0, N_ELEMENTS(xx)-1 DO BEGIN
         IF keyword_set(rene) THEN dbflux[0,xx[i]]=getxspecdbb(tinlow[i], normup[i], rene) ELSE dbflux[0,xx[i]]=normup[i]*2.1374e-11*tinlow[i]^4.
         dbflux[1,xx[i]]=-1
     ENDFOR
   ENDIF
ENDELSE

END

function getxspecdbb, tin, norm, enr

; write a tcl script

openw,1,'calf.xcm'
printf,1,'model diskbb & /* '
printf,1,'newpar 1 '+strtrim(string(tin),0)+' '
printf,1,'newpar 2 '+strtrim(string(norm),0)+' '
printf,1,'log fluxout.dat '
printf,1,'flux '+strtrim(string(enr[0]),0)+' '+strtrim(string(enr[1]),0)+' noerr'

printf,1,'exit'
close,1

;run xspec, write result into a log file

spawn, 'xspec calf.xcm > xspec.out'

;read flux fomr log file

s=' '
openr,1,'fluxout.dat'
readf,1,s
;print,strmid(s,0,7) 
WHILE strmid(s,0,7) NE '# Model' DO readf,1,s
close,1

spawn,'rm fluxout.dat'
spawn,'rm calf.xcm'

fc=strpos(s,'(')+1
lc=strpos(s,'ergs')
sflux=strmid(s, fc, lc-fc)

return, float(sflux)

END

