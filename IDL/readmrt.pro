pro readmrt, MJD, Vmag, f_Vmag, e_Vmag, SV, f_SV, e_SV, Imag, f_Imag, e_Imag, SI, f_SI, e_SI, Jmag, f_Jmag, e_Jmag, SJ, f_SJ, e_SJ, Hmag, f_Hmag, e_Hmag, SH, f_SH, e_SH
  
; This program is written to read the machine readable format table of
; GX 339-4 observations in AJ paper of Buxton et al.
;
; INPUTS
;
; NONE
; 
; OUTPUTS
;
; MJD: modified Julian Date of observaiton
; (V, I, J, H)mag : magnitde in given band
; f_(V, I, J, H)mag : flag relatd to that particular observation
; e_(V, I, J, H)mag : error in magnitude in given band
; S(V, I, J, H) : flux in given band in MJy
; e_S(V, I, J, H) : error in flux in given band in MJy
;
; USED BY
;
; getoirinfo_gxXX programs
;
; USES
;
; NONE
;
; Created by E. Kalemci October 2014
;


;readcol,'aj427910t3_mrt.txt',MJD, Vmag, f_Vmag, e_Vmag, SV, f_SV, e_SV, Imag, f_Imag, e_Imag, SI, f_SI, e_SI, Jmag, f_Jmag, e_Jmag, SJ, f_SJ, e_SJ, Hmag, f_Hmag, e_Hmag, SH, f_SH, e_SH, format='F,F,A,F,F,A,F,F,A,F,F,A,F,F,A,F,F,A,F,F,A,F,F,A,F]

rootdir='~/RXTE/DATA_AN/JILA/GX339/GX339data/'

openr,1,rootdir+'aj427910t3_mrtv2.txt'

s=''
MJD=fltarr(1738)
Vmag=fltarr(1738)
f_Vmag=strarr(1738)
e_Vmag=fltarr(1738)
SV=fltarr(1738)
f_SV=strarr(1738)
e_SV=fltarr(1738)
Imag=fltarr(1738)
f_Imag=strarr(1738)
e_Imag=fltarr(1738)
SI=fltarr(1738)
f_SI=strarr(1738)
e_SI=fltarr(1738)
Jmag=fltarr(1738)
f_Jmag=strarr(1738)
e_Jmag=fltarr(1738)
SJ=fltarr(1738)
f_SJ=strarr(1738)
e_SJ=fltarr(1738)
Hmag=fltarr(1738)
f_Hmag=strarr(1738)
e_Hmag=fltarr(1738)
SH=fltarr(1738)
f_SH=strarr(1738)
e_SH=fltarr(1738)

for i=0,1737 do begin

readf,1,s
JDT=strmid(s,0,9)
MJD[i]=float(JDT)+50000.-0.5

;check if V values exist
Vch=strmid(s,11,6)

IF Vch NE '      ' THEN BEGIN

   Vmag[i]=float(strmid(s,11,6))
   f_Vmag[i]=strmid(s,18,1)
   e_Vmag[i]=float(strmid(s,19,5))
   SV[i]=float(strmid(s,25,7))
   f_SV[i]=strmid(s,33,1)
   e_SV[i]=float(strmid(s,34,7))

ENDIF

Ich=strmid(s,42,6)

IF Ich NE '      ' THEN BEGIN

   Imag[i]=float(strmid(s,42,6))
   f_Imag[i]=strmid(s,49,1)
   e_Imag[i]=float(strmid(s,50,5))
   SI[i]=float(strmid(s,56,7))
   f_SI[i]=strmid(s,64,1)
   e_SI[i]=float(strmid(s,65,7))

ENDIF

Jch=strmid(s,73,6)

IF Jch NE '      ' THEN BEGIN

   Jmag[i]=float(strmid(s,73,6))
   f_Jmag[i]=strmid(s,80,1)
   e_Jmag[i]=float(strmid(s,81,5))
   SJ[i]=float(strmid(s,87,7))
   f_SJ[i]=strmid(s,95,1)
   e_SJ[i]=float(strmid(s,96,7))

ENDIF

Hch=strmid(s,104,6)

IF Hch NE '      ' THEN BEGIN

   Hmag[i]=float(strmid(s,104,6))
   f_Hmag[i]=strmid(s,111,1)
   e_Hmag[i]=float(strmid(s,112,5))
   SH[i]=float(strmid(s,118,7))
   f_SH[i]=strmid(s,126,1)
   e_SH[i]=float(strmid(s,127,7))

ENDIF

ENDFOR

close,1

END
