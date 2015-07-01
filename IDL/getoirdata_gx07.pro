pro getoirdata_gx07, oirinfo, name, year

; this program gets oir data from the given directory below, and
; writes the information neatly into a structure
; created by Emrah Kalemci, October 2014
;
; INPUTs
;
; name : Name of the source
; year : Year of the outburst
;
; OUTPUTs
;
; oirinfo :  structure holding the oir information
;
; USES
;
; NONE
;
; USED BY
;
; allinfogx07r.pro
;
; LOGS
;
; adding flux to the structure, fixing assigning I values to V band
;

rootdir='~/RXTE/DATA_AN/JILA/GX339/GX339data/'

readmrt,MJD, Vmag, f_Vmag, e_Vmag, SV, f_SV, e_SV, Imag, f_Imag, e_Imag, SI, f_SI, e_SI, Jmag, f_Jmag, e_Jmag, SJ, f_SJ, e_SJ, Hmag, f_Hmag, e_Hmag, SH, f_SH, e_SH

dt=where((MJD GT 54100) AND (MJD LE 54170.))
nod=n_elements(dt)

oirinfo=create_struct('name',name,'year',year,'dates',fltarr(1000),'mag',fltarr(2,1000),'flux',fltarr(2,1000),'band',strarr(1000),'inst','SMARTS')

cdata=where(Jmag(dt) NE 0.)

i=0

IF cdata[0] NE -1 THEN BEGIN
   oirinfo.dates[0:nod-1]=MJD[dt]
   oirinfo.mag[0,0:nod-1]=Jmag[dt]
   oirinfo.mag[1,0:nod-1]=e_Jmag[dt]
   oirinfo.band[0:nod-1]='J'
   oirinfo.flux[0,0:nod-1]=SJ[dt]*1000. ;mJy
   oirinfo.flux[1,0:nod-1]=e_SJ[dt]*1000.
   i=i+1
ENDIF

cdata=where(Hmag(dt) NE 0.)

IF cdata[0] NE -1 THEN BEGIN
   oirinfo.dates[i*nod:(i+1)*nod-1]=MJD[dt]
   oirinfo.mag[0,i*nod:(i+1)*nod-1]=Hmag[dt]
   oirinfo.mag[1,i*nod:(i+1)*nod-1]=e_Hmag[dt]
   oirinfo.band[i*nod:(i+1)*nod-1]='H'
   oirinfo.flux[0,i*nod:(i+1)*nod-1]=SH[dt]*1000.
   oirinfo.flux[1,i*nod:(i+1)*nod-1]=e_SH[dt]*1000.
   i=i+1
ENDIF

cdata=where(Imag(dt) NE 0.)

IF cdata[0] NE -1 THEN BEGIN
   oirinfo.dates[i*nod:(i+1)*nod-1]=MJD[dt]
   oirinfo.mag[0,i*nod:(i+1)*nod-1]=Imag[dt]
   oirinfo.mag[1,i*nod:(i+1)*nod-1]=e_Imag[dt]
   oirinfo.band[i*nod:(i+1)*nod-1]='I'
   oirinfo.flux[0,i*nod:(i+1)*nod-1]=SI[dt]*1000.
   oirinfo.flux[1,i*nod:(i+1)*nod-1]=e_SI[dt]*1000.
   i=i+1
ENDIF

cdata=where(Vmag(dt) NE 0.)

IF cdata[0] NE -1 THEN BEGIN
   oirinfo.dates[i*nod:(i+1)*nod-1]=MJD[dt]
   oirinfo.mag[0,i*nod:(i+1)*nod-1]=Vmag[dt]
   oirinfo.mag[1,i*nod:(i+1)*nod-1]=e_Vmag[dt]
   oirinfo.band[i*nod:(i+1)*nod-1]='V'
   oirinfo.flux[0,i*nod:(i+1)*nod-1]=SV[dt]*1000.
   oirinfo.flux[1,i*nod:(i+1)*nod-1]=e_SV[dt]*1000.
ENDIF

END
