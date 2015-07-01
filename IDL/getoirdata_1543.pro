pro getoirdata_1543, oirinfo, name, year

; this program gets oir data from the given directory below, and
; writes the information neatly into a structure
; created by Emrah Kalemci, September 2014
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
; allinfo1543r.pro
;
; LOGS
;
; flux added to the structure, but have not been calculated yet!
;
;

oirinfo=create_struct('name',name,'year',year,'dates',fltarr(1000),'mag',fltarr(2,1000),'flux',fltarr(2,1000),'band',strarr(1000),'inst','SMARTS')

rootdir='~/RXTE/DATA_AN/JILA/1543/OIR/'

allj=dblarr(7,150)
openr,1,rootdir+'1543j_cal_mod.mag'
readf,1,allj
close,1

datej=allj(0,*)-2400000.5
magj=allj(1,*)
magjerr=allj(2,*)


oirinfo.dates[0:149]=datej
oirinfo.mag[0,0:149]=magj
oirinfo.mag[1,0:149]=magjerr
oirinfo.band[0:149]='J'

magjx=fltarr(2,150)
magjx[0,*]=magj
magjx[1,*]=magjerr
magtoflux, magjx, 'J', fluxj, EBmV=[0.5,0.05]
oirinfo.flux[*,0:149] = fluxj

allk=dblarr(7,150)
openr,1,rootdir+'1543K_cal_mod.mag'
readf,1,allk
close,1

datek=allk(0,*)-2400000.5
magk=allk(1,*)
magkerr=allk(2,*)

oirinfo.dates[150:299]=datek
oirinfo.mag[0,150:299]=magk
oirinfo.mag[1,150:299]=magkerr
oirinfo.band[150:299]='K'

magkx=fltarr(2,150)
magkx[0,*]=magk
magkx[1,*]=magkerr
magtoflux, magkx, 'K', fluxk, EBmV=[0.5,0.05]
oirinfo.flux[*,150:299] = fluxk


allv=dblarr(8,105)
openr,1,rootdir+'magV_cal_mod.tab'
readf,1,allv
close,1

datev=allv(1,*)+49999.5
magv=allv(2,*)
magverr=allv(3,*)

oirinfo.dates[300:404]=datev
oirinfo.mag[0,300:404]=magv
oirinfo.mag[1,300:404]=magverr
oirinfo.band[300:404]='V'


magvx=fltarr(2,105)
magvx[0,*]=magv
magvx[1,*]=magverr
magtoflux, magvx, 'V', fluxv, EBmV=[0.5,0.05]
oirinfo.flux[*,300:404] = fluxv


allb=dblarr(7,26)
openr,1,rootdir+'magB_cal_mod.tab'
readf,1,allb
close,1

dateb=allb(0,*)+49999.5
magb=allb(1,*)
magberr=allb(2,*)

oirinfo.dates[405:430]=dateb
oirinfo.mag[0,405:430]=magb
oirinfo.mag[1,405:430]=magberr
oirinfo.band[405:430]='B'


magbx=fltarr(2,26)
magbx[0,*]=magb
magbx[1,*]=magberr
magtoflux, magbx, 'B', fluxb, EBmV=[0.5,0.05]
oirinfo.flux[*,405:430] = fluxb


alli=dblarr(8,30)
openr,1,rootdir+'magI_cal_mod.tab'
readf,1,alli
close,1

datei=alli(1,*)+49999.5
magi=alli(2,*)
magierr=alli(3,*)
 
oirinfo.dates[431:460]=datei
oirinfo.mag[0,431:460]=magi
oirinfo.mag[1,431:460]=magierr
oirinfo.band[431:460]='I'

magix=fltarr(2,30)
magix[0,*]=magi
magix[1,*]=magierr
magtoflux, magix, 'I', fluxi, EBmV=[0.5,0.05]
oirinfo.flux[*,431:460] = fluxi


END
