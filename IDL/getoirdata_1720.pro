pro getoirdata_1720, oirinfo, name, year

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
; allinfo1720r.pro
;
; LOGS
;
; adding flux keyword, has not calculated yet
;


oirinfo=create_struct('name',name,'year',year,'dates',fltarr(1000),'mag',fltarr(2,1000),'flux',fltarr(2,1000),'band',strarr(1000),'inst','NTT')

rootdir='~/RXTE/DATA_AN/JILA/1720/OIR/'

readcol,rootdir+'Jband_1720.txt',dates,magj
readcol,rootdir+'Jbandtop_1720.txt',datest,magjt

oirinfo.dates=dates
oirinfo.mag[0,0:12]=magj
oirinfo.mag[1,0:12]=magj-magjt
oirinfo.band[0:12]='J'

magjx=fltarr(2,13)
magjx[0,*]=magj
magjx[1,*]=magj-magjt
magtoflux, magjx, 'J', fluxj, EBmV=[7.,1.]/3.1
oirinfo.flux[*,0:12]=fluxj

END
