pro getoirdata_1550, oirinfo, name, year

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
; allinfo1550r.pro
;
; LOGS
;
; added flux keyword, has not calculated yet
;

;oirinfo=create_struct('name',name,'year',year,'dates',fltarr(179),'mag',fltarr(2,179),'flux',fltarr(2,179),'band',strarr(179),'inst','SMARTS')

oirinfo=create_struct('name',name,'year',year,'dates',fltarr(1000),'mag',fltarr(2,1000),'flux',fltarr(2,1000),'band',strarr(1000),'inst','SMARTS')


rootdir='~/RXTE/DATA_AN/JILA/1550/OIR/'

readcol, rootdir+'1550_hmag.txt', datir, magir, format='F,F'

nelh=n_elements(magir)

oirinfo.dates[0:nelh-1]=datir+51000.
oirinfo.mag[0,0:nelh-1]=magir
oirinfo.band[0:nelh-1]='H'

magh=fltarr(2,nelh)
magh[0,*]=magir
magtoflux, magh, 'H', fluxh, EBmV=[5.,0.3]/3.1
oirinfo.flux[*,0:nelh-1] = fluxh


readcol, rootdir+'1550_imag.txt', dati, magi, format='F,F'

neli=n_elements(magi)

oirinfo.dates[nelh:nelh+neli-1]=dati+51000.
oirinfo.mag[0,nelh:nelh+neli-1]=magi
oirinfo.band[nelh:nelh+neli-1]='I'

magix=fltarr(2,neli)
magix[0,*]=magi
magtoflux, magix, 'I', fluxi, EBmV=[5.,0.3]/3.1
oirinfo.flux[*,nelh:nelh+neli-1] = fluxi

readcol, rootdir+'1550_vmag.txt', datev, magv, format='F,F'

nind=nelh+neli
nelv=n_elements(magv)

oirinfo.dates[nind:nind+nelv-1]=datev+51000.
oirinfo.mag[0,nind:nind+nelv-1]=magv
oirinfo.band[nind:nind+nelv-1]='V'

magvx=fltarr(2,nelv)
magvx[0,*]=magv
magtoflux, magvx, 'V', fluxv, EBmV=[5.,0.3]/3.1
oirinfo.flux[*,nind:nind+nelv-1] = fluxv

END
