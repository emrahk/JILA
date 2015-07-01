pro getoirdata_1655, oirinfo, name, year

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
; allinfo1655r.pro
;

;oirinfo=create_struct('name',name,'year',year,'dates',fltarr(185),'mag',fltarr(2,185),'band',strarr(185),'inst','SMARTS')

oirinfo=create_struct('name',name,'year',year,'dates',fltarr(1000),'mag',fltarr(2,1000),'flux',fltarr(2,1000),'band',strarr(1000),'inst','SMARTS')

rootdir='~/RXTE/DATA_AN/JILA/1655/OIR/'

readcol, rootdir+'1655_ir.dat', fn, band, datir, magir, format='A,A,F,F'

jl=where((band eq 'J') and (datir LE 53470.))
nelj=n_elements(jl)

oirinfo.dates[0:nelj-1]=datir[jl]
oirinfo.mag[0,0:nelj-1]=magir[jl]
oirinfo.band[0:nelj-1]='J'

magj=fltarr(2,nelj)
magj[0,*]=magir[jl]

magtoflux, magj, 'J', fluxj, EBmV=[1.3,0.1]
oirinfo.flux[*,0:nelj-1]=fluxj
 

kl=where((band eq 'K') and (datir LE 53470.))
nelk=n_elements(kl)

oirinfo.dates[nelj:nelj+nelk-1]=datir[kl]
oirinfo.mag[0,nelj:nelj+nelk-1]=magir[kl]
oirinfo.band[nelj:nelj+nelk-1]='K'

magk=fltarr(2,nelk)
magk[0,*]=magir[kl]

magtoflux, magk, 'K', fluxk, EBmV=[1.3,0.1]
oirinfo.flux[*,nelj:nelj+nelk-1]=fluxk
 
readcol, rootdir+'1655_b.dat', dateb, magb, format='F,F'

nind=nelj+nelk

bl=where(dateb LE 53470.)
nelb=n_elements(bl)

oirinfo.dates[nind:nind+nelb-1]=dateb[bl]
oirinfo.mag[0,nind:nind+nelb-1]=magb[bl]
oirinfo.band[nind:nind+nelb-1]='B'

magbx=fltarr(2,nelb)
magbx[0,*]=magb[bl]

magtoflux, magbx, 'B', fluxb, EBmV=[1.3,0.1]
oirinfo.flux[*,nind:nind+nelb-1]=fluxb
 

readcol, rootdir+'1655_i.dat', datei, magi, format='F,F'

nind=nind+nelb

il=where(datei LE 53470.)
neli=n_elements(il)

oirinfo.dates[nind:nind+neli-1]=datei[il]
oirinfo.mag[0,nind:nind+neli-1]=magi[il]
oirinfo.band[nind:nind+neli-1]='I'

magix=fltarr(2,neli)
magix[0,*]=magi[il]

magtoflux, magix, 'I', fluxi, EBmV=[1.3,0.1]
oirinfo.flux[*,nind:nind+neli-1]=fluxi
 

readcol, rootdir+'1655_v.dat', datev, magv, format='F,F'

nind=nind+neli

vl=where(datev LE 53470.)
nelv=n_elements(vl)

oirinfo.dates[nind:nind+nelv-1]=datev[vl]
oirinfo.mag[0,nind:nind+nelv-1]=magv[vl]
oirinfo.band[nind:nind+nelv-1]='V'

magvx=fltarr(2,nelv)
magvx[0,*]=magv[vl]

magtoflux, magvx, 'V', fluxv, EBmV=[1.3,0.1]
oirinfo.flux[*,nind:nind+nelv-1]=fluxv
 

END
