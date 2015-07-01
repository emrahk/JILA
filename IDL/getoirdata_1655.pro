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

rootdir='~/RXTE/DATA_AN/JILA/1655/OIR/from_michelle/'

readcol,rootdir+'Jcal.tab',fname, time, magj, FORMAT=('A,F,F')
datej=time-2400000.5

jl=where((datej LE 53470.) AND (datej GE 53420.))
nelj=n_elements(jl)

oirinfo.dates[0:nelj-1]=datej[jl]
oirinfo.mag[0,0:nelj-1]=magj[jl]
oirinfo.band[0:nelj-1]='J'

magj2=fltarr(2,nelj)
magj2[0,*]=magj[jl]

magtoflux, magj2, 'J', fluxj, EBmV=[1.3,0.1]
oirinfo.flux[*,0:nelj-1]=fluxj
 
;K

readcol,rootdir+'Kcal.tab',fname, timeK, magK, FORMAT=('A,F,F')
datek=timek-2400000.5

kl=where((datek LE 53470.) AND (datek GE 53420.))
nelk=n_elements(kl)

oirinfo.dates[nelj:nelj+nelk-1]=datek[kl]
oirinfo.mag[0,nelj:nelj+nelk-1]=magK[kl]
oirinfo.band[nelj:nelj+nelk-1]='K'

magk2=fltarr(2,nelk)
magk2[0,*]=magK[kl]

magtoflux, magk2, 'K', fluxk, EBmV=[1.3,0.1]
oirinfo.flux[*,nelj:nelj+nelk-1]=fluxk
 
;B

readcol, rootdir+'Btrue_9912.tab', timeB, magB
dateB=timeB-0.5+50000.

nind=nelj+nelk

bl=where((dateB LE 53470.) AND (dateB GE 53420.))
nelb=n_elements(bl)

oirinfo.dates[nind:nind+nelb-1]=dateb[bl]
oirinfo.mag[0,nind:nind+nelb-1]=magb[bl]
oirinfo.band[nind:nind+nelb-1]='B'

magbx=fltarr(2,nelb)
magbx[0,*]=magb[bl]

magtoflux, magbx, 'B', fluxb, EBmV=[1.3,0.1]
oirinfo.flux[*,nind:nind+nelb-1]=fluxb
 
;I

readcol, rootdir+'Itrue_9912.tab', timeI, magI
dateI=timeI-0.5+50000.

nind=nind+nelb

il=where((dateI LE 53470.) AND (dateI GE 53420.))
neli=n_elements(il)

oirinfo.dates[nind:nind+neli-1]=dateI[il]
oirinfo.mag[0,nind:nind+neli-1]=magI[il]
oirinfo.band[nind:nind+neli-1]='I'

magix=fltarr(2,neli)
magix[0,*]=magi[il]

magtoflux, magix, 'I', fluxi, EBmV=[1.3,0.1]
oirinfo.flux[*,nind:nind+neli-1]=fluxi
 
;V

readcol, rootdir+'Vtrue_9912.tab', timeV, magV
dateV=timeV-0.5+50000.

nind=nind+neli

vl=where((dateV LE 53470.) AND (dateV GE 53420.))
nelv=n_elements(vl)

oirinfo.dates[nind:nind+nelv-1]=datev[vl]
oirinfo.mag[0,nind:nind+nelv-1]=magv[vl]
oirinfo.band[nind:nind+nelv-1]='V'

magvx=fltarr(2,nelv)
magvx[0,*]=magv[vl]

magtoflux, magvx, 'V', fluxv, EBmV=[1.3,0.1]
oirinfo.flux[*,nind:nind+nelv-1]=fluxv
 

END
