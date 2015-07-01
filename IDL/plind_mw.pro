pro plind_mw, ps=ps, bw=bw

;This program plots the evolution of power-law index and
;multiwavelength data for several sources for outburst rise, similar
;to Fig 1 in Kalemci et al. 2013

; for now I will use individual input structures from individual
; directories, that will be fixed later.



IF NOT keyword_set(ps) then ps=0
IF NOT keyword_set(bw) THEN bw=0

if ps then begin
   set_plot, 'ps'
   device,/color,/encap
IF bw THEN loadct,0 ELSE loadct,5
IF bw THEN device, filename = 'plindmw_bw.eps' ELSE $
           device, filename = 'plindmw.eps'
   device, yoffset = 8
   device, ysize = 32.
   device, xsize = 18.0
   !p.font=0
   device,/times
endif

;this is for GX 339-4 + +1550-564

;1543 (no IR / X-ray in rise), so this part will be empty

!p.multi=[0,1,6] ;keeping this to make it parallel to Fig 1.

xr=[-28,8]

a = findgen(32)*(!pi*2.0/32.0)
usersym, 0.70*cos(a), 0.70*sin(a), /fill
usym = 8

syms=1.
;
;1543

;dates=inpstr[4].xdates
;da1=dates-inpstr[4].ttrans[0]
;mwtrans=inpstr[4].mwtrans
;ind1=inpstr[4].ind
;jdates=inpstr[4].mwdates-inpstr[4].ttrans[0]
;jmag=inpstr[4].mwmag

cs=2.
csm=1.7
csr=1.7
x0=0.07
xe=0.9

;dummy for empty graph
da1=[0.,0.]
ind1=[[0.,0.],[0.,0.]]

ploterror,da1,ind1(0,*),ind1(1,*),psym=8,ytitle='!9G !X',$
xtickname=[' ',' ',' ',' ',' '],charthick=1.5,charsize=cs,$
xrange=xr,/xstyle,yrange=[1.55,2.5],/nohat,ystyle=1,$
posit=[x0,0.83,xe,0.98];,ytickv=[1.7,1.9,2.1,2.3,2.5],yticks=5

xyouts,-20.,1.8,'4U 1543-47',charsize=1.3
;oplot,[mwtrans[0],mwtrans[0]],[1.5,2.6],line=2

oplot,[0,0],[2.4,2.4],psym=8,color=0
xyouts,1,2.37,'RXTE !9G !X', charsize=1.0
oplot,[0,0],[2.27,2.27],psym=8,color=120
xyouts,1,2.24,'SMARTS NIR', charsize=1.0, color=120
;xyouts,25,2.08,'NIR mag.', charsize=1.1, color=120

usersym, 0.70*cos(a), 0.70*sin(a)
usym = 8

oplot,[0,0],[2.15,2.15],psym=8,color=50
xyouts,1,2.11,'ATCA, 4.8 GHz ', charsize=1.0, color=50
oplot,[0,0],[2.02,2.02],psym=5,color=50
xyouts,1,1.98,'ATCA, 8.4 GHz', charsize=1.0, color=50
;oplot,[27,27],[1.89,1.89],psym=4,color=50
;xyouts,28,1.85,'MOST, 0.84 GHz', charsize=1.0, color=50


;axis, yaxis=1, /ystyle,charsize=csm, ytitle='mag J', yr=[14.8,13.4],/save,color=120, ytickv=[14.5,14.0,13.5],yticks=3,yticklen=-0.01

;oplot,jdates,jmag[0,*],psym=8,color=120


;rdat=[52487.,52490]-inpstr[4].ttrans[0]
;r084=[[5.2,0.9],[0.,0.]]
;r48=[[0.,0.],[4.00,0.15]]
;r84=[[0.,0.],[4.19,0.16]]

;R. Flux (mJy)
;axis, xr[1],1.38, yaxis=0, /ystyle,charsize=csr, ytitle='',/save,color=50,yr=[3.3,6.4],ytickv=[4.,5.,6.],yticks=3,yticklen=-0.01

;usersym, 0.70*cos(a), 0.70*sin(a)
;usym = 8

;oploterror,rdat,r48[0,*],r48[1,*],psym=8,color=50,/nohat

;usersym, 0.70*cos(a), 0.70*sin(a),/fill
;usym = 8


;oploterror,rdat,r84[0,*],r84[1,*],psym=5,color=50,/nohat,symsize=syms
;oploterror,rdat,r084[0,*],r084[1,*],psym=4,color=50,/nohat,symsize=syms


; XTE J1550-564

usersym, 0.70*cos(a), 0.70*sin(a),/fill
usym = 8

restore,'~/RXTE/DATA_AN/JILA/1550/outstr1550r.sav'
inpstr=outstr1550r
dates=inpstr.xdates
da1=dates-inpstr.ttrans[0]
;mwtrans=inpstr[5].mwtrans
mwtrans=-6.
ind1=inpstr.ind

xx=where(inpstr.oirinfo.band eq 'H')
hdates=inpstr.oirinfo.dates[xx]-inpstr.ttrans[0]
hmag=inpstr.oirinfo.mag[*,xx]

rdat=inpstr.radinfo.dates-inpstr.ttrans[0]
r48=inpstr.radinfo.flux[*,0]
r84=inpstr.radinfo.flux[*,1]

ploterror,da1,ind1(0,*),ind1(1,*),psym=8,ytitle='!9G !X',$
xtickname=[' ',' ',' ',' ',' '],charthick=1.5,charsize=cs,$
xrange=xr,/xstyle,yrange=[1.45,2.55],/nohat,ystyle=9,$
posit=[x0,0.68,xe,0.83]

xyouts,-20.,2.0,'XTE J1550-564',charsize=1.3

oplot,[mwtrans[0],mwtrans[0]],[1.5,2.6],line=2

;oplot,[8.,8.],[2.45,2.45],psym=8,color=0
;xyouts,9.,2.4,'RXTE !9G !X', charsize=1.1
;oplot,[8.,8.],[2.27,2.27],psym=8,color=120
;xyouts,9.,2.22,'SMARTS', charsize=1.1, color=120
;xyouts,9.,2.10,'NIR mag.', charsize=1.1, color=120
;oplot,[6.,6.],[1.8,1.8],psym=8,color=50
;xyouts,7.,1.87,'RADIO', charsize=1.1, color=50

axis, yaxis=1, /ystyle,charsize=csm, ytitle='mag H', yr=[14.5,13.3],/save,color=120,yticklen=-0.01

oplot,hdates,hmag[0,*],psym=8,color=120

axis, xr[1],xr[0], yaxis=0, /ystyle,charsize=csr, ytitle='R. Flux (mJy)',/save,color=50,yr=[4.2,8.9],yticklen=-0.01 ;,ytickv=[2.,3.,4.],yticks=3,yticklen=-0.01

usersym, 0.70*cos(a), 0.70*sin(a)
usym = 8

oploterror,rdat,replicate(r48[0,*],2),replicate(r48[1,*],2),psym=8,color=50,/nohat
oploterror,rdat,r84[0,*],r84[1,*],psym=5,color=50,/nohat



;GX 339-4 2005

usersym, 0.70*cos(a), 0.70*sin(a),/fill
usym = 8

restore,'~/RXTE/DATA_AN/JILA/GX339/2005/outstrgx05r.sav'
inpstr=outstrgx05r

dates=inpstr.xdates
da1=dates-inpstr.ttrans[0]
mwtrans=-10.
ind1=inpstr.ind

xx=where(inpstr.oirinfo.band eq 'H')
hdates=inpstr.oirinfo.dates[xx]-inpstr.ttrans[0]
hmag=inpstr.oirinfo.mag[*,xx]


ploterror,da1,ind1(0,*),ind1(1,*),psym=8,ytitle='!9G !X',$
xtickname=[' ',' ',' ',' ',' '],charthick=1.5,charsize=cs,$
xrange=xr,/xstyle,yrange=[1.45,2.8],/nohat,ystyle=9,$
posit=[x0,0.53,xe,0.68]

xyouts,-20.,1.9,'GX 339-4 2005',charsize=1.3

oplot,[mwtrans[0],mwtrans[0]],[1.5,2.8],line=2

;axis, 


axis, xr[1],1.45, yaxis=1, /ystyle,charsize=csm, ytitle='mag H',/save,color=120,yr=[14.5,12.1], yticks=3,yticklen=-0.01,ytickv=[15.,14.,13.]

oplot,hdates,hmag[0,*],psym=8,color=120

;axis, xr[1],1.45, yaxis=0, /ystyle,charsize=csr, ytitle='R. Flux (mJy)',/save,color=50,yr=[1.55,4.95],ytickv=[2.,3.,4.],yticks=3,yticklen=-0.01

;usersym, 0.70*cos(a), 0.70*sin(a)
;usym = 8

;oploterror,rdat,r48[0,*],r48[1,*],psym=8,color=50,/nohat

;usersym, 0.70*cos(a), 0.70*sin(a),/fill
;usym = 8

;oploterror,rdat,r84[0,*],r84[1,*],psym=5,color=50,/nohat,symsize=syms

;oploterror,rdat,(r48[0,*]*ratt)+1.38,r48[1,*]*ratt,psym=8,color=50
;oploterror,rdat,(r84[0,*]*ratt)+1.38,r84[1,*]*ratt,psym=5,color=50


;e GX 339-4 2007

restore,'~/RXTE/DATA_AN/JILA/GX339/2007/outstrgx07r.sav'
inpstr=outstrgx07r

dates=inpstr.xdates
da1=dates-inpstr.ttrans[0]
mwtrans=-11.
ind1=inpstr.ind

xx=where(inpstr.oirinfo.band eq 'H')
hdates=inpstr.oirinfo.dates[xx]-inpstr.ttrans[0]
hmag=inpstr.oirinfo.mag[*,xx]



ploterror,da1[0:39],ind1[0,0:39],ind1[1,0:39],psym=8,$
charthick=1.5,charsize=cs, ytitle='!9G !X', $
xrange=xr,/xstyle,ystyle=9,yrang=[1.45,2.8],ytickname=['1.4',' ','1.8',' ','2.2',' ','2.6',' '],$
posit=[x0,0.38,xe,0.53],/nohat,xtickname=replicate(' ',5)

;axis, yaxis=0, ytitle='!9G !X', yrange=[1.4,2.75],/ystyle,charsize=cs


xyouts,-20.,2.4,'GX 339-4 2007',charsize=1.3

oplot,[mwtrans[0],mwtrans[0]],[1.4,2.8],line=2

axis, xr[1],1.45, yaxis=1, /ystyle,charsize=csm, ytitle='mag H',/save,color=120,yr=[14.4,10.7], yticks=3,yticklen=-0.01;,ytickv=[15.,14.,13.]

oplot,hdates,hmag[0,*],psym=8,color=120


;axis, yaxis=1, ytitle='mag H', yrange=[15.4,12.7],/ystyle,charsize=cs,/save,color=120

;oplot,jdates,jmag[0,*],psym=8,color=120
;
;print,date,scalej

;
rdat=inpstr.radinfo.dates-inpstr.ttrans[0]
xx=where(inpstr.radinfo.freq eq 8.4)
yy=where(inpstr.radinfo.freq eq 4.8)
r48=inpstr.radinfo.flux[*,yy]
r84=inpstr.radinfo.flux[*,xx]

axis, xr[1],xr[0], yaxis=0, /ystyle,charsize=csr, ytitle='R. Flux (mJy)',/save,color=50,yr=[8.2,28.],yticklen=-0.01 ;,ytickv=[2.,3.,4.],yticks=3,yticklen=-0.01

usersym, 0.70*cos(a), 0.70*sin(a)
usym = 8

oploterror,rdat[yy],r48[0,*],r48[1,*],psym=8,color=50,/nohat
oploterror,rdat[xx],r84[0,*],r84[1,*],psym=5,color=50,/nohat




;e GX 339-4 2011

usersym, 0.70*cos(a), 0.70*sin(a),/fill
usym = 8

restore,'~/RXTE/DATA_AN/JILA/GX339/2010/outstrgx10r.sav'
inpstr=outstrgx10r

dates=inpstr.xdates
da1=dates-inpstr.ttrans[0]
mwtrans=-8.5
ind1=inpstr.indp

xx=where(inpstr.oirinfo.band eq 'H')
hdates=inpstr.oirinfo.dates[xx]-inpstr.ttrans[0]
hmag=inpstr.oirinfo.mag[*,xx]


;add radio

ploterror,da1,ind1[0,*],ind1[1,*],psym=8,$
charthick=1.5,charsize=cs, ytitle='!9G !X', $
xrange=xr,/xstyle,ystyle=9,yrang=[1.35,2.9],$
posit=[x0,0.23,xe,0.38],/nohat,ytickname=['1.4',' ','1.8',' ','2.2',' ','2.6',' ']

;stop
;axis, yaxis=0, ytitle='!9G !X', yrange=[1.4,2.75],/ystyle,charsize=cs

;axis, yaxis=1, ytitle='Flux (mJy)', yrange=[0.02,0.48],/ystyle,charsize=cs,/save,color=50


xyouts,-20.,1.8,'GX 339-4 2011',charsize=1.3

oplot,[1.,1.]*mwtrans,[1.35,2.8],line=2

axis, xr[1],1.35, yaxis=1, /ystyle,charsize=csm, ytitle='mag H',/save,$
  color=120,yr=[14.4,10.7], yticks=3,yticklen=-0.01

oplot,hdates,hmag[0,*],psym=8,color=120

axis, xr[1],1.35, yaxis=0, /ystyle,charsize=csr, ytitle='',/save,color=50,yr=[0.15,29.95],yticklen=-0.01 ;ytickv=[1.,2.,3.,4.],yticks=4,yticklen=-0.01


;
rdat=inpstr.radinfo.dates-inpstr.ttrans[0]
xx=where(inpstr.radinfo.freq eq 8.64)
yy=where(inpstr.radinfo.freq eq 4.8)
r48=inpstr.radinfo.flux[*,yy]
r84=inpstr.radinfo.flux[*,xx]

usersym, 0.70*cos(a), 0.70*sin(a)
usym = 8

oploterror,rdat[yy],r48[0,*],r48[1,*],psym=8,color=50,/nohat

usersym, 0.70*cos(a), 0.70*sin(a),/fill
usym = 8

oploterror,rdat[xx],r84[0,*],r84[1,*],psym=5,color=50,/nohat,symsize=syms


dorest=1

IF dorest THEN BEGIn
;axis, yaxis=1, ytitle='mag H', yrange=[15.4,12.7],/ystyle,charsize=cs,/save,color=120

;oplot,jdates,jmag[0,*],psym=8,color=120


;GX 339-4 2003

restore,'~/RXTE/DATA_AN/JILA/GX339/2002/outstrgx02r.sav'
inpstr=outstrgx02r

dates=inpstr[0].xdates-inpstr[0].ttrans[0]
ind=inpstr[0].ind[0,*]
inde=inpstr[0].ind[1,*]

ploterror,dates,ind,inde,psym=8,$
charthick=1.5,charsize=cs,$
xrange=[-28,8],/xstyle,/nohat,ystyle=9,ytitle='!9G !X',yrange=[1.5,2.75], $
posit=[x0,0.05,xe,0.20],xtitle='Time (days)'

irtrans=-12.

oplot,[irtrans,irtrans],[1.0,3.0],line=2.

xyouts,-25.,2.0,'GX 339-4, 2003',charsize=1.3

xx=where(inpstr.oirinfo.band eq 'H')
hdates=inpstr.oirinfo.dates[xx]-inpstr.ttrans[0]
hmag=inpstr.oirinfo.mag[*,xx]

axis, yaxis=1,ytitle='mag H',yrange=[14.2,10.7],/ystyle, charsize=csm,/save,$
color=120,yticklen=-0.01, yticks=3 

;ytickv=[15.,14.,13.],yticks=3,yticklen=-0.01
    ;  ytickname=['15.4',' ','15.0',' ','14.6',' ','14.2',' ']


;oploterror,hdate,hmag[0,*],hmag[1,*],psym=8,color=120,/nohat

oplot,hdates,hmag[0,*],psym=8,color=120


;
rdat=inpstr.radinfo.dates-inpstr.ttrans[0]
xx=where(inpstr.radinfo.freq eq 8.64)
yy=where(inpstr.radinfo.freq eq 4.8)
r48=inpstr.radinfo.flux[*,yy]
r84=inpstr.radinfo.flux[*,xx]

axis, xr[1],xr[0], yaxis=0, /ystyle,charsize=csr, ytitle='',/save,color=50,yr=[0.15,59.95],yticklen=-0.01 ;ytickv=[1.,2.,3.,4.],yticks=4,yticklen=-0.01


usersym, 0.70*cos(a), 0.70*sin(a)
usym = 8

oploterror,rdat[yy],r48[0,*],r48[1,*],psym=8,color=50,/nohat

usersym, 0.70*cos(a), 0.70*sin(a),/fill
usym = 8

oploterror,rdat[xx],r84[0,*],r84[1,*],psym=5,color=50,/nohat,symsize=syms


ENDIF

if ps then begin
  device,/close
  IF !VERSION.OS eq 'Win32' THEN set_plot,'win' ELSE set_plot,'x'
endif


end
