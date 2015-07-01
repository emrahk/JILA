pro plotallrise_mod5, inpstr, inx, radonly=radonly, $
ps=ps,namef=fname, noplotrin=noplotrin, facr=fr,$
rmsyr=yrrms, indyr=yrind, tinyr=yrtin, rinyr=yrrin,$
plfyr=yrplf, mwyr=yrmw, xrpl=plxr, bolcor=bolcor,$
usecol=usecol, onlycert=onlycert, realtime=realtime, $
band=mwband, rfreq=freqr, plotrad=plotrad, othin=rthin, $
noclose=noclose, pcaonly=pcaonly

;This program is an attempt to automatically plot all parameters,
;derived from plotallrise_mod4 based on Mitch's comments and
;also to transfer to input structure
;
;INPUTS
;
; inpstr: input structure that holds all required parameters
; inx: index of the structure that will point out the correct source
;      and year
;
;OPTIONAL INPUT
;
; ps: postscript output
; namef: ps output filename
; usecol:use color scheme
; radonly: use only radio data if NIR not present
; noplotrin: do not plot rin default plot
; facr: renormalize radio flux to fit in plot (if automatic scaling
;       does not work best)
; rmsyr: yrange of rms
; indyr: yrange of ind
; tinyr: yrange of tin
; rinyr: yrange of rin
; plfyr: yrange of plf
; mwyr: yrange of mw
; xrpl: xrange
; bolcor: do bolometric corrections
; realtime: if set, use actual MJD rather than timing transition
; onlycert: plot jet observed data beyond the first actual observation
;           (this could be obsolete)
; band: decide which band to use in multiwavelength plot, default "J"
; rfreq: decide which radio frequency to use in multiwavlength plot,
;        default 4.8
; plotrad: plot radio info with near infrared
; othin: time before optically thin flare, but after a compact jet (think about a
;        smart way later)
; noclose: do not close the ps file
; pcaonly: there is only pca data present
;
;USES
; multiplot
; calcfluxv2
;
;USED BY
;
;allinfo programs
;
;Created by Emrah Kalemci, 01/11/2014
;
;
;MODIFIED FROM plotalldecay_mod4.pro
;
; LOGS
; 
; Dec 4: Fixed some problems with the OIR plotting routines. Removed
;        undefined variable mfv?.
;
; Dec 15: radonly part written
;

IF NOT keyword_set(radonly) THEN radonly=0  ;only radio data present?
IF NOT keyword_set(noplotrin) THEN noplotrin=0 ; do not plot rin

IF NOT keyword_set(ps) THEN ps=0
IF (ps AND NOT keyword_set(fname)) THEN fname='alldatarise.eps'
IF NOT keyword_set(usecol) THEN usecol=0
IF NOT keyword_set(onlycert) THEN onlycert=0
IF NOT keyword_set(realtime) THEN realtime=0
IF NOT keyword_set(bolcor) THEN bolcor=0
IF NOT keyword_set(mwband) THEN mwband='J'
IF NOT keyword_set(freqr) THEN freqr=4.8
IF NOT keyword_set(plotrad) THEN plotrad=0
IF NOT keyword_set(noclose) THEN noclose=0
IF NOT keyword_set(pcaonly) THEN pcaonly=0

device,decomposed=0
IF usecol THEN BEGIN 
   loadct,4
   colcode=[184,120,40,150,130] ; color cordes for HS, HIMS, SIMS, US, SS
ENDIF ELSE BEGIN
   loadct,0
   colcode=[195,155,95,15,0]
ENDELSE

if ps then begin
   set_plot, 'ps' 
   device,/color
;   loadct,5
   device,/encapsulated
   device, filename = fname
   device, yoffset = 2
   device, ysize = 28.
   ;device, xsize = 12.0
   !p.font=0
   device,/times
endif

IF NOT ps THEN window, 4, retain=2, xsize=800, ysize=800
cs=1.3

dates=inpstr[inx].xdates
ttrans=inpstr[inx].ttrans
zz=where(dates ne 0.)
IF realtime THEN time=dates[zz] ELSE time=dates[zz]-ttrans[0]

states=inpstr[inx].states[where(inpstr[inx].states NE -1)]

;aa=hard state 0
;bb=hims 1 
;cc=sims 2
;dd=ultrasoft 3
;ee=soft state 4

;;expected order hs, hims, sims, ss (us can replace those)

IF keyword_set(plxr) THEN xr=plxr ELSE xr=[min(time)-2.,max(time)+2.]

PLOTSYM,0,1,/FILL

IF realtime THEN BEGIN
    IF noplotrin THEN multiplot, [1,5], mxtitle='Time (MJD-50000 days)',mxtitsize=1.2 ELSE $
    multiplot, [1,6], mxtitle='Time (MJD-50000 days)',mxtitsize=1.2
    fac=ttrans[0]
ENDIF ELSE BEGIN
  IF noplotrin THEN multiplot, [1,5], mxtitle='Time (Days from timing transiton)',mxtitsize=1.2 ELSE $
  multiplot, [1,6], mxtitle='Time (Days from timing transition)',mxtitsize=1.2
  fac=0.
ENDELSE

;RMS

rms=inpstr[inx].rms

;take out upperlimits

 aaxx=where((rms[1,*] gt 0.) AND (states eq 0))
 bbxx=where((rms[1,*] gt 0.) AND (states eq 1))
 ccxx=where((rms[1,*] gt 0.) AND (states eq 2))
 ddxx=where((rms[1,*] gt 0.) AND (states eq 3))
 eexx=where((rms[1,*] gt 0.) AND (states eq 4))

 aayy=where((rms[1,*] eq -1) AND (states eq 0))
 bbyy=where((rms[1,*] eq -1) AND (states eq 1))
 ccyy=where((rms[1,*] eq -1) AND (states eq 2))
 ddyy=where((rms[1,*] eq -1) AND (states eq 3))
 eeyy=where((rms[1,*] eq -1) AND (states eq 4))

IF keyword_set(yrrms) THEN yr=yrrms ELSE yr=[0.9*min(rms[0,*]),1.1*max(rms[0,*])]

xrr=xr
ploterror, time+fac-ttrans[0], [200,200],[0,0], psym=3, ytitle='rms amp. (%)',$
xrange=xrr,/xstyle,/nohat,/ystyle,yr=yr, chars=cs


oplot,[0,0]+fac,!y.crange


IF aaxx[0] ne -1 THEN oploterror,time[aaxx],rms(0,aaxx),rms(1,aaxx),psym=8,/nohat,color=colcode[0], errcol=colcode[0]
IF bbxx[0] ne -1 THEN oploterror,time[bbxx],rms(0,bbxx),rms(1,bbxx),psym=8,/nohat,color=colcode[1],errcol=colcode[1]
IF ccxx[0] ne -1 THEN oploterror,time[ccxx],rms(0,ccxx),rms(1,ccxx),psym=8,/nohat,color=colcode[2],errcol=colcode[2]
IF ddxx[0] ne -1 THEN oploterror,time[ddxx],rms(0,ddxx),rms(1,ddxx),psym=8,/nohat,color=colcode[3],errcol=colcode[3]
IF eexx[0] ne -1 THEN oploterror,time[eexx],rms(0,eexx),rms(1,eexx),psym=8,/nohat,color=colcode[4],errcol=colcode[4]


IF aayy[0] ne -1 THEN FOR k=0,n_elements(aayy)-1 DO arrow, time[aayy[k]], rms[0,aayy[k]], time[aayy[k]], yr[0], /data, color=colcode[0]
IF bbyy[0] ne -1 THEN FOR k=0,n_elements(bbyy)-1 DO arrow, time[bbyy[k]], rms[0,bbyy[k]], time[bbyy[k]], yr[0], /data, color=colcode[1]
IF ccyy[0] ne -1 THEN FOR k=0,n_elements(ccyy)-1 DO arrow, time[ccyy[k]], rms[0,ccyy[k]], time[ccyy[k]], yr[0], /data, color=colcode[2]
IF ddyy[0] ne -1 THEN FOR k=0,n_elements(ddyy)-1 DO arrow, time[ddyy[k]], rms[0,ddyy[k]], time[ddyy[k]], yr[0], /data, color=colcode[3]
IF eeyy[0] ne -1 THEN FOR k=0,n_elements(eeyy)-1 DO arrow, time[eeyy[k]], rms[0,eeyy[k]], time[eeyy[k]], yr[0], /data, color=colcode[4]

poslx=!x.crange[0]+(!x.crange[1]-!x.crange[0])*0.9
posly=!y.crange[0]+(!y.crange[1]-!y.crange[0])*0.8
xyouts,poslx,posly,'a',size=1.5

multiplot

; SPECTRAL INDEX

IF pcaonly THEN ind=inpstr[inx].indp ELSE ind=inpstr[inx].ind

aa=where(states eq 0)
bb=where(states eq 1)
cc=where(states eq 2)
dd=where(states eq 3)
ee=where(states eq 4)


IF keyword_set(yrind) THEN yr=yrind ELSE yr=[0.9*min(ind[0,*]),1.1*max(ind[0,*])]

ploterror, time,[100,100],[0,0],psym=3, ytitle='!9G !X',$
xrange=xrr,/xstyle,/nohat,/ystyle, yr=yr, color=0, chars=cs ; dummy frame

oplot,[0,0]+fac,!y.crange


IF aa[0] ne -1 THEN oploterror,time[aa],ind[0,aa],ind[1,aa],psym=8,$
/nohat,color=colcode[0], errcol=colcode[0]

IF bb[0] NE -1 THEN oploterror,time[bb],ind[0,bb],ind[1,bb],psym=8,$
/nohat,color=colcode[1],errcol=colcode[1]

IF cc[0] NE -1 THEN oploterror,time[cc],ind[0,cc],ind[1,cc],psym=8,$
/nohat,color=colcode[2],errcol=colcode[2]

IF dd[0] NE -1 THEN oploterror,time[dd],ind[0,dd],ind[1,dd],psym=8,$
/nohat,color=colcode[3],errcol=colcode[3]

IF ee[0] NE -1 THEN oploterror,time[ee],ind[0,ee],ind[1,ee],psym=8,$
/nohat,color=colcode[4],errcol=colcode[4]

poslx=!x.crange[0]+(!x.crange[1]-!x.crange[0])*0.9
posly=!y.crange[0]+(!y.crange[1]-!y.crange[0])*0.8
xyouts,poslx,posly,'b',size=1.5

multiplot

;INNER DISK TEMPERATURE

IF pcaonly THEN tin=inpstr[inx].tinp ELSE tin=inpstr[inx].tin

IF keyword_set(yrtin) THEN yr=yrtin ELSE yr=[0.9*min(tin[0,*]),1.1*max(tin[0,*])]

ploterror,time,[100,100],[0,0],psym=3,ytitle='Tin',$
  xrange=xrr,/xstyle,/nohat,/ystyle,yr=yr, chars=cs

  oplot,[0,0]+fac,!y.crange


IF aa[0] ne -1 THEN oploterror,time[aa],tin[0,aa],tin[1,aa],psym=5,/nohat,color=colcode[0],errcol=colcode[0],syms=1.3,thick=1.5
IF bb[0] ne -1 THEN oploterror,time[bb],tin[0,bb],tin[1,bb],psym=5,/nohat,color=colcode[1],errcol=colcode[1],syms=1.3,thick=1.5
IF cc[0] ne -1 THEN oploterror,time[cc],tin[0,cc],tin[1,cc],psym=5,/nohat,color=colcode[2],errcol=colcode[2],syms=1.3,thick=1.5
IF dd[0] ne -1 THEN oploterror,time[dd],tin[0,dd],tin[1,dd],psym=5,/nohat,color=colcode[3],errcol=colcode[3],syms=1.3,thick=1.5
IF ee[0] ne -1 THEN oploterror,time[ee],tin[0,ee],tin[1,ee],psym=5,/nohat,color=colcode[4],errcol=colcode[4],syms=1.3,thick=1.5


poslx=!x.crange[0]+(!x.crange[1]-!x.crange[0])*0.9
posly=!y.crange[0]+(!y.crange[1]-!y.crange[0])*0.5
xyouts,poslx,posly,'c',size=1.5

  multiplot

; PLF and DBB, here we need to do bolometric corrections and plot ELFs

IF bolcor THEN BEGIN
   calcfluxv2, inpstr, inx, outdelf, outpelf, outtelf, /dcor, /pcor, pcaonly=pcaonly
   IF keyword_set(yrplf) THEN yr=yrplf ELSE BEGIN
      miny=min(outdelf[0,*]-outdelf[1,*]) < min(outpelf[0,*]-outpelf[1,*])
      miny=miny*0.9
      maxy=max(outdelf[0,*]+outdelf[1,*]) > max(outpelf[0,*]+outpelf[1,*])
      maxy=maxy*1.1
      yr=[miny,maxy]
   ENDELSE

  
   ploterror,time, [0,0],[0,0],psym=3,ytitle='PLF, DBB ', $ ;x 10!E-10!N',$
        xrange=xrr,/xstyle,/ystyle,/ylog,yr=yr, chars=cs*0.8, ytickformat='exponent',/nodata

   oplot,[0,0]+fac,10^(!y.crange)

   IF aa[0] ne -1 THEN BEGIN
      oploterror,time[aa],outpelf[0,aa],outpelf[1,aa],psym=8,color=colcode[0],/nohat,errcol=colcode[0]
      oploterror,time[aa],outdelf[0,aa],outdelf[1,aa],psym=5,color=colcode[0],syms=1.3,thick=1.5,/nohat,errcol=colcode[0]
   ENDIF

   IF bb[0] ne -1 THEN BEGIN
      oploterror,time[bb],outpelf[0,bb],outpelf[1,bb],psym=8,color=colcode[1],/nohat,errcol=colcode[1]
      oploterror,time[bb],outdelf[0,bb],outdelf[1,bb],psym=5,color=colcode[1],syms=1.3,thick=1.5,/nohat,errcol=colcode[1]
   ENDIF

   IF cc[0] ne -1 THEN BEGIN
      oploterror,time[cc],outpelf[0,cc],outpelf[1,cc],psym=8,color=colcode[2],/nohat,errcol=colcode[2]
      oploterror,time[cc],outdelf[0,cc],outdelf[1,cc],psym=5,color=colcode[2],syms=1.3,thick=1.5,/nohat,errcol=colcode[2]
   ENDIF

   IF dd[0] ne -1 THEN BEGIN
      oploterror,time[dd],outdelf[0,dd],outdelf[1,dd],psym=5,color=colcode[3],syms=1.3,thick=1.5,/nohat,errcol=colcode[3]
      oploterror,time[dd],outpelf[0,dd],outpelf[1,dd],psym=8,color=colcode[3],/nohat,errcol=colcode[3]
   ENDIF

   IF ee[0] ne -1 THEN BEGIN
      oploterror,time[ee],outpelf[0,ee],outpelf[1,ee],psym=8,color=colcode[4],/nohat,errcol=colcode[4]
      oploterror,time[ee],outdelf[0,ee],outdelf[1,ee],psym=5,color=colcode[4],syms=1.3,thick=1.5,/nohat,errcol=colcode[4]
   ENDIF

ENDIF ELSE BEGIN
   IF keyword_set(yrplf) THEN yr=yrplf ELSE yr=[0.7*min([dbb,plf]),1.5*max([dbb,plf])]

   plot,[100,100],[100,100],psym=3,ytitle='PLF, DBB ', $ ;x 10!E-10!N',$
        xrange=xrr,/xstyle,/ystyle,/ylog,yr=yr, chars=cs*0.8, ytickformat='exponent'

   oplot,[0,0]+fac,10^(!y.crange)

   IF aa[0] ne -1 THEN BEGIN
      oplot,time[aa],plf[aa],psym=8,color=colcode[0]
      oplot,time[aa],dbb[aa],psym=5,color=colcode[0],syms=1.3,thick=1.5
   ENDIF

   IF bb[0] ne -1 THEN BEGIN
      oplot,time[bb],plf[bb],psym=8,color=colcode[1]
      oplot,time[bb],dbb[bb],psym=5,color=colcode[1],syms=1.3,thick=1.5
   ENDIF

   IF cc[0] ne -1 THEN BEGIN
      oplot,time[cc],plf[cc],psym=8,color=colcode[2]
      oplot,time[cc],dbb[cc],psym=5,color=colcode[2],syms=1.3,thick=1.5
   ENDIF

   IF dd[0] ne -1 THEN BEGIN
      oplot,time[dd],dbb[dd],psym=5,color=colcode[3],syms=1.3,thick=1.5
      oplot,time[dd],plf[dd],psym=8,color=colcode[3]
   ENDIF

   IF ee[0] ne -1 THEN BEGIN
      oplot,time[ee],plf[ee],psym=8,color=colcode[4]
      oplot,time[ee],dbb[ee],psym=5,color=colcode[4],syms=1.3,thick=1.5
   ENDIF
ENDELSE

poslx=!x.crange[0]+(!x.crange[1]-!x.crange[0])*0.9
posly=10.^(!y.crange[1])*0.05
xyouts,poslx,posly,'d',size=1.5

;rin

multiplot

IF NOT NOPLOTRIN THEN BEGIN

drat=inpstr[inx].distance[0]/10. 
cosi=cos(inpstr[inx].inc[0]*!PI/180.)
normp=inpstr[inx].normp

dv=where(normp[0,*] gt 0.)
rin=normp
rin[0,*]=drat*sqrt(1./cosi)*sqrt(normp[0,*])
rin[1,dv]=0.5 * drat*sqrt(1./cosi) * normp[1,dv] / sqrt(normp[0,dv])

IF KEYWORD_SET(yrrin) THEN yr=yrrin ELSE yr=[0.9*min(rin[0,(where(rin[0,*] NE 0.))]),1.1*max(rin[0,(where(rin[0,*] NE 0.))])]
;yr=yrrin


ploterror, time,[100,100],[0,0],psym=3, ytitle='Rin*',$
xrange=xrr,/xstyle,/nohat,/ystyle, yr=yr, color=0, chars=cs ; dummy frame


oplot,[0,0]+fac,!y.crange


IF aa[0] ne -1 THEN oploterror,time[aa],rin[0,aa],rin[1,aa],psym=5,syms=1.3,thick=1.5,$
/nohat,color=colcode[0], errcol=colcode[0]

IF bb[0] NE -1 THEN oploterror,time[bb],rin[0,bb],rin[1,bb],psym=5,syms=1.3,thick=1.5,$
/nohat,color=colcode[1],errcol=colcode[1]

IF cc[0] NE -1 THEN oploterror,time[cc],rin[0,cc],rin[1,cc],psym=5,syms=1.3,thick=1.5,$
/nohat,color=colcode[2],errcol=colcode[2]

IF dd[0] NE -1 THEN oploterror,time[dd],rin[0,dd],rin[1,dd],psym=5,syms=1.3,thick=1.5,$
/nohat,color=colcode[3],errcol=colcode[3]

 IF ee[0] NE -1 THEN oploterror,time[ee],rin[0,ee],rin[1,ee],psym=5,syms=1.3,thick=1.5,$
/nohat,color=colcode[4],errcol=colcode[4]

xx=where(normp[1,*] eq -1)
IF xx[0] NE -1 THEN BEGIN
   FOR k=0,n_elements(xx)-1 DO BEGIN
      rin=sqrt(normp[0,xx[k]]*((0.8)^2.)/0.5)
      IF rin GT yr[1] THEN rin=yr[1]
      arrow, time[xx[k]]+fac, rin, time[xx[k]], yr[0], color=colcode[states[xx[k]]],/data
   ENDFOR
ENDIF

poslx=!x.crange[0]+(!x.crange[1]-!x.crange[0])*0.9
posly=!y.crange[0]+(!y.crange[1]-!y.crange[0])*0.5
xyouts,poslx,posly,'e',size=1.5

ENDIF

; MULTIWAVELENGTH DATA

multiplot

plotsym,0,/fill

IF radonly THEN BEGIN

   inforad=inpstr[inx].radinfo
   fre1=where(inforad.freq eq freqr)
   IF realtime THEN rtime=inforad.dates[fre1] ELSE rtime=inforad.dates[fre1]-ttrans[0]
   yy=where(inforad.flux[1,fre1] GT 0.) ;actual data
   xx=where(inforad.flux[1,fre1] EQ -1) ;upper limits
   mwflux=inforad.flux[0,yy]
   ytit='Radio flux'
   
   aam=getboundary(states, time, rtime, 0)
   bbm=getboundary(states, time, rtime, 1)
   ccm=getboundary(states, time, rtime, 2)
   ddm=getboundary(states, time, rtime, 3)
   eem=getboundary(states, time, rtime, 4)
 
    IF keyword_set(yrmw) THEN yr=yrmw ELSE yr=[0.9*min(mwflux),1.1*max(mwflux)]
                            
        ploterror, [200,200], [100,100],[0,0], psym=3, ytitle=ytit, $
                            yr=yr, xrange=xr,/xstyle,/ystyle,/nohat,chars=cs
   IF yy[0] NE -1 THEN BEGIN
      IF keyword_set(rthin) THEN BEGIN
               yyc=where(rtime[yy] LT rthin)
               plotsym, 5, /fill
               IF yyc[0] NE -1 THEN oploterror, rtime[yy[yyc]], inforad.flux[0,fre1[yy[yyc]]], inforad.flux[1,fre1[yy[yyc]]],psym=8,/nohat,symsize=1. 
               yyf=where(rtime[yy] GE rthin)
               plotsym, 5
               IF yyf[0] NE -1 THEN oploterror, rtime[yy[yyf]], inforad.flux[0,fre1[yy[yyf]]], inforad.flux[1,fre1[yy[yyf]]],psym=8,/nohat,symsize=1. 
            ENDIF ELSE BEGIN
               plotsym, 5, /fill
               oploterror, rtime[yy], inforad.flux[0,fre1[yy]], inforad.flux[1,fre1[yy]],psym=8,/nohat,symsize=1. 
            ENDELSE
         ENDIF
   
ENDIF ELSE BEGIN

   mwi=where(inpstr[inx].oirinfo.band EQ mwband)
   IF realtime THEN mwtime=inpstr[inx].oirinfo.dates[mwi] ELSE mwtime=inpstr[inx].oirinfo.dates[mwi]-ttrans[0]

   ytit='NIR&Radio flux'
   mwi=where(inpstr[inx].oirinfo.band EQ mwband) 
   mwmag=inpstr[inx].oirinfo.mag
   sz=size(mwmag)

     mwmag1=mwmag[*,mwi]
    mwtime=mwtime[where(mwmag1[0,*] NE 0.)]
    mwmag=mwmag1[*,where(mwmag1[0,*] NE 0.)]

   aam=getboundary(states, time, mwtime, 0)
   bbm=getboundary(states, time, mwtime, 1)
   ccm=getboundary(states, time, mwtime, 2)
   ddm=getboundary(states, time, mwtime, 3)
   eem=getboundary(states, time, mwtime, 4)

   IF sz(1) eq 2 THEN BEGIN

         mwflux=mag2flux(mwmag[0,*])
         mwflux=mwflux/min(mwflux)
         mwfluxe=mwflux*mwmag[1,*]/mwmag[0,*]

        
      IF keyword_set(yrmw) THEN yr=yrmw ELSE yr=[0.9*min(mwflux),1.1*max(mwflux)]
                            
        ploterror, [200,200], [100,100],[0,0], psym=3, ytitle=ytit, $
                            yr=yr, xrange=xr,/xstyle,/ystyle,/nohat,chars=cs
                            
;xtickformat='(I4)',/nohat, chars=1.2

        IF aam[0] ne -1 THEN oploterror, mwtime[aam], mwflux[aam], mwfluxe[aam], psym=8, /nohat, color=colcode[0],errcol=colcode[0]
        IF bbm[0] ne -1 THEN oploterror, mwtime[bbm], mwflux[bbm], mwfluxe[bbm], psym=8, /nohat, color=colcode[1],errcol=colcode[1]
        IF ccm[0] ne -1 THEN oploterror, mwtime[ccm], mwflux[ccm], mwfluxe[ccm], psym=8, /nohat, color=colcode[2],errcol=colcode[2]
        IF ddm[0] ne -1 THEN oploterror, mwtime[ddm], mwflux[ddm], mwfluxe[ddm], psym=8, /nohat, color=colcode[3],errcol=colcode[3]
        IF eem[0] ne -1 THEN oploterror, mwtime[eem], mwflux[eem], mwfluxe[eem], psym=8, /nohat, color=colcode[4],errcol=colcode[4]

   ENDIF ELSE BEGIN

      mwflux=mag2flux(mwmag[0,*])
      mwflux=mwflux/min(mwflux)

      IF keyword_set(yrmw) THEN yr=yrmw ELSE yr=[0.9*min(mwflux),1.1*max(mwflux)]

      plot, [200.,200.], [100.,100.], psym=8, ytitle=ytit,yr=yr, $
            xrange=xr,/xstyle,/ystyle,xtickformat='(I4)', chars=1.2

        IF aam[0] ne -1 THEN oplot, mwtime[aam], mwflux[aam], psym=8, color=colcode[0]
        IF bbm[0] ne -1 THEN oplot, mwtime[bbm], mwflux[bbm], psym=8, color=colcode[1]
        IF ccm[0] ne -1 THEN oplot, mwtime[ccm], mwflux[ccm], psym=8, color=colcode[2]
        IF ddm[0] ne -1 THEN oplot, mwtime[ddm], mwflux[ddm], psym=8, color=colcode[3]
        IF eem[0] ne -1 THEN oplot, mwtime[eem], mwflux[eem], psym=8, color=colcode[4]

        ENDELSE
ENDELSE

IF keyword_set(plotrad) THEN BEGIN

        inforad=inpstr[inx].radinfo
        fre1=where(inforad.freq eq freqr)
        IF realtime THEN rtime=inforad.dates[fre1] ELSE rtime=inforad.dates[fre1]-ttrans[0]
        yy=where(inforad.flux[1,fre1] GT 0.) ;actual data
        xx=where(inforad.flux[1,fre1] EQ -1) ;upper limits

        IF NOT keyword_set(fr) THEN fr=0.8*yr[1]/max(inforad.flux[0,fre1[yy]])        
        print,fr

        
         IF yy[0] NE -1 THEN BEGIN
            IF keyword_set(rthin) THEN BEGIN
               yyc=where(rtime[yy] LT rthin)
               plotsym, 5, /fill
               IF yyc[0] NE -1 THEN oploterror, rtime[yy[yyc]], inforad.flux[0,fre1[yy[yyc]]]*fr, inforad.flux[1,fre1[yy[yyc]]]*fr,psym=8,/nohat,symsize=1. 
               yyf=where(rtime[yy] GE rthin)
               plotsym, 5
               IF yyf[0] NE -1 THEN oploterror, rtime[yy[yyf]], inforad.flux[0,fre1[yy[yyf]]]*fr, inforad.flux[1,fre1[yy[yyf]]]*fr,psym=8,/nohat,symsize=1. 
            ENDIF ELSE BEGIN
               plotsym, 5, /fill
               oploterror, rtime[yy], inforad.flux[0,fre1[yy]]*fr, inforad.flux[1,fre1[yy]]*fr,psym=8,/nohat,symsize=1. 
            ENDELSE
         ENDIF



;       oplot,[-15,-15],[5.3,5.3],psym=8,color=colcode[0]
 ;       xyouts,-14.5,4.8,'Rel. NIR flux'         
;         plotsym,8, /fill

;        oplot,[-15,-15],[4.0,4.0],psym=8,color=0
;        xyouts,-14.5,3.8,'Rel. Radio flare flux'         
 

;            oploterror, rtime[yy[8:14]], inforad.flux[0,fre1[yy[8:14]]]*fr, inforad.flux[1,fre1[yy[8:14]]]*fr,psym=8,/nohat,symsize=1.
         IF xx[0] ne -1 THEN FOR i=0,n_elements(xx)-1 DO ARROW, rtime[xx[i]], inforad.flux[0,fre1[xx[i]]]*fr, rtime[xx[i]], yr[0], /data
   ;         oplot,[rtime[xx[i]]-1,rtime[xx[i]]+1],[1.,1.]*inforad.flux[0,fre1[xx[i]]]*fr
    ;     ENDFOR

;         oplot,[-17.5,-17.5],[3.6,3.6],psym=8,color=0
;        xyouts,-17,3.4,'Radio (flare) flux'          
  
        ENDIF

;oplot,[ustrans[0],ustrans[0]]+fac,!y.crange,line=2
oplot,[0,0]+fac,!y.crange


poslx=!x.crange[0]+(!x.crange[1]-!x.crange[0])*0.9
posly=!y.crange[0]+(!y.crange[1]-!y.crange[0])*0.5
xyouts,poslx,posly,'f',size=1.5

multiplot,/default

IF NOT noclose THEN BEGIn
 IF ps THEN BEGIN
   device,/close
  IF !VERSION.OS eq 'Win32' THEN set_plot,'win' ELSE set_plot,'x'
  ENDIF
ENDIF

END

function getboundary, states, time, mwtime, sval

xx=where(states eq sval)

;check if region exists

IF xx[0] EQ -1 THEN return, -1 ; no boundary then impossible region

;check if only one region exists

singlebound=0
IF max(xx)-min(xx) EQ n_elements(xx)-1 THEN singlebound=1 ;are elements consecutive?

IF singlebound THEN BEGIN
   boundary=fltarr(2)
   IF xx[0] EQ 0 THEN boundary[0]=-1e5 ELSE boundary[0]=(time[xx[0]-1]+time[xx[0]])/2.
   IF max(xx) EQ n_elements(states)-1 THEN boundary[1]=1E5 ELSE boundary[1]=(time[max(xx)]+time[max(xx)+1])/2.
   return, where((mwtime gt boundary[0]) AND (mwtime le boundary[1]))
ENDIF ELSE BEGIN
     IF xx[0] EQ 0 THEN boundary_first=-1e5 ELSE boundary_first=(time[xx[0]-1]+time[xx[0]])/2.
     IF max(xx) EQ n_elements(states)-1 THEN boundary_last=1E5 ELSE boundary_last=(time[max(xx)]+time[max(xx)+1])/2.

   k=0

   indm=-100
   WHILE k LT N_ELEMENTS(xx)-1 DO BEGIN
      ccon=xx[k+1]-xx[k]
      IF ccon NE 1 THEN BEGIN
         boundary_end=(time[xx[k]]+time[xx[k]+1])/2.
         indcs=where((mwtime gt boundary_first) AND (mwtime le boundary_end))
         indm=[indm,indcs]
         boundary_first=(time[xx[k+1]]+time[xx[k+1]-1])/2.
      ENDIF
      k=k+1
   ENDWHILE
   indcs=where((mwtime gt boundary_first) AND (mwtime le boundary_last))
   indm=[indm,indcs]
   return, indm[1:n_elements(indm)-1]
ENDELSE

END
