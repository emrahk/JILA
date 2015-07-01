pro plotallrise_mod4, dates, ttrans, states, $
plf, dbb, ind, tin, rms, mwdate, mwmag, normp, rad=rad, $
ps=ps,namef=fname, noplotrin=noplotrin, inforad=inforad, $
facr=fr, rmsyr=yrrms, indyr=yrind, tinyr=yrtin, rinyr=yrrin,  $
plfyr=yrplf, mwyr=yrmw, xrpl=plxr, bolcor=bolcor,$
usecol=usecol, onlycert=onlycert, realtime=realtime

;This program is an attempt to automatically plot all parameters
;
;INPUTS
;
;dates: X-ray dates
;ttrans: timing transition in MJD? do we need additional?
;himstrans : hard intermediate state?
;simstrans : soft intermediate state? usually corresponds to timing?
;or is it the hims?
;ustrans : ultrasoft state, if present
;sstrans : soft state if present
;plf: power law flux
;dbb: diskbb flux
;ind: index
;tin: inner disk temperature
;rms: rms
;mwdate: multiwavelength dates
;mwmag: multiwavelength mags
;
;OPTIONAL INPUT
;
;rad: if present use radio data
;ps: postscript output
;namef: ps output filename
;noplotrin: do not plot rin? default plot
;rmsyr: yrange of rms
;indyr: yrange of ind
;tinyr: yrange of tin
;plfyr: yrange of plf
;mwyr: yrange of mw
;xrpl: xrange
;usecol:use color scheme
;onlycert: plot jet observed data beyond the first actual observation

;USES
;multiplot
;
;USED BY
;
;allinfo programs
;
;Created by Emrah Kalemci, 01/06/2012
;
;added color option
;
;added onlycert option, 11/08/2012
;
;several improvements in coloring and timing plot, 30/06/2013
;
;MODIFIED FROM plotalldecay.pro
;

IF NOT keyword_set(rad) THEN rad=0  ;is there radio data?
IF NOT keyword_set(noplotrin) THEN noplotrin=0 ; do not plot tin

IF NOT keyword_set(ps) THEN ps=0
IF (ps AND NOT keyword_set(fname)) THEN fname='alldatarise.eps'
IF NOT keyword_set(usecol) THEN usecol=0
;IF NOT keyword_setx(fr) THEN fr=1. ;this is to rescale radio, I need to find a smart way of doing this

IF NOT keyword_set(onlycert) THEN onlycert=0
IF NOT keyword_set(realtime) THEN realtime=0
IF NOT keyword_set(bolcor) THEN bolcor=0

device,decomposed=0
IF usecol THEN BEGIN 
   loadct,4
   colcode=[184,120,40,150,15];
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
;time=dates
zz=where(dates ne 0.)
time=dates[zz]-ttrans[0]
;change in paradigm, do everything in realtime, and then change during plotting
;IF realtime THEN fac=ttrans[0]-50000. else fac=0

;aa=hard state 0
;bb=hims 1 
;cc=sims 2
;dd=ultrasoft 3
;ee=soft state 4

;;expected order hs, hims, sims, ss (us can replace those)
;set once!
;need to be smart here

IF keyword_set(plxr) THEN xr=plxr ELSE xr=[min(time)-2.,max(time)+2.]

PLOTSYM,0,1,/FILL

IF realtime THEN BEGIN
    IF noplotrin THEN multiplot, [1,5], mxtitle='Time (MJD-50000 days)',mxtitsize=1.2 ELSE $
  multiplot, [1,6], mxtitle='Time (MJD-50000 days)',mxtitsize=1.2
ENDIF ELSE BEGIN
  IF noplotrin THEN multiplot, [1,5], mxtitle='Time (Days from timing transiton)',mxtitsize=1.2 ELSE $
  multiplot, [1,6], mxtitle='Time (Days from timing transition)',mxtitsize=1.2
ENDELSE

;RMS

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

 fac=0
IF keyword_set(yrrms) THEN yr=yrrms ELSE yr=[0.9*min(rms[0,*]),1.1*max(rms[0,*])]

xrr=xr+fac
ploterror, time+fac-ttrans[0], [200,200],[0,0], psym=3, ytitle='rms amp. (%)',$
xrange=xrr,/xstyle,/nohat,/ystyle,yr=yr, chars=cs


oplot,[0,0]+fac,!y.crange


IF aaxx[0] ne -1 THEN oploterror,time[aaxx]+fac,rms(0,aaxx),rms(1,aaxx),psym=8,/nohat,color=colcode[0], errcol=colcode[0]
IF bbxx[0] ne -1 THEN oploterror,time[bbxx]+fac,rms(0,bbxx),rms(1,bbxx),psym=8,/nohat,color=colcode[1],errcol=colcode[1]
IF ccxx[0] ne -1 THEN oploterror,time[ccxx]+fac,rms(0,ccxx),rms(1,ccxx),psym=8,/nohat,color=colcode[2],errcol=colcode[2]
IF ddxx[0] ne -1 THEN oploterror,time[ddxx]+fac,rms(0,ddxx),rms(1,ddxx),psym=8,/nohat,color=colcode[3],errcol=colcode[3]
IF eexx[0] ne -1 THEN oploterror,time[eexx]+fac,rms(0,eexx),rms(1,eexx),psym=8,/nohat,color=colcode[4],errcol=colcode[4]


IF aayy[0] ne -1 THEN FOR k=0,n_elements(aayy)-1 DO arrow, time[aayy[k]]+fac, rms[0,aayy[k]], time[aayy[k]]+fac, yr[0], /data, color=colcode[0]
IF bbyy[0] ne -1 THEN FOR k=0,n_elements(bbyy)-1 DO arrow, time[bbyy[k]]+fac, rms[0,bbyy[k]], time[bbyy[k]]+fac, yr[0], /data, color=colcode[1]
IF ccyy[0] ne -1 THEN FOR k=0,n_elements(ccyy)-1 DO arrow, time[ccyy[k]]+fac, rms[0,ccyy[k]], time[ccyy[k]]+fac, yr[0], /data, color=colcode[2]
IF ddyy[0] ne -1 THEN FOR k=0,n_elements(ddyy)-1 DO arrow, time[ddyy[k]]+fac, rms[0,ddyy[k]], time[ddyy[k]]+fac, yr[0], /data, color=colcode[3]
IF eeyy[0] ne -1 THEN FOR k=0,n_elements(eeyy)-1 DO arrow, time[eeyy[k]]+fac, rms[0,eeyy[k]], time[eeyy[k]]+fac, yr[0], /data, color=colcode[3]

poslx=!x.crange[0]+(!x.crange[1]-!x.crange[0])*0.9
posly=!y.crange[0]+(!y.crange[1]-!y.crange[0])*0.8
xyouts,poslx,posly,'a',size=1.5

multiplot

aa=where(states eq 0)
bb=where(states eq 1)
cc=where(states eq 2)
dd=where(states eq 3)
ee=where(states eq 4)


IF keyword_set(yrind) THEN yr=yrind ELSE yr=[0.9*min(ind[0,*]),1.1*max(ind[0,*])]

ploterror, time+fac-ttrans[0],[100,100],[0,0],psym=3, ytitle='!9G !X',$
xrange=xrr,/xstyle,/nohat,/ystyle, yr=yr, color=0, chars=cs ; dummy frame

oplot,[0,0]+fac,!y.crange


IF aa[0] ne -1 THEN oploterror,time[aa]+fac,ind[0,aa],ind[1,aa],psym=8,$
/nohat,color=colcode[0], errcol=colcode[0]

IF bb[0] NE -1 THEN oploterror,time[bb]+fac,ind[0,bb],ind[1,bb],psym=8,$
/nohat,color=colcode[1],errcol=colcode[1]

IF cc[0] NE -1 THEN oploterror,time[cc]+fac,ind[0,cc],ind[1,cc],psym=8,$
/nohat,color=colcode[2],errcol=colcode[2]

IF dd[0] NE -1 THEN oploterror,time[dd]+fac,ind[0,dd],ind[1,dd],psym=8,$
/nohat,color=colcode[3],errcol=colcode[3]

IF ee[0] NE -1 THEN oploterror,time[ee]+fac,ind[0,ee],ind[1,ee],psym=8,$
/nohat,color=colcode[3],errcol=colcode[3]


poslx=!x.crange[0]+(!x.crange[1]-!x.crange[0])*0.9
posly=!y.crange[0]+(!y.crange[1]-!y.crange[0])*0.8
xyouts,poslx,posly,'b',size=1.5

multiplot

IF keyword_set(yrtin) THEN yr=yrtin ELSE yr=[0.9*min(tin[0,*]),1.1*max(tin[0,*])]

ploterror,time+fac-ttrans[0],[100,100],[0,0],psym=3,ytitle='Tin',$
  xrange=xrr,/xstyle,/nohat,/ystyle,yr=yr, chars=cs

  oplot,[0,0]+fac,!y.crange


IF aa[0] ne -1 THEN oploterror,time[aa]+fac,tin[0,aa],tin[1,aa],psym=8,/nohat,color=colcode[0],errcol=colcode[0]
IF bb[0] ne -1 THEN oploterror,time[bb]+fac,tin[0,bb],tin[1,bb],psym=8,/nohat,color=colcode[1],errcol=colcode[1]
IF cc[0] ne -1 THEN oploterror,time[cc]+fac,tin[0,cc],tin[1,cc],psym=8,/nohat,color=colcode[2],errcol=colcode[2]
IF dd[0] ne -1 THEN oploterror,time[dd]+fac,tin[0,dd],tin[1,dd],psym=8,/nohat,color=colcode[3],errcol=colcode[3]
IF ee[0] ne -1 THEN oploterror,time[ee]+fac,tin[0,ee],tin[1,e],psym=8,/nohat,color=colcode[3],errcol=colcode[3]


poslx=!x.crange[0]+(!x.crange[1]-!x.crange[0])*0.9
posly=!y.crange[0]+(!y.crange[1]-!y.crange[0])*0.5
xyouts,poslx,posly,'c',size=1.5

  multiplot

IF keyword_set(yrplf) THEN yr=yrplf ELSE yr=[0.7*min([dbb,plf]),1.5*max([dbb,plf])]

plot,[100,100],[100,100],psym=3,ytitle='PLF, DBB ', $  ;x 10!E-10!N',$
xrange=xrr,/xstyle,/ystyle,/ylog,yr=yr, chars=cs

oplot,[0,0]+fac,10^(!y.crange)

IF bolcor THEN BEGIN
   nzdf=where(dbb NE 0.)
   FOR k=0, N_ELEMENTS(nzdf)-1 DO BEGIN
      corr=bolcor_diskbb(tin[0,nzdf[k]])
      dbb[nzdf[k]]=corr*dbb[nzdf[k]]
   ENDFOR
ENDIF


IF aa[0] ne -1 THEN BEGIN
 oplot,time[aa]+fac,plf[aa],psym=8,color=colcode[0]
 oplot,time[aa]+fac,dbb[aa],psym=5,color=colcode[0],syms=1.3,thick=1.5
ENDIF

IF bb[0] ne -1 THEN BEGIN
 oplot,time[bb]+fac,plf[bb],psym=8,color=colcode[1]
 oplot,time[bb]+fac,dbb[bb],psym=5,color=colcode[1],syms=1.3,thick=1.5
ENDIF

IF cc[0] ne -1 THEN BEGIN
 oplot,time[cc]+fac,plf[cc],psym=8,color=colcode[2]
 oplot,time[cc]+fac,dbb[cc],psym=5,color=colcode[2],syms=1.3,thick=1.5
ENDIF

IF dd[0] ne -1 THEN BEGIN
 oplot,time[dd]+fac,dbb[dd],psym=5,color=colcode[3],syms=1.3,thick=1.5
 oplot,time[dd]+fac,plf[dd],psym=8,color=colcode[3]
; yy=where(dbb[dd] LT 0.)
; IF yy[0] NE -1 THEN BEGIN
;  for k=0, n_elements(yy)-1 do arrow,time[dd[yy[k]]]+fac,-dbb[dd[yy[k]]],time[dd[yy[k]]]+fac,yr[0],color=colcode[3],/data
 ENDIF

IF ee[0] ne -1 THEN BEGIN
 oplot,time[ee]+fac,plf[ee],psym=8,color=colcode[2]
 oplot,time[ee]+fac,dbb[ee],psym=5,color=colcode[2],syms=1.3,thick=1.5
ENDIF

poslx=!x.crange[0]+(!x.crange[1]-!x.crange[0])*0.9
posly=!y.crange[1]*0.03
xyouts,poslx,posly,'d',size=1.5

;rin
multiplot

IF NOT NOPLOTRIN THEN BEGIN

drat=0.44  ;   0.32 1655
cosi=0.26  ;    0.5 1655

dv=where(normp[0,*] gt 0.)
rin=normp
;sqrt(norm/cosi)=(Rin/drat)
;rin=drat*sqrt(norm/cosi)=drat*sqrt(1/cosi)*sqrt(norm)
;rin=C n^0.5
;drin = 0.5 C n^-0.5 dn

;rin[0,*]=sqrt(normp[0,*]*((mrat^2.)^2.)/cosi)
;rin[1,dv]=sqrt(normp[1,dv]*((mrat^2.)^2.)/cosi)/sqrt(rin[0,dv])

rin[0,*]=drat*sqrt(1./cosi)*sqrt(normp[0,*])
rin[1,dv]=0.5 * drat*sqrt(1./cosi) * normp[1,dv] / sqrt(normp[0,dv])

IF KEYWORD_SET(yrrin) THEN yr=yrrin ELSE yr=[0.9*min(rin[0,(where(rin[0,*] NE 0.))]),1.1*max(rin[0,(where(rin[0,*] NE 0.))])]
;yr=yrrin

ploterror, time+fac,[100,100],[0,0],psym=3, ytitle='Rin*',$
xrange=xrr,/xstyle,/nohat,/ystyle, yr=yr, color=0, chars=cs ; dummy frame


oplot,[0,0]+fac,!y.crange


IF aa[0] ne -1 THEN oploterror,time[aa]+fac,rin[0,aa],rin[1,aa],psym=8,$
/nohat,color=colcode[0], errcol=colcode[0]

IF bb[0] NE -1 THEN oploterror,time[bb]+fac,rin[0,bb],rin[1,bb],psym=8,$
/nohat,color=colcode[1],errcol=colcode[1]

IF cc[0] NE -1 THEN oploterror,time[cc]+fac,rin[0,cc],rin[1,cc],psym=8,$
/nohat,color=colcode[2],errcol=colcode[2]

IF dd[0] NE -1 THEN oploterror,time[dd]+fac,rin[0,dd],rin[1,dd],psym=8,$
/nohat,color=colcode[3],errcol=colcode[3]

 IF ee[0] NE -1 THEN oploterror,time[ee]+fac,rin[0,ee],rin[1,dd],psym=8,$
/nohat,color=colcode[4],errcol=colcode[4]


xx=where(normp[1,*] eq -1)
IF xx[0] NE -1 THEN BEGIN
   FOR k=0,n_elements(xx)-1 DO BEGIN
      rin=sqrt(normp[0,xx[k]]*((0.8)^2.)/0.5)
      IF rin GT yr[1] THEN rin=yr[1]
      arrow, time[xx[k]]+fac, rin, time[xx[k]]+fac, yr[0], color=colcode[states[xx[k]]],/data
   ENDFOR
ENDIF

poslx=!x.crange[0]+(!x.crange[1]-!x.crange[0])*0.9
posly=!y.crange[0]+(!y.crange[1]-!y.crange[0])*0.5
xyouts,poslx,posly,'e',size=1.5

ENDIF

multiplot

plotsym,0,/fill
mwtime=mwdate-ttrans[0]
zz=where(mwdate eq 0.)
mfv=zz[0]-1


aam=where((mwtime gt min(time[aa])) AND (mwtime le max(time[aa])))
bbm=where((mwtime gt min(time[bb])) AND (mwtime le max(time[bb])))
ccm=where((mwtime gt min(time[cc])) AND (mwtime le max(time[cc])))
ddm=where((mwtime gt min(time[dd])) AND (mwtime le max(time[dd])))
eem=where((mwtime gt min(time[ee])) AND (mwtime le max(time[ee])))

ytit='NIR&Radio flux'
sz=size(mwmag)

   IF sz(1) eq 2 THEN BEGIN
    
      IF rad THEN BEGIN
         mwflux=mwmag[0,*]
         mwfluxe=mwmag[1,*]
      ENDIF ELSE BEGIN
         rz=where(mwmag[0,*] NE 0.)
         mwtime=mwtime[rz]
         mwmag=mwmag[*,rz]
         mwflux=mag2flux(mwmag[0,*])
         mwflux=mwflux/min(mwflux)
         mwfluxe=mwflux*mwmag[1,*]/mwmag[0,*]
      ENDELSE

      IF keyword_set(yrmw) THEN yr=yrmw ELSE yr=[0.9*min(mwflux),1.1*max(mwflux[0:mfv])]
                            
        ploterror, [200,200], [100,100],[0,0], psym=3, ytitle=ytit, $
                            yr=yr, xrange=xr,/xstyle,/ystyle,/nohat,chars=cs
                            
;xtickformat='(I4)',/nohat, chars=1.2

        IF aam[0] ne -1 THEN oploterror, mwtime[aam], mwflux[aam], mwfluxe[aam], psym=8, /nohat, color=colcode[0],errcol=colcode[0]
        IF bbm[0] ne -1 THEN oploterror, mwtime[bbm], mwflux[bbm], mwfluxe[bbm], psym=8, /nohat, color=colcode[1],errcol=colcode[1]
        IF ccm[0] ne -1 THEN oploterror, mwtime[ccm], mwflux[ccm], mwfluxe[ccm], psym=8, /nohat, color=colcode[2],errcol=colcode[2]
        IF ddm[0] ne -1 THEN oploterror, mwtime[ddm], mwflux[ddm], mwfluxe[ddm], psym=8, /nohat, color=colcode[3],errcol=colcode[3]
        IF eem[0] ne -1 THEN oploterror, mwtime[eem], mwflux[eem], mwfluxe[eem], psym=8, /nohat, color=colcode[4],errcol=colcode[4]


   ENDIF ELSE BEGIN

      IF rad THEN mwflux=mwmag ELSE BEGIN
         mwflux=mag2flux(mwmag)
         mwflux=mwflux/mwflux[0]
      ENDELSE

      IF keyword_set(yrmw) THEN yr=yrmw ELSE yr=[0.9*min(mwflux),1.1*max(mwflux)]
      

      plot, mwtime, mwflux, psym=8, ytitle=ytit,yr=yr, $
            xrange=xr,/xstyle,/ystyle,xtickformat='(I4)', chars=1.2

        IF aam[0] ne -1 THEN oplot, mwtime[aam], mwflux[aam], psym=8, color=colcode[0]
        IF bbm[0] ne -1 THEN oplot, mwtime[bbm], mwflux[bbm], psym=8, color=colcode[1]
        IF ccm[0] ne -1 THEN oplot, mwtime[ccm], mwflux[ccm], psym=8, color=colcode[2]
        IF ddm[0] ne -1 THEN oplot, mwtime[ddm], mwflux[ddm], psym=8, color=colcode[3]
        IF eem[0] ne -1 THEN oplot, mwtime[eem], mwflux[eem], psym=8, color=colcode[3]

        ENDELSE

IF keyword_set(inforad) THEN BEGIN

        fre1=where(inforad.freq eq 4.8)
        rtime=inforad.dates[fre1]-ttrans[0]
        yy=where(inforad.flux[1,fre1] GT 0.) ;actual data
        xx=where(inforad.flux[1,fre1] EQ -1) ;upper limits
        
 ;       oplot,[-15,-15],[5.3,5.3],psym=8,color=colcode[0]
 ;       xyouts,-14.5,4.8,'Rel. NIR flux'         

        IF NOT keyword_set(fr) THEN fr=0.8*yr[1]/max(inforad.flux[0,fre1[yy]])        
        print,fr

        plotsym, 5, /fill
         IF yy[0] NE -1 THEN $
            oploterror, rtime[yy], inforad.flux[0,fre1[yy]]*fr, inforad.flux[1,fre1[yy]]*fr,psym=8,/nohat,symsize=1.    

;        oplot,[-15,-15],[4.0,4.0],psym=8,color=0
;        xyouts,-14.5,3.8,'Rel. Radio flare flux'         
 
         plotsym,8, /fill
;            oploterror, rtime[yy[8:14]], inforad.flux[0,fre1[yy[8:14]]]*fr, inforad.flux[1,fre1[yy[8:14]]]*fr,psym=8,/nohat,symsize=1.
 ;        IF xx[0] ne -1 THEN FOR i=0,n_elements(xx)-1 DO BEGIN 
  ;          ARROW, rtime[xx[i]], inforad.flux[0,fre1[xx[i]]]*fr, rtime[xx[i]], yr[0], /data
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

IF ps THEN BEGIN
  device,/close
  IF !VERSION.OS eq 'Win32' THEN set_plot,'win' ELSE set_plot,'x'
  ENDIF
  
END
