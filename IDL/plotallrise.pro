pro plotallrise, dates, ttrans, itrans, mwtrans, mwpeak,  $
plf, dbb, ind, tin, rms, mwdate, mwmag, rad=rad, ps=ps,namef=fname, $
noplottin=noplottin, inforad=inforad, facr=fr, $
rmsyr=yrrms, indyr=yrind, tinyr=yrtin, plfyr=yrplf, mwyr=yrmw, xrpl=plxr,$
usecol=usecol, onlycert=onlycert, realtime=realtime

;This program is an attempt to automatically plot all parameters
;
;INPUTS
;
;dates: X-ray dates
;ttrans: timing transition in MJD
;itrans: index transition after ttrans
;mwtrans: mw transition after ttrans
;mwpeak: peak of mw emission after ttrans
;strans: date of softening transition
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
;noplottin: do not plot tin? default plot
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
IF NOT keyword_set(noplottin) THEN noplottin=0 ; do not plot tin

IF NOT keyword_set(ps) THEN ps=0
IF (ps AND NOT keyword_set(fname)) THEN fname='alldatarise.eps'
IF NOT keyword_set(usecol) THEN usecol=0
IF NOT keyword_setx(fr) THEN fr=1. ;?

IF NOT keyword_set(onlycert) THEN onlycert=0
IF NOT keyword_set(realtime) THEN realtime=0

device,decomposed=0
IF usecol THEN BEGIN 
   loadct,4
   colcode=[184,120,40,150];
ENDIF ELSE BEGIN
   loadct,0
   colcode=[195,155,95,15]
ENDELSE

if ps then begin
   set_plot, 'ps' 
   device,/color
;   loadct,5
   device,/encapsulated
   device, filename = fname
   device, yoffset = 2
   device, ysize = 23.5
   ;device, xsize = 12.0
   !p.font=0
   device,/times
endif

IF NOT ps THEN window, 4, retain=2, xsize=800, ysize=800
cs=1.3
time=dates
zz=where(dates ne 0.)
time[zz]=dates[zz]-ttrans[0]
IF realtime THEN fac=ttrans[0]-50000. else fac=0

aa=where(time le 0.)

IF onlycert THEN BEGIN
	bb=where((time gt 0.) AND (time le (itrans[0]+itrans[1])))
	cc=where((time gt (itrans[0]+itrans[1])) AND (time le (mwtrans[0]+mwtrans[1])))
  dd=where(time gt (mwtrans[0]+mwtrans[1]))
ENDIF ELSE BEGIN
  bb=where((time gt 0.) AND (time le itrans[0]))
  cc=where((time gt itrans[0]) AND (time le mwtrans[0]))
  dd=where(time gt mwtrans[0])
ENDELSE

;IF realtime THEN fac=ttrans[0]-50000. else fac=0
IF keyword_set(plxr) THEN xr=plxr ELSE xr=[min(time)-2.,max(time)+2.]

PLOTSYM,0,1,/FILL

IF realtime THEN BEGIN
    IF noplottin THEN multiplot, [1,4], mxtitle='Time (MJD-50000 days)',mxtitsize=1.2 ELSE $
  multiplot, [1,5], mxtitle='Time (MJD-50000 days)',mxtitsize=1.2
ENDIF ELSE BEGIN
  IF noplottin THEN multiplot, [1,4], mxtitle='Time (Days from timing transiton)',mxtitsize=1.2 ELSE $
  multiplot, [1,5], mxtitle='Time (Days from timing transition)',mxtitsize=1.2
ENDELSE

;RMS

;take out upperlimits

IF onlycert THEN BEGIN
	aaxx=where((rms[1,*] gt 0.) AND (time le 0.))
	bbxx=where((rms[1,*] gt 0.) AND (time gt 0.) AND (time le (itrans[0]+itrans[1])))
	ccxx=where((rms[1,*] gt 0.) AND (time gt (itrans[0]+itrans[1])) AND (time le (mwtrans[0]+mwtrans[1])))
	ddxx=where((rms[1,*] gt 0.) AND (time gt (mwtrans[0]+mwtrans[1])))

  aayy=where((rms[1,*] eq -1) AND (time le 0.))
  bbyy=where((rms[1,*] eq -1) AND (time gt 0.) AND (time le (itrans[0]+itrans[1])))
  ccyy=where((rms[1,*] eq -1) AND (time gt (itrans[0]+itrans[1])) AND (time le (mwtrans[0]+mwtrans[1])))
  ddyy=where((rms[1,*] eq -1) AND (time gt (mwtrans[0]+mwtrans[1])))
ENDIF ELSE BEGIN
  aaxx=where((rms[1,*] gt 0.) AND (time le 0.))
  bbxx=where((rms[1,*] gt 0.) AND (time gt 0.) AND (time le itrans[0]))
  ccxx=where((rms[1,*] gt 0.) AND (time gt itrans[0]) AND (time le mwtrans[0]))
  ddxx=where((rms[1,*] gt 0.) AND (time gt mwtrans[0]))

  aayy=where((rms[1,*] eq -1) AND (time le 0.))
  bbyy=where((rms[1,*] eq -1) AND (time gt 0.) AND (time le itrans[0]))
  ccyy=where((rms[1,*] eq -1) AND (time gt itrans[0]) AND (time le mwtrans[0]))
  ddyy=where((rms[1,*] eq -1) AND (time gt mwtrans[0]))
ENDELSE


;rmstp=rms[*,xx]

IF keyword_set(yrrms) THEN yr=yrrms ELSE yr=[0.9*min(rms[0,*]),1.1*max(rms[0,*])]

;IF realtime THEN ploterror, time+ttrans[0]-50000., [200,200],[0,0], psym=3, ytitle='rms amp. (%)',$
;xrange=xr,/xstyle,/nohat,/ystyle,yr=yr, chars=1.2 ELSE $

xrr=xr+fac
ploterror, time+fac, [200,200],[0,0], psym=3, ytitle='rms amp. (%)',$
xrange=xrr,/xstyle,/nohat,/ystyle,yr=yr, chars=cs

;for i=0,25 do oplot,[i,i],[5,5],color=(i*10), psym=8


;oplot,[itrans[0],itrans[0]]+fac,!y.crange,line=1
;oplot,[mwtrans[0],mwtrans[0]]+fac,!y.crange,line=2
oplot,[0,0]+fac,!y.crange
;IF plstrans THEN oplot,[strans[0],strans[0]]+fac,!y.crange,line=4

IF aaxx[0] ne -1 THEN oploterror,time[aaxx]+fac,rms(0,aaxx),rms(1,aaxx),psym=8,/nohat,color=colcode[0], errcol=colcode[0]
oploterror,time[bbxx]+fac,rms(0,bbxx),rms(1,bbxx),psym=8,/nohat,color=colcode[1],errcol=colcode[1]
oploterror,time[ccxx]+fac,rms(0,ccxx),rms(1,ccxx),psym=8,/nohat,color=colcode[2],errcol=colcode[2]
oploterror,time[ddxx]+fac,rms(0,ddxx),rms(1,ddxx),psym=8,/nohat,color=colcode[3],errcol=colcode[3]

IF aayy[0] ne -1 THEN FOR k=0,n_elements(aayy)-1 DO arrow, time[aayy[k]]+fac, rms[0,aayy[k]], time[aayy[k]]+fac, yr[0], /data, color=colcode[0]
IF bbyy[0] ne -1 THEN FOR k=0,n_elements(bbyy)-1 DO arrow, time[bbyy[k]]+fac, rms[0,bbyy[k]], time[bbyy[k]]+fac, yr[0], /data, color=colcode[1]
IF ccyy[0] ne -1 THEN FOR k=0,n_elements(ccyy)-1 DO arrow, time[ccyy[k]]+fac, rms[0,ccyy[k]], time[ccyy[k]]+fac, yr[0], /data, color=colcode[2]
IF ddyy[0] ne -1 THEN FOR k=0,n_elements(ddyy)-1 DO arrow, time[ddyy[k]]+fac, rms[0,ddyy[k]], time[ddyy[k]]+fac, yr[0], /data, color=colcode[3]


poslx=!x.crange[0]+(!x.crange[1]-!x.crange[0])*0.9
posly=!y.crange[0]+(!y.crange[1]-!y.crange[0])*0.8
xyouts,poslx,posly,'a',size=1.5

multiplot

IF keyword_set(yrind) THEN yr=yrind ELSE yr=[0.9*min(ind[0,*]),1.1*max(ind[0,*])]

ploterror, time+fac,[100,100],[0,0],psym=3, ytitle='!9G !X',$
xrange=xrr,/xstyle,/nohat,/ystyle, yr=yr, color=0, chars=cs ; dummy frame

;oplot,[itrans[0],itrans[0]]+fac,!y.crange,line=1
;oplot,[mwtrans[0],mwtrans[0]]+fac,!y.crange,line=2
oplot,[0,0]+fac,!y.crange
;IF plstrans THEN oplot,[strans[0],strans[0]]+fac,!y.crange,line=4

IF aa[0] ne -1 THEN oploterror,time[aa]+fac,ind[0,aa],ind[1,aa],psym=8,$
/nohat,color=colcode[0], errcol=colcode[0]

IF bb[0] NE -1 THEN oploterror,time[bb]+fac,ind[0,bb],ind[1,bb],psym=8,$
/nohat,color=colcode[1],errcol=colcode[1]

IF cc[0] NE -1 THEN oploterror,time[cc]+fac,ind[0,cc],ind[1,cc],psym=8,$
/nohat,color=colcode[2],errcol=colcode[2]

IF dd[0] NE -1 THEN oploterror,time[dd]+fac,ind[0,dd],ind[1,dd],psym=8,$
/nohat,color=colcode[3],errcol=colcode[3]


poslx=!x.crange[0]+(!x.crange[1]-!x.crange[0])*0.9
posly=!y.crange[0]+(!y.crange[1]-!y.crange[0])*0.8
xyouts,poslx,posly,'b',size=1.5

multiplot

IF NOT noplottin THEN BEGIN
  IF keyword_set(yrtin) THEN yr=yrtin ELSE yr=[0.9*min(tin[0,*]),1.1*max(tin[0,*])]

  ploterror,time+fac,[011,100],[0,0],psym=3,ytitle='Tin',$
  xrange=xrr,/xstyle,/nohat,/ystyle,yr=yr, chars=cs

;  oplot,[itrans[0],itrans[0]]+fac,!y.crange,line=1
;  oplot,[mwtrans[0],mwtrans[0]]+fac,!y.crange,line=2
  oplot,[0,0]+fac,!y.crange
;  IF plstrans THEN oplot,[strans[0],strans[0]]+fac,!y.crange,line=4

  oploterror,time[aa]+fac,tin[0,aa],tin[1,aa],psym=8,/nohat,color=colcode[0],errcol=colcode[0]
  oploterror,time[bb]+fac,tin[0,bb],tin[1,bb],psym=8,/nohat,color=colcode[1],errcol=colcode[1]
  oploterror,time[cc]+fac,tin[0,cc],tin[1,cc],psym=8,/nohat,color=colcode[2],errcol=colcode[2]
  oploterror,time[dd]+fac,tin[0,dd],tin[1,dd],psym=8,/nohat,color=colcode[3],errcol=colcode[3]

  multiplot
ENDIF

IF keyword_set(yrplf) THEN yr=yrplf ELSE yr=[0.7*min([dbb,plf]),1.5*max([dbb,plf])]

plot,[100,100],[100,100],psym=3,ytitle='PLF, DBB ', $  ;x 10!E-10!N',$
xrange=xrr,/xstyle,/ystyle,/ylog,yr=yr, chars=cs

;oplot,[itrans[0],itrans[0]]+fac,10^(!y.crange),line=1
;oplot,[mwtrans[0],mwtrans[0]]+fac,10^(!y.crange),line=2
oplot,[0,0]+fac,10^(!y.crange)
;IF plstrans THEN oplot,[strans[0],strans[0]]+fac,10^(!y.crange),line=4

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
 yy=where(dbb[dd] LT 0.)
 IF yy[0] NE -1 THEN BEGIN
  for k=0, n_elements(yy)-1 do arrow,time[dd[yy[k]]]+fac,-dbb[dd[yy[k]]],time[dd[yy[k]]]+fac,yr[0],color=colcode[3],/data
 ENDIF
ENDIF


poslx=!x.crange[0]+(!x.crange[1]-!x.crange[0])*0.9
posly=!y.crange[1]*2.5
xyouts,poslx,posly,'c',size=1.5

multiplot

mwtime=mwdate-ttrans[0]
zz=where(mwdate eq 0.)
mfv=zz[0]-1
aam=where(mwtime le 0.)


IF onlycert THEN BEGIN
	bbm=where((mwtime gt 0.) AND (mwtime le (itrans[0]+itrans[1])))
	ccm=where((mwtime gt (itrans[0]+itrans[1])) AND (mwtime le (mwtrans[0]+mwtrans[1])))
	ddm=where(mwtime gt (mwtrans[0]+mwtrans[1]))
ENDIF ELSE BEGIN
  bbm=where((mwtime gt 0.) AND (mwtime le itrans[0]))
  ccm=where((mwtime gt itrans[0]) AND (mwtime le mwtrans[0]))
  ddm=where(mwtime gt mwtrans[0])
ENDELSE

   IF rad THEN ytit='Radio (mJy)' ELSE ytit='Rel. NIR flux'
   sz=size(mwmag)

   IF sz(1) eq 2 THEN BEGIN
    
      IF rad THEN BEGIN
         mwflux=mwmag[0,*]
         mwfluxe=mwmag[1,*]
      ENDIF ELSE BEGIN
         mwflux=mag2flux(mwmag[0,*])
         mwflux=mwflux/mwflux[0]
         mwfluxe=mwflux*mwmag[1,*]/mwmag[0,*]
      ENDELSE

      IF keyword_set(yrmw) THEN yr=yrmw ELSE yr=[0.9*min(mwflux),1.1*max(mwflux[0:mfv])]

       
      IF rad THEN BEGIN
         lim=where(mwtime eq -1)
         ;4.86 Ghz
         yy=where((mwfluxe[0:lim-1]) GT 0.) ;actual data
         xx=where((mwfluxe[0:lim-1]) EQ -1) ;upper limits
;         print,lim,yy,xx,mwtime[yy]
         IF yy[0] NE -1 THEN $
            ploterror, mwtime[yy]+fac, mwflux[yy], mwfluxe[yy],psym=8, $ 
                    ytitle=ytit, yr=yr, chars=1.2, $
                 xrange=xrr,/xstyle,/ystyle,xtickformat='(I4)',/nohat
         
         IF xx[0] ne -1 THEN FOR i=0,n_elements(xx)-1 DO $
            ARROW, mwtime[xx[i]]+fac, mwflux[xx[i]], mwtime[xx[i]]+fac, yr[0], /data

         ;8.64 GHz
         lim2ind=where(mwtime eq 0.)
         lim2=lim2ind[0]
         yy=where((mwfluxe[lim[0]+1:lim2]) GT 0.) ;actual data
         xx=where((mwfluxe[lim[0]+1:lim2]) EQ -1) ;upper limits
         yy=yy+lim[0]+1
         IF xx[0] ne -1 THEN xx=xx+lim[0]+1
;         print,lim2, yy,mwtime[yy],mwflux[yy]
         IF yy[0] NE -1 THEN $
            oploterror, mwtime[yy]+fac, mwflux[yy], mwfluxe[yy],psym=5, $ 
                    ytitle=ytit, yr=yr, chars=1.2, $
                 xrange=xrr,/xstyle,/ystyle,xtickformat='(I4)',/nohat
         
         IF xx[0] ne -1 THEN FOR i=0,n_elements(xx)-1 DO $
            ARROW, mwtime[xx[i]]+fac, mwflux[xx[i]], mwtime[xx[i]]+fac, yr[0], /data
      ENDIF ELSE BEGIN
      
        IF realtime THEN BEGIN
        xrr=xr+ttrans[0]-50000.
        ploterror, [200,200], [100,100],[0,0], psym=3, ytitle=ytit, $
                            yr=yr, xrange=xrr,/xstyle,/ystyle,$
                            xtickformat='(I4)',/nohat, chars=1.2

        IF aam[0] ne -1 THEN oploterror, mwtime[aam]+ttrans[0]-50000., mwflux[aam], mwfluxe[aam], psym=8, /nohat, color=colcode[0],errcol=colcode[0]
        IF bbm[0] ne -1 THEN oploterror, mwtime[bbm]+ttrans[0]-50000., mwflux[bbm], mwfluxe[bbm], psym=8, /nohat, color=colcode[1],errcol=colcode[1]
        IF ccm[0] ne -1 THEN oploterror, mwtime[ccm]+ttrans[0]-50000., mwflux[ccm], mwfluxe[ccm], psym=8, /nohat, color=colcode[2],errcol=colcode[2]
        IF ddm[0] ne -1 THEN oploterror, mwtime[ddm]+ttrans[0]-50000., mwflux[ddm], mwfluxe[ddm], psym=8, /nohat, color=colcode[3],errcol=colcode[3]
        ENDIF ELSE BEGIN        
                            
        ploterror, [200,200], [100,100],[0,0], psym=3, ytitle=ytit, $
                            yr=yr, xrange=xr,/xstyle,/ystyle,$
                            xtickformat='(I4)',/nohat, chars=1.2

        IF aam[0] ne -1 THEN oploterror, mwtime[aam], mwflux[aam], mwfluxe[aam], psym=8, /nohat, color=colcode[0],errcol=colcode[0]
        IF bbm[0] ne -1 THEN oploterror, mwtime[bbm], mwflux[bbm], mwfluxe[bbm], psym=8, /nohat, color=colcode[1],errcol=colcode[1]
        IF ccm[0] ne -1 THEN oploterror, mwtime[ccm], mwflux[ccm], mwfluxe[ccm], psym=8, /nohat, color=colcode[2],errcol=colcode[2]
        IF ddm[0] ne -1 THEN oploterror, mwtime[ddm], mwflux[ddm], mwfluxe[ddm], psym=8, /nohat, color=colcode[3],errcol=colcode[3]
        ENDELSE
        
        ENDELSE


   ENDIF ELSE BEGIN

      IF rad THEN mwflux=mwmag ELSE BEGIN
         mwflux=mag2flux(mwmag)
         mwflux=mwflux/mwflux[0]
      ENDELSE

      IF keyword_set(yrmw) THEN yr=yrmw ELSE yr=[0.9*min(mwflux),1.1*max(mwflux)]
      
      IF realtime THEN BEGIN
      xrr=xr+ttrans[0]-50000.
      plot, mwtime+ttime[0]-50000., mwflux, psym=8, ytitle=ytit,yr=yr, $
            xrange=xrr,/xstyle,/ystyle,xtickformat='(I4)', chars=1.2
      
        IF aam[0] ne -1 THEN oplot, mwtime[aam]+ttrans[0]-50000., mwflux[aam], psym=8, color=colcode[0]
        IF bbm[0] ne -1 THEN oplot, mwtime[bbm]+ttrans[0]-50000., mwflux[bbm], psym=8, color=colcode[1]
        IF ccm[0] ne -1 THEN oplot, mwtime[ccm]+ttrans[0]-50000., mwflux[ccm], psym=8, color=colcode[2]
        IF ddm[0] ne -1 THEN oplot, mwtime[ddm]+ttrans[0]-50000., mwflux[ddm], psym=8, color=colcode[3]
      
      ENDIF ELSE BEGIN
      plot, mwtime, mwflux, psym=8, ytitle=ytit,yr=yr, $
            xrange=xr,/xstyle,/ystyle,xtickformat='(I4)', chars=1.2

        IF aam[0] ne -1 THEN oplot, mwtime[aam], mwflux[aam], psym=8, color=colcode[0]
        IF bbm[0] ne -1 THEN oplot, mwtime[bbm], mwflux[bbm], psym=8, color=colcode[1]
        IF ccm[0] ne -1 THEN oplot, mwtime[ccm], mwflux[ccm], psym=8, color=colcode[2]
        IF ddm[0] ne -1 THEN oplot, mwtime[ddm], mwflux[ddm], psym=8, color=colcode[3]
        ENDELSE
        ENDELSE
;   ENDELSE  

IF keyword_set(inforad) THEN BEGIN
        fr=1.
        fre1=where(inforad.freq eq '4.86')
        rtime=inforad.dates[fre1]-ttrans[0]
        yy=where(inforad.flux[1,fre1] GT 0.) ;actual data
        xx=where(inforad.flux[1,fre1] EQ -1) ;upper limits
         
         IF yy[0] NE -1 THEN $
            oploterror, rtime[yy], inforad.flux[0,fre1[yy]]*fr, inforad.flux[1,fre1[yy]]*fr,psym=5,/nohat,symsize=1.3
        
         IF xx[0] ne -1 THEN FOR i=0,n_elements(xx)-1 DO BEGIN 
            ARROW, rtime[xx[i]], inforad.flux[0,fre1[xx[i]]]*fr, rtime[xx[i]], yr[0], /data
            oplot,[rtime[xx[i]]-1,rtime[xx[i]]+1],[1.,1.]*inforad.flux[0,fre1[xx[i]]]*fr
            ENDFOR
        ENDIF



;oplot,[itrans[0],itrans[0]]+fac,!y.crange,line=1
;oplot,[mwtrans[0],mwtrans[0]]+fac,!y.crange,line=2
oplot,[0,0]+fac,!y.crange
;IF plstrans THEN oplot,[strans[0],strans[0]]+fac,!y.crange,line=4


poslx=!x.crange[0]+(!x.crange[1]-!x.crange[0])*0.9
posly=!y.crange[0]+(!y.crange[1]-!y.crange[0])*0.8
xyouts,poslx,posly,'d',size=1.5


multiplot,/default

IF ps THEN BEGIN
  device,/close
  IF !VERSION.OS eq 'Win32' THEN set_plot,'win' ELSE set_plot,'x'
  ENDIF
  
END
