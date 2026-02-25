pro plotallrise_mod7, inpstr, inx, radonly=radonly, $
ps=ps,namef=fname, noplotrin=noplotrin, facr=fr,$
rmsyr=yrrms, indyr=yrind, tinyr=yrtin, rinyr=yrrin,$
plfyr=yrplf, mwyr=yrmw, xrpl=plxr, $
usecol=usecol, realtime=realtime, $
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
; realtime: if set, use actual MJD rather than timing transition
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
;
;  multiplot
;  calcfluxv2
;  plotsinglepar
;
;USED BY
;
;allinfo programs
;
;Created by Emrah Kalemci, July 2015
;
;MODIFIED FROM plotalldecay_mod6.pro, 
;revert back to decay coloring scheme, and using plotsinglepar and plotoir
;
; LOGS
; 
;

IF NOT keyword_set(radonly) THEN radonly=0  ;only radio data present?
IF NOT keyword_set(noplotrin) THEN noplotrin=0 ; do not plot rin

IF NOT keyword_set(ps) THEN ps=0
IF (ps AND NOT keyword_set(fname)) THEN fname='alldatarise.eps'
IF NOT keyword_set(usecol) THEN usecol=0
IF NOT keyword_set(realtime) THEN realtime=0
IF NOT keyword_set(mwband) THEN mwband='J'
IF NOT keyword_set(freqr) THEN freqr=4.8
IF NOT keyword_set(plotrad) THEN plotrad=0
IF NOT keyword_set(noclose) THEN noclose=0
IF NOT keyword_set(pcaonly) THEN pcaonly=0

device,decomposed=0
IF usecol THEN BEGIN 
   loadct,4
;   colcode=[184,120,40,150,130] ; color cordes for HS, HIMS, SIMS, US, SS
    colcode=[150,40,120,184,184]
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

IF keyword_set(plxr) THEN nxr=plxr ELSE nxr=[min(time)-2.,max(time)+2.]

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

IF keyword_set(yrrms) THEN nyr=yrrms ELSE $
   nyr=[0.9*min(rms[0,where(rms[1,*] NE -1)]),$
       1.1*max(rms[0,where(rms[1,*]NE -1)])]

plotsinglepar, time, rms, 'rms (%)', states, nxr, nyr, colcode, usecol=usecol

poslx=!x.crange[0]+(!x.crange[1]-!x.crange[0])*0.9
posly=!y.crange[0]+(!y.crange[1]-!y.crange[0])*0.8
xyouts,poslx,posly,'a',size=1.5

multiplot

; SPECTRAL INDEX

IF pcaonly THEN ind=inpstr[inx].indp ELSE ind=inpstr[inx].ind

IF keyword_set(yrind) THEN nyr=yrind ELSE $
   nyr=[0.9*min(ind[0,where(ind[1,*] NE -1)]),$
       1.1*max(ind[0,where(ind[1,*]NE -1)])]

plotsinglepar, time, ind, '!9G !X', states, nxr, nyr, colcode, usecol=usecol

poslx=!x.crange[0]+(!x.crange[1]-!x.crange[0])*0.9
posly=!y.crange[0]+(!y.crange[1]-!y.crange[0])*0.8
xyouts,poslx,posly,'b',size=1.5

multiplot

;INNER DISK TEMPERATURE

IF pcaonly THEN tin=inpstr[inx].tinp ELSE tin=inpstr[inx].tin

IF keyword_set(yrtin) THEN nyr=yrtin ELSE $
   nyr=[0.9*min(tin[0,where(tin[1,*] NE -1)]),$
       1.1*max(tin[0,where(tin[1,*]NE -1)])]

plotsinglepar, time, tin, 'Tin (keV)', states, nxr, nyr, colcode, $
               usecol=usecol, npsm=4

poslx=!x.crange[0]+(!x.crange[1]-!x.crange[0])*0.9
posly=!y.crange[0]+(!y.crange[1]-!y.crange[0])*0.5
xyouts,poslx,posly,'c',size=1.5

  multiplot

; PLF and DBB, here we need to do bolometric corrections and plot ELFs

calcfluxv2, inpstr, inx, outdelf, outpelf, outtelf, /dcor, /pcor, $
            pcaonly=pcaonly
IF keyword_set(yrplf) THEN nyr=yrplf ELSE BEGIN
      xx=where(outdelf[1,*] NE -1)
      miny=min(outdelf[0,xx]-outdelf[1,xx]) < min(outpelf[0,*]-outpelf[1,*])
      miny=miny*0.9
      maxy=max(outdelf[0,xx]+outdelf[1,xx]) > max(outpelf[0,*]+outpelf[1,*])
      maxy=maxy*1.1
      nyr=[miny,maxy]
ENDELSE

plotsinglepar, time, outpelf, 'PLF, DBB', states, nxr, nyr, colcode, usecol=usecol,/ylog

plotsinglepar, time, outdelf, 'PLF, DBB', states, nxr, nyr, colcode, usecol=usecol, /oplot, npsm=4

poslx=!x.crange[0]+(!x.crange[1]-!x.crange[0])*0.9
posly=10.^(!y.crange[1])*0.05
xyouts,poslx,posly,'d',size=1.5

multiplot

;RIN

IF NOT NOPLOTRIN THEN BEGIN

   drat=inpstr[inx].distance[0]/10. 
   cosi=cos(inpstr[inx].inc[0]*!PI/180.)
   normp=inpstr[inx].normp

   dv=where(normp[0,*] gt 0.)
   rin=normp
   rin[0,*]=drat*sqrt(1./cosi)*sqrt(normp[0,*])
   rin[1,dv]=0.5 * drat*sqrt(1./cosi) * normp[1,dv] / sqrt(normp[0,dv])

   IF KEYWORD_SET(yrrin) THEN nyr=yrrin ELSE nyr=[0.9*min(rin[0,(where(rin[0,*] NE 0.))]),1.1*max(rin[0,(where(rin[0,*] NE 0.))])]
;yr=yrrin

   plotsinglepar, time, rin, 'R!Din!N (km)', states, nxr, nyr, colcode, usecol=usecol, npsm=4

   poslx=!x.crange[0]+(!x.crange[1]-!x.crange[0])*0.9
   posly=!y.crange[0]+(!y.crange[1]-!y.crange[0])*0.5
   xyouts,poslx,posly,'e',size=1.5

ENDIF


multiplot

; MULTIWAVELENGTH DATA


;oplot,[0,0]+fac,!y.crange

pl_mw, inpstr, inx, colcode, time, band=mwband, rfreq=freqr, radonly=radonly, $
          othin=rthin, plotrad=plotrad, mwxr=nxr, facr=fr, mwyr=yrmw

poslx=!x.crange[0]+(!x.crange[1]-!x.crange[0])*0.9
posly=!y.crange[0]+(!y.crange[1]-!y.crange[0])*0.5
xyouts,poslx,posly,'f',size=1.5

multiplot,/default

IF NOT noclose THEN BEGIN
 IF ps THEN BEGIN
   device,/close
  IF !VERSION.OS eq 'Win32' THEN set_plot,'win' ELSE set_plot,'x'
  ENDIF
ENDIF

END
