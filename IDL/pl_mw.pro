pro pl_mw, inpstr, inx, colcode, time, band=mwband, mwyr=yrmw, rfreq=freqr, $
              realtime=realtime, facr=fr, othin=rthin, plotrad=plotrad, $
              mwxr=xrmw, mwopl=mwopl, radonly=radonly, sepraxis=sepraxis
  
;This program plots radio+infrared, remove clutter from other plotting programs

; INPUTS
;
; inpstr: input structure that holds all required parameters
; inx: index of input structure
; colcode: codes of colors of different states
; time: times of observation to correctly color states
;
; OPTIONAL INPUTS
;
;
; facr: renormalize radio flux to fit in plot (if automatic scaling
;       does not work best, this will be replaced with a y axis on the right)
; realtime: if set, use actual MJD rather than timing transition
; band: decide which band to use in multiwavelength plot, default "J"
; rfreq: decide which radio frequency to use in multiwavlength plot,
;        default 4.8
; plotrad: plot radio info with near infrared
; othin: time before optically thin flare, but after a compact jet 
;        (think about a smart way later)
; mwxr: xrange
; mwopl: if set, just overplot the data
; radonly: if set, only radio data is present, or only plot radio info
; sepraxis : if set, radio axis is on the right
;
; USES
;
; NONE
;
; USED BY
;
; plotallrise, and could be a basis for other sources mw plots
;
; Created by E Kalemci, May 2015
;
; Rewriting based on succesfull pl1655mw.pro, April 2016
;

IF NOT keyword_set(realtime) THEN realtime=0
IF NOT keyword_set(mwband) THEN mwband='J'
IF NOT keyword_set(freqr) THEN freqr=4.8
IF NOT keyword_set(plotrad) THEN plotrad=0
IF NOT keyword_set(rthin) THEN rthin=53445.
;IF NOT keyword_set(fr) THEN fr=30.
IF NOT keyword_set(mwopl) THEN mwopl=0
IF NOT keyword_set(sepraxis) THEN sepraxis=0

IF NOT keyword_set(radonly) THEN radonly=0

IF NOT radonly THEN BEGIN
   mwi=where(inpstr[inx].oirinfo.band EQ mwband)
   
   IF realtime THEN mwtime=inpstr[inx].oirinfo.dates[mwi]-50000. ELSE mwtime=inpstr[inx].oirinfo.dates[mwi]-inpstr[inx].ttrans[0]

   mwmag=inpstr[inx].oirinfo.mag
   mwflux=inpstr[inx].oirinfo.flux
   sz=size(mwmag)

   mwmag1=mwmag[*,mwi]
   mwflux1=mwflux[*,mwi]
   mwtime=mwtime[where(mwmag1[0,*] NE 0.)]
   mwmag=mwmag1[*,where(mwmag1[0,*] NE 0.)]
   mwflux=mwflux1[0,where(mwmag1[0,*] NE 0.)]
   mwfluxe=mwflux1[1,where(mwmag1[0,*] NE 0.)]


   xx=where(inpstr[inx].states NE -1)
   states1=inpstr[inx].states(xx)
   aam=getboundary(states1, time, mwtime, 0)
   bbm=getboundary(states1, time, mwtime, 1)
   ccm=getboundary(states1, time, mwtime, 2)
   ddm=getboundary(states1, time, mwtime, 3)
   eem=getboundary(states1, time, mwtime, 4)

   IF NOT mwopl THEN BEGIN 
      plotsym,0,/fill

      IF sepraxis THEN ytit='OIR Flux (mJy)' ELSE ytit='OIR&Radio Flux'

      IF keyword_set(yrmw) THEN yr=yrmw ELSE yr=[0.9*min(mwflux),1.1*max(mwflux)]
      IF keyword_set(xrmw) THEN xr=xrmw ELSE xr=[min(time)-1.,max(time)+1]                         
      cs=1.2
       IF sepraxis THEN ploterror, [200,200], [100,100],[0,0], psym=3, $
                                   ytitle=ytit, yr=yr, xrange=xr,$
                                   /xstyle,ystyle=9,/nohat,chars=cs $
               ELSE ploterror, [200,200], [100,100],[0,0], psym=3, $
                               ytitle=ytit, yr=yr, xrange=xr,$
                               /xstyle,/ystyle,/nohat,chars=cs
   ENDIF
                         
;xtickformat='(I4)',/nohat, chars=1.2

        IF aam[0] ne -1 THEN oploterror, mwtime[aam], mwflux[aam], mwfluxe[aam], psym=8, /nohat, color=colcode[0],errcol=colcode[0]
        IF bbm[0] ne -1 THEN oploterror, mwtime[bbm], mwflux[bbm], mwfluxe[bbm], psym=8, /nohat, color=colcode[1],errcol=colcode[1]
        IF ccm[0] ne -1 THEN oploterror, mwtime[ccm], mwflux[ccm], mwfluxe[ccm], psym=8, /nohat, color=colcode[2],errcol=colcode[2]
        IF ddm[0] ne -1 THEN oploterror, mwtime[ddm], mwflux[ddm], mwfluxe[ddm], psym=8, /nohat, color=colcode[3],errcol=colcode[3]
        IF eem[0] ne -1 THEN oploterror, mwtime[eem], mwflux[eem], mwfluxe[eem], psym=8, /nohat, color=colcode[4],errcol=colcode[4]

     ENDIF

;radio
IF plotrad THEN BEGIN

   inforad=inpstr[inx].radinfo
   fre1=where(inforad.freq eq freqr)
 
   IF realtime THEN rtime=inforad.dates[fre1]-50000. ELSE rtime=inforad.dates[fre1]-inpstr[inx].ttrans[0]

   yy=where(inforad.flux[1,fre1] GT 0.) ;actual data
   xx=where(inforad.flux[1,fre1] EQ -1) ;upper limits

   IF NOT keyword_set(fr) THEN fr=0.9*yr[1]/max(inforad.flux[0,fre1[yy]])        
   ;print,fr

   IF sepraxis THEN BEGIN
      ryr=yr/fr
      axis, yaxis=1, yrange=ryr, /ystyle, ytitle='Radio Flux (mJy)',$
            yticks=3,ytickv=[1,2,3,4]
     ENDIF

IF radonly THEN BEGIN

      ytit='Radio flux (mJy)'

      mwflux=inforad.flux[0,*]
      IF keyword_set(yrmw) THEN yr=yrmw ELSE yr=[0.9*min(mwflux),1.1*max(mwflux)]
      IF keyword_set(xrmw) THEN xr=xrmw ELSE xr=[min(rtime)-1.,max(rtime)+1]                         
      cs=1.2
      ploterror, [200,200], [100,100],[0,0], psym=3, ytitle=ytit, $
                            yr=yr, xrange=xr,/xstyle,/ystyle,/nohat,chars=cs
   ENDIF

    ss=1.2
        
   IF yy[0] NE -1 THEN BEGIN
         IF keyword_set(rthin) THEN BEGIN
               yyc=where(rtime[yy] LT rthin)
               plotsym, 5, /fill
               IF yyc[0] NE -1 THEN oploterror, rtime[yy[yyc]], inforad.flux[0,fre1[yy[yyc]]]*fr, inforad.flux[1,fre1[yy[yyc]]]*fr,psym=8,/nohat,symsize=ss 
               yyf=where(rtime[yy] GE rthin)
               plotsym, 5
               IF yyf[0] NE -1 THEN oploterror, rtime[yy[yyf]], inforad.flux[0,fre1[yy[yyf]]]*fr, inforad.flux[1,fre1[yy[yyf]]]*fr,psym=8,/nohat,symsize=ss 
            ENDIF ELSE BEGIN
               plotsym, 5, /fill
               oploterror, rtime[yy], inforad.flux[0,fre1[yy]]*fr, inforad.flux[1,fre1[yy]]*fr,psym=8,/nohat,symsize=ss
            ENDELSE
         ENDIF


   IF xx[0] ne -1 THEN FOR i=0,n_elements(xx)-1 DO BEGIN
      IF ((rtime[xx[i]] GE xr[0]) AND (rtime[xx[i]] LE xr[1])) THEN $
      ARROW, rtime[xx[i]], inforad.flux[0,fre1[xx[i]]]*fr, rtime[xx[i]], yr[0], /data
   ENDFOR

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
