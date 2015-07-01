pro plotpsd_single, ff, pf, pfe, r, ps=ps, fname=fname, yr=ry

; This program plots a single power spectrum with lorentzian fits
;
; INPUTS
;
; ff: frequency
; pf: power
; pfe: eror in power
;
; OUTPUTS
;
; NONE (postscript file if ps chosen)
;
; OPTIONAL INPUTS
;
; ps: postscript output
; fname: name of postscript output
; yr: yrange of plots
;
; USED BY
; 
; NONE
;
; USES
;
; NONE
;
; created by E. Kalemci, november 2014

IF NOT keyword_set(ps) THEN ps=0
IF NOT keyword_set(fname) THEN fname='psd.eps'
IF NOT keyword_set(ry) THEN ry=[4e-4,0.2]

IF ps then begin
   set_plot, 'ps'
   device,/encapsulated
   device, filename = fname
   !p.font=0
   device,/times
endif

PLOTERROR,ff,pf*ff,pfe*ff,/xlog,/ylog,psym=10,/nohat,xrange=[min(ff),max(ff)],/xstyle,position=[0.2,0.2,0.98,0.98],xtitle='Frequency (Hz)',yrange=ry, ytitle = 'fxPSD (rms/mean)!E2!N', charsize=2.,/ystyle,xtickname='exponent',thick=4, chartick=2.5,ytickname='exponent'

nlor=n_elements(r)/3
pow=0
FOR i=0, 3*nlor-1, 3 DO BEGIN
        OPLOT,ff,f_lor(ff,r[i:i+2])*ff,thick=4,linestyle=2
        pow=pow+f_lor(ff,r[i:i+2])
     ENDFOR
OPLOT,ff,pow*ff,thick=5

IF ps THEN BEGIN
   device,/close
   set_plot, 'x'
ENDIF

END
