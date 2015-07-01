pro plot_timtrans, inpstr, inx, ps=ps, fname=fname, yr=ry, xr=rx

; This program plots two psd right before and after the timing
; transition
;
; INPUTS
;
; inpstr: input structure
; inx: index of input structure that determines source and outburst
;
; OUTPUTS
; 
; NONE (a postscript plot if specified)
;
; OPTIONAL INPUTS
; 
; ps: postscipt plot
; fname: name of output file
; yr: yrange of plot
; USES
;
; NONE
;
; USED BY
;
; allstr programs
;
; created by EK, november 2014
;

IF NOT keyword_set(ps) THEN ps=0
IF NOT keyword_set(fname) THEN fname='ttranspsd.eps'
IF NOT keyword_set(ry) THEN ry=[4e-4,0.2]
IF NOT keyword_set(rx) THEN rx=[0.003,128.]

 IF ps then begin
      set_plot, 'ps'
;   device,/color
;   loadct,5
      device,/encapsulated
      device, filename = fname
                                ;device, yoffset = 2
                                ;device, ysize = 23.5
                                ;device, xsize = 12.0
      !p.font=0
      device,/times
ENDIF

;get the obsids before and after the transition

xx=where(inpstr[inx].xdates GT inpstr[inx].ttrans[0])
bt=xx[0]-1
at=xx[0]

multiplot, [1,2], mxtitle='Frequency (Hz)', mxtitsize=1.2, mytitle='fxPSD (rms/mean)!E2!N'

restore,'XRAY/'+inpstr[inx].obsid[bt]+'/an/result.sav'


PLOTERROR,ff,pf*ff,pfe*ff,/xlog,/ylog,psym=10,/nohat,xrange=rx,/xstyle,yrange=ry, charsize=1.,/ystyle,thick=4, chartick=2.5

nlor=n_elements(r)/3
pow=0
FOR i=0, 3*nlor-1, 3 DO BEGIN
        OPLOT,ff,f_lor(ff,r[i:i+2])*ff,thick=4,linestyle=2
        pow=pow+f_lor(ff,r[i:i+2])
     ENDFOR
OPLOT,ff,pow*ff,thick=5

multiplot

restore,'XRAY/'+inpstr[inx].obsid[at]+'/an/result.sav'

PLOTERROR,ff,pf*ff,pfe*ff,/xlog,/ylog,psym=10,/nohat,xrange=rx,/xstyle,yrange=ry, charsize=1.,/ystyle,thick=4, chartick=2.5
nlor=n_elements(r)/3
pow=0
FOR i=0, 3*nlor-1, 3 DO BEGIN
        OPLOT,ff,f_lor(ff,r[i:i+2])*ff,thick=4,linestyle=2
        pow=pow+f_lor(ff,r[i:i+2])
     ENDFOR
OPLOT,ff,pow*ff,thick=5

multiplot,/default


   IF ps THEN BEGIN
      device,/close
      set_plot, 'x'
   ENDIF


END

