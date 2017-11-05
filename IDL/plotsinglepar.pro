pro plotsinglepar, time, par, ytit, states, nxr, nyr, colcode, usecol=usecol, oplot=oplot, ylog=ylog, nul=uln, nll=lln, npsm=psmn

;This program plots a single parameter as a function of time,
;including upper and lower limits
;
; INPUTS
;
; time: time
; par: parameter (has to include error, if no error, set errors to 0)
; ytit: title of y axis
; states: spectral states for coloring
; nyr: IF set, new yrange in plot
; nxr: IF set, new xrange in plot
; colcode: to use different coloring scheme
;
; OPTIONAL INPUTS
;
; usecol: use color
; oplot: over plot data
; ylog: y axis logarithmic
; nul: new uper limit
; nll: new lower limit
; npsm: new plot symbol
;
; USES
;
; NONE
;
; USED BY
; 
; plotall_hecut
;
; Created by EK, May 2015
;
; LOGS
;
; added header
; 
;

IF not keyword_set(oplot) THEN oplot=0
IF not keyword_set(usecol) THEN usecol=0
IF not keyword_set(ylog) THEN ylog=0
IF not keyword_set(uln) THEN uln=nyr[1]
IF not keyword_set(lln) THEN lln=nyr[0]
IF not keyword_set(psmn) THEN psmn=0

cs=1.2

;IF oplot THEN plotsym,5,/fill ELSE BEGIN

;   plotsym,0,/fill

IF NOT oplot THEN ploterror,time,[100,100],[0,0],psym=3,ytitle=ytit,$
  xrange=nxr,/xstyle,/nohat,/ystyle,yr=nyr, chars=cs, ylog=ylog

;ENDELSE


;  oplot,[0,0]+fac,!y.crange

plotsym, psmn, /fill

FOR si=0,4 DO BEGIN

   aa=where(states eq si)

   IF aa[0] ne -1 THEN BEGIN
      aaok=where((par[1,aa] GE 0.) AND (par[0,aa] NE 0.))
      IF aaok[0] NE -1 THEN oploterror,time[aa[aaok]],par[0,aa[aaok]],par[1,aa[aaok]],psym=8,/nohat,color=colcode[si],errcol=colcode[si],syms=1.3,thick=1.5

      aaup=where(par[1,aa] EQ -1)
      IF aaup[0] NE -1 THEN BEGIN
         FOR ix=0,n_elements(aaup)-1 DO BEGIN
            IF ((par[0,aa[aaup[ix]]] GT nyr[0]) AND $
                (par[0,aa[aaup[ix]]] LT nyr[1]) AND $
                (time[aa[aaup[ix]]] gt nxr[0]) AND $
                (time[aa[aaup[ix]]] lt nxr[1])) THEN $
               arrow,time[aa[aaup[ix]]],par[0,aa[aaup[ix]]], $
                     time[aa[aaup[ix]]], lln, /data, color=colcode[si]
         ENDFOR
      oplot,time[aa[aaup]],par[0,aa[aaup]],psym=8,color=colcode[si],$
            syms=1.3,thick=1.5
   ENDIF

      aalow=where(par[1,aa] EQ -2)
      IF aalow[0] NE -1 THEN BEGIN
         oplot,time[aa[aalow]],par[0,aa[aalow]],psym=8,color=colcode[si],syms=1.3,thick=1.5
         FOR ix=0,n_elements(aalow)-1 DO BEGIN
            IF ((par[0,aa[aalow[ix]]] LT nyr[1]) AND $
               (time[aa[aalow[ix]]] GT nxr[0]) AND $
               (time[aa[aalow[ix]]] LT nxr[1])) THEN $
               arrow,time[aa[aalow[ix]]],par[0,aa[aalow[ix]]], $
                     time[aa[aalow[ix]]], uln, /data, color=colcode[si]
         ENDFOR
      ENDIF
   ENDIF
ENDFOR


END
