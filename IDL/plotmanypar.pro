pro plotmanypar, inpstr, inx, plstr, $
                 ps=ps,namef=fname, strmw=mwstr, state2=state2

;This program is an attempt to automatically plot all parameters,
;provided in the structure named plstr. This program needs a wrapper
;to create plstr. The form of plstr is as follows
;
; plstr.nop      : (int) number of plots, excluding multiwavelength
; plstr.pars     : (strarr(10)) maximum 10 parameters to plot
; plstr.yr       : (fltarr(2,10)) yrange of plots, if left 0,0 then 
;                  automatic scaling
; plstr.xr       : (fltarr(2))  xrange of plots, if left 0,0 then automatic
;                  scaling
; plstr.realtime : (bool) if set use real times in mjd, otherwise use
;                  relative times to transition
; plstr.ttrans   : (float) normally inpstr.ttrans if inpstr is not fed
;                  externally
; plstr.pcaonly  : (bool) only pca data is present
; plstr.usecol   : (bool) use color? default 1
; plstr.ytitle   : (strarr(10)) ytitle
; plstr.ylog     : (boolarr (10))  is the y axis logarithmic
; plstr.oplot    : (boolarr (10)) if set, NEXT parameter is overplotted.
; plstr.lims     : (fltarr(2,10)) upper and lower limit ranges 
;
; multiwavelength plot parameters
; plstr.incmw    : (bool) if set then include mw plot
; plstr.oirinfo  : the oir structure if fed extrnally
; plstr.radinfo  : the radio structure if fed externally
; plstr.mwpt     : (0-3) 0: same scale, rescale radio by fr
;                        1: same scale, use mJy for all
;                        2: left / right has different scales 
; plstr.radonly  : (bool) if set, only plot radio
; plstr.oironly  : (bool) if set, only plot oir
; plstr.rfreq    : (float) default 4.8, frequency to be used in radio
;                 plots
; plstr.band     : (string) default 'J', band to be used in oir plots
; plstr.mwyr     : fltarr(2,2) 0,* for oir and 1,* for radio
; plstr.facr     : (float) if automatinc scale does not work in mwpt=0
;                 use this factor
; plstr.othin    : (float) time before optically thin flare
; plstr.plotrad  : (bool) if set plot radio along with oir
;
; flux plot parameters
;
; plstr.edd      : (bool) if set, plot in eddington luminosity fraction
;
; general plot parameters
;
; plstr.addlegend : if set xyouts a, b, c etc
;
;
;INPUTS
;
; inpstr: input structure that holds all required parameters
; inx: index of the structure that will point out the correct source
;      and year
; plstr: structure that holds all information about what and how to
;plot.
;
;OPTIONAL INPUT
;
; ps: postscript output
; namef: ps output filename
; mwstr: if set, use this for the multiwavelength plot
; state2: if set, use states2 in the structure
;
;USES
;
;  multiplot
;  calcfluxv3
;  plotsinglepar
;
;USED BY
;
; NONE
;
;Created by Emrah Kalemci, Jan 2016
;
; MODIFIED FROM plotalldecay_mod7.pro, 
;
; LOGS
; 
; Now it takes into account overplotted parameters when deciding on
;number of plot panels
; 
; added capcaity to plot eddington luinosity fractions by specifying
;/edd
;
; Eddington luminosities now calculated by calcfluxv3
;
; Need to omit disk elf with very large errors...
  
IF NOT keyword_set(ps) THEN ps=0
IF NOT keyword_set(state2) THEN state2=0
  
IF (ps AND NOT keyword_set(fname)) THEN fname='plmanypars.eps'

device,decomposed=0
IF plstr.usecol THEN BEGIN 
   loadct,4
    colcode=[184,120,40,150,130] ; color codes for HS, HIMS, SIMS, US, SS
;    colcode=[150,40,120,184,184]
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

instr=inpstr[inx]

cs=1.3
dates=inpstr[inx].xdates
ttrans=plstr.ttrans
zz=where(dates ne 0.)

realtime=plstr.realtime
IF realtime THEN time=dates[zz]-50000. ELSE time=dates[zz]-ttrans[0]

IF state2 THEN states=instr.states2[where(instr.states2 NE -1)] ELSE $
   states=instr.states[where(instr.states NE -1)]

IF plstr.xr[1] NE 0 THEN nxr=plstr.xr ELSE nxr=[min(time)-2.,max(time)+2.]
PLOTSYM,0,1,/FILL

nop=plstr.nop+plstr.incmw-total(plstr.oplot)

IF realtime THEN BEGIN
   multiplot, [1,nop], mxtitle='Time (MJD-50000 days)',mxtitsize=1.2
   fac=ttrans[0] ;?
ENDIF ELSE BEGIN
   multiplot, [1,nop], mxtitle='Time (Days from timing transition)', $
              mxtitsize=1.2
   fac=0.
ENDELSE

;legends
legds=['a','b','c','d','e','f','g','h','i']

IF plstr.edd THEN calcfluxv3,instr,0,outdelf, outpelf, outtelf,$
                    /pcor,/dcor,pcaonly=plstr.pcaonly

FOR i=0, plstr.nop-1 DO BEGIN
   
   ;get tag index
   sta=tag_exist(instr, plstr.pars[i], index=in)
   IF sta eq 0 THEN PRINT, 'no '+plstr.pars[i]+' exist as a tagname in the '+$
                            'input structure, a syntax error?' $
   ELSE BEGIN
      par=instr.(in)
   ;determine if this has errors
      sz=size(par)
      IF sz(0) eq 2 THEN noerror=0 ELSE noerror=1 

    ;plotsinglepar always asks for error so set errors to 0
      IF noerror THEN BEGIN
         newpar=fltarr(2,n_elements(par))
         newpar[0,*]=par
         par=newpar
      ENDIF

    ;IF edd is set use calcfluxv3
        IF plstr.edd THEN BEGIN 
         IF ((plstr.pars[i] EQ 'dbb') OR (plstr.pars[i] EQ 'plf') $
             (plstr.pars[i] EQ 'totf')) THEN BEGIN

         noerror=0   
         par=fltarr(2,100)
         omitdisk=where(((outdelf[0,*]+outdelf[3,*]) GE outpelf[0,*]) AND (outdelf[1,*]/outdelf[0,*] GT 0.5))
         ;get the indices of non zero fluxes
         ;nzf=where(instr.plf NE 0) 

         CASE plstr.pars[i] OF

            'dbb': BEGIN
            par[0,*]=outdelf[0,*]
            par[1,*]=outdelf[3,*]
            par[*,omitdisk]=0.
         END
            
            'plf': BEGIN
            par[0,*]=outpelf[0,*]
            par[1,*]=outpelf[3,*]
         END
            
            'totf': BEGIN
            par[0,*]=outtelf[0,*]
            par[1,*]=outtelf[3,*]
            par[*,omitdisk]=outpelf[*,omitdisk]
         END

            ELSE: why=5
         ENDCASE
         
       ENDIF
      ENDIF
        

      ;do we need automatic scaling?
      IF total(plstr.yr[*,i]) EQ 0 THEN BEGIN
         IF noerror THEN nyr=[0.9*min(par[where(par NE 0)]),$
                              1.1*max(par[where(par NE 0)])] $
         ELSE nyr=[0.9*min(par[0,where((par[1,*] NE -1) AND (par[0,*] NE 0))]),$
                   1.1*max(par[0,where(par[1,*] NE -1)])]
      ENDIF ELSE nyr=plstr.yr[*,i]    
            
            
    ; upper and lower limit ranges
      IF plstr.lims[0,i] EQ 0. THEN lln=nyr[0] ELSE lln=plstr.lims[0,i]
      IF plstr.lims[1,i] EQ 0. THEN uln=nyr[0] ELSE uln=plstr.lims[1,i]

      IF i EQ 0 THEN BEGIN
         plotsinglepar, time, par, plstr.ytitle[i], states, nxr, nyr, colcode, usecol=plstr.usecol, ylog=plstr.ylog[i],nul=uln, nll=lln 
         IF plstr.oplot[0] EQ 0 THEN multiplot
      ENDIF ELSE BEGIN
         IF plstr.oplot[i-1] EQ 1 THEN BEGIN
            plotsinglepar, time, par, plstr.ytitle[i], states, $
                           nxr, nyr, colcode, usecol=plstr.usecol, $
                           ylog=plstr.ylog[i], /oplot, npsm=4, nul=uln, nll=lln
            multiplot
            ENDIF ELSE BEGIN
            plotsinglepar, time, par, plstr.ytitle[i], states, nxr, nyr, colcode, usecol=plstr.usecol, ylog=plstr.ylog[i], nul=uln, nll=lln
             IF plstr.oplot[i] EQ 0 THEN multiplot
          ENDELSE
         ENDELSE

;think about this

;poslx=!x.crange[0]+(!x.crange[1]-!x.crange[0])*0.9
;posly=10.^(!y.crange[1])*0.05
;xyouts,poslx,posly,'d',size=1.5

   ENDELSE

ENDFOR

IF NOT keyword_set(mwstr) THEN mwstr=instr
yrmw=reform(plstr.mwyr[*,0])


IF plstr.incmw THEN BEGIN
   IF NOT keyword_set(mwstr) THEN mwstr=instr
   yrmw=reform(plstr.mwyr[*,0])
   IF plstr.mwpt EQ 2 THEN sepraxis=1 ELSE sepraxis=0
   pl_mw, mwstr, inx, colcode, time, band=plstr.band, $
                           rfreq=plstr.rfreq, radonly=plstr.radonly, $
                           othin=plstr.rthin, plotrad=plstr.plotrad, $
                           mwxr=plstr.xr, facr=plstr.fr, mwyr=yrmw, $
                           realtime=realtime, sepraxis=sepraxis
ENDIF


multiplot,/default

IF ps THEN BEGIN
   device,/close
  IF !VERSION.OS eq 'Win32' THEN set_plot,'win' ELSE set_plot,'x'
ENDIF

END

  
