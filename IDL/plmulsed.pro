pro plmulsed, inpstr, inx, mjds, plotrad=plotrad, plotx=plotx, ct=tc, $
                 ps=ps, fname=namef, plote=plote, vfv=vfv, exfreq=freqex, $
                 rband=oiband, rflux=oiflux, opltsed=opltsed, yrn=nyr, $
              Tin=inT, fixind=indfix, xrn=nxr, auto=auto, fres=resf, $
              incqsc=incqsc, extind=indext, crflxtxt=crflxtxt, oplqsc=oplqsc

;This program plots SEDs of given observation dates. If asked, checks
;radio and X-ray observations within ct and plots them as well.

;INPUTS
;
; inpstr: input structure
; inx: index of structure that determines the source and outburst
; mjds: days of observations. data from mjd+-ct will be plotted
;
;OUTPUTS
;
; A plot of the SED
;
; OPTIONAL INPUTS
;
; plotrad: If set, radio fluxes will be plotted
; plotx: If set, X-ray fluxes will be plotted (to be implemented later)
; ct: contamperanous time to search for multiwavelength data
; opltsed: if set, plot the best fit blackbody spectrum to the OIR SED 
; Tin: set of initial temperatures
; fixind: array that indices if the index is fixed
; auto: use the best fir automatically
; ps: postscript output
; fname: optional name of the postscript output
; plote: x axis in eV instead of frequency
; vfv: nu - f - nu plot, multiply fluxes by frequencies
; yrn: new yrange 
; xrn: new xrange
; fres: structure for fit results
; incqsc: remove quiescence data
; oplqsc: overplot quiescent data
; exfreq: exclude these frequencies from the fit
;
; LOGS
;
; Created by E. Kalemci May, 2015
;
; April 2016, forgot intialization of auto keyword 
; April 2016, fres keyword added to keep track of fit results
; April 2016, added incqsc to include quiescence fluxes if necessary
; April 2016, added indext to provide an external index for radio
; April 2016, added crflxtxt to allow creating flux tables
; May 2016, option not to include certain data
; May 2016, oplqsc keyword added

IF NOT keyword_set(plotrad) THEN plotrad=0
IF NOT keyword_set(plotx) THEN plotx=0
IF NOT keyword_set(ps) THEN ps=0
IF NOT keyword_set(plote) THEN plote=0
IF NOT keyword_set(vfv) THEN vfv=0
IF NOT keyword_set(opltsed) THEN opltsed=0
IF NOT keyword_set(tc) THEN tc=0.75 ;check +- 0.75 day
IF NOT keyword_set(namef) THEN namef='mulsed.eps'
IF NOT keyword_set(usecol) THEN usecol=0
IF NOT keyword_set(nxr) THEN nxr=[1E9,1E15]
IF NOT keyword_set(inT) THEN inT=replicate(8000.,n_elements(mjds))
IF NOT keyword_set(indfix) THEN indfix=intarr(n_elements(mjds))
IF NOT keyword_set(auto) THEN auto=0
IF NOT keyword_set(incqsc) THEN incqsc=0
IF NOT keyword_set(indext) THEN indext=fltarr(n_elements(mjds))
IF NOT keyword_set(crflxtxt) THEN crflxtxt=0
IF NOT keyword_set(oplqsc) THEN oplqsc=0
IF NOT keyword_set(freqex) THEN freqex=0

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
   device, filename = namef
   device, yoffset = 2
   device, ysize = 28.
   ;device, xsize = 12.0
   !p.font=0
   device,/times
endif

IF NOT ps THEN window, 4, retain=2, xsize=800, ysize=800


oirdates=inpstr[inx].oirinfo.dates
oirdates=oirdates[where(oirdates NE 0.)]

nrows=n_elements(mjds)

multiplot, [1,nrows], mxtitle='Frequency (Hz)', mytitle='Flux (mJy)', $
           mxtitsize=1.2,mytitsize=1.2

 FOR j=0, nrows-1 DO BEGIN

    oidex=where((oirdates GT (mjdS[j]-tc)) AND (oirdates LT (mjds[j]+tc)))

    cont=1

    IF oidex[0] NE -1 THEN BEGIN
       ;for each band check 
       oiflux=inpstr[inx].oirinfo.flux[*,oidex]
       oiband=inpstr[inx].oirinfo.band[oidex]
       oidates=inpstr[inx].oirinfo.dates[oidex]

   ;need to group observations during this time
       nobs=n_elements(oidates)
       groups=intarr(7,nobs)
       groups[*,*]=-1
       used=-1
       ngr=0

   FOR i=0, nobs-1 DO BEGIN
      chk=where(used eq i)
      IF chk[0] EQ -1 THEN BEGIN
         kk=where(oidates EQ oidates[i])
         used=[used,kk]
         groups[0:n_elements(kk)-1,ngr]=kk
         print, ngr, oidates[kk[0]], oiband[kk], reform(oiflux[0,kk])
         ngr=ngr+1
      ENDIF
   ENDFOR

   IF auto THEN grn=100 ELSE BEGIN
      print, 'choose the group you want to have in the SED, or type 100 for best guess:'
      read, grn
   ENDELSE
   
   IF grn EQ 100 THEN BEGIN
      ngrn=0
      abnds=['B','V','R','I','J','H','K']
      FOR bc=0, n_elements(abnds)-1 DO BEGIN
         xx=where(oiband eq abnds[bc])
         IF xx[0] NE -1 THEN BEGIN
;            print,oiband[xx]
            srt=sort(abs(oidates[xx]-mjds[j]))
            ngrn=[ngrn,xx[srt[0]]]
         ENDIF
      ENDFOR
      ngrn=ngrn[1:n_elements(ngrn)-1]
      oiflux=oiflux[*,ngrn]
      oiband=oiband[ngrn]
      print, oidates[ngrn], oiflux, oiband
   ENDIF ELSE BEGIN

      aa=where(groups[*,grn] NE -1)
      yy=where(oiflux[0,groups[aa,grn]] ne 0.)
      oiflux=oiflux[*,groups[aa[yy],grn]]
      oiband=oiband[groups[aa[yy],grn]]
      print, oidates[aa[yy[0]]], oiflux, oiband 
  ENDELSE
ENDIF ELSE BEGIN
   print, 'No oir observation corresponding to given mjd and ct, change a parameter'
   cont=0
ENDELSE

IF plotrad THEN BEGIN
   raddates=inpstr[inx].radinfo.dates
   ridex=where((raddates GT (mjds[j]-tc)) AND (raddates LT (mjds[j]+tc)))
   IF ridex[0] NE -1 THEN BEGIN
      fluxrad=inpstr[inx].radinfo.flux[*,ridex]
      freqrad=inpstr[inx].radinfo.freq[ridex]*1E9
      print,freqrad, fluxrad
   ENDIF ELSE BEGIN
      print, 'No radio observation corresponding to given mjd and ct, change a parameter if you want radio flux in plot'
      plotrad=0
   ENDELSE
ENDIF

IF cont THEN BEGIN
   
   IF (plotrad AND plotx) THEN BEGIN

   ENDIF

   IF (plotrad AND (NOT plotx)) THEN BEGIN

      IF keyword_set(nyr) THEN $
         plotsed, oiflux, oiband, radflux=fluxrad, radfreq=freqrad, ps=ps, $ 
                  fname=namef, plote=plote, vfv=vfv, opltsed=opltsed, $
                  mjd=mjds[j], yrn=nyr, Tin=inT[j], /strip, extind=indext[j], $
                  fixind=indfix[j], xrn=nxr, res=fitres, incqsc=incqsc,$
                  crflxtxt=crflxtxt, oplqsc=oplqsc, exfreq=freqex ELSE $
                     plotsed, oiflux, oiband, radflux=fluxrad, exfreq=freqex,$
                              radfreq=freqrad, ps=ps, fname=namef, $
                              plote=plote, vfv=vfv, opltsed=opltsed, $
                              mjd=mjds[j],fixind=indfix[j], xrn=nxr, $
                             /strip, Tin=inT[j], res=fitres, extind=indext[j], $
                              incqsc=incqsc, crflxtxt=crflxtxt, oplqsc=oplqsc
   ENDIF

   IF ((NOT plotrad) AND (not plotx)) THEN BEGIN

      IF keyword_set(nyr) THEN $
         plotsed, oiflux, oiband, ps=ps, fname=namef, oplqsc=oplqsc, $
                  plote=plote, vfv=vfv, opltsed=opltsed, res=fitres, $
                  mjd=mjds[j], yrn=nyr,/strip, Tin=inT[j], xrn=nxr,$
                  incqsc=incqsc, extind=indext[j], crflxtxt=crflxtxt, $
                  exfreq=freqex ELSE $
                      plotsed, oiflux, oiband, ps=ps, fname=namef, $
                  plote=plote, vfv=vfv, opltsed=opltsed, res=fitres, $
                  mjd=mjd,/strip,  Tin=inT[j], xrn=nxr, oplqsc=oplqsc,$
                 incqsc=incqsc, extind=indext[j], crflxtxt=crflxtxt,$
                               exfreq=freqex

   ENDIF

multiplot
ENDIF

IF (j EQ 0) THEN resf=replicate(fitres, nrows) ELSE resf[j]=fitres

ENDFOR


multiplot,/default
IF ps THEN BEGIN
   device,/close
   set_plot,'x'
ENDIF

END
