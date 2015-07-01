pro plsinglesed, inpstr, inx, mjd, plotrad=plotrad, plotx=plotx, ct=tc, $
                 ps=ps, fname=namef, plote=plote, vfv=vfv, $
                 rband=oiband, rflux=oiflux, opltsed=opltsed, yrn=nyr, $
                 Tin=inT, fixind=fixind, xrn=nxr, res=rstr, auto=auto

;This program plots SED of given observation date. If asked, checks
;radio and X-ray observations within ct and plots them as well.

;INPUTS
;
; inpstr: input structure
; inx: index of structure that determines the source and outburst
; mjd: day of observation. data from mjd+-ct will be plotted
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
; Tin: initial bb temperature for the fit
; fixind: if set use fixed radio index
; res: resulting structure from the fit
; auto: use best guess sed dates
;
; ps: postscript output
; fname: optional name of the postscript output
; plote: x axis in eV instead of frequency
; vfv: nu - f - nu plot, multiply fluxes by frequencies
; yrn: new yrange 
; xrn: new xrange
;
; LOGS
;
; Created by E. Kalemci Feb 22, 2015
;
; FIXED a problem with the case that the ORI dates are close to each
;other, but not exactly the same date. Now, when prompted, one can
;choose option 100 which finds the set of dafa closest to the given
;date.
;
; Added opltbb option to overplot the bb fit
;
;


IF NOT keyword_set(plotrad) THEN plotrad=0
IF NOT keyword_set(plotx) THEN plotx=0
IF NOT keyword_set(ps) THEN ps=0
IF NOT keyword_set(namef) THEN namef='sed.eps'
IF NOT keyword_set(plote) THEN plote=0
IF NOT keyword_set(vfv) THEN vfv=0
IF NOT keyword_set(opltbb) THEN opltbb=0
IF NOT keyword_set(tc) THEN tc=0.75 ;check +- 0.75 day
IF NOT keyword_set(inT) THEN inT=8000.
IF NOT keyword_set(fixind) THEN fixind=0
IF NOT keyword_set(opltsed) THEN opltsed=0
IF NOT keyword_set(nxr) THEN nxr=[1E9,1E15]

oirdates=inpstr[inx].oirinfo.dates
oirdates=oirdates[where(oirdates NE 0.)]

oidex=where((oirdates GT (mjd-tc)) AND (oirdates LT (mjd+tc)))

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
            srt=sort(abs(oidates[xx]-mjd))
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
   ridex=where((raddates GT (mjd-tc)) AND (raddates LT (mjd+tc)))
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
                  fname=namef, plote=plote, vfv=vfv, opltsed=opltsed, mjd=mjd, $
                  yrn=nyr, Tin=inT, fixind=fixind, xrn=nxr, res=rstr ELSE $
                     plotsed, oiflux, oiband, radflux=fluxrad, $
                              radfreq=freqrad, ps=ps, fname=namef, $
                              plote=plote, vfv=vfv, opltsed=opltsed, mjd=mjd,$
                              Tin=inT, fixind=fixind, xrn=nxr, res=rstr

   ENDIF

   IF ((NOT plotrad) AND (not plotx)) THEN BEGIN

      IF keyword_set(nyr) THEN $
         plotsed, oiflux, oiband, ps=ps, fname=namef, $
                  plote=plote, vfv=vfv, opltsed=opltsed, $
                  mjd=mjd, yrn=nyr, Tin=inT, xrn=nxr, res=rstr ELSE $
                      plotsed, oiflux, oiband, ps=ps, fname=namef, $
                  plote=plote, vfv=vfv, opltsed=opltsed, $
                  mjd=mjd, Tin=inT, xrn=nxr, res=rstr

   ENDIF

ENDIF


END
