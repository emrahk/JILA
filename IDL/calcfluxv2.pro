pro calcfluxv2, inpstr, inx, outdelf, outpelf, outtelf, dcor=dcor, pcor=pcor, $
mac=mac, plmin=minpe, diagplot=diagplot, noderr=noderr, pcaonly=pcaonly

;This program does the eddington luminosity fraction conversions for
;the given power-law flux and spectral index and diskbb flux and tin

;INPUTS
;
; inpstr: input structure that holds all the spectral information
; inx: to select the source and outburst, index of the structure
; 
; OUTPUTS
;
; outdelf : output diskbb ELF, and its flux error, and its md error
; outpelf : output power ELF, error
; outtelf : output total ELF, error
; 
; OPTIONAL INPUTS
;
; dcor : do diskbb conversion, 0.1-200 keV; otherwise no correction 
; pcor : do power-law conversion, minpe-200 keV; otherwise 3-200 keV 
; minpe : minimum energy to be used for power law correction, default
;       2*tin if TIN NE 0, 0.5 if TIN=0
; mac: make a correction same as maccarone et al. 
; diagplot: make diagnostic plot
; noderr : there is no diskbb error, this is to be able to use this
;program with the decay data
; pcaonly: only pca data is present
;
; USES
;
; NONE
;
; USED BY
;
; plotting programs
;
; LOGS
;
; Dec 4: when there is no hexte, the program could shift the
;        values. Fixed by checking both hexte and pca fluxes
;
; Dec 10: added only pca to obtain pca fit values for the case of
;nohexte
;
; DEC 16: Bug fix, the power law fluxes are provided 10-9 ergs cm-2/s
;not 10-10. Need to redo all the graphs
;
; DEC 17: now disk upper limits can be handled
; JAN 12: corrected handling of upper limits (was placing -1*error in error)
; JAN 23: was placing -1 to the last element when all diskbb values
; were regular (not upper limits), fixed
;
; MAR 2017
;
; fixed a bug regarding wrong tag for disk normalization, use dnormph,
; not normp
;
; MAY 2017
;
; Fixing Maccarone correction, numerical integration fails because
;large energies do not contribute to total but difficult to evaluate
; using analytical solutions with Gamma functions (igamma(0.2,E/200.))
;
;
; NOV 2017
;
; Changing error calculation to assume independent errors, with no
;covariance. Adding an additional element to calculate total error
;
; These changes should be implemented in calcfluxv3.pro. Eventually
;calcfluxv2.pro may be obsolete.



IF NOT keyword_set(dcor) THEN dcor=0
IF NOT keyword_set(pcor) THEN pcor=0
IF NOT keyword_set(mac) THEN mac=0
IF NOT keyword_set(noderr) THEN noderr=0
IF NOT keyword_set(pcaonly) THEN pcaonly=0

;only consider existing plf values
;when there is no hexte for some or all observations, we need to take
;care of that. Current fix will not work with older structure, the
;best maybe to write a program to convert older structure to new structure

IF pcaonly THEN BEGIN
   xx=where((inpstr[inx].plf+inpstr[inx].plfp) NE 0.)

   ind=inpstr[inx].indp[*,xx]
   tin=inpstr[inx].tinp[*,xx]
   plf=inpstr[inx].plfp[xx]
   dbb=inpstr[inx].dbbp[xx]
   norm=inpstr[inx].dnormp[*,xx]
   totf=inpstr[inx].totfp[*,xx]
   untotf=inpstr[inx].untotfp[xx]
   untotf200=inpstr[inx].untotf200[*,xx] ;chek this later
   xdates=inpstr[inx].xdates[xx]
ENDIF ELSE BEGIN
   xx=where((inpstr[inx].plf+inpstr[inx].plfp) NE 0.)

   ind=inpstr[inx].ind[*,xx]
   tin=inpstr[inx].tin[*,xx]
   plf=inpstr[inx].plf[xx]
   dbb=inpstr[inx].dbb[xx]
   norm=inpstr[inx].dnormph[*,xx]
   totf=inpstr[inx].totf[*,xx]
   untotf=inpstr[inx].untotf[xx]
   untotf200=inpstr[inx].untotf200[*,xx]
   xdates=inpstr[inx].xdates[xx]
ENDELSE

IF NOT keyword_set(minpe) THEN BEGIN
   minpa=fltarr(n_elements(plf))
   xx=where(tin[0,*] EQ 0)
   IF xx[0] NE -1 THEN minpa[xx]=0.5
   yy=where(tin[0,*] NE 0)
   IF yy[0] NE -1 THEN minpa[yy] = 2.*tin[0,yy] < 3.
ENDIF ELSE minpa=replicate(minpe,n_elements(plf))

IF NOT keyword_set(diagplot) THEN diagplot=0


kpc=3.08*1d21 ;centimeters
Elum=1.26D38*inpstr[inx].mass[0]

corrp=fltarr(n_elements(plf))


;   assume 3-25 keV error dominated by error in power law, for now do
;   not consider 25-200 erorrs separately
 
   errorf=totf[1,*]/totf[0,*] 

;   error due to mass and distance
   
;   errat=2*(inpstr[inx].distance[1]/inpstr[inx].distance[0])+(inpstr[inx].mass[1]/inpstr[inx].mass[0]) OVERESTIMATE

errat=sqrt(4*(inpstr[inx].distance[1]/inpstr[inx].distance[0])^2.+(inpstr[inx].mass[1]/inpstr[inx].mass[0])^2.)


;convert power law

IF mac THEN BEGIN
  ;corrp=qsimp('cutoffpl',0.5, 10000.)/qsimp('cutoffpl',3.,25.)
  ;3-25 keV correction factor is fixed
  corrp=2.857 
  pflux=untotf*corrp
ENDIF ELSE BEGIN

   pflux=fltarr(n_elements(plf))
   xx=where(ind[0,*] eq 2.0)
   yy=where(ind[0,*] NE 2.0)

   IF xx[0] NE -1 THEN BEGIN
      FOR i=0, N_ELEMENTS(xx)-1 DO BEGIN
         IF untotf200[xx[i]] EQ 0 THEN BEGIN
            IF pcor THEN corrp=(alog(200.)-alog(minpa[xx[i]]))/(alog(25.)-alog(3.)) ELSE $
                         corrp=(alog(200.)-alog(3.))/(alog(25.)-alog(3.))
            pflux[xx[i]]=plf[xx[i]]*corrp
         ENDIF ELSE BEGIN
            IF pcor THEN corrp=(alog(25.)-alog(minpa[xx[i]]))/(alog(25.)-alog(3.)) ELSE corrp=1.
            pfluxp=plf[xx[i]]*corrp
            pflux[xx[i]]=pfluxp+untotf200[xx[i]]
         ENDELSE
      ENDFOR
   ENDIF
  
   FOR i=0, N_ELEMENTS(yy)-1 DO BEGIN
        IF untotf200[yy[i]] EQ 0 THEN BEGIN
           IF pcor THEN corrp=(200.^(-ind[0,yy[i]]+2.)-minpa[yy[i]]^(-ind[0,yy[i]]+2.))/(25.^(-ind[0,yy[i]]+2.)-3.^(-ind[0,yy[i]]+2.)) ELSE corrp=(200.^(-ind[0,yy[i]]+2.)-3.^(-ind[0,yy[i]]+2.))/(25.^(-ind[0,yy[i]]+2.)-3.^(-ind[0,yy[i]]+2.))
           pflux[yy[i]]=plf[yy[i]]*corrp
        ENDIF ELSE BEGIN
           IF pcor THEN corrp=(25.^(-ind[0,yy[i]]+2.)-minpa[yy[i]]^(-ind[0,yy[i]]+2.))/(25.^(-ind[0,yy[i]]+2.)-3.^(-ind[0,yy[i]]+2.)) ELSE corrp=1.
           pfluxp=plf[yy[i]]*corrp
           pflux[yy[i]]=pfluxp+untotf200[yy[i]]
        ENDELSE
     ENDFOR
ENDELSE

;;;;values were in terms of 1e-9!

outpelf=fltarr(4,n_elements(pflux)) ;MAKE IT 4 elements for the total error
outpelf[0,*]=1D-9*pflux*4*!PI*(inpstr[inx].distance[0]*kpc)^2./Elum
outpelf[1,*]=outpelf[0,*]*errorf
outpelf[2,*]=outpelf[0,*]*errat
outpelf[3,*]=outpelf[0,*]*sqrt(errat^2.+errorf^2.)

; now the disk part

outdelf=outpelf

IF noderr THEN BEGIN
   dbflux=dbb*1D-9
   outdelf[0,*]=dbflux*4*!PI*(inpstr[inx].distance[0]*kpc)^2./Elum
   outdelf[1,*]=0.  
   outdelf[2,*]=outdelf[0,*]*errat
   outdelf[3,*]=outdelf[0,*]*errat 
   ENDIF ELSE BEGIN

   IF dcor THEN bolcor_diskbbv2, tin, norm, dbflux ELSE bolcor_diskbbv2, tin, norm, dbflux, erange=[3., 25.]

   xx=where(dbflux[1,*] NE -1)
   yy=where(dbflux[1,*] EQ -1)
   outdelf[0,*]=dbflux[0,*]*4*!PI*(inpstr[inx].distance[0]*kpc)^2./Elum
   outdelf[1,xx]=dbflux[1,xx]*4*!PI*(inpstr[inx].distance[0]*kpc)^2./Elum
   errorfd=outdelf[1,xx]/outdelf[0,xx]
   IF yy[0] NE -1 THEN outdelf[1,yy]=-1
   outdelf[2,*]=outdelf[0,*]*errat
   outdelf[3,xx]=outdelf[0,xx]*sqrt(errat^2.+errorfd^2.)
ENDELSE

outtelf=outpelf ;
outtelf[0,*]=outpelf[0,*]+outdelf[0,*]
outtelf[1,*]=outpelf[1,*]+outdelf[1,*] ;not sure, could be an overestimate
outtelf[2,*]=outtelf[0,*]*errat ;yes, mass and distance error enter the total the same way
errorft=outtelf[1,*]/outtelf[0,*]
outtelf[3,*]=outtelf[0,*]*sqrt(errat^2.+errorft^2.)

IF diagplot THEN BEGIN

  plotsym, 0,1,/fill
  cs=1.3
  multiplot, [1,4], mxtitle='Time (MJD-50000 days)',mxtitsize=1.2

  ploterror, xdates-50000.,ind[0,*],ind[1,*],psym=8, ytitle='!9G !X',$
  /xstyle,/nohat,/ystyle, yr=[min(ind[0,*]-ind[1,*])-0.2,max(ind[0,*]+ind[1,*])+0.2], chars=cs ;

  multiplot
     
     ploterror, xdates-50000.,pflux,pflux*errorf,psym=8, ytitle='PLF',/ylog, $
                /xstyle,/nohat,/ystyle, yr=[min(plf+untotf200)/2.,max(pflux*(1.+errorf))*1.2], chars=cs ;


     oplot, xdates-50000., plf+untotf200, psym=1
     multiplot

ploterror, xdates-50000.,tin[0,*],tin[1,*],psym=8, ytitle='Tin',$
/xstyle,/nohat,/ystyle, yr=[min(tin[0,*]-tin[1,*])-0.2,max(tin[0,*]+tin[1,*])+0.2], chars=cs ;

multiplot

ploterror, xdates-50000.,dbflux[0,*]*1D10,dbflux[1,*]*1D10,psym=8, ytitle='DBB',/ylog,$
/xstyle,/nohat,/ystyle, yr=[min(dbb[where(dbb Ne 0.)])/2., max(dbflux[0,*]+dbflux[1,*])*1.2*1D10],chars=cs ;

oplot, xdates-50000., dbb, psym=1

multiplot,/default

ENDIF


END

FUNCTION cutoffpl, X
  return, X^(-0.8)*exp(-X/200)
END

