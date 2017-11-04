pro calcfluxv3, inpstr, inx, outdelf, outpelf, outtelf, dcor=dcor, pcor=pcor, $
mac=mac, plmin=minpe, diagplot=diagplot, noderr=noderr, pcaonly=pcaonly

;This program does the eddington luminosity fraction conversions for
;the given power-law flux and spectral index and diskbb flux and tin
;  this version uses cflux outputs

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
;
;
; October 2017
;
; with the conversion to cflux now plf and dbb has errors but the
;program still assumes that it is a one dimensional array. This
;problem has been fixed.
;
;November 2017
;
;Changing error calculation to assume independent errors, with no
;covariance. Adding an additional element to calculate total error
;
;I need to be careful indexing nz values, especially with bolcor_diskbbv2
;
; Fixed an error indexing plfs. This version may not work fine with
;older data.
;
;When some PCA+HEXTE fits missing it screws up! Must check and fill
;missing pca+hexte values with pca values
  
 
IF NOT keyword_set(dcor) THEN dcor=0
IF NOT keyword_set(pcor) THEN pcor=0
IF NOT keyword_set(mac) THEN mac=0
IF NOT keyword_set(noderr) THEN noderr=0
IF NOT keyword_set(pcaonly) THEN pcaonly=0

;only consider existing plf values
;when there is no hexte for some or all observations, we need to take
;care of that. Current fix will not work with older structure, the
;best maybe to write a program to convert older structure to new structure

;find the dimensions of the arrays
ndim=size(inpstr[inx].plf,/n_dimensions)
nelplf=size(inpstr[inx].plf,/n_elements)

IF ndim EQ 1 THEN nz=where((inpstr[inx].plf+inpstr[inx].plfp) NE 0.,nelnz) $
ELSE nz=where((inpstr[inx].plf[0,*]+inpstr[inx].plfp[0,*]) NE 0.,nelnz) ;non zero means all elements zero.

;Check if pca+hexte missing any data
IF NOT pcaonly THEN phmis=where((inpstr[inx].plf[0,nz] EQ 0) AND (+inpstr[inx].plfp[0,nz] NE 0.),nelhm)
   
   
plf=fltarr(2,nelplf)
dbb=fltarr(2,nelplf)

IF pcaonly THEN BEGIN      
   ind=inpstr[inx].indp[*,nz]
   tin=inpstr[inx].tinp[*,nz]
   IF ndim EQ 1 THEN plf[0,*]=inpstr[inx].plfp[nz] ELSE plf=inpstr[inx].plfp[*,nz]
   IF ndim EQ 1 THEN dbb[0,*]=inpstr[inx].dbbp[nz] ELSE dbb=inpstr[inx].dbbp[*,nz]
   norm=inpstr[inx].dnormp[*,nz]
   totf=inpstr[inx].totfp[*,nz]
   untotf=inpstr[inx].untotfp[nz]
   untotf200=inpstr[inx].untotf200[0,nz]
   xdates=inpstr[inx].xdates[nz]
ENDIF ELSE BEGIN

   ind=inpstr[inx].ind[*,nz]
   tin=inpstr[inx].tin[*,nz]
   IF ndim EQ 1 THEN plf[0,*]=inpstr[inx].plf[nz] ELSE plf=inpstr[inx].plf[*,nz]
   IF ndim EQ 1 THEN dbb[0,*]=inpstr[inx].dbb[nz] ELSE dbb=inpstr[inx].dbb[*,nz]   
   norm=inpstr[inx].dnormph[*,nz]
   totf=inpstr[inx].totf[*,nz]
   untotf=inpstr[inx].untotf[nz]
   untotf200=inpstr[inx].untotf200[0,nz]
   xdates=inpstr[inx].xdates[nz]
   IF nelhm NE 0 THEN BEGIN
         ind[*,phmis]=inpstr[inx].indp[*,nz[phmis]]
         tin[*,phmis]=inpstr[inx].tinp[*,nz[phmis]]
         IF ndim EQ 1 THEN plf[0,phmis]=inpstr[inx].plfp[nz[phmis]] ELSE plf[*,phmis]=inpstr[inx].plfp[*,nz[phmis]]
         IF ndim EQ 1 THEN dbb[0,phmis]=inpstr[inx].dbbp[nz[phmis]] ELSE dbb[*,phmis]=inpstr[inx].dbbp[*,nz[phmis]]   
         norm[*,phmis]=inpstr[inx].dnormp[*,nz[phmis]]
         totf[*,phmis]=inpstr[inx].totfp[*,nz[phmis]]
         untotf[phmis]=inpstr[inx].untotf[nz[phmis]]
         untotf200[*,phmis]=inpstr[inx].untotf200[0,nz[phmis]]
      ENDIF
ENDELSE

IF NOT keyword_set(minpe) THEN BEGIN
   minpa=fltarr(nelnz)
   xx=where(tin[0,*] EQ 0)
   IF xx[0] NE -1 THEN minpa[xx]=0.5
   yy=where(tin[0,*] NE 0)
   IF yy[0] NE -1 THEN minpa[yy] = 2.*tin[0,yy] < 3.
ENDIF ELSE minpa=replicate(minpe,nelplf)

IF NOT keyword_set(diagplot) THEN diagplot=0


kpc=3.086*1d21 ;centimeters
Elum=1.26D38*inpstr[inx].mass[0]

corrp=fltarr(nelplf)


;   assume 3-25 keV error dominated by error in power law, for now do
;   not consider 25-200 errors separately
 
    errorf=totf[1,*]/totf[0,*] 

;   error due to mass and distance

  errat=sqrt(4*(inpstr[inx].distance[1]/inpstr[inx].distance[0])^2.+(inpstr[inx].mass[1]/inpstr[inx].mass[0])^2.)

;convert power law

IF mac THEN BEGIN
  ;corrp=qsimp('cutoffpl',0.5, 10000.)/qsimp('cutoffpl',3.,25.)
  ;3-25 keV correction factor is fixed
  corrp=2.857 
  pflux=untotf*corrp
ENDIF ELSE BEGIN

   pflux=fltarr(nelnz)
   xx=where(ind[0,*] eq 2.0)
   yy=where(ind[0,*] NE 2.0)

   IF xx[0] NE -1 THEN BEGIN
      FOR i=0, N_ELEMENTS(xx)-1 DO BEGIN
         IF untotf200[0,xx[i]] EQ 0 THEN BEGIN 
            IF pcor THEN corrp=(alog(200.)-alog(minpa[xx[i]]))/(alog(25.)-alog(3.)) ELSE $
                         corrp=(alog(200.)-alog(3.))/(alog(25.)-alog(3.))
            pflux[xx[i]]=plf[0,xx[i]]*corrp  
         ENDIF ELSE BEGIN
            IF pcor THEN corrp=(alog(25.)-alog(minpa[xx[i]]))/(alog(25.)-alog(3.)) ELSE corrp=1.
            pfluxp=plf[0,xx[i]]*corrp  
            pflux[xx[i]]=pfluxp+untotf200[0,xx[i]] 
         ENDELSE
      ENDFOR
   ENDIF
  
   FOR i=0, N_ELEMENTS(yy)-1 DO BEGIN
        IF untotf200[0,yy[i]] EQ 0 THEN BEGIN
           IF pcor THEN corrp=(200.^(-ind[0,yy[i]]+2.)-minpa[yy[i]]^(-ind[0,yy[i]]+2.))/(25.^(-ind[0,yy[i]]+2.)-3.^(-ind[0,yy[i]]+2.)) ELSE corrp=(200.^(-ind[0,yy[i]]+2.)-3.^(-ind[0,yy[i]]+2.))/(25.^(-ind[0,yy[i]]+2.)-3.^(-ind[0,yy[i]]+2.))
           pflux[yy[i]]=plf[0,yy[i]]*corrp
        ENDIF ELSE BEGIN
           IF pcor THEN corrp=(25.^(-ind[0,yy[i]]+2.)-minpa[yy[i]]^(-ind[0,yy[i]]+2.))/(25.^(-ind[0,yy[i]]+2.)-3.^(-ind[0,yy[i]]+2.)) ELSE corrp=1.
           pfluxp=plf[0,yy[i]]*corrp
           pflux[yy[i]]=pfluxp+untotf200[0,yy[i]]
        ENDELSE
     ENDFOR
ENDELSE

;;;;values were in terms of 1e-9!

;outpelf=fltarr(3,n_elements(pflux))
outpelf=fltarr(4,100)
outpelf[0,nz]=1D-9*pflux*4*!PI*(inpstr[inx].distance[0]*kpc)^2./Elum
;IF power law error is present use it
IF ndim eq 2 THEN errorfp=plf[1,*]/plf[0,*] ELSE errorfp=errorf
outpelf[1,nz]=outpelf[0,nz]*errorfp
outpelf[2,nz]=outpelf[0,nz]*errat
outpelf[3,nz]=outpelf[0,nz]*sqrt(errat^2.+errorfp^2.)

; now the disk part

outdelf=fltarr(4,100)

IF noderr THEN BEGIN
   dbflux=dbb[0,*]*1D-9
   outdelf[0,nz]=dbflux*4*!PI*(inpstr[inx].distance[0]*kpc)^2./Elum
   outdelf[1,nz]=0.
   outdelf[2,nz]=outdelf[0,nz]*errat
   outdelf[3,nz]=outdelf[0,nz]*errat
   ENDIF ELSE BEGIN

   IF dcor THEN bolcor_diskbbv2, tin, norm, dbflux ELSE bolcor_diskbbv2, tin, norm, dbflux, erange=[3., 25.] ;already non zero elements enter

   xx=where(dbflux[1,*] NE -1)
   yy=where(dbflux[1,*] EQ -1)
   outdelf[0,nz]=dbflux[0,*]*4*!PI*(inpstr[inx].distance[0]*kpc)^2./Elum
   outdelf[1,nz[xx]]=dbflux[1,xx]*4*!PI*(inpstr[inx].distance[0]*kpc)^2./Elum
   errorfd=outdelf[1,nz[xx]]/outdelf[0,nz[xx]]
   nans=where(finite(errorfd) EQ 0)
   errorfd[nans]=0
   IF yy[0] NE -1 THEN outdelf[1,nz[yy]]=-1
   outdelf[2,*]=outdelf[0,*]*errat
   outdelf[3,nz[xx]]=outdelf[0,nz[xx]]*sqrt(errat^2.+errorfd^2.)
ENDELSE

;What are we going to do with the disk upper limits?????
;Use option 1 do not include them in this calculation, but during plotting
;and tables, if disk is upper limit, treat it correctly. Let's
;think about this.
;in cflux implementation currently there is no upper limit. not
;worrying about it now


outtelf=fltarr(4,100)
outtelf[0,*]=outpelf[0,*]+outdelf[0,*]
outtelf[1,*]=outpelf[1,*]+outdelf[1,*];not sure, could be an overestimate
outtelf[2,*]=outtelf[0,*]*errat
errorft=outtelf[1,nz]/outtelf[0,nz]
outtelf[3,nz]=outtelf[0,nz]*sqrt(errat^2.+errorft^2.)

IF diagplot THEN BEGIN

  plotsym, 0,1,/fill
  cs=1.3
  multiplot, [1,4], mxtitle='Time (MJD-50000 days)',mxtitsize=1.2

  ploterror, xdates-50000.,ind[0,*],ind[1,*],psym=8, ytitle='!9G !X',$
  /xstyle,/nohat,/ystyle, yr=[min(ind[0,*]-ind[1,*])-0.2,max(ind[0,*]+ind[1,*])+0.2], chars=cs ;

  multiplot
     
     ploterror, xdates-50000.,pflux,pflux*errorf,psym=8, ytitle='PLF',/ylog, $
                /xstyle,/nohat,/ystyle, yr=[min(plf[0,*]+untotf200[0,*])/2.,max(pflux*(1.+errorf))*1.2], chars=cs ;


     oplot, xdates-50000., plf[0,*]+untotf200[0,*], psym=1
     multiplot

ploterror, xdates-50000.,tin[0,*],tin[1,*],psym=8, ytitle='Tin',$
/xstyle,/nohat,/ystyle, yr=[min(tin[0,*]-tin[1,*])-0.2,max(tin[0,*]+tin[1,*])+0.2], chars=cs ;

multiplot

ploterror, xdates-50000.,dbflux[0,*]*1D10,dbflux[1,*]*1D10,psym=8, ytitle='DBB',/ylog,$
/xstyle,/nohat,/ystyle, yr=[min(dbflux[0,where(dbflux[0,*] Ne 0.)])/2., max(dbflux[0,*]+dbflux[1,*])*1.2*1D10],chars=cs ;

oplot, xdates-50000., dbb, psym=1

multiplot,/default

ENDIF


END

FUNCTION cutoffpl, X
  return, X^(-0.8)*exp(-X/200)
END
