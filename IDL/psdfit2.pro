pro psdfit2, obsid, binp=pbin, noupdate=noupdate, manual=manual, nulor=lorn

; this program attempts to automatically fit power spectra given the
; previous power spectrum in result.sav
;
; INPUTS
;
; obsid : observation id to determine correct directory for power
;         spectrum
;
; OUTPUTS
;
; NONE (result.sav if required)
;
; OPTIONAL INPUTS
;
; noupdate: if set, do not update result.sav, default= update
; manual: if set, do not use result.sav and use the parameters defined
;         in the program for lorentzian (useful for first time fits, 
;         major corrections
; binp: binning parameter, default=1.08 if manual is chosen. If manual
;       is not chosen, it uses the binning parameter from the previous run
;       otherwise specifies
;
; nlor: number of lorentzians
;
; USED BY
;
; alldo (shell program)
;
; USES
;
; MPFIT
;
; created by Emrah Kalemci, November 2014

;PRO PSDFIT
;Global variables to be used in error calculation
Common qpoerr, ff,pf,pfe,r,chi,parinfo,expr

IF NOT keyword_set(manual) THEN manual=0
IF NOT keyword_set(noupdate) THEN noupdate=0
IF NOT keyword_set(pbin) THEN BEGIN
   pbinx=1.08
   pbin=0
   ENDIF ELSE pbinx=pbin
IF NOT keyword_set(lorn) THEN lorn=3

obspl=strsplit(obsid,'-',/extract) ; 

save,obsid,obspl,pbin,lorn,filename='restidl.sav' ;save these so that they are not overwritten


; IF results.sav is not provided we need to provide some initial
; parameters

;;;;;;;;;;;;;;;;;;;;;;;;;;
;model information
;CHOOSE THE MODEL
lormodel='f_lor(x,P[0:2])+f_lor(x,P[3:5])+f_lor(x,P[6:8])+f_lor(x,P[9:11])+f_lor(x,P[12:14])+f_lor(x,P[15:17])'
nlor=lorn

parinfo = replicate({fixed:0, limited:[1,0], limits:[0.0,0.0]}, 3*nlor)

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;


IF nlor EQ 1 THEN BEGIN
expr=STRMID(lormodel, 0, 15) 
s=[0.056,7.552,1.863]
ENDIF

IF nlor EQ 2 THEN BEGIN
expr=STRMID(lormodel, 0, 31)
s=[0.1,0.2,0.05,$
   0.14,4.6,.1]
ENDIF


IF nlor EQ 3 THEN BEGIN
expr=STRMID(lormodel, 0, 47)
s=[0.02,1.9,0.4,$
   0.003,1.5,3.2,$
   0.007,1.0,5.81]; initial parameters for 3 lorentzians
ENDIF

IF nlor EQ 4 THEN BEGIN
expr=STRMID(lormodel, 0, 64) 
s=[0.015,2.,0.2,$
   0.03,1.56,3.,$
   0.007,11.3,9.5,$
   0.007,1.1,5.81]
ENDIF

IF nlor EQ 5 THEN BEGIN
expr=STRMID(lormodel, 0, 82) 
s=[0.01,0.6,0.25,$
   0.0035,0.6,1.12,$
   0.044,9.8,0.5,$
   0.0046,0.11,1.67,$
   0.0031,.33,3.333]
ENDIF

IF nlor EQ 6 THEN BEGIN
expr=STRMID(lormodel, 0, 100) 
s=[0.0009,0.0035,0.0053,$
   0.0005,2.2720,9.1870,$
   0.0034,24.7943,10.7729,$
   0.002,3.8840,1.3219,$
   0.0012,125.792,194.60,$
   0.0001,1.37,5.628]
ENDIF


IF NOT manual THEN BEGIN
   print, 'restoring prev results'
   restore,'result.sav'         ;this overrides previous settings
   s=r
   restore,'restidl.sav'        ;not to override current parameters
   IF keyword_set(pbin) THEN pbinx=pbin
ENDIF


base1='../'+obspl[2]+'.'+obspl[3]+'t.all/'
;base1='/Users/emka1155/RXTE/DATA_AN/JILA/GX339/2007/XRAY/'
;base2='92052_02.05/02.05t.all/'
base2='light/fourier/high/onelength/'
xdrfu_r1,base1+base2+'0065536_signormpsd_corr.xdrfu',f128s,p128s
xdrfu_r1,base1+base2+'0065536_errnormpsd_corr.xdrfu',f128s,p128se

;Binning 

; tricky part is to determine which index to use, always use the last index

sz=size(p128se)

fy=f128s
pye=p128se(*,sz[2]-1)
py=p128s(*,sz[2]-1)
REBIN_GEOT,pbinx,fy,py,pye ; rebinning


ff=fy;[0:106]
pf=py;[0:106]
pfe=pye;[0:106]


;Plot to show the data.
window,0,xsize=700,ysize=700,retain=2

PLOTERROR,ff,pf,pfe,/xlog,/ylog,psym=10,/nohat,xrange=[min(ff),max(ff)],/xstyle,position=[0.15,0.4,0.98,0.98],xtitle='Frequency',yrange=[min(pf(where(pf gt 0.))),max(pf)*2.],/ystyle


;;;;;;;;;;;;;;;;;;;;;;;;;;


pow=0
FOR i=0, 3*nlor-1, 3 DO BEGIN
    OPLOT,ff,f_lor(ff,s[i:i+2]),thick=2,linestyle=2
    pow=pow+f_lor(ff,s[i:i+2])
ENDFOR
OPLOT,ff,pow,thick=3
pow=0

WHILE (1) DO BEGIN
    PRINT,'Which parameter you want to change? or 100 to exit'
    PRINT,'Parameter #:',0,3*nlor-1
    Print,'Par0: Amplitude'
    Print,'Par1: FWHM'
    Print,'Par2: Resonance Frequency'
    
    READ,ind
    ca=ind
    IF ca eq 100 THEN BREAK 
    Print,s(ind)
    READ,var,fixit
    s(ind)=var
    parinfo[ind].fixed=fixit
    
    PLOTERROR,ff,pf,pfe,/xlog,/ylog,psym=10,/nohat,xrange=[min(ff),max(ff)],/xstyle,yrange=[min(pf(where(pf gt 0.))),max(pf)*2.]
    FOR i=0, 3*nlor-1, 3 DO BEGIN
        OPLOT,ff,f_lor(ff,s[i:i+2]),thick=2,linestyle=2
    pow=pow+f_lor(ff,s[i:i+2])
    ENDFOR 

    OPLOT,ff,pow,thick=3
    pow=0
ENDWHILE


r = MPFITEXPR(expr,ff,pf,pfe,s,dof=dof,perror=perror,/quiet,bestnorm=chi,parinfo=parinfo)

For i=0,n_elements(r)/3-1 DO BEGIN
;vpeak=r[3*i+2]*sqrt(r[3*i+1]/(2.*r[3*i+2])+1.)
;vpeakerr=(sqrt(r[3*i+1]/(2.*r[3*i+2])+1.)+(1./sqrt(r[3*i+1]/(2.*r[3*i+2])+1.))*(r[3*i+1]/r[3*i+2])^2./4.*perror[3*i+2])
Print,FORMAT='("Lorentzian ", I0)', i+1
Print,FORMAT='(16HNorm.(err)     : , F10.4, F10.4)', r[3*i+0],perror[3*i+0]
Print,FORMAT='(16HFWHM (err)     : , F10.4, F10.4)', r[3*i+1],perror[3*i+1]
Print,FORMAT='(16HRes.Freq.(err) : , F10.4, F10.4)', r[3*i+2],perror[3*i+2]
;Print,FORMAT='(16HPeak Freq.(err): , F10.4, F10.4)', vpeak,vpeakerr

ENDFOR

;FOR j=0,n_elements(r)-1,3 DO BEGIN
;calrms,r[j:j+2],perror[j:j+2],rms0tw,rms0inf
;print, 'frac_rms (1, 0 to inf)=',rms0inf(0),rms0inf(1)
;print, 'frac_rms (1, 0 to 20)=',rms0tw(0),rms0tw(1)
;ENDFOR


!p.multi=[0,1,2]
PLOTERROR,ff,pf*ff,pfe*ff,/xlog,/ylog,psym=10,/nohat,xrange=[min(ff),max(ff)],/xstyle,position=[0.15,0.4,0.98,0.98],xtickname=REPLICATE(' ', 6),yrange=[1e-5,max(pf*ff)*2.],/ystyle
FOR i=0, 3*nlor-1, 3 DO BEGIN
        OPLOT,ff,f_lor(ff,r[i:i+2])*ff,thick=2,linestyle=2
        pow=pow+f_lor(ff,r[i:i+2])
    ENDFOR
OPLOT,ff,pow*ff,thick=2
Plot,ff,((pf-pow)/pfe)^2.*ff,/xlog,xrange=[min(ff),max(ff)],/xstyle,psym=10,position=[0.15,0.1,0.98,0.4],xtitle='Frequency';,yrange=[1e-7,1e-1]
!p.multi=0
XYOUTS,575,650,'chi/dof:',/device
XYOUTS,595,650,chi/dof,/device



;Calculate rms
rinf=0.
rtw=0.
rtwer=0.
rinfer=0.

FOR j=0,n_elements(r)-1,3 DO BEGIN
calrms,r[j:j+2],perror[j:j+2],rms0tw,rms0inf
print, 'frac_rms (1, 0 to inf)=',rms0inf(0),rms0inf(1)
print, 'frac_rms (1, 0 to 20)=',rms0tw(0),rms0tw(1)

rtw=rtw+(rms0tw(0))^2.
rtwer=rtwer+rms0tw(0)*rms0tw(1)

rinf=rinf+(rms0inf(0))^2.
rinfer=rinfer+rms0inf(0)*rms0inf(1)

ENDFOR
Print,'Total Frac. RMS(0-20):',sqrt(rtw)*100.,rtwer/sqrt(rtw)*100.
Print,'Total Frac. RMS(0-inf):',sqrt(rinf)*100.,rinfer/sqrt(rinf)*100.

IF NOT noupdate THEN Save,/variables,filename='result.sav'
END
