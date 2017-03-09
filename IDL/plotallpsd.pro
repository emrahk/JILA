PRO plotallpsd, inpstr, ix, fname=namef, chind=indch, inpdir=dirinp, ps=ps

;This program plots all power spectra to see state transitions better
;
; INPUTS
;
; inpstr: input structure that holds timing tag
; ix: index of input structure to get the correct source and outburst
;
; OUTPUTS
;
; NONE 
;
;
; OPTIONAL INPUTS
;
; fname: name of postscript file
; chind: indices of the structure that will be plotted
; inpdir: directory to look for obsids
; ps: IF set plot postscript
;
; created by EK, Jan 6
;
;
;

IF NOT KEYWORD_SET(namef) THEN namef='allpsd.eps'
IF NOT KEYWORD_SET(dirinp) THEN dirinp='./'
IF NOT KEYWORD_SET(ps) THEN ps=0

IF ps THEN BEGIN
   SET_PLOT, 'PS'
   DEVICE, FILENAME = namef,/COLOR,BITS_PER_PIXEL=8,/ENCAP,/landscape
;    DEVICE, XOFFSET =-300
;    DEVICE, YSIZE = 29.7
;    DEVICE, XSIZE = 21
   !P.FONT=0.
   DEVICE,/TIMES
   LOADCT,12
ENDIF
   
IF KEYWORD_SET(indch) THEN BEGIN
   obsid=inpstr[ix].obsid[indch]
   time=inpstr[ix].xdates[indch]
ENDIF ELSE BEGIN
    obsid=inpstr[ix].obsid[indch]
    time=inpstr[ix].xdates[indch]
ENDELSE

xx=where(time NE 0.)
obs=obsid[xx]
time=time[xx]

nobs=N_ELEMENTS(obs)
ncol=ceil(nobs/4.) ; number of columns

!P.MULTI=[0,4,ncol]

xr=[0.003,256]
yr=[2e-5,1e-1]
colind=[0,0] ; coloring scheme to be fixed later
yse=0.94/ncol

FOR stu=0,nobs-1 DO BEGIN

  col=colind[1]
  IF stu LE 2 THEN col=colind[0]  

  xind=((stu) mod 4)
  yind=floor((stu)/4) 
  print,stu,xind,yind

  IF yind EQ ncol-1 THEN BEGIN
    xtitl='Frequency (Hz)'
    xtickn=['0.01','0.1','1','10','100']
 ENDIF ELSE BEGIN
    xtitl=''
    xtickn=replicate(' ',5)
 ENDELSE

 IF xind eq 0 THEN BEGIN
    ytitl='PSD'
    ytickn=['10!E-4!N','10!E-3!N','10!E-2!N','10!E-1!N']
 ENDIF ELSE BEGIN
    ytitl=''
    ytickn=replicate(' ',4)
 ENDELSE

  result=file_search(dirinp+obs[stu]+'/an/','result.sav')
  IF result NE '' THEN BEGIN
     restore,dirinp+obs[stu]+'/an/result.sav'

     nlor=n_elements(r)/3
     PLOTERROR,ff,pf*ff,pfe*ff,/xlog,/ylog,$
position=[0.120+(xind*0.220),(0.99-yse)-(yind*yse),$
0.340+(xind*0.220),0.990-(yind*yse)],xr=xr,yr=yr,/xstyle,$
xtickname=xtickn,xtitle=xtitl,ytickname=ytickn,ytitle=ytitl,$
charsize=1.3,/ystyle,/nohat,col=col, errcol=col

;OPLOTERROR,ff,pf*ff,pfe*ff,psym=10,/nohat

     IF rinfer NE -1 THEN BEGIN
        pow=0.
        FOR i=0, 3*nlor-1, 3 DO BEGIN
           OPLOT,ff,f_lor(ff,r[i:i+2])*ff,thick=2,linestyle=2
           pow=pow+f_lor(ff,r[i:i+2])
        ENDFOR
        OPLOT,ff,pow*ff,thick=2
     ENDIF
  ENDIF
  
XYOUTS,0.004,4e-2,'day:'+strtrim(string(time[stu]),1),charsize=0.7
;XYOUTS,0.004,2e-2,'hmag:'+strtrim(string(hmags[stu-8]),1),charsize=0.7
;IF stu eq 6 THEN XYOUTS,0.12, 4e-2, 'SOFTENING STARTS', charsize=0.7

ENDFOR

IF ps THEN BEGIN
   DEVICE,/CLOSE
   SET_PLOT,'X'
ENDIF

!P.MULTI=0

END
