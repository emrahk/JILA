pro stretch_fit, inpstr, ps=ps,namef=fname, names=sname, usecol=usecol, trnir=nirtr

  ;This program takes the evolution of indices as a function of time and
;find transition points for the rise

;INPUTS
;inpstr: input structure with all OIR data
;
;OUTPUTS
;
;itrans: transition times
;
;plot showing stretched and fitted NIR visible light curves
;
;OPTIONAL INPUTS
;
;ps: filename: postscript output and associated filename
;sname: source name optionally can be placed on top of the graph
;trnir: transition times, if given, overplotted
;
;OPTIONAL OUTPUTS
;fpar: fit parameters

IF NOT keyword_set(ps) then ps=0
IF NOT keyword_set(fname) then fname='stretched.eps'
IF NOT keyword_set(sname) then sname=''
IF NOT keyword_set(usecol) THEN usecol=0

device,decomposed=0

IF usecol THEN BEGIN
   loadct,4
   colcode=[0, 184,120,60,205];
ENDIF ELSE BEGIN
   loadct,0
   colcode=[0, 195,155,95,15]
ENDELSE


;determine bands

allbands=['H','J','I','V']          ;extend this list if necessary later

;start with the first band

allh=where(inpstr.oirinfo.band eq 'H' AND inpstr.oirinfo.mag[0,*] NE 0. $
           AND (inpstr.oirinfo.dates GT inpstr.ttrans[0]-30.) AND $
           (inpstr.oirinfo.dates LT inpstr.ttrans[0]+30.))
dateh=inpstr.oirinfo.dates[allh]
magh=inpstr.oirinfo.mag[*,allh]

PLOTSYM,0,1,/FILL
ploterror,dateh,magh[0,*],magh[1,*],xtitle='Time (days)',$
          ytitle='del mag',psym=8,$
          /xstyle,/ystyle, xr=[min(dateh)-2,max(dateh)+2]

cond1 = 'No' ;Set initial prompt response.

PRINT,'Setting initial region to bolster lc'

WHILE cond1 EQ 'No' do begin

  print,'Please set the range initial flat part !!!'
  print,'Move the mouse to the startpoint of region and click'
  cursor, x1, y1, /down
  oplot,[x1,x1],!y.crange,line=0,color=0
  print,'Move the mouse to the endpoint of region and click'
  if !mouse.button ne 4 then cursor, x2, y2, /down
  oplot,[x2,x2],!y.crange,line=0,color=0
  wait,0.5
  rt1=[x1,x2]
  cond1 = DIALOG_MESSAGE('Are you happy with the range you chose?', /Ques)

ENDWHILE

cond1='No'

WHILE cond1 EQ 'No' do begin

  PRINT,'Please set the range for final flat part'
  print,'Move the mouse to the startpoint of region and click'
  cursor, x1, y1, /down
  oplot,[x1,x1],!y.crange,line=0,color=0
  print,'Move the mouse to the endpoint of region and click'
  if !mouse.button ne 4 then cursor, x2, y2, /down
  oplot,[x2,x2],!y.crange,line=0,color=0
  wait,0.5
  rt2=[x1,x2]
  cond1 = DIALOG_MESSAGE('Are you happy with the range you chose?', /Ques)

ENDWHILE

;obtain flat averagesg

lo=where((dateh GE rt1[0]) AND (dateh LE rt1[1]))
loh=avg(magh[0,lo])

hi=where((dateh GE rt2[0]) AND (dateh LE rt2[1]))
hih=avg(magh[0,hi])

tr=max(lo)+1+indgen(min(hi)-max(lo)-1)

;replot after rescaling time
rtime=dateh-dateh[min(lo)]


if ps then begin
   set_plot, 'ps'
   device,/color
;   loadct,5
   device,/encapsulated
   device, filename = fname
   device, yoffset = 2
   device, ysize = 23.5
   ;device, xsize = 12.0
   !p.font=0
   device,/times
endif

ploterror,rtime[lo],magh[0,lo]-loh,magh[1,lo],xtitle='Rel. Time (days)',$
          ytitle='Delta mag',psym=8,$
          /xstyle,/ystyle, xr=[0,dateh[max(hi)]+2-dateh[min(lo)]],$
          yr=[-0.2,max(magh[0,hi])-loh+0.5],color=colcode[0]

oploterror, rtime[tr], magh[0,tr]-loh, magh[1,tr], psym=8, color=colcode[0]
oploterror, rtime[hi], magh[0,hi]-loh, magh[1,hi], psym=8, color=colcode[0]

oplot, !x.crange[1]*0.8*[1.,1.], !y.crange[1]*0.4*[1.,1.],color=colcode[0], $
       psym=8
xyouts,!x.crange[1]*0.82, !y.crange[1]*0.39, 'H', color=colcode[0], size=1.2

;now other bands

FOR i=1, N_ELEMENTS(allbands)-1 DO BEGIN
   allbi=where((inpstr.oirinfo.band eq allbands[i]) AND (inpstr.oirinfo.mag[0,*] NE 0.),nmag)

   IF nmag NE 0 THEN BEGIN
      datebi=inpstr.oirinfo.dates[allbi]
      magbi=inpstr.oirinfo.mag[*,allbi]

   ;match times
      rtime=datebi-dateh[min(lo)]


      loi=where((datebi GE rt1[0]) AND (datebi LE rt1[1]))
      lobi=avg(magbi[0,loi])

      hii=where((datebi GE rt2[0]) AND (datebi LE rt2[1]))
      hibi=avg(magbi[0,hii])

      IF min(hii)-max(loi) GT 1. THEN trbi=max(loi)+1+indgen(min(hii)-max(loi)-1)

   ;equalize flat bottoms and stretch

      rat=(hih-loh)/(hibi-lobi)
   
      oploterror, rtime[loi], magbi[0,loi]-lobi, magbi[1,loi], psym=8, color=colcode[i]
      oploterror, rtime[hii], (magbi[0,hii]-lobi)*rat, magbi[1,hii], psym=8, color=colcode[i]
      IF min(hii)-max(loi) GT 1. THEN oploterror, rtime[trbi], (magbi[0,trbi]-lobi)*rat, magbi[1,trbi], psym=8, color=colcode[i]
   

      oplot, !x.crange[1]*0.8*[1.,1.], !y.crange[1]*0.38-(i*0.2)*[1.,1.],$
          color=colcode[i], psym=8
      xyouts,!x.crange[1]*0.82, !y.crange[1]*0.37-(i*0.2), allbands[i], color=colcode[i], size=1.2
   ENDIF
   
ENDFOR

IF keyword_set(nirtr) THEN BEGIN
   oplot,nirtr[0,0]*[1.,1.]-dateh[min(lo)],!y.crange,color=colcode[0]
   oplot,nirtr[1,0]*[1.,1.]-dateh[min(lo)],!y.crange,color=colcode[0]
ENDIF


IF ps THEN BEGIN
  device,/close
  IF !VERSION.OS eq 'Win32' THEN set_plot,'win' ELSE set_plot,'x'
  ENDIF

END



   
