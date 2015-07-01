pro plotqpocor, inpstr, ps=ps, namef=fname,  nxr=uxr, nyr=uyr, $
li=li, hi=hi, si=si, ni=ni, bw=bw


IF NOT keyword_set(ps) THEN ps=0
If NOT keyword_set(bw) THEN bw=0

IF not keyword_set(uxr) THEN uxr=[0,400]
IF not keyword_set(uyr) THEN uyr=[0,8]

IF NOT keyword_set(fname) THEN BEGIN
   IF bw THEN fname='qpocorbw.eps' ELSE fname='qpocor.eps'
ENDIF

if ps then begin
   set_plot, 'ps'
   device,/color
   device,/encapsulated
IF bw THEN loadct,0 ELSE  loadct,4
   device,decomposed=0
   device, filename = fname
   device, yoffset = 2
   device, ysize = 18.
   device, xsize = 18.
   !p.font=0
   device,/times
endif


IF bw THEN colcode=[195, 155, 95, 15, 10] ELSE colcode=[184,120,40,150, 130];,10,200

FOR i=0, 8 DO BEGIN
   getqpo, inpstr, i, outidx, qpof, rms, type
   states=inpstr[i].states[outidx]
   var1=inpstr[i].efold[*,outidx]
   IF total(var1) EQ 0 THEN print, 'No Efold for '+inpstr[i].name+' '+inpstr[i].year
   IF outidx[0] NE -1 THEN BEGIN
      If (i EQ 0) THEN plotsinglecor, var1, qpof, i, 'Efold (keV)', states, uxr,uyr,colcode,/usecol ELSE plotsinglecor, var1, qpof, i, 'Efold (keV)', states, uxr, uyr,colcode,/usecol, /oplot
   ENDIF ELSE print, 'No QPO for '+inpstr[i].name+' '+inpstr[i].year
ENDFOR

IF ps THEN BEGIN
   device, /close
   set_plot,'x'
ENDIF

END

pro plotsinglecor, var1, var2, idx, xtit, states, nxr, nyr, colcode, usecol=usecol, oplot=oplot, ylog=ylog, xlog=xlog

IF not keyword_set(oplot) THEN oplot=0
IF not keyword_set(usecol) THEN usecol=0
IF not keyword_set(ylog) THEN ylog=0
IF not keyword_set(xlog) THEN xlog=0


cs=1.3
a = findgen(32)*(!pi*2.0/32.0)
thk=1.
symsz=1.1

CASE idx OF
   0: BEGIN
      usersym, 0.90*cos(a), 0.90*sin(a), /fill
      psm=8
   END

   1: BEGIN
      x3=[-1,1,1,-1,-1]
      y3=[-1,-1,1,1,-1]
      usersym, x3, y3, /fill
      psm=8
   END

   2: BEGIN
      x2=[-1,0,1,0,-1]
      y2=[0,1,0,-1,0]
      usersym, x2, y2, /fill
      psm=8
   END

   3: BEGIN
      x4=[-2/sqrt(3),2/sqrt(3),0,-2/sqrt(3)]
      y4=[-2/3.,-2./3.,4/3.,-2/3.]
      usersym, x4, y4, /fill
      psm=8
   END

   4: BEGIN
      x5=[-2/sqrt(3),2/sqrt(3),0,-2/sqrt(3)]*1.3
      y5=[2/3.,2./3.,-4/3.,2/3.]*1.3
      usersym,x5,y5,/fill
      psm=8
   END

   5: BEGIN
      psm=4
      thk=1.5
      symsz=1.3
   END


   6: BEGIN
      psm=5
      thk=1.5
      symsz=1.3
   END


   7: BEGIN
      psm=1
      thk=1.5
      symsz=1.3
   END


   8: psm=2
ENDCASE

IF NOT oplot THEN BEGIN

   plotsym,0,/fill
   ploterror,[0.1,0.1],[0.1,0.1],[0.,0.],[0.,0.],/nodata,psym=3,xtitle=xtit,$
  xrange=nxr,/xstyle,/nohat,/ystyle,yr=nyr, chars=cs, ylog=ylog, xlog=xlog,$
             ytitle='QPO Freq. (Hz)'

ENDIF

FOR si=0,4 DO BEGIN

   aa=where(states eq si)

   IF aa[0] ne -1 THEN BEGIN

      aaok=where(var1[1,aa] GT 0.)
      IF aaok[0] NE -1 THEN oploterror,var1[0,aa[aaok]],var2[0,aa[aaok]],var1[1,aa[aaok]],var2[1,aa[aaok]],psym=psm,/nohat,color=colcode[si],errcol=colcode[si],syms=1.3,thick=1.5

      aaup=where(var1[1,aa] EQ -1)
      IF aaup[0] NE -1 THEN BEGIN
         oploterror,var1[0,aa[aaup]],var2[0,aa[aaup]],var2[1,aa[aaup]],psym=psm,color=colcode[si],syms=1.3,thick=1.5
         FOR ix=0,n_elements(aaup)-1 DO BEGIN
            IF var1[0,aa[aaup[ix]]] GT nxr[0] THEN $
               arrow,var1[0,aa[aaup[ix]]],var2[0,aa[aaup[ix]]], $
                     nxr[0], var2[0,aa[aaup[ix]]],  /data, color=colcode[si]
         ENDFOR
   ENDIF

      aalow=where(var1[1,aa] EQ -2)
      IF aalow[0] NE -1 THEN BEGIN
         oploterror,var1[0,aa[aalow]],var2[0,aa[aalow]],var2[1,aa[aalow]], psym=psm,color=colcode[si],syms=1.3,thick=1.5
         FOR ix=0,n_elements(aalow)-1 DO BEGIN
            IF var1[0,aa[aalow[ix]]] LT nxr[1] THEN $
               arrow,var1[0,aa[aalow[ix]]],var2[0,aa[aalow[ix]]], $
                     nxr[1], var2[0,aa[aalow[ix]]], /data, color=colcode[si]
         ENDFOR
      ENDIF
   ENDIF
ENDFOR


END


