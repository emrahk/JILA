pro plotall_elf, inpstr, ps=ps, namef=fname, plotplf=plotplf, plotdbb=plotdbb,$
noerr=noerr,zpit=zpit, nxr=uxr, nyr=uyr,inc1543=inc1543, inc1720=inc1720,$
inc1752=inc1752, notext=notext, onlycert=onlycert, bw=bw

IF NOT keyword_set(ps) THEN ps=0
IF NOT keyword_set(bw) THEN bw=0

IF NOT keyword_set(fname) THEN BEGIN
   IF NOT bw THEN fname='allelf.eps' ELSE fname='allelf_bw.eps'
ENDIF

IF NOT keyword_set(plotplf) THEN plotplf=0
IF NOT keyword_set(plotdbb) THEN plotdbb=0
IF NOT keyword_set(noerr) THEN noerr=0
IF NOT keyword_set(zpit) THEN zpit=0

If NOT keyword_set(inc1743) THEN inc1743=0
IF NOT keyword_set(inc1720) THEN inc1720=0
IF NOT keyword_set(inc1752) THEN inc1752=0

IF NOT keyword_set(notext) THEN notext=0



;device,/decomposed
;loadct,5
;device,/decomposed

if ps then begin
   set_plot, 'ps'   
   device,/color
   device,/encapsulated
IF bw THEN loadct,0 ELSE loadct,4
   device,decomposed=0
   device, filename = fname
   device, yoffset = 2
   device, ysize = 23
   device, xsize = 18
   !p.font=0
   device,/times
endif

IF NOT ps THEN window, 4, xsize=800, ysize=800

a = findgen(32)*(!pi*2.0/32.0)
usersym, 0.90*cos(a), 0.90*sin(a), /fill
usym = 8

;user symbols
; open and close circles, use x1,y1, gx339, 2003
a = findgen(32)*(!pi*2.0/32.0)
x1=0.90*cos(a)
y1=0.90*sin(a)

;psym 4 1550
;psym 5 1752 for now

;colcode
;before timing transition
;before index transition
;before mw transition
;before softening
;after softening

;IF bw THEN colcode=[195, 125, 75, 15] ELSE colcode=[30,10,100,150];,10,200
IF bw THEN colcode=[195, 155, 95, 15] ELSE colcode=[184,120,40,150];,10,200


;colcode=[30,100,170,230]
;convert to eddington

yr=[5e-3,1.1]
nytn=['0.01','0.1','1.']

IF plotplf THEN BEGIN 
   yr=[2e-3,1.1]
   nytn=['0.01','0.1','1.']
ENDIF

;IF plotdbb THEN BEGIN 
; yr=[1e-6,2e-1]
; nytn=['10!E-6!N','10!E-5!N','10!E-4!N','10!E-3!N','10!E-2!N','10!E-1!N']
;ENDIF

IF keyword_set(uyr) THEN yr=uyr

IF zpit THEN xr=[-40,100] ELSE xr=[-20,100]
IF keyword_set(uxr) THEN xr=uxr

ploterror,[-100,-100],[5e-3,5e-3],/ylog,psym=3, $
xtitle='Time (days)',ytitle='ELF',$
xr=xr,yr=yr,/xstyle,/ystyle,ytickname=nytn, charsize=1.4


IF zpit THEN xp=!x.crange[1]*0.60 ELSE xp=!x.crange[1]*0.60
ypw=10^(!y.crange[1])/4.5

IF plotdbb THEN yp=10^(!y.crange[1])/25.
IF plotplf THEN yp=10^(!y.crange[1])/1.2

;color code table

IF zpit THEN xpw=xp ELSE xpw=!x.crange[1]*0.6

IF NOT (notext) THEN BEGIN
  oplot,-0.25*[xpw,xpw]-7.,[ypw,ypw]*3.3,psym=8,col=colcode[0]
  xyouts,-0.25*xpw-5.,ypw*3.1,'SOFT',col=colcode[0],size=1.2

  oplot,-0.25*[xpw,xpw]-7.,[ypw,ypw]*2.7,psym=8,col=colcode[1]
  xyouts,-0.25*xpw-5.,ypw*2.55,'SIMS',col=colcode[1],size=1.2

  oplot,-0.25*[xpw,xpw]-7,[ypw,ypw]*2.2,psym=8,col=colcode[2]
  xyouts,-0.25*xpw-5.,ypw*2.07,'HIMS',col=colcode[2],size=1.2

  oplot,-0.25*[xpw,xpw]-7,[ypw,ypw]*1.8,psym=8,col=colcode[3]
  xyouts,-0.25*xpw-5.,ypw*1.69,'HARD',col=colcode[3],size=1.2
ENDIF

                           ;choose color

;for k=0,60 do oplot, [k-10,k-10]*2,[2e-4,2e-4], psym=8,syms=1.4,color=k
;for k=61,120 do oplot, [k-70,k-70]*2,[1.5e-4,1.5e-4], psym=8,syms=1.4,color=k
;for k=121,180 do oplot, [k-130,k-130]*2.,[1.2e-4,1.2e-4], psym=8,syms=1.4,color=k
;for k=181,240 do oplot, [k-190,k-190]*2.,[1.e-4,1.e-4], psym=8,syms=1.4,color=k

plotone_elf, inpstr, 0, yr=yr, /usecol, $
                 plotplf=plotplf, plotdbb=plotdbb,/opl


IF NOT (notext) THEN BEGIN
  oplot,[xp,xp],[yp,yp], psym=8, col=colcode[2]
  xyouts,xp+2.,yp*0.95,'GX 339-4, 2003'
ENDIF

plotone_elf, inpstr, 1, yr=yr, /usecol, $
                 plotplf=plotplf, plotdbb=plotdbb,/opl


IF NOT (notext) THEN BEGIN
  yp=yp/1.2
  oplot,[xp,xp],[yp,yp], psym=8, col=colcode[2]
  xyouts,xp+2.,yp*0.95,'GX 339-4, 2005'
ENDIF


plotone_elf, inpstr, 2, yr=yr, /usecol, $
                 plotplf=plotplf, plotdbb=plotdbb,/opl


IF NOT (notext) THEN BEGIN
  yp=yp/1.2
  oplot,[xp,xp],[yp,yp], psym=8, col=colcode[2]
  xyouts,xp+2.,yp*0.95,'GX 339-4, 2007'
ENDIF

plotone_elf, inpstr, 3, /pcaonly, yr=yr, /usecol, $
                 plotplf=plotplf, plotdbb=plotdbb,/opl

IF NOT (notext) THEN BEGIN
  yp=yp/1.2
  oplot,[xp,xp],[yp,yp], psym=8, col=colcode[2] 
  xyouts,xp+2.,yp*0.95,'GX 339-4, 2011'
ENDIF

If inc1543 THEN BEGIN
   plotone_elf, inpstr, 4, yr=yr, /usecol, $
                 plotplf=plotplf, plotdbb=plotdbb,/opl


   IF NOT (notext) THEN BEGIN
      yp=yp/1.2
      oplot,[xp,xp],[yp,yp], psym=8, col=colcode[2]
      xyouts,xp+2.,yp*0.93,'4U 1543-47'
   ENDIF
ENDIF

plotone_elf, inpstr, 5, yr=yr, /usecol, $
                 plotplf=plotplf, plotdbb=plotdbb,/opl

IF NOT (notext) THEN BEGIN
  yp=yp/1.25
  oplot,[xp,xp],[yp,yp], psym=4, col=colcode[2], symsize=1.3
  xyouts,xp+2.,yp*0.93,'XTE J1550-564'
ENDIF

IF inc1752 THEN BEGIN
   plotone_elf, inpstr, 6, yr=yr, /usecol, /pcaonly, $
                 plotplf=plotplf, plotdbb=plotdbb,/opl


   IF NOT (notext) THEN BEGIN
      yp=yp/1.28
      oplot,[xp,xp],[yp,yp], psym=5, col=colcode[2], symsize=1.3
      xyouts,xp+2.,yp*0.95,'XTE J1752-223'
   ENDIF

ENDIF


plotone_elf, inpstr, 7, yr=yr, /usecol, $
                 plotplf=plotplf, plotdbb=plotdbb,/opl

IF NOT (notext) THEN BEGIN
  yp=yp/1.28
  oplot,[xp,xp],[yp,yp], psym=1, col=colcode[2], symsize=1.3
  xyouts,xp+2.,yp*0.95,'GRO J1655-40'
ENDIF

IF inc1720 THEN BEGIN
   plotone_elf, inpstr, 8, yr=yr, /usecol, $
                 plotplf=plotplf, plotdbb=plotdbb,/opl

   IF NOT (notext) THEN BEGIN
      yp=yp/1.25
      oplot,[xp,xp],[yp,yp], psym=2, col=colcode[2], symsize=1.3
      xyouts,xp+2.,yp*0.93,'XTE J1720-318'
   ENDIF
ENDIF


IF ps THEN BEGIN
  device,/close
  IF !VERSION.OS eq 'Win32' THEN set_plot,'win' ELSE set_plot,'x'
  ENDIF

end
