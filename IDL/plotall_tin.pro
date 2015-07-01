pro plotall_tin, inpstr, ps=ps, namef=fname, $
noerr=noerr,zpit=zpit, nxr=uxr, nyr=uyr, inc1543=inc1543, inc1752=inc1752,$
inc1720=inc1720, bw=bw

IF NOT keyword_set(ps) THEN ps=0
If NOT keyword_set(bw) THEN bw=0

IF NOT keyword_set(fname) THEN BEGIN
   IF bw THEN fname='risetin_bw.eps' ELSE fname='risetin.eps'
ENDIF

IF NOT keyword_set(zpit) THEN zpit=0
IF NOT keyword_set(inc1543) THEN inc1543=0
IF NOT keyword_set(inc1720) THEN inc1720=0
IF NOT keyword_set(inc1752) THEN inc1752=0

;device,/decomposed
;loadct,5
;device,/decomposed

if ps then begin
   set_plot, 'ps'   
   device,/color
   device,/encapsulated
IF bw THEN loadct,0 ELSE  loadct,4
   device,decomposed=0
   device, filename = fname
   device, yoffset = 2
   device, ysize = 23.
   device, xsize = 18.
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

IF bw THEN colcode=[195, 155, 95, 15] ELSE colcode=[184,120,40,150];,10,200


;colcode=[30,100,170,230]
;convert to eddington

yr=[0.3,1.8]
cs=1.3

IF keyword_set(uyr) THEN yr=uyr

IF zpit THEN xr=[-40,100] ELSE xr=[-50,50]
IF keyword_set(uxr) THEN xr=uxr

ploterror,[-40,0],[0,0],psym=3, xtitle='Time (days)',$
ytitle='T!Din!N (keV)',xr=xr,yr=yr,/xstyle,/ystyle, chars=cs


xp=!x.crange[0]*0.8
yp=!y.crange[1]*0.96

;color code table


plotone_tin, inpstr, 0, yr=yr, /usecol,/opl


yp=yp-0.05
oplot,[xp,xp],[yp,yp], psym=8, col=colcode[2]
xyouts,xp+2.,yp-0.01,'GX 339-4, 2002'

plotone_tin, inpstr, 1, yr=yr, /usecol,/opl

yp=yp-0.05
oplot,[xp,xp],[yp,yp], psym=8, col=colcode[2]
xyouts,xp+2.,yp-0.01,'GX 339-4, 2005'

plotone_tin, inpstr, 2, yr=yr, /usecol,/opl

yp=yp-0.05
oplot,[xp,xp],[yp,yp], psym=8, col=colcode[2]
xyouts,xp+2.,yp-0.01,'GX 339-4, 2007'

plotone_tin, inpstr, 3, yr=yr, /usecol,/opl,/pcaonly

yp=yp-0.05
oplot,[xp,xp],[yp,yp], psym=8, col=colcode[2]
xyouts,xp+2.,yp-0.01,'GX 339-4, 2011'

plotone_tin, inpstr, 4, yr=yr, /usecol,/opl

yp=yp-0.05
oplot,[xp,xp],[yp,yp], psym=8, col=colcode[2]
xyouts,xp+2.,yp-0.01,'4U 1543-47'

plotone_tin, inpstr, 5, yr=yr, /usecol,/opl


yp=yp-0.05
oplot,[xp,xp],[yp,yp], psym=4, col=colcode[2], symsize=1.3
xyouts,xp+2.,yp-0.01,'XTE J1550-564'

IF inc1752 THEN BEGIN

   plotone_tin, inpstr, 6, yr=yr, /usecol,/opl,/pcaonly

   yp=yp-0.05
   oplot,[xp,xp],[yp,yp], psym=5, col=colcode[2], symsize=1.3
   xyouts,xp+2.,yp-0.01,'XTE J1752-223'

ENDIF

plotone_tin, inpstr, 7, yr=yr, /usecol,/opl

yp=yp-0.05
oplot,[xp,xp],[yp,yp], psym=1, col=colcode[2], symsize=1.3
xyouts,xp+2.,yp-0.01,'GRO J1655-40'


IF inc1720 THEN BEGIN

   plotone_tin, inpstr, 8, yr=yr, /usecol,/opl

   yp=yp-0.05
   oplot,[xp,xp],[yp,yp], psym=2, col=colcode[2], symsize=1.3
   xyouts,xp+2.,yp-0.01,'XTE J1720-330'

ENDIF
 

IF ps THEN BEGIN
  device,/close
  IF !VERSION.OS eq 'Win32' THEN set_plot,'win' ELSE set_plot,'x'
  ENDIF

end
