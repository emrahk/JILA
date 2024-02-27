pro plotall_indnir, inpstr, ntrnsall, ps=ps, namef=fname, $
noerr=noerr,zpit=zpit, nxr=uxr, nyr=uyr, bw=bw

;This program plots photon indices of all outburst
;
; INPUTS
;
; inpstr: input structure
; 
; OPTIONAL INPUTS
;
; nyr: IF set, new yrange in plot
; nxr: IF set, new xrange in plot
; ps: IF set, postscript output
; namef: IF set, name of the output ps file
; plotplf: IF set, only plot plfs
; plotdbb: IF set, only plot dbbs
; noerr: if set, do not plot errors
; zpit: zero point index transition (possibly obsolote)
; inc1543: IF set, plot 1543 values
; inc1720: IF set, plot 1720 values
; inc1752: IF set, plot 1752 values
; notext: IF set, do not write any text on plot window
; bw: if set, plot black and white 
;
; USES
;
; NONE
;
; USED BY
; 
; NONE
;
; Created by EK, Feb 2015
;
; LOGS
;
; added header, fixing labels, July 24, 2015
; 
;


IF NOT keyword_set(ps) THEN ps=0
If NOT keyword_set(bw) THEN bw=0

IF NOT keyword_set(fname) THEN BEGIN
   IF bw THEN fname='indallnr_bw.eps' ELSE fname='indallnr.eps'
ENDIF

IF NOT keyword_set(zpit) THEN zpit=0

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


yr=[1.3,4.2]
cs=1.3

IF keyword_set(uyr) THEN yr=uyr

IF zpit THEN xr=[-40,100] ELSE xr=[-50,50]
IF keyword_set(uxr) THEN xr=uxr

ploterror,[-40,0],[0,0],psym=3, xtitle='Time (days)',$
ytitle='Photon index (!9G!X)',xr=xr,yr=yr,/xstyle,/ystyle, chars=cs


xp=!x.crange[0]*0.8
yp=!y.crange[1]*0.98

;color code table


plotone_indnir, inpstr,  ntrnsall, 0, yr=yr, /usecol,/opl


txtsz=1.2

yp=yp-0.1
oplot,[xp,xp],[yp,yp], psym=8, col=colcode[2], syms=1.5
xyouts,xp+1.,yp-0.01,'GX 339-4, 2002', size=txtsz, col=colcode[2]

plotone_indnir, inpstr, ntrnsall, 1, yr=yr, /usecol,/opl

yp=yp-0.1
oplot,[xp,xp],[yp,yp], psym=8, col=colcode[2], syms=1.5
xyouts,xp+1.,yp-0.01,'GX 339-4, 2005', size=txtsz, col=colcode[2]

plotone_indnir, inpstr, ntrnsall, 2, yr=yr, /usecol,/opl

yp=yp-0.1
oplot,[xp,xp],[yp,yp], psym=8, col=colcode[2], syms=1.5
xyouts,xp+1.,yp-0.01,'GX 339-4, 2007', size=txtsz, col=colcode[2]

plotone_indnir, inpstr, ntrnsall, 3, yr=yr, /usecol,/opl,/pcaonly

yp=yp-0.1
oplot,[xp,xp],[yp,yp], psym=8, col=colcode[2], syms=1.5
xyouts,xp+1.,yp-0.01,'GX 339-4, 2011', size=txtsz, col=colcode[2]

plotone_indnir, inpstr, ntrnsall, 5, yr=yr, /usecol,/opl


yp=yp-0.1
oplot,[xp,xp],[yp,yp], psym=4, col=colcode[2], symsize=1.3
xyouts,xp+1.,yp-0.01,'XTE J1550-564', size=txtsz, col=colcode[2]


IF ps THEN BEGIN
  device,/close
  IF !VERSION.OS eq 'Win32' THEN set_plot,'win' ELSE set_plot,'x'
  ENDIF

end
