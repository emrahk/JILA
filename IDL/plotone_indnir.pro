pro plotone_indnir, inpstr, ntrnsall, inx, usecol=usecol, pcaonly=pcaonly, $
                 yr=nyr, xr=nxr, opl=opl, ps=ps, namef=fname

;This program plots a single outburst plf, dbb or total elf
;
; INPUTS
;
; inpstr: input structure
; inx: index of input structure
; ntrnsall: dates for nir transition
; 
; OPTIONAL INPUTS
;
; pcaonly: If set only PCA data is available
; yr: IF set, new yrange in plot
; xr: IF set, new xrange in plot
; opl: IF set, only overplot
; ps: IF set, postscript output
; namef: IF set, name of the output ps file
; usecol: IF set, use color
;
; USES
;
; NONE
;
; USED BY
; 
; plotall_ind.pro
;
; Created by EK, Feb, 2024

IF NOT keyword_set(pcaonly) THEN pcaonly=0
IF NOT keyword_set(opl) THEN opl=0
IF NOT keyword_set(ps) THEN ps=0
IF NOT keyword_set(usecol) THEN usecol=0
IF (ps AND NOT keyword_set(fname)) THEN fname='indsinglenir.eps'

IF usecol THEN BEGIN
   loadct,4
   colcode=[150,40,120,184] ; color codes for BEFTR, DURING, AFTER NIR TR
ENDIF ELSE BEGIN
   loadct,0
   colcode=[195,155,95,15,0]
ENDELSE

;some required variables

dates=inpstr[inx].xdates
ttrans=inpstr[inx].ttrans
zz=where(dates ne 0.)
time=dates[zz]-ttrans[0]
nirstart=ntrnsall[inx,0,0]-ttrans[0]
nirend=ntrnsall[inx,1,0]-ttrans[0]

IF NOT opl THEN BEGIN
   device,decomposed=0
   IF ps THEN BEGIN
      set_plot, 'ps'
      device,/color
;   loadct,5
      device,/encapsulated
      device, filename = fname
      device, yoffset = 2
      device, ysize = 28.
                                ;device, xsize = 12.0
      !p.font=0
      device,/times
   ENDIF ELSE window, 4, retain=2, xsize=800, ysize=800
   cs=1.2
   IF keyword_set(nxr) THEN xr=nxr ELSE xr=[min(time)-2.,max(time)+2.]
ENDIF


states=inpstr[inx].states[where(inpstr[inx].states NE -1)]

; PLF and DBB, here we need to do bolometric corrections and plot ELFs

aa=where(time LT nirstart)
bb=where((time GE nirstart) And (time LE nirend))
cc=where(time GT nirend)


IF pcaonly THEN ind=inpstr[inx].indp ELSE ind=inpstr[inx].ind

;Need to pass user symbols, maybe not?

a = findgen(32)*(!pi*2.0/32.0)
x1=0.90*cos(a)
y1=0.90*sin(a)
thk=1.
symsz=1.5

CASE inx OF 
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
      symsz=1.5
   END


   6: BEGIN
      psm=5
      thk=1.5
      symsz=1.5
   END


   7: BEGIN
      psm=1
      thk=1.5
      symsz=1.5
   END


   8: psm=2
ENDCASE

   IF keyword_set(nyr) THEN yr=nyr ELSE yr=[0.9*min(ind[0,*]),1.1*max(ind[0,*])]

IF NOT opl THEN BEGIN

   ploterror, time,[100,100],[0,0],psym=3, ytitle='!9G !X',$
xrange=xrr,/xstyle,/nohat,/ystyle, yr=yr, color=0, chars=cs ; dummy frame

ENDIF

oplot,[0,0],10^(!y.crange)

IF aa[0] ne -1 THEN oploterror,time[aa],ind[0,aa],ind[1,aa],psym=psm,$
/nohat,color=colcode[0], errcol=colcode[0],syms=symsz,thick=thk

IF bb[0] NE -1 THEN oploterror,time[bb],ind[0,bb],ind[1,bb],psym=psm,$
/nohat,color=colcode[1],errcol=colcode[1],syms=symsz,thick=thk

IF cc[0] NE -1 THEN oploterror,time[cc],ind[0,cc],ind[1,cc],psym=psm,$
/nohat,color=colcode[2],errcol=colcode[2],syms=symsz,thick=thk

END
