pro plotone_tin, inpstr, inx, pcaonly=pcaonly, usecol=usecol, $
                 yr=nyr, xr=nxr, opl=opl, ps=ps, namef=fname

;This program plots a single outburst tin
;
; INPUTS
;
; inpstr: input structure
; inx: index of input structure
; 
; OPTIONAL INPUTS
;
; pcaonly: IF yes, only pca data is present
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
; plotall_tin.pro
;
; Created by EK, Feb 17, 2015

IF NOT keyword_set(pcaonly) THEN pcaonly=0 ;bolometric correction is default
IF NOT keyword_set(opl) THEN opl=0
IF NOT keyword_set(ps) THEN ps=0
IF NOT keyword_set(usecol) THEN usecol=0
IF (ps AND NOT keyword_set(fname)) THEN fname='tinsingle.eps'

print, 'opl ',opl

IF usecol THEN BEGIN
   loadct,4
   colcode=[150,40,120,184] ; color cordes for HS, HIMS, SIMS, US, SS
ENDIF ELSE BEGIN
   loadct,0
   colcode=[195,155,95,15,0]
ENDELSE

;some required variables

dates=inpstr[inx].xdates
ttrans=inpstr[inx].ttrans
zz=where(dates ne 0.)
time=dates[zz]-ttrans[0]

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

aa=where(states eq 0)
bb=where(states eq 1)
cc=where(states eq 2)
dd=where(states eq 3)
ee=where(states eq 4)


;Need to pass user symbols, maybe not?

a = findgen(32)*(!pi*2.0/32.0)
x1=0.90*cos(a)
y1=0.90*sin(a)
thk=1.
symsz=1.1

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


 tin=inpstr[inx].tin

 IF keyword_set(nyr) THEN yr=nyr ELSE BEGIN
         xx=where((tin[1,*] NE -1) AND (tin[0,*] NE 0.))
         miny=0.9*min(tin[0,xx]-tin[1,xx]) 
         maxy=1.1*max(tin[0,xx]+tin[1,xx])
         yr=[miny,maxy]
      ENDELSE

IF NOT opl THEN BEGIN

     ploterror,time, [0,0],[0,0],psym=3,ytitle='T!Din!N', $ ;x 10!E-10!N',$
        xrange=xr,/xstyle,/ystyle,/ylog,yr=yr, chars=cs*0.8, ytickformat='exponent',/nodata

  ENDIF

   oplot,[0,0],10^(!y.crange)

   IF aa[0] ne -1 THEN BEGIN
;      oploterror,time[aa],tin[0,aa],tin[1,aa],psym=8,color=colcode[0],/nohat,errcol=colcode[0]
      aaok=where(tin[1,aa] NE -1)
      oploterror,time[aa[aaok]],tin[0,aa[aaok]],tin[1,aa[aaok]],psym=psm,color=colcode[0],syms=symsz,thick=thk,/nohat,errcol=colcode[0]
      aaup=where(tin[1,aa] EQ -1)
      IF aaup[0] NE -1 THEN BEGIN
         FOR ix=0,n_elements(aaup)-1 DO BEGIN
            IF ((tin[0,aa[aaup[ix]]] GT yr[0]) AND ((tin[0,aa[aaup[ix]]] LT yr[1])))  THEN arrow,time[aa[aaup[ix]]],tin[0,aa[aaup[ix]]], time[aa[aaup[ix]]], yr[0], /data, color=colcode[0]
            ENDFOR
      ENDIF
   ENDIF

   IF bb[0] ne -1 THEN BEGIN
;      oploterror,time[bb],outpelf[0,bb],outpelf[1,bb],psym=8,color=colcode[1],/nohat,errcol=colcode[1]
      bbok=where(tin[1,bb] NE -1)
      IF bbok[0] NE -1 THEN  oploterror,time[bb[bbok]],tin[0,bb[bbok]],tin[1,bb[bbok]],psym=psm,color=colcode[1],syms=symsz,thick=thk,/nohat,errcol=colcode[1]
      bbup=where(tin[1,bb] EQ -1)
      IF bbup[0] NE -1 THEN BEGIN
         FOR ix=0,n_elements(bbup)-1 DO BEGIN
            IF ((tin[0,bb[bbup[ix]]] GT yr[0]) AND ((tin[0,bb[bbup[ix]]] LT yr[1]))) THEN arrow,time[bb[bbup[ix]]],tin[0,bb[bbup[ix]]], time[bb[bbup[ix]]], yr[0], /data, color=colcode[1]
            ENDFOR
      ENDIF

   ENDIF

   IF cc[0] ne -1 THEN BEGIN
      oploterror,time[cc],tin[0,cc],tin[1,cc],psym=psm,color=colcode[2],syms=symsz,thick=thk,/nohat,errcol=colcode[2]
   ENDIF

   IF dd[0] ne -1 THEN BEGIN
      oploterror,time[dd],tin[0,dd],tin[1,dd],psym=psm,color=colcode[3],syms=symsz,thick=thk,/nohat,errcol=colcode[3]
   ENDIF

   IF ee[0] ne -1 THEN BEGIN
      oploterror,time[ee],tin[0,ee],tin[1,ee],psym=psm,color=colcode[3],syms=symsz,thick=thk,/nohat,errcol=colcode[3]
   ENDIF

END
