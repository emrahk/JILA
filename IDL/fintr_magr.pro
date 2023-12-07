pro fintr_magr, time, mag, itrans, ps=ps, filename=filename, sname=sname, fpar=parf, revc=revc

;This program takes the evolution of indices as a function of time and
;find transition points for the rise

;INPUTS
;time: observation dates in MJD
;mag: infrared magnitudes
;
;OUTPUTS
;
;itrans: transition date 
;
;OPTIONAL INPUTS
;
;ps: filename: postscript output and associated filename
;sname: source name optionally can be placed on top of the graph
;revc: reverse color for overplotted lines
;
;OPTIONAL OUTPUTS
;fpar: fit parameters


IF NOT keyword_set(ps) then ps=0
IF NOT keyword_set(filename) then filename='fitmag.eps'
IF NOT keyword_set(sname) then sname=''
IF keyword_set(revc) THEN linec=0 ELSE linec=255

;rescale time

rtime=time

PLOTSYM,0,1,/FILL
ploterror,rtime,mag[0,*],mag[1,*],xtitle='Relative time (days)',$
          ytitle='mag',psym=8,$
          /xstyle,/ystyle, xr=[min(rtime)-2,max(rtime)+2]

cond1 = 'No' ;Set initial prompt response.

PRINT,'Finding the softening transition' 

WHILE cond1 EQ 'No' do begin

  print,'Please set the range flat part !!!'
  print,'Move the mouse to the startpoint of region and click'
  cursor, x1, y1, /down
  oplot,[x1,x1],!y.crange,line=0,color=linec
  print,'Move the mouse to the endpoint of region and click'
  if !mouse.button ne 4 then cursor, x2, y2, /down
  oplot,[x2,x2],!y.crange,line=0,color=linec
  wait,0.5
  rt=[x1,x2]
  cond1 = DIALOG_MESSAGE('Are you happy with the range you chose?', /Ques)

ENDWHILE

;ENDIF

cond1='No'

WHILE cond1 EQ 'No' do begin

  PRINT,'Please set the range for changing mag' 
  print,'Move the mouse to the startpoint of region and click'
  cursor, x1, y1, /down
  oplot,[x1,x1],!y.crange,line=0,color=linec
  print,'Move the mouse to the endpoint of region and click'
  if !mouse.button ne 4 then cursor, x2, y2, /down
  oplot,[x2,x2],!y.crange,line=0,color=linec
  wait,0.5
  rd=[x1,x2]
  cond1 = DIALOG_MESSAGE('Are you happy with the range you chose?', /Ques)

ENDWHILE


ti=where((rtime gt rt[0]) AND (rtime lt rt[1]))

tt=rtime[ti]
it=mag[*,ti]

;first fit the bottom

bottomfit=linfit(tt,it[0,*],measure_errors=it[1,*],sigma=sigma_bottom) 

;then fit the rise

ti=where((rtime gt rd[0]) AND (rtime lt rd[1]))

td=rtime[ti]
id=mag[*,ti]

rdfit=linfit(td,id[0,*],measure_errors=id[1,*],sigma=sigma_rd) 

xx=findgen(floor(max(rtime)-min(rtime))+1)+min(rtime)

oplot,xx,bottomfit[0]+xx*bottomfit[1],line=2,color=linec
oplot,xx,rdfit[0]+xx*rdfit[1],line=2,color=linec

;get the intersection point

itrans=fltarr(2)
itrans[0]=(bottomfit[0]-rdfit[0])/(rdfit[1]-bottomfit[1])
itrans[1]=abs(((sigma_bottom[0]+sigma_rd[0])/(bottomfit[0]-rdfit[0])))+$
  abs(((sigma_bottom[1]+sigma_rd[1])/(rdfit[1]-bottomfit[1])))

print,itrans

ask=DIALOG_MESSAGE('Is this fit ok? If No, choose by hand', /Ques)

IF ask eq 'No' THEN BEGIN
 ;determine point yourself
  cond1='No'
  WHILE cond1 EQ 'No' do begin

  print,'Please set the range for the turning point'
  print,'Move the mouse to the startpoint of region and click'
  cursor, x1, y1, /down
  oplot,[x1,x1],!y.crange,line=0
  print,'Move the mouse to the endpoint of region and click'
  if !mouse.button ne 4 then cursor, x2, y2, /down
  oplot,[x2,x2],!y.crange,line=0
  wait,0.5
  itl=[x1,x2]
  cond1 = DIALOG_MESSAGE('Are you happy with the range you chose?', /Ques)

  ENDWHILE
 
  itrans[0]=(itl[0]+itl[1])/2.
  itrans[1]=(itl[1]-itl[0])/2.
  
  ENDIF

parf=fltarr(4)
parf[0:1]=rdfit
parf[2:3]=sigma_rd

end

