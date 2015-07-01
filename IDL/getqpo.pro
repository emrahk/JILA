pro getqpo, inpstr, inpidx, outidx, qpof, rms, type, qcut=cutq

;This program takes inpstr as input, and for the outburst denoted by
;inpidx, find QPOs and outputs the indices of outbursts with QPOs,
;their frequencies, rms and type

;
;INPUTS
;
; inpstr: input structure with all outbursts
; inpidx: index of the outburst
;
;OPTIONAL INPUT
;
; qcut: cut off for the Q value, default 3
;
;OUTPUTS
; 
; outidx: indices of observations with QPOs, -1 if none found
; qpof: Frequency and the error of the main QPO
; rms: rms of the main QPO
; type: A, B, C 
;
; LOGS
;
; Created May 2015, EK
;

tmstr=inpstr[inpidx].tinfo
states=inpstr[inpidx].states
nobs=n_elements(where(states NE -1))

IF NOT keyword_set(cutq) THEN cutq=3. ;Q value > 3 for QPO

outidx=-1
qpof1=fltarr(2,nobs)
rms1=fltarr(2,nobs)
type='0'
ni=0

FOR i=0, nobs-1 DO BEGIN
   lors=tmstr[i].lors
   xx=where(lors.qval GE cutq)
   IF xx[0] NE -1 THEN BEGIN
      qpo=1
      IF n_elements(xx) EQ 1 THEN ilx=0 ELSE BEGIN
         srtrms=sort(lors[xx].rmsinf)
         ilx=srtrms[n_elements(sort)-1]
         print, i, ilx
      ENDELSE
      qpof1(*,ni)=[lors[xx[ilx]].freq,lors[xx[ilx]].freqerr]
      rms1(*,ni)=[lors[xx[ilx]].rmsinf,lors[xx[ilx]].rmsinferr]
      ni=ni+1
      IF states[i] eq 2 THEN type=[type,'B'] ELSE type=[type,'C']
   ENDIF ELSE qpo=0
   IF qpo THEN outidx=[outidx,i]
ENDFOR

IF n_elements(outidx) EQ 1 THEN BEGIN
   PRINT, 'No QPO found' 
   qpof=0
   rms=0
ENDIF ELSE BEGIN
   outidx=outidx[1:n_elements(outidx)-1]
   type=type[1:n_elements(type)-1]
   qpof=qpof1[*,0:ni-1]
   rms=rms1[*,0:ni-1]
ENDELSE

END



