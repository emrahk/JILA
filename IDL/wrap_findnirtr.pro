pro wrap_findnirtr, inpstr, nirtrans
  
;This program is a wrapper around findtr_magr to find transition times
;during the decay and the rise

;INPUTS
;
; inpstr: input structure with the oirinfo
;
; OUTPUTS
;
; nirtrans: transition times in MJD at start and end with errors
;
; OPTIONAL INPUTS
;
; NONE
;
; LOGS

; Find bottom transition
; Use H band and do not extend more than 50 days around the transition


  xx=where((inpstr.oirinfo.band eq 'H') and (inpstr.oirinfo.mag[0,*] GT 0) $
           and (inpstr.oirinfo.dates GT inpstr.ttrans[0]-50.) and $
           (inpstr.oirinfo.dates LT inpstr.ttrans[0]+50.))

  nirtrans=fltarr(2,2)
  
  print, 'Choose values for start'
  fintr_magr, inpstr.oirinfo.dates[xx],inpstr.oirinfo.mag[*,xx], irtranss
  nirtrans[0,*]=irtranss
  print, 'Choose values for end'
  fintr_magr, inpstr.oirinfo.dates[xx],inpstr.oirinfo.mag[*,xx], irtranse
  nirtrans[1,*]=irtranse

END  
  
