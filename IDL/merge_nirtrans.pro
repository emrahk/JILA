pro merge_nirtrans, ntrnsall

;This program restores individual structures for the nir transitions
;them inside a single structure for programs that uses a bunch of
;these sources

;For this prgram to work right, wrap_findnirtr programs must be run to
;provide nir structures, and they should be saved as ...sav
;IDL file

; INPUT
;
; NONE
;
; OUTPUTS
;
; ntransallr : combined structure
;
; USED BY
;
; All IDL programs for plotting
;
; USES
;
; Outputs of wrap_findnirtr
;

ntrnsall=fltarr(9,2,2)
restore,'~/RXTE/DATA_AN/JILA/GX339/2002/nirgx02tr.sav'
ntrnsall[0,*,*]=nirgx02tr
restore,'~/RXTE/DATA_AN/JILA/GX339/2005/nirgx05tr.sav'
ntrnsall[1,*,*]=nirgx05tr
restore,'~/RXTE/DATA_AN/JILA/GX339/2007/nirgx07tr.sav'
ntrnsall[2,*,*]=nirgx07tr
restore,'~/RXTE/DATA_AN/JILA/GX339/2010/nirgx10tr.sav'
ntrnsall[3,*,*]=nirgx10tr
restore,'~/RXTE/DATA_AN/JILA/1543/str1543r.sav'
;no rise tr
restore,'~/RXTE/DATA_AN/JILA/1550/nir1550tr.sav'
ntrnsall[5,*,*]=nir1550tr

END
