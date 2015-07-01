pro merge_strs, strallr

;This program restores individual structures for the rise and place
;them inside a single structure for programs that uses a bunch of
;these sources

;For this prgram to work right, allinfo programs must be run to
;provide strXXXr structures, and they should be saved as strXXXr.dat
;IDL file

; INPUT
;
; NONE
;
; OUTPUTS
;
; strallr : combined structure
;
; USED BY
;
; All IDL programs for plotting
;
; USES
;
; Outputs of allinfoXXXr

; str1655r, str1550r, strgx02r, strgx05r, strgx07r, strgx10r,
; str1543r, str1720r, str1752r
;


restore,'~/RXTE/DATA_AN/JILA/GX339/2002/strgx02r.sav'
restore,'~/RXTE/DATA_AN/JILA/GX339/2005/strgx05r.sav'
restore,'~/RXTE/DATA_AN/JILA/GX339/2007/strgx07r.sav'
restore,'~/RXTE/DATA_AN/JILA/GX339/2010/strgx10r.sav'
restore,'~/RXTE/DATA_AN/JILA/1543/str1543r.sav'
restore,'~/RXTE/DATA_AN/JILA/1550/str1550r.sav'
restore,'~/RXTE/DATA_AN/JILA/1752/str1752r.sav'
restore,'~/RXTE/DATA_AN/JILA/1655/str1655r.sav'
restore,'~/RXTE/DATA_AN/JILA/1720/str1720r.sav'


strallr=[strgx02r,strgx05r,strgx07r,strgx10r, str1543r, $
         str1550r,str1752r,str1655r,str1720r]
END
