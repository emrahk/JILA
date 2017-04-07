pro wrap_sttemp, iostr, crstr=crstr

;This is a wrapper that would be present for all sources in all
;outbursts that one can change states, and recalculated transition
;luminosities if necessary
;
; INPUTS
;
; iopstr: input structure (if created before)
;
; OUTPUTS
;
; iostr: output structure as well
;
; OPTIONAL INPUTS
;
; crstr: if set, run prep_datastructure to create the structure
;(writes over any given input structure)
;
; USES
;
; prep_datastructure
;
; USED BY
;
; NONE
;
; LOGS
;
;


;information on source (an example is provided after ;)

nname=''         ;'4U1543-47'
nyear=''         ;'2002'
nmass=[0.,0.]    ;[9.4,1.]
ndistance=[0.,0] ;[7.5,0.5]
ninc=[0.,0.]     ;[20.7,1.5]
nbperiod=0.      ;26.8
nbsep=0.         ;23. 
;radinfo (if present, can be added here)
;oirinfo (if present, can be added here)

IF keyword_set(crstr) THEN prep_datastructure, iostr, name=nname, $
   year=nyear, mass=nmass, distance=ndistance, inc=ninc, bperiod=nbperiod, $
                        bsep=nbsep


collectalldo, iostr, nname, nyear

;remember state definitions
;
; Kalemci 2013
;
; 0: Bef Timing, 1:Af Timing-Bef Index 2:Af index-Bef MW, 3:after MW
; 0: hard, 1: hims 2:sims 3:us 4:ss
;

;CHANGE state information for each source!

iostr.states[0:5]=0
iostr.states[6:10]=1
iostr.states[11:13]=2
iostr.states[14:31]=3

iostr.states2[0:5]=4
iostr.states2[6:10]=1
iostr.states2[11:13]=1
iostr.states2[14:31]=0

;add softening info if present TO BE IMPLEMENTED LATER IF NECESSARY.

iostr.vsoft=0                  ;1 if present


;IF a PCA ONLY outburst, you MUST add ,/pcaonly keyword to
;calctranslum

calctranslum, iostr, /dcor, /pcor

;stop

END

