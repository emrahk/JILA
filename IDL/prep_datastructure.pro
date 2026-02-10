pro prep_datastructure, outstr, name=nname, year=nyear, mass=nmass,$
                        distance=ndistance, inc=ninc, bperiod=nbperiod, $
                        bsep=nbsep, radinfo=nradinfo, oirinfo=noirinfo

; This program creates the generic data structure with all relevant
; information. With optional information, source specific information
; can be provided
;  
;
; INPUTS
;
; NONE
;
; OUTPUTS
;
; outstr: structure containing relevant info
;
; OPTIONAL INPUTS
;
; name: name of source
; year: year of outburst
; mass: mass of source
; distance: distance to source
; inc: inclination angle
; bperiod: binary period
; bsep: binary seperation
; radinfo: radio data info (structure)
; oirinfo: optical/infrare data info
;
; USES
;
; NONE
;
; USED BY
;
; collectalldo
;
; created by Emrah Kalemci, March 2017
;
; LOGS
;
; BSEP and BPERIOD must be float, March 20
;
; fixing totf25200 array definition
;
; itrans2, mwtrans2 added, may break some older codes Feb 2026
;

;set optional parameters

IF NOT KEYWORD_SET(nname) THEN nname=''
IF NOT KEYWORD_SET(nyear) THEN nyear=''
IF NOT KEYWORD_SET(nmass) THEN nmass=[0.,0.]
IF NOT KEYWORD_SET(ndistance) THEN ndistance=[0.,0.]
IF NOT KEYWORD_SET(ninc) THEN ninc=[0.,0.]
IF NOT KEYWORD_SET(nbperiod) THEN nbperiod=0.
IF NOT KEYWORD_SET(nbsep) THEN nbsep=0.

  
;first take care of the timing structure

lors1=create_struct('freq',0.,'fwhm',0.,'norm',0.,'freqerr',0.,'fwhmerr',0.,$
                   'normerr',0.,'peakf',0.,'peakferr',0.,'qval',0.,$
                   'qvalerr',0.,'rmsinf',0.,'rmsinferr',0.,'flag1',0,$
                   'flag2',0,'flag3',0,'flag4',0,'flag5',0)

;freq, fwhm, norm : Lorentzian fit parameters, ...err their errors
;peakf(err) : peak frequency and error
;qval : quality value
;rmsinf(err): 0 to infinity rms
;flag(s) : Tolga dincer flags?

lors=replicate(lors1,6)

tinfo1=create_struct('name','','year','','obsid','','dates',0.,$
                    'lors',lors,'chi',0.,'dof',0,$
                    'totalrmsinf',0.,'totalrmsinferr',0.)

tinfo=replicate(tinfo1,100)

;optical and infrared info, tags clear:

IF keyword_set(noirinfo) THEN oirinfo=noirinfo ELSE BEGIN
   oirinfo=create_struct('name','','year','','dates',fltarr(1000),'mag',fltarr(2,1000),'flux',fltarr(2,1000),'band',strarr(1000),'inst','')
ENDELSE

;radio info, tags clear

IF keyword_set(nradinfo) THEN radinfo=nradinfo ELSE BEGIN
   radinfo=create_struct('name','','year','','dates',fltarr(100),'flux',fltarr(2,100),'freq',fltarr(100),'inst',strarr(100))
ENDELSE

outstr=create_struct('name',nname,'year',nyear, 'mass',nmass,$
       'distance',ndistance,'inc',ninc,'states',replicate(-1,100),$
       'states2', replicate(-1,100),'bperiod',nbperiod,'bsep',nbsep,$
       'obsid',strarr(100), 'xdates',fltarr(100),'tin',fltarr(2,100),$
       'ind',fltarr(2,100), 'tinp',fltarr(2,100),'indp',fltarr(2,100),$
       'eqwp',fltarr(100),'eqwh',fltarr(100),'normp',fltarr(2,100),$
       'dnormp',fltarr(2,100),'dnormph',fltarr(2,100),$
       'pnormp',fltarr(2,100),'pnormph',fltarr(2,100),$
       'ftest',fltarr(2,100),'ecut',fltarr(2,100),'efold',fltarr(2,100),$
       'totf',fltarr(2,100),'untotf',fltarr(100), 'totfp',fltarr(2,100),$
       'untotfp',fltarr(100),'totf200',fltarr(2,100), 'untotf200',fltarr(100),$
       'plfp',fltarr(100), 'plf',fltarr(100), 'dbbp',fltarr(100),$
       'dbb',fltarr(100),'rms',fltarr(2,100),$
       'ttrans',fltarr(2),'itrans1',fltarr(2),'mwtrans1',fltarr(2),$
       'itrans2',fltarr(2),'mwtrans2',fltarr(2)
       'strans',fltarr(2),'hstrans',fltarr(2),'simstrans',fltarr(2),$
       'himstrans',fltarr(2),'mwpeak',fltarr(2),'eddplf',fltarr(3,7),$
       'edddf',fltarr(3,7),'eddtf',fltarr(3,7),'indtr',fltarr(2,7),'vsoft',0,$
       'tinfo',tinfo, 'radinfo',radinfo,'oirinfo',oirinfo, $
       'transflag',intarr(6))

;explanation for some of the tags:

; inc: inclination
; states: Kalemci 2013 states; not valid for rise
; states2: standard states in Kalemci 2016
; bperiod, bsep: binary period and seperation
; XXXp : value of parameter for pca-only fits
; eqw: equivalent width of gaussian
; d(p)normp(h): disk(power-law) normalization p pca only, ph pca+hexte
; ftest: whatever tested (cutoff)
; totf: 3-25 keV flux
; totf200: 25-200 keV flux
; un....: unabsorbed values
; plf, dbbb: power law flux, diskbb flux
; ttrans, itrans1, mwtrans1, strans: dates for transitions described in
;                                  Kalemci2013
; itrans2, mwtrans2: second transitions signifying end of HIMS/SIMS to soft state      
; hstrans: date hardening transition during decay- equivalent to hard state
; simstrans: if present, date of first transition to SIMS 
; himstrans: if present, date of first transition to HIMS
; mwpeak: peak of oir flux, relic from old work
; edd(plf,df,tf): eddington ratio for plf, dbb and total flux with
;                 errors
; indtr: spectral indices during transitions
; vsoft: 1 means spectrum softened at the end of decay
; tinfo: timing structure
; oirinfo, radinfo : structures for oir and radio information.
;
;transflags intarr(6):

;1: distance measured
;2. mass measured
;3. <3 days observing period
;4. hexte
;5. also swift during transitions (to be used later for refining)
;6. Not defined yet

    END


