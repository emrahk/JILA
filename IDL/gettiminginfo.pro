pro gettiminginfo, obs, name, year, dates, tinfo

;This is a program to automatically read and record all timing info
;nicely into a structure. Based on Tolga Dincer's programs.

;created by EK August 2014

;INPUTS
;
; obs: directories that hold the spectro-timing info
; name: name of the source
; year: year of the outburst
; dates: dates of observation in MJD for self consistent structure
;
;OUTPUTS
;
; tinfo: structure that carries all the timing info
;
;OPTIONS
;
; NONE
;
; USES
;
; calrms.pro
;
;USED BY
;
; allinfoXXX
;
; last updates 28 October 2014


lors1=create_struct('freq',0.,'fwhm',0.,'norm',0.,'freqerr',0.,'fwhmerr',0.,$
                   'normerr',0.,'peakf',0.,'peakferr',0.,'qval',0.,$
                   'qvalerr',0.,'rmsinf',0.,'rmsinferr',0.,'flag1',0,$
                   'flag2',0,'flag3',0,'flag4',0,'flag5',0)

lors=replicate(lors1,6)

tinfo1=create_struct('name',name,'year',year,'obsid',obs[0],'dates',dates[0],$
                    'lors',lors,'chi',0.,'dof',0,$
                    'totalrmsinf',0.,'totalrmsinferr',0.)

;tinfo=replicate(tinfo1,n_elements(obs))
tinfo=replicate(tinfo1,100)
;find result.sav for each directory

print,'This program should be run at the directory XRAY resides'

dirroot='XRAY/'
FOR k=0,n_elements(obs)-1 DO BEGIN
   rfile=file_search(dirroot+obs[k]+'/an/','result.sav')
   IF (rfile eq '') THEN print,'no results in '+obs[k]+', skipping...' ELSE BEGIN
   restore,rfile
   tinfo[k].obsid=obs[k]
   tinfo[k].dates=dates[k]
   tinfo[k].chi=chi
   tinfo[k].dof=dof
   tinfo[k].totalrmsinf=sqrt(rinf)*100.
   tinfo[k].totalrmsinferr=rinfer/sqrt(rinf)*100.
   inds=indgen(nlor)*3
   tinfo[k].lors[0:nlor-1].freq=r[inds+2]
   tinfo[k].lors[0:nlor-1].fwhm=r[inds+1]
   tinfo[k].lors[0:nlor-1].norm=r[inds]
   tinfo[k].lors[0:nlor-1].freqerr=perror[inds+2]
   tinfo[k].lors[0:nlor-1].fwhmerr=perror[inds+1]
   tinfo[k].lors[0:nlor-1].normerr=perror[inds]
   tinfo[k].lors[0:nlor-1].qval=r[inds+2]/r[inds+1]
   tinfo[k].lors[0:nlor-1].qvalerr=tinfo[k].lors[0:nlor-1].qval*$
                                   ((perror[inds+2]/r[inds+2])+$
                 (perror[inds+1]/r[inds+1]))/sqrt(2.)     
   FOR j=0,n_elements(r)-1,3 DO BEGIN
      calrms,r[j:j+2],perror[j:j+2],rms0tw,rms0inf,/silent
      tinfo[k].lors[j/3].rmsinf=rms0inf[0]*100.
      tinfo[k].lors[j/3].rmsinferr=rms0inf[1]*100.
      peak, r[j+2],perror[j+2],r[j+1],perror[j+1], fp, fpe
      tinfo[k].lors[j/3].peakf=fp
      tinfo[k].lors[j/3].peakferr=fpe      
   ENDFOR
   ENDELSE
ENDFOR

END
