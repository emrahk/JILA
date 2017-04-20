pro collectalldo, outstr, name, year, indir=dirin, crstr=crstr

; This program automatically collects spectral info from the logs of
; alldo programs
;
; INPUTS
;
; outstr: the structure can be provided as input as well
; name: name of the source
; year: year of the outburst
;
; OUTPUTS
;
; outstr: structure containing relevant info if creates
;
; indir: initial directory to search for files
;
; USES
;
; prep_datastructure
;
; USED BY
;
; allinfoXXXr programs
;
; created by Emrah Kalemci, November 2014
;
;
; LOG
;
; Jan 12
;
; adding a routine to also get diskbb upper limits, EK
; upper limits are written also to the pca+hexte variables 
; 
; FIXING TINFO so that it will merge with the remaining structures
;
; Jan 07 2016
; added indir for the initial directory
;
; Jan 09 2016
;
; copying rms in tinfo so that plotting routines work ok
;
; March 2017
;
; changing to accommodate upper limits
;
; March 2017
;
;   creating structure usint prep_datastructure which has more tags
;   a first attempt to consolidate rise and decay analysis
;
; fixed a bug regarding qval_err
;
; April 2017
;
; Made it optional to create the structure
;
  
; create the structure that will hold relevant info

IF keyword_set(crstr) THEN prep_datastructure, outstr, name=name, year=year

; get observation times from the given directory

IF NOT keyword_set(dirin) THEN dirin='./'
mjdfs=file_search(dirin,'mjdstart.txt',count=nfm)

; read and sort
;

xdatesbs=fltarr(nfm)
obsbs=strarr(nfm)

FOR i=0, nfm-1 DO BEGIN
   
   ;get the obs

   parts=strsplit(mjdfs[i],'/',/extract)
   obsbs[i]=parts[1]

   ;get mjds

   openr,1,mjdfs[i]
   readf,1,mjd
   xdatesbs[i]=mjd
   close,1
ENDFOR

;sort
srt=sort(xdatesbs)
xdates=xdatesbs[srt]
obs=obsbs[srt]
outstr.obsid=obs
outstr.xdates=xdates

;now read pca data and collect relevant info

FOR i=0, nfm-1 DO BEGIN
   ;first check if file exists
   pfile=file_search(dirin+obs[i],'pca_result.dat',count=fc)
   IF fc eq 0 THEN BEGIN
      print, 'NO pca_result.dat found in'+dirin+obs[i]+' ... check!'
   ENDIF ELSE BEGIN
      openr,1,pfile
      str=''

    ; first determine which part of the data to read, check the presence
    ; of ftest
      REPEAT readf,1,str UNTIL strmid(str,0,5) EQ 'Ftest'
      vals=strsplit(str,' ',/extract)
      IF ((float(vals[5]) GT 0.005) OR (EOF(1) EQ 1)) THEN BEGIN
         close,1  ;close the file and go back to start
         openr,1,pfile
         str=''
         print,obs[i],'ftest fail'
      ENDIF ;otherwise continue from that point

      par='ind' ;not to confuse normalizations
      WHILE NOT EOF(1) DO BEGIN
         readf,1,str
         sc=strmid(str,1,1)
         CASE sc OF

            ' ': BEGIN

               vals=strsplit(str,' ',/extract)
               CASE vals[1] OF

                  'PhoIndex': BEGIN
                     indm=float(vals[2])
                     indp=float(vals[3])
                     outstr.indp[0,i]=(indp+indm)/2.
                     outstr.indp[1,i]=(indp-indm)/2.
                     par='ind'
                  END

                  'Tin': BEGIN
                     tinm=float(vals[2])
                     tinp=float(vals[3])
                     outstr.tinp[0,i]=(tinp+tinm)/2.
                     outstr.tinp[1,i]=(tinp-tinm)/2.
                     par='tin'
                  END

                  'norm': BEGIN
                     normm=float(vals[2])
                     normp=float(vals[3])
                     IF par eq 'ind' THEN BEGIN
                        outstr.pnormp[0,i]=(normp+normm)/2.
                        outstr.pnormp[1,i]=(normp-normm)/2.
                     ENDIF
                     IF par eq 'tin' THEN BEGIN
                        outstr.dnormp[0,i]=(normp+normm)/2.
                        outstr.dnormp[1,i]=(normp-normm)/2.
                        par=''
                     ENDIF
                  END

                  ELSE: x=5     ;random statement 
               ENDCASE
            END

            'q': BEGIN
               vals=strsplit(str,' ',/extract)
               outstr.eqwp[i]=float(vals[1])
            END

            '_': BEGIN
               vals=strsplit(str,' ',/extract)
               CASE vals[0] OF
                  '3_25abs': BEGIN
                     flm=float(vals[2])
                     flp=float(vals[3])
                     outstr.totfp[0,i]=1E9*(flp+flm)/2.
                     outstr.totfp[1,i]=1E9*(flp-flm)/2.
                  END
                  '3_25unabs': BEGIN
                     fl=float(vals[1])
                     outstr.untotfp[i]=1E9*fl
                  END
                  '3_25plf': BEGIN
                     fl=float(vals[1])
                     outstr.plfp[i]=1E9*fl
                  END
                  '3_25dbb': BEGIN
                     fl=float(vals[1])
                     outstr.dbbp[i]=1E9*fl
                  END
               ENDCASE
            END

            ELSE: x=5 ;random statement

         ENDCASE

      ENDWHILE

      close,1

   ENDELSE

;Now upper limits

   upfile=file_search(dirin+obs[i],'uplim_result.dat',count=fc)
   rni=0
   IF fc eq 1 THEN BEGIN
      print, 'uplim_result.dat found in '+dirin+obs[i]
      openr,1,upfile
      str=''
      readf,1,str
       WHILE NOT EOF(1) DO BEGIN
         readf,1,str
         vals=strsplit(str,' ',/extract)
         IF ((vals[1] EQ 'norm') AND (rni EQ 0)) THEN rni=1
         IF ((vals[1] EQ 'norm') AND (rni EQ 1)) THEN BEGIN
            outstr.dnormp[0,i]=float(vals[3])
            outstr.dnormp[1,i]=-1
            outstr.dnormph[0,i]=float(vals[3])
            outstr.dnormph[1,i]=-1
            outstr.tinp[0,i]=0.4
            outstr.tinp[1,i]=-1
            outstr.tin[0,i]=0.4
            outstr.tin[1,i]=-1
         ENDIF
      ENDWHILE
       close,1
    ENDIF
                  

ENDFOR

;now read pca+hexte data and collect relevant info

FOR i=0, nfm-1 DO BEGIN
   print,i
   ;first check if file exists
   pfile=file_search(dirin+obs[i],'ph_result.dat',count=fc)
   IF fc eq 0 THEN BEGIN
      print, 'NO ph_result.dat found in '+dirin+' '+obs[i]+' check.!'
   ENDIF ELSE BEGIN
      openr,1,pfile
      str=''

    ; first determine which part of the data to read, check the presence
    ; of ftest
      REPEAT readf,1,str UNTIL strmid(str,0,5) EQ 'Ftest'
      vals=strsplit(str,' ',/extract)
      IF ((float(vals[5]) GT 0.005) OR (EOF(1) EQ 1)) THEN BEGIN
         close,1  ;close the file and go back to start
         openr,1,pfile
         str=''
      ENDIF ELSE BEGIN ;continue from here, record ftest
         ftest=float(vals[5])
         outstr.ftest[1,i]=ftest
                                ;using mpftest from markwardt, but it
                                ;foes not match with xspec ftest
         chi1=double(vals[1])
         chi2=double(vals[3])
         dof1=double(vals[2])
         dof2=double(vals[4])
         F=((chi1-chi2)/(dof1-dof2)) / (chi2/dof2)
         outstr.ftest[0,i]=mpftest(f, dof1-dof2, dof2,/sigma) 
         ENDELSE
         
      par='ind' ;not to confuse normalizations
      WHILE NOT EOF(1) DO BEGIN
         readf,1,str
         vals=strsplit(str,' ',/extract,length=len)
         IF len[0] le 2 THEN BEGIN
               CASE vals[1] OF

                  'PhoIndex': BEGIN
                     indm=float(vals[2])
                     indp=float(vals[3])
                     IF indp NE 0 THEN BEGIN
                        outstr.ind[0,i]=(indp+indm)/2.
                        outstr.ind[1,i]=(indp-indm)/2.
                        par='ind'
                        ENDIF
                  END

                  'Tin': BEGIN
                     tinm=float(vals[2])
                     tinp=float(vals[3])
                     IF tinp NE 0 THEN BEGIN
                        outstr.tin[0,i]=(tinp+tinm)/2.
                        outstr.tin[1,i]=(tinp-tinm)/2.
                        par='tin'
                        ENDIF
                  END

                  'norm': BEGIN
                     normm=float(vals[2])
                     normp=float(vals[3])
                     IF normp NE 0 THEN BEGIN
                        IF par eq 'ind' THEN BEGIN
                           outstr.pnormph[0,i]=(normp+normm)/2.
                           outstr.pnormph[1,i]=(normp-normm)/2.
                        ENDIF 
                        IF par eq 'tin' THEN BEGIN
                           outstr.dnormph[0,i]=(normp+normm)/2.
                           outstr.dnormph[1,i]=(normp-normm)/2.
                           par=''
                        ENDIF
                     ENDIF
                  END

                  'cutoffE': BEGIN
                     cofm=float(vals[2])
                     cofp=float(vals[3])
                     IF cofp NE 0 THEN BEGIN
                        outstr.ecut[0,i]=(cofp+cofm)/2.
                        outstr.ecut[1,i]=(cofp-cofm)/2.
                        ENDIF
                  END

                  'foldE': BEGIN
                     foldm=float(vals[2])
                     foldp=float(vals[3])
                     IF foldp NE 0 THEN BEGIN
                        outstr.efold[0,i]=(foldp+foldm)/2.
                        outstr.efold[1,i]=(foldp-foldm)/2.
                        ENDIF
                  END

                  ELSE: x=5     ;random statement 
               ENDCASE
            ENDIF ELSE BEGIN

               CASE vals[0] OF

                  'eqw:': BEGIN
                     vals=strsplit(str,' ',/extract)
                     outstr.eqwh[i]=float(vals[1])
                  END

                  '3_25abs': BEGIN
                     flm=float(vals[2])
                     flp=float(vals[3])
                     outstr.totf[0,i]=1E9*(flp+flm)/2.
                     outstr.totf[1,i]=1E9*(flp-flm)/2.
                  END

                  '3_25unabs': BEGIN
                     fl=float(vals[1])
                     outstr.untotf[i]=1E9*fl
                  END

                  '3_25plf': BEGIN
                     fl=float(vals[1])
                     outstr.plf[i]=1E9*fl
                  END

                  '3_25dbb': BEGIN
                     fl=float(vals[1])
                     outstr.dbb[i]=1E9*fl
                  END

                  '25_25abs': BEGIN
                     flm=float(vals[2])
                     flp=float(vals[3])
                     outstr.totf200[0,i]=1E9*(flp+flm)/2.
                     outstr.totf200[1,i]=1E9*(flp-flm)/2.
                  END

                  '25_20unabs': BEGIN
                     fl=float(vals[1])
                     outstr.untotf200[i]=1E9*fl
                  END
               
                  ELSE: x=5 ;random statement

               ENDCASE

            ENDELSE

      ENDWHILE

      close,1

   ENDELSE

  ;now get the timing info
  rfile=file_search(dirin+obs[i]+'/an/','result.sav')
   IF (rfile eq '') THEN print,'no results in '+obs[i]+', skipping...' ELSE BEGIN
   inow=i
   restore,rfile
   i=inow
   print,i,rfile
   outstr.tinfo[i].obsid=obs[i]
   outstr.tinfo[i].dates=xdates[i]
   outstr.tinfo[i].chi=chi
   outstr.tinfo[i].dof=dof
   outstr.tinfo[i].totalrmsinf=sqrt(rinf)*100.
   IF rinfer EQ -1 THEN outstr.tinfo[i].totalrmsinferr=-1 ELSE outstr.tinfo[i].totalrmsinferr=rinfer/sqrt(rinf)*100.
   outstr.rms[0,i]=outstr.tinfo[i].totalrmsinf
   outstr.rms[1,i]= outstr.tinfo[i].totalrmsinferr
   inds=indgen(nlor)*3
   IF rinfer NE -1 THEN BEGIN
      outstr.tinfo[i].lors[0:nlor-1].freq=r[inds+2]
      outstr.tinfo[i].lors[0:nlor-1].fwhm=r[inds+1]
      outstr.tinfo[i].lors[0:nlor-1].norm=r[inds]
      outstr.tinfo[i].lors[0:nlor-1].freqerr=perror[inds+2]
      outstr.tinfo[i].lors[0:nlor-1].fwhmerr=perror[inds+1]
      outstr.tinfo[i].lors[0:nlor-1].normerr=perror[inds]
      outstr.tinfo[i].lors[0:nlor-1].qval=r[inds+2]/r[inds+1]
      outstr.tinfo[i].lors[0:nlor-1].qvalerr=$
         outstr.tinfo[i].lors[0:nlor-1].qval*((perror[inds+2]/r[inds+2])+$
                 (perror[inds+1]/r[inds+1]))/sqrt(2.)     
      FOR j=0,n_elements(r)-1,3 DO BEGIN
         calrms,r[j:j+2],perror[j:j+2],rms0tw,rms0inf,/silent
         outstr.tinfo[i].lors[j/3].rmsinf=rms0inf[0]*100.
         outstr.tinfo[i].lors[j/3].rmsinferr=rms0inf[1]*100.
         peak, r[j+2],perror[j+2],r[j+1],perror[j+1], fp, fpe
         outstr.tinfo[i].lors[j/3].peakf=fp
         outstr.tinfo[i].lors[j/3].peakferr=fpe      
      ENDFOR
   ENDIF
  ENDELSE
ENDFOR
 
END

