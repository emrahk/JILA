pro collect_eqpair, topdir, outstr, stind=indst, name=sname, year=syear, $
    hims=hims, ss=ss, us3=us3, us4=us4, update=update

; This program automatically collects eqpair 
; fit results in the hard state
;
; INPUTS
;
; topdir: directory to search for output files
;
; OUTPUTS
;
; outstr: structure containing relevant info
;
; OPTIONAL INPUTS
;
; stind: start index
; name: name of the source
; year: year of the outburst
; hims: get data from hims fits
; ss: get data from ss fits
; us3: get data from ultrasoft, single absorption
; us4: get data from ultrasoft, double absorption
; update: if set, update the given structure, do not recreate
;
; USES
;
; NONE
;
; USED BY
;
;
; created by Emrah Kalemci, March 2015
;
;
; LOGS
;
; modified to use new fits with corrected optical depths
; modified to use with the new hims fits
;
; defaults

IF NOT keyword_set(sname) THEN sname='GRO J1655-40'
IF NOT keyword_set(syear) THEN syear='2005'
IF NOT keyword_set(hims) THEN hims=0
IF NOT keyword_set(ss) THEN ss=0
IF NOT keyword_set(us3) THEN us3=0
IF NOT keyword_set(us4) THEN us4=0
IF NOT keyword_set(indst) THEN indst=0
IF NOT keyword_set(update) THEN update=0

; create the structure that will hold relevant info

; some relevant info
; ..wr: with reflection and xi free
; ..th: thermal comptonization only

IF update THEN outeqp=outstr ELSE $
   outeqp=create_struct('xdates',fltarr(100),'obsid',strarr(100),$
      'state',replicate(-1,100),$
      'tin', fltarr(2,100), 'tinth', fltarr(2,100), $
      'normtin',fltarr(2,100), 'normtinth', fltarr(2,100),$         
      'hsrat',fltarr(2,100), 'hsratth',fltarr(2,100), $
      'nthrat', fltarr(2,100),'ftest', fltarr(2,100), $
      'tau', fltarr(2,100), 'tauth', fltarr(2,100), $
      'ginj', fltarr(2,100), 'ginjth', fltarr(2,100), $
      'ref',fltarr(2,100), 'refth',fltarr(2,100), $
      'xi', fltarr(2, 100), 'xith', fltarr(2, 100), $
      'normeqp', fltarr(2,100), 'normeqpth', fltarr(2,100),$
      'factor',fltarr(2,100), 'factorth', fltarr(2, 100),$
      'chidof', fltarr(2,100), 'chidofth', fltarr(2,100),$
      'eqw', fltarr(100), 'eqwth', fltarr(100), $
      'gsigma',fltarr(2,100),'gsigmath',fltarr(2,100),$
      'gnorm',fltarr(2,100),'gnormth',fltarr(2,100),$
      'edge',fltarr(2,100), 'smtau', fltarr(2,100),$
      'edgeth',fltarr(2,100), 'smtauth', fltarr(2,100),$
      'eqwa1',fltarr(100), 'eqwa1th',fltarr(100),$
      'eqwa2',fltarr(100), 'eqwa2th',fltarr(100),$
      'agsigma1',fltarr(2,100),'agsigma1th',fltarr(2,100),$
      'agsigma2',fltarr(2,100),'agsigma2th',fltarr(2,100),$
      'agnorm1',fltarr(2,100),'agnorm1th',fltarr(2,100),$
      'agnorm2',fltarr(2,100),'agnorm2th',fltarr(2,100), $
      'pnorm',fltarr(2,100),'pnormth',fltarr(2,100),$
      'index',fltarr(2,100),'indexth',fltarr(2,100),$
      'totf',fltarr(100),'untotf',fltarr(100),$
      'totf200',fltarr(100),'untotf200',fltarr(100),$
      'eqp',fltarr(100), 'eqp200',fltarr(100), $
      'dbb',fltarr(100), 'fref',fltarr(100), 'fref200',fltarr(100))

; read and sort
;

; times

mjdfs=file_search(topdir,'mjdstart.txt',count=nfm)
xdatesbs=fltarr(nfm)
obsbs=strarr(nfm)

; To be able to run the script from any directory I need to obtain
; the position of obsid in the string
parts_td=strsplit(topdir,'/',/extract)
parts=strsplit(mjdfs[0],'/',/extract)
mscr=where(parts EQ parts_td[n_elements(parts_td)-1])+1

FOR i=0, nfm-1 DO BEGIN
   
   ;get the obs

   parts=strsplit(mjdfs[i],'/',/extract)
   obsbs[i]=parts[mscr]
   print,i,obsbs[i]
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
outeqp.obsid[indst:indst+nfm-1]=obs
outeqp.xdates[indst:indst+nfm-1]=xdates

;now read pca data and collect relevant info

;start with compps

outeqp.state[indst:indst+nfm-1]=0
filetoch=['ph_eqprxi.dat','ph_eqp2.dat']

IF ss THEN BEGIN
   filetoch=['ph_eqprxiss.dat','ph_eqp2ss.dat']
   outeqp.state[indst:indst+nfm-1]=2
ENDIF

IF hims THEN BEGIN
   filetoch1=['ph_eqprxismfi.dat','ph_eqprxismfipt.dat']
   filetoch=['ph_eqprxihims2.dat','ph_eq2hims2.xcm']
   outeqp.state[indst:indst+nfm-1]=1
   outeqp.ginj[0,indst:indst+nfm-1]=2.
ENDIF

IF us3 THEN filetoch=['ph_eqprxius3bn.dat','ph_eqp2us3bn.dat']
IF us4 THEN filetoch=['ph_eqprxius4bn.dat','ph_eqp2us4bn.dat']

IF (us3 or us4) THEN outeqp.state[indst:indst+nfm-1]=3

print, hims, filetoch, indst

FOR k=0, 1 DO BEGIN ; with and without reflection

   FOR i=indst, indst+nfm-1 DO BEGIN ;for all items in the output file
   
      ;first check if file exists
      pfile=file_search(topdir+'/'+obs[i-indst],filetoch[k],count=fc)
      IF fc eq 0 THEN BEGIN
         print, 'NO '+filetoch[k]+' found in XRAY/'+obs[i-indst]+' ... check!'
      ENDIF ELSE BEGIN
         openr,1,pfile
         str=''

         ; first determine which part of the data to read, check the presence
         ; of ftest
         IF ((k EQ 0) AND (NOT (us3 OR us4 OR hims))) THEN BEGIN
            REPEAT readf,1,str UNTIL strmid(str,0,5) EQ 'Ftest'
            vals=strsplit(str,' ',/extract)
                                ; record ftest
            ftest=float(vals[5])
            outeqp.ftest[1,i]=ftest
                                ;using mpftest from markwardt, but it
                                ;foes not match with xspec ftest
            chi1=double(vals[1])
            chi2=double(vals[3])
            dof1=double(vals[2])
            dof2=double(vals[4])
            F=((chi1-chi2)/(dof1-dof2)) / (chi2/dof2)
            outeqp.ftest[0,i]=mpftest(f, dof1-dof2, dof2,/sigma) 
	    readf,1,str ;to skip model
         ENDIF ELSE BEGIN
            readf,1,str
            readf,1,str
         ENDELSE
            
         par='tin'              ;not to confuse normalizations
         stp=0
     
         WHILE NOT EOF(1) DO BEGIN
            readf,1,str
            vals=strsplit(str,' ',/extract,length=len)
            IF ((len[0] le 2) AND (stp eq 0)) THEN BEGIN
               CASE vals[1] OF

	        'edgeE': BEGIN
                     edgem=float(vals[2])
                     edgep=float(vals[3])
                     evalpar, edgem, edgep, nedge
                     IF k EQ 0 THEN outeqp.edge[*,i]=nedge ELSE $
                                    outeqp.edgeth[*,i]=nedge
                  END

	        'MaxTau': BEGIN
                     mtaum=float(vals[2])
                     mtaup=float(vals[3])
                     evalpar, mtaum, mtaup, nmtau
                     IF k EQ 0 THEN outeqp.smtau[*,i]=nmtau ELSE $
                                    outeqp.smtauth[*,i]=nmtau
                  END


                  'Tin': BEGIN
                     tinm=float(vals[2])
                     tinp=float(vals[3])
                     evalpar, tinm, tinp, ntin
                     IF k EQ 0 THEN outeqp.tin[*,i]=ntin ELSE $
                                    outeqp.tinth[*,i]=ntin
                     par='tin'
                  END

                  'norm': BEGIN
                     normm=float(vals[2])
                     normp=float(vals[3])
                     evalpar, normm, normp, nnorm
                     IF k EQ 0 THEN BEGIN
		     	CASE par OF                    
			     'tin': outeqp.normtin[*,i]=nnorm
                             'eqp': outeqp.normeqp[*,i]=nnorm
                             'gauss': outeqp.gnorm[*,i]=nnorm
			     'agauss1': outeqp.agnorm1[*,i]=nnorm
			     'agauss2': outeqp.agnorm2[*,i]=nnorm
			     'power': outeqp.pnorm[*,i]=nnorm
			     ENDCASE			
                     ENDIF ELSE BEGIN
		     	CASE par OF 
                             'tin': outeqp.normtinth[*,i]=nnorm
                             'eqp': outeqp.normeqpth[*,i]=nnorm
                             'gauss': outeqp.gnormth[*,i]=nnorm
			     'agauss1': outeqp.agnorm1th[*,i]=nnorm
			     'agauss2': outeqp.agnorm2th[*,i]=nnorm
			     'power': outeqp.pnormth[*,i]=nnorm
			     ENDCASE
                     ENDELSE

                  END

                  'l_h/l_s': BEGIN
                     hsratm=float(vals[2])
                     hsratp=float(vals[3])
                     evalpar, hsratm, hsratp, nhsrat
                     IF k eq 0 THEN outeqp.hsrat[*,i]=nhsrat ELSE $ 
                                    outeqp.hsratth[*,i]=nhsrat
                  END

                'l_nt/l_h': BEGIN 
                     nthratm=float(vals[2])
                     nthratp=float(vals[3])
                     evalpar, nthratm, nthratp, nnthrat
                     IF k eq 0 THEN outeqp.nthrat[*,i]=nnthrat
                  END

                'tau_p': BEGIN
                     par='eqp'
                     taum=float(vals[2])
                     taup=float(vals[3])
                     evalpar, taum, taup, ntau
                     IF k eq 0 THEN outeqp.tau[*,i]=ntau ELSE $
                                    outeqp.tauth[*,i]=ntau
                    
                  END

		  'G_inj': BEGIN
                     ginjm=float(vals[2])
                     ginjp=float(vals[3])
                     evalpar, ginjm, ginjp, nginj
                     IF k eq 0 THEN outeqp.ginj[*,i]=nginj ELSE $
                                    outeqp.ginjth[*,i]=nginj
                    
                  END

                 'Refl': BEGIN
                     refm=float(vals[2])
                     refp=float(vals[3])
                     evalpar, refm, refp, nref
		     IF k eq 0 THEN outeqp.ref[*,i]=nref ELSE $
                                    outeqp.refth[*,i]=nref
                  END

                'xi': BEGIN
                     xim=float(vals[2])
                     xip=float(vals[3])
                     evalpar, xim, xip, nxi
		     IF k eq 0 THEN outeqp.xi[*,i]=nxi ELSE $
                                    outeqp.xith[*,i]=nxi
                  END


               'sigma': BEGIN
                     IF (hs or hims or ss) THEN par='gauss' ELSE BEGIN
                        IF par eq 'agauss1' THEN par='agauss2' $
                                            ELSE par='agauss1'
                     ENDELSE
                     sigm=float(vals[2])
                     sigp=float(vals[3])
                     evalpar, sigm, sigp, nsig
                     IF k eq 0 THEN outeqp.gsigma[*,i]=nsig ELSE $
                                    outeqp.gsigmath[*,i]=nsig
                  END

                'PhoIndex': BEGIN
                     par='power'
                     indm=float(vals[2])
                     indp=float(vals[3])
                     evalpar, indm, indp, nind
                     IF k EQ 0 THEN outeqp.index[*,i]=nind ELSE $
                                    outeqp.indexth[*,i]=nind 
                  END

                'factor': BEGIN
                     stp=1
                     facm=float(vals[2])
                     facp=float(vals[3])
                     nfac=[(facp+facm)/2.,(facp-facm)/2.]
                     IF k EQ 0 THEN outeqp.factor[*,i]=nfac ELSE $
                                    outeqp.factorth[*,i]=nfac 
                  END

                  ELSE: x=5     ;random statement 
               ENDCASE
            ENDIF ELSE BEGIN

               CASE vals[0] OF

                  'eqw:': BEGIN
                     vals=strsplit(str,' ',/extract)
                     IF k eq 0 THEN outeqp.eqw[i]=float(vals[1]) ELSE $
                        outeqp.eqwth[i]=float(vals[1])
                  END      

                  'aeqw1:': BEGIN
                     vals=strsplit(str,' ',/extract)
                     IF k eq 0 THEN outeqp.eqwa1[i]=float(vals[1]) ELSE $
                        outeqp.eqwa1th[i]=float(vals[1])
                  END      

                  'aeqw2:': BEGIN
                     vals=strsplit(str,' ',/extract)
                     IF k eq 0 THEN outeqp.eqwa2[i]=float(vals[1]) ELSE $
                        outeqp.eqwa2th[i]=float(vals[1])
                  END          
               
                  'CHI2/DOF:': BEGIN
                     vals=strsplit(str,' ',/extract)
                     IF k eq 0 THEN BEGIN
                        outeqp.chidof[0,i]=float(vals[1])
                        outeqp.chidof[1,i]=float(vals[2])
                     ENDIF ELSE BEGIN
                         outeqp.chidofth[0,i]=float(vals[1])
                         outeqp.chidofth[1,i]=float(vals[2])
                         ENDELSE
                  END

                  '3_25abs': BEGIN
                     vals=strsplit(str,' ',/extract)
                     IF k eq 0 THEN outeqp.totf[i]=float(vals[1])
                  END

                  '25_200abs': BEGIN
                     vals=strsplit(str,' ',/extract)
                     IF k eq 0 THEN outeqp.totf200[i]=float(vals[1])
                  END

                  '3_25unabs': BEGIN
                     vals=strsplit(str,' ',/extract)
                     IF k eq 0 THEN outeqp.untotf[i]=float(vals[1])
                  END

                 '25_200unabs': BEGIN
                     vals=strsplit(str,' ',/extract)
                     IF k eq 0 THEN outeqp.untotf200[i]=float(vals[1])
                  END

                  '3_25dbb': BEGIN
                     vals=strsplit(str,' ',/extract)
                     IF k eq 0 THEN outeqp.dbb[i]=float(vals[1])
                  END

                  '3_25eqp': BEGIN
                     vals=strsplit(str,' ',/extract)
                     IF k eq 0 THEN outeqp.eqp[i]=float(vals[1])
                  END

                  '25_200eqp': BEGIN
                     vals=strsplit(str,' ',/extract)
                     IF k eq 0 THEN outeqp.eqp200[i]=float(vals[1])
                  END

                  '3_25ref': BEGIN
                     vals=strsplit(str,' ',/extract)
                     IF k eq 0 THEN outeqp.fref[i]=float(vals[1])
                  END

                  '25_200ref': BEGIN
                     vals=strsplit(str,' ',/extract)
                     IF k eq 0 THEN outeqp.fref200[i]=float(vals[1])
                  END

                  ELSE: x=5 ;random statement

               ENDCASE

            ENDELSE

         ENDWHILE


      close,1

   ENDELSE

 ENDFOR

ENDFOR


outstr=outeqp

END

pro evalpar, parm, parp, npar

IF ((parm EQ 0) AND (parp EQ 0)) THEN npar=[0,-3] ;unconstrained
IF ((parm EQ 0) AND (parp NE 0)) THEN npar=[parp,-1]
IF ((parp EQ 0) AND (parm NE 0)) THEN npar=[parm,-2]
IF ((parp NE 0) AND (parm NE 0)) THEN $
                        npar=[(parp+parm)/2.,(parp-parm)/2.]

END
