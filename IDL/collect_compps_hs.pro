pro collect_compps_hs, topdir, outstr, name=sname, year=syear

; This program automatically collects compps 
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
; name: name of the source
; year: year of the outburst
;
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

; defaults

IF NOT keyword_set(sname) THEN sname='GRO J1655-40'
IF NOT keyword_set(syear) THEN syear='2005'

; create the structure that will hold relevant info

; some relevant info
; ..wr: with reflection and xi free

outcompps=create_struct('xdates',fltarr(100),'obsid',strarr(100),$
                        'tin', fltarr(2,100), 'tinwr', fltarr(2,100),$
                        'normtin',fltarr(2,100), 'normtinwr', fltarr(2,100),$
                        'kTe', fltarr(2,100), 'kTewr', fltarr(2,100),$
                        'kTbb', fltarr(2,100), 'kTbbwr', fltarr(2,100),$
                        'tau', fltarr(2,100), 'tauwr', fltarr(2,100),$
                        'normcps', fltarr(2,100), 'normcpswr', fltarr(2,100),$
                        'factor',fltarr(2,100), 'factorwr', fltarr(2, 100),$
                        'ref',fltarr(2,100), 'xi', fltarr(2, 100),$
                        'ftest',fltarr(2,100), 'chidof', fltarr(2,100), $
                        'chidofwr', fltarr(2,100),$
                        'eqw', fltarr(100), 'eqwwr', fltarr(100), $
                        'gsigma',fltarr(2,100),'gsigmawr',fltarr(2,100),$
                        'gnorm',fltarr(2,100),'gnormwr',fltarr(2,100))


; some relevant info
; ..fi: injection index fixed to 3.

outeqp=create_struct('xdates',fltarr(100),'obsid',strarr(100),$
                     'tin', fltarr(2,100), 'tinfi', fltarr(2,100),$
                     'normtin',fltarr(2,100), 'normtinfi', fltarr(2,100),$
                     'hsrat',fltarr(2,100), 'hsratfi',fltarr(2,100), $
                     'nthrat', fltarr(2,100), 'nthratfi', fltarr(2,100),$
                     'tau', fltarr(2,100), 'taufi', fltarr(2,100),$
                     'ginj', fltarr(2,100), 'ginjfi', fltarr(2,100),$
                     'normeq', fltarr(2,100), 'normeqfi', fltarr(2,100),$
                     'factor',fltarr(2,100), 'factorfi', fltarr(2, 100),$
                     'ftest',fltarr(2,100), 'chidof', fltarr(2,100), $
                     'eqw', fltarr(100), 'eqwfi', fltarr(100), $
                     'gsigma',fltarr(2,100),'gsigmafi',fltarr(2,100),$
                     'gnorm',fltarr(2,100),'gnormfi',fltarr(2,100))

outeqpwr=create_struct('xdates',fltarr(100),'obsid',strarr(100),$
      'tin', fltarr(2,100), 'tinth', fltarr(2,100),'tinnt', fltarr(2,100),$
      'normtin',fltarr(2,100), 'normtinth', fltarr(2,100),$
      'normtinnt', fltarr(2,100),$             
      'hsrat',fltarr(2,100), 'hsratth',fltarr(2,100),'hsratnt',fltarr(2,100), $
      'nthrat', fltarr(2,100),$
      'tau', fltarr(2,100), 'tauth', fltarr(2,100), 'taunt', fltarr(2,100),$
      'ginj', fltarr(2,100), 'ginjth', fltarr(2,100),'ginjnt', fltarr(2,100),$
      'ref',fltarr(2,100), 'refth',fltarr(2,100), 'refnt',fltarr(2,100), $
      'xi', fltarr(2, 100), 'xith', fltarr(2, 100), 'xint', fltarr(2, 100), $
      'normeq', fltarr(2,100), 'normeqth', fltarr(2,100),$
      'normeqnt', fltarr(2,100),$
      'factor',fltarr(2,100), 'factorth', fltarr(2, 100),$
      'factornt', fltarr(2, 100),$
      'chidof', fltarr(2,100), 'chidofth', fltarr(2,100),$
      'chidofnt', fltarr(2,100), $
      'eqw', fltarr(100), 'eqwth', fltarr(100), 'eqwnt', fltarr(100), $
      'gsigma',fltarr(2,100),'gsigmath',fltarr(2,100),$
      'gsigmant',fltarr(2,100),$
      'gnorm',fltarr(2,100),'gnormth',fltarr(2,100),$
      'gnormnt',fltarr(2,100))


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
outcompps.obsid=obs
outcompps.xdates=xdates

;now read pca data and collect relevant info

;start with compps

filetoch=['ph_compps1.dat','ph_comppsrxi.dat']

FOR k=0, 1 DO BEGIN ; with and without reflection

   FOR i=0, nfm-1 DO BEGIN ;for all items in the output file
   
      ;first check if file exists
      pfile=file_search(topdir+'/'+obs[i],filetoch[k],count=fc)
      IF fc eq 0 THEN BEGIN
         print, 'NO '+filetoch[k]+' found in XRAY/'+obs[i]+' ... check!'
      ENDIF ELSE BEGIN
         openr,1,pfile
         str=''

         ; first determine which part of the data to read, check the presence
         ; of ftest
         REPEAT readf,1,str UNTIL strmid(str,0,5) EQ 'Ftest'
         vals=strsplit(str,' ',/extract)
         ; record ftest
         ftest=float(vals[5])
         outcompps.ftest[1,i]=ftest
                                ;using mpftest from markwardt, but it
                                ;foes not match with xspec ftest
         chi1=double(vals[1])
         chi2=double(vals[3])
         dof1=double(vals[2])
         dof2=double(vals[4])
         F=((chi1-chi2)/(dof1-dof2)) / (chi2/dof2)
         outcompps.ftest[0,i]=mpftest(f, dof1-dof2, dof2,/sigma) 

         par='Tin'              ;not to confuse normalizations
         stp=0
     
         WHILE NOT EOF(1) DO BEGIN
            readf,1,str
            vals=strsplit(str,' ',/extract,length=len)
            IF ((len[0] le 2) AND (stp eq 0)) THEN BEGIN
               CASE vals[1] OF

                  'Tin': BEGIN
                     tinm=float(vals[2])
                     tinp=float(vals[3])
                     IF tinm EQ 0 THEN ntin=[tinp,-1] ELSE $
                        ntin=[(tinp+tinm)/2.,(tinp-tinm)/2.]
                     IF k EQ 0 THEN outcompps.tin[*,i]=ntin ELSE $
                                    outcompps.tinwr[*,i]=ntin
                     par='tin'
                  END

                  'norm': BEGIN
                     normm=float(vals[2])
                     normp=float(vals[3])
                     IF normm EQ 0 THEN nnorm=[normp,-1.] ELSE $
                        nnorm=[(normp+normm)/2.,(normp-normm)/2.]
                     IF k EQ 0 THEN BEGIN
                        IF par eq 'Tin' THEN outcompps.normtin[*,i]=nnorm
                        IF par eq 'cps' THEN outcompps.normcps[*,i]=nnorm
                        IF par eq 'gauss' THEN outcompps.gnorm[*,i]=nnorm
                     ENDIF ELSE BEGIN
                        IF par eq 'Tin' THEN outcompps.normtinwr[*,i]=nnorm
                        IF par eq 'cps' THEN outcompps.normcpswr[*,i]=nnorm
                        IF par eq 'gauss' THEN outcompps.gnormwr[*,i]=nnorm
                     ENDELSE

                  END

                  'kTe': BEGIN
                     ktem=float(vals[2])
                     ktep=float(vals[3])
                     IF ktem EQ 0 THEN nkte=[ktep,-1] ELSE $
                        nkte=[(ktep+ktem)/2.,(ktep-ktem)/2.]
                     IF k eq 0 THEN outcompps.kTe[*,i]=nkte ELSE $ 
                                    outcompps.kTewr[*,i]=nkte
                  END

                'kTbb': BEGIN ;weird, should have been kT, keep for now
                     ktbbm=float(vals[2])
                     ktbbp=float(vals[3])
                     IF ktbbm EQ 0 THEN nkTbb=[ktbbp,-1] ELSE $
                        nkTbb=[(ktbbp+ktbbm)/2.,(ktbbp-ktbbm)/2.]
                     IF k eq 0 THEN outcompps.kTbb[*,i]=nkTbb ELSE $
                                    outcompps.kTbbwr[*,i]=nkTbb
                  END

                'tau-y': BEGIN
                     par='cps'
                     taum=float(vals[2])
                     taup=float(vals[3])
                     IF taum EQ 0 THEN ntau=[taup,-1] ELSE $
                        ntau=[(taup+taum)/2.,(taup-taum)/2.]
                     IF k eq 0 THEN outcompps.tau[*,i]=ntau ELSE $
                                    outcompps.tauwr[*,i]=ntau
                    
                  END

                 'rel_refl': BEGIN
                     refm=float(vals[2])
                     refp=float(vals[3])
                     IF refm EQ 0 THEN BEGIN
                        outcompps.ref[0,i]=refp
                        outcompps.ref[1,i]=-1.
                     ENDIF ELSE BEGIN
                        outcompps.ref[0,i]=(refp+refm)/2.
                        outcompps.ref[1,i]=(refp-refm)/2.
                        ENDELSE
                  END

                'xi': BEGIN
                     xim=float(vals[2])
                     xip=float(vals[3])
                     IF xim EQ 0 THEN BEGIN
                        outcompps.xi[0,i]=xip
                        outcompps.xi[1,i]=-1.
                     ENDIF ELSE BEGIN
                        outcompps.xi[0,i]=(xip+xim)/2.
                        outcompps.xi[1,i]=(xip-xim)/2.
                        ENDELSE
                  END


               'sigma': BEGIN
                     par='gauss'
                     sigm=float(vals[2])
                     sigp=float(vals[3])
                     IF sigm EQ 0 THEN nsig=[sigp,-1] ELSE $
                        nsig=[(sigp+sigm)/2.,(sigp-sigm)/2.]
                     IF k eq 0 THEN outcompps.gsigma[*,i]=nsig ELSE $
                                    outcompps.gsigmawr[*,i]=nsig
                  END


                'factor': BEGIN
                     stp=1
                     facm=float(vals[2])
                     facp=float(vals[3])
                     nfac=[(facp+facm)/2.,(facp-facm)/2.]
                     IF k EQ 0 THEN outcompps.factor[*,i]=nfac ELSE $
                                    outcompps.factorwr[*,i]=nfac 
                  END

                  ELSE: x=5     ;random statement 
               ENDCASE
            ENDIF ELSE BEGIN

               CASE vals[0] OF

                  'eqw:': BEGIN
                     vals=strsplit(str,' ',/extract)
                     IF k eq 0 THEN outcompps.eqw[i]=float(vals[1]) ELSE $
                        outcompps.eqwwr[i]=float(vals[1])
                  END          
               
                  'CHI2/DOF:': BEGIN
                     vals=strsplit(str,' ',/extract)
                     IF k eq 0 THEN BEGIN
                        outcompps.chidof[0,i]=float(vals[1])
                        outcompps.chidof[1,i]=float(vals[2])
                     ENDIF ELSE BEGIN
                         outcompps.chidofwr[0,i]=float(vals[1])
                         outcompps.chidofwr[1,i]=float(vals[2])
                         ENDELSE
                  END


                  ELSE: x=5 ;random statement

               ENDCASE

            ENDELSE

         ENDWHILE


      close,1

   ENDELSE

 ENDFOR

ENDFOR


outstr=outcompps

END



