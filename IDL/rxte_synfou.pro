pro rxte_synfou, obsid, eafile=fileea

; This program runs syncseg and fourier programs of Katja Pottschmidt
;
; INPUTS
;
; obsid:full obsid
;
; OPTIONAL INPUTS
;
; eafile: a file that holds the correct ea and channel range information
;
; OUTPUTS
;
; NONE (files with power spectra)
;
; USES
;
; rxte_syncseg
; rxte_fourier
; rxte_fourier2
;
; USED BY
;
; alldo (bash shell)
;
; created by Emrah Kalemci, november 2014
;
;

obspl=strsplit(obsid,'-',/extract)
IF NOT keyword_set(fileea) THEN fileea=obspl[0]+'_eainfo.txt'

; determine channel ranges and number 

readcol,'../'+fileea,ea,minc,maxc, FORMAT = 'A, A, A'

; determine number of pcus

gtifil=file_search(obspl[2]+'.'+obspl[3]+'/filter','good*.gti')
ogtfx=strsplit(gtifil,'/',/extract)
detoffx=strsplit(ogtfx[2],'_',/extract)
detsoff=strpos(detoffx[1],'off') ; number of pcus off

; initialize parameters
;syncseg
path                = obspl[2]+'.'+obspl[3]+'t.all'
dseg                = 256L*256L
username            = 'Kalemci'
date                = systime(0)
chatty              = 0

;fourier
type                = 'high'
maxdim              = 256L*256L
dim                 = 256L*256L
normindiv           = 0
miyamoto            = 1
pca_bkg             = 1
nof                 = 1
logf                = 0.11
zhang_dt            = 1
ninstr              = 5-detsoff
deadtime            = 1D-5
;pca_dt            = 1
nonparalyzable      = 0
fcut                = 256D0
xmin                = [0.003,0.003,0.003]       &  xmax        = [256.,256.,256.]
ymin                = [1E-6,-4.,1E-5]        &  ymax        = [4.,4.,1.]
xtenlog             = [1,1,1]                &  ytenlog     = [1,0,1]
sym                 = [4,-3]
color               = [50,50,50]
postscript          = 1

IF N_ELEMENTS(ea) EQ 1 THEN BEGIN
   channels=[minc+'-'+maxc,minc+'-'+maxc] 
   ebounds=[[float(minc),float(maxc)],[float(minc),float(maxc)]]
   orgbin              = [-8D0,-8D0]
   newbin              = [1L,1L]

  rxte_syncseg,path,channels, $
  hexte=hexte,bkg=bkg,back_p=back_p,back_m=back_m, $
  orgbin=orgbin,newbin=newbin,dseg=dseg, $
  obsid=obsid,username=username,date=date, $  
  chatty=chatty,novle=novle 

  rxte_fourier,path,type=type, $
  maxdim=maxdim,dim=dim,normindiv=normindiv, $
  schlittgen=schlittgen,leahy=leahy,miyamoto=miyamoto, $
  hexte_bkg=hexte_bkg,pca_bkg=pca_bkg, $
  linf=linf,logf=logf,nof=nof, $
  zhang_dt=zhang_dt,   ninstr=ninstr,deadtime=deadtime, $
  nonparalyzable=nonparalyzable, $
  pca_dt=pca_dt, $     
  hexte_dt=hexte_dt, $
  cluster_a=cluster_a,cluster_b=cluster_b, $                   
  fmin=fmin,fmax=fmax,fcut=fcut, $
  xmin=xmin,xmax=xmax, $
  ymin=ymin,ymax=ymax, $
  xtenlog=xtenlog,ytenlog=ytenlog,sym=sym, $
  ebounds=ebounds,obsid=obsid,username=username,date=date, $
  color=color,postscript=postscript,chatty=chatty

ENDIF ELSE BEGIN

   nel=n_elements(ea)
   channels=strarr(nel)
   ebounds=fltarr(2,nel+1)
   FOR i=0,nel-1 DO BEGIN
      channels[i]=minc[i]+'-'+maxc[i]
      ebounds[*,i]=[float(minc[i]),float(maxc[i])]
   ENDFOR
   ebounds[*,nel]=[float(minc[0]),float(maxc[nel-1])]
   orgbin=replicate(-8D0,nel)
   newbin=replicate(1L,nel)
   addch=indgen(nel)
 
   rxte_syncseg,path,channels, $
  hexte=hexte,bkg=bkg,back_p=back_p,back_m=back_m, $
  orgbin=orgbin,newbin=newbin,dseg=dseg, $
  obsid=obsid,username=username,date=date, $  
  chatty=chatty,novle=novle 

   rxte_fourier2,path,type=type, $
  maxdim=maxdim,dim=dim,normindiv=normindiv, $
  schlittgen=schlittgen,leahy=leahy,miyamoto=miyamoto, $
  hexte_bkg=hexte_bkg,pca_bkg=pca_bkg, $
  linf=linf,logf=logf,nof=nof, $
  zhang_dt=zhang_dt,   ninstr=ninstr,deadtime=deadtime, $
  nonparalyzable=nonparalyzable, $
  pca_dt=pca_dt, $     
  hexte_dt=hexte_dt, $
  cluster_a=cluster_a,cluster_b=cluster_b, $                   
  fmin=fmin,fmax=fmax,fcut=fcut, $
  xmin=xmin,xmax=xmax, $
  ymin=ymin,ymax=ymax, $
  xtenlog=xtenlog,ytenlog=ytenlog,sym=sym, $
  ebounds=ebounds,obsid=obsid,username=username,date=date, $
  color=color,postscript=postscript,chatty=chatty,addch=addch


ENDELSE


END
