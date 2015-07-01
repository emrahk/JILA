pro magtoflux, mags, band, flux,  NH=NHc, EBmV=EBV

;This program takes an array of magnitudes (with errors) and converts
;them to an array of fluxes in mJy
;
; INPUTS
;
; mags: magnitudes, errors, array(2,X)
; band: filter for observation
;
; OPTIONAL INPUTS
;
; NH : Hydrogen column density of the source if the E(B-V) will be
;    calculated this way, in units of 1E22 atoms/cm^2
; EBmV : E(B-V) if known. Either this or NH must be provided
;
; OUTPUTS
;
; flux : flux and its errors in mJy
;
; LOGS
;
; Created by Emrah Kalemci Feb 18, 2015
;

cont=1
IF (keyword_set(NHc) AND keyword_set(EBV)) THEN BEGIN
   print, 'You can either specify NH or EBmV, NOT both!'
   cont=0
ENDIF

IF ((NOT keyword_set(NHc)) AND (NOT keyword_set(EBV))) THEN BEGIN
   print, 'You must either specify NH or EBmV!'
   cont=0
ENDIF

IF cont THEN BEGIN

   RV = 3.1                  ; standard ISM,  or 3.0 Predehl/Schmidt

   IF keyword_set(NHc) THEN BEGIN
   ;Method 1: Predehl 95

   ;   AV=NHc/0.179

   ;Method 2: 
  
      EBV=NHc/0.53             ;Predehl Schmidt
  

      AV = RV * EBV

   ENDIF

   IF keyword_set(EBV) THEN AV = RV * EBV

   ;Wavelengths

   BW = 438 ;nm
   VW = 545 ;nm
   IW = 798 ;nm
   JW = 1250 ;nm
   HW = 1650 ;nm
   KW = 2190 ;nm Check this

   ;calculate extinction coefficients based on A(W)=AV*(a(x)+b(x)/RV)
   ;for infrared bands, use Cardelli

   AJ=fltarr(2)
   AH=fltarr(2)
   AK=fltarr(2)
   AB=fltarr(2)
   AI=fltarr(2)
   
   AJ[0]=AV[0]*(0.4008-(0.3679/RV)) ; single sigma 0.03
   AH[0]=AV[0]*(0.2693-(0.2473/RV)) ; single sigma 0.034, not the exact wavelength?
   AK[0]=AV[0]*(0.1615-(0.1483/RV)) ;single sigma, 0.04
   AB[0]=AV[0]*(1.0+(1./RV))  ;no error
;   AI[0]=AV[0]*(0.661-(0.555/RV)) ; this is for johnson
   
   AI[0]=AV[0]*(0.771-(0.499/RV)) 

   AVp = (AV[1]/AV[0]) ; error percentage in AV
   AJ[1] = AJ[0]*(0.03+AVp)
   AH[1] = AH[0]*(0.034+AVp)
   AK[1] = AK[0]*(0.04+AVp)
   AB[1] = AB[0]*AVp
   AI[1] = AI[0]*(0.028+AVp)
;   AI[1] = (AV[0]*0.036)+(AV[1]*(0.661-(0.555/RV)))+(AV[0]*0.130/RV)
;   AI[1] = AI[0]*(AVp+(0.036/0.661)+(0.130/(0.555*RV)))
   
;a=1.+0.104*y-0.609*(y^2.)+0.701*(y^3.)+1.137*(y^4.)-1.718*(y^5.)-0.827*(y^6.)+1.647*(y^7.)-0.505*(y^8.)
;b=1.952*y+2.908*(y^2.)-3.989*(y^3.)-7.985*(y^4.)+11.102*(y^5.)+5.491*(y^6.)-10.805*(y^7.)+3.347*(y^8.)
;from O'Donnell


   print, AV, AJ, AH, AK, AB, AI

   ;dereddened magnitudes, and fluxes

   flux=mags

   CASE band OF
      'B' : BEGIN
         Bdr=mags[0,*]-AB[0] ; deredden, implement errors later
         flux[0,*] = magtoflb(Bdr) ; What is zero point for B, could not get fomr Bessell?
         Bdrp=mags[0,*]-AB[0]-(mags[1,*]+AB[1])/sqrt(2.) ; divide by sqrt(2.)???
         Bdrm=mags[0,*]-AB[0]+(mags[1,*]+AB[1])/sqrt(2.)
         flp=magtoflh(Bdrp)
         flm=magtoflh(Bdrm)
         flux[1,*]=(flp-flm)/2.
      END
 
      'V' : BEGIN
         Vdr=mags[0,*]-AV[0] ; deredden, implement errors later
         flux[0,*] = magtoflv(Vdr)
         Vdrp=mags[0,*]-AV[0]-(mags[1,*]+AV[1])/sqrt(2.) ; divide by sqrt(2.)???
         Vdrm=mags[0,*]-AV[0]+(mags[1,*]+AV[1])/sqrt(2.)
         flp=magtoflh(Vdrp)
         flm=magtoflh(Vdrm)
         flux[1,*]=(flp-flm)/2.
      END

      'I' : BEGIN
         Idr=mags[0,*]-AI[0] ; deredden, implement errors later
         flux[0,*] = magtofli(Idr)
         ;error, is there a better way?
         Idrp=mags[0,*]-AI[0]-(mags[1,*]+AI[1])/sqrt(2.)
         Idrm=mags[0,*]-AI[0]+(mags[1,*]+AI[1])/sqrt(2.)
         flp=magtofli(Idrp)
         flm=magtofli(Idrm)
         flux[1,*]=(flp-flm)/2.
      END

     'J' : BEGIN
         Jdr=mags[0,*]-AJ[0] ; deredden, implement errors later
         flux[0,*] = magtoflj(Jdr)
         Jdrp=mags[0,*]-AJ[0]-(mags[1,*]+AJ[1])/sqrt(2.) ; divide by sqrt(2.)???
         Jdrm=mags[0,*]-AJ[0]+(mags[1,*]+AJ[1])/sqrt(2.)
         flp=magtoflh(Jdrp)
         flm=magtoflh(Jdrm)
         flux[1,*]=(flp-flm)/2.
      END

     'H' : BEGIN
         Hdr=mags[0,*]-AH[0] ; deredden, implement errors later
         flux[0,*] = magtoflh(Hdr)
;error, is there a better way?
         Hdrp=mags[0,*]-AH[0]-(mags[1,*]+AH[1])/sqrt(2.) ; divide by sqrt(2.)???
         Hdrm=mags[0,*]-AH[0]+(mags[1,*]+AH[1])/sqrt(2.)
         flp=magtoflh(Hdrp)
         flm=magtoflh(Hdrm)
         flux[1,*]=(flp-flm)/2.
      END

     'K' : BEGIN
         Kdr=mags[0,*]-AK[0] ; deredden, implement errors later
         flux[0,*] = magtoflk(Kdr) ;I took 2MASS, discuss???
         Kdrp=mags[0,*]-AK[0]-(mags[1,*]+AK[1])/sqrt(2.) ; divide by sqrt(2.)???
         Kdrm=mags[0,*]-AK[0]+(mags[1,*]+AK[1])/sqrt(2.)
         flp=magtoflh(Kdrp)
         flm=magtoflh(Kdrm)
         flux[1,*]=(flp-flm)/2.
      END
  ENDCASE

ENDIF

END

      
