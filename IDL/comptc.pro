pro comptc, inps, ind

;This program uses the output of pl euf data to calculate the Compton
;Temperature of a given X-ray spectrum

;INPUTS
;
; inps: input spectral density
; ind: to extend the power law
;prints out the Compton temperature
;assumes powerlaw+diskbb spectrum

readcol, inps, en, en_err, toteuf, toteuf_err, totmod, powmod, diskmod

;plot, en, totmod, /xlog, /ylog

k=1.380658E-16 ; erg K-1
keV=1.6021772E-9 ; erg

tot=0.
toti=0.
totp=0.
totpi=0.

;sanity check

FOR i=0, N_ELEMENTS(en)-2 DO BEGIN
   tot=tot+(en[i+1]-en[i])*totmod[i]
   toti=toti+(en[i+1]-en[i])*totmod[i]*en[i]
   totp=totp+(en[i+1]-en[i])*powmod[i]
   totpi=totpi+(en[i+1]-en[i])*powmod[i]*en[i]
;   print,en[i],(en[i+1]-en[i]),(en[i+1]-en[i])*totmod[i]*en[i]
ENDFOR

;print,tot,tot*keV
print,toti, toti/tot, toti*keV/(tot*4.*k)
print,totp, totpi,totpi*keV/(totp*4.*k)

ee1=((min(en))^(3-ind))/(3-ind)
ee2=((max(en))^(3-ind))/(3-ind)
ee3=(1000^(3-ind))/(3-ind)

lum1=((min(en))^(ind-2))/(ind-2)
lum2=((max(en))^(ind-2))/(ind-2)
lum3=(1000^(ind-2))/(ind-2)


print,totp*(lum3-lum2)/(lum2-lum1)
print,totpi*(ee3-ee2)/(ee2-ee1)

ttotpi=totpi+totpi*(ee3-ee2)/(ee2-ee1)
ttotp=totp+totp*(lum3-lum2)/(lum2-lum1)

print,(ttotpi+toti)*keV/((tot+ttotp)*4*k)

print,lum1,lum2
stop
end
