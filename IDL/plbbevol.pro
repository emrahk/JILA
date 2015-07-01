pro plbbevol, str1655r


plsinglesed, str1655r, 0, 53424.6, /plotrad, /ps, fname='53424.6.sed.eps', /opltsed, /fixind, Tin=8000., yrn=[0.4,300.], res=rstr1,/auto
plsinglesed, str1655r, 0, 53428.5, /plotrad, /ps, fname='53428.5.sed.eps', /opltsed, /fixind, Tin=8000., yrn=[0.4,300.], res=rstr2,/auto
plsinglesed, str1655r, 0, 53433.5, /plotrad, /ps, fname='53433.5.sed.eps', /opltsed, /fixind, Tin=8000., yrn=[0.4,300.], res=rstr3,/auto
plsinglesed, str1655r, 0, 53435.5, /plotrad, /ps, fname='53435.5.sed.eps', /opltsed, /fixind, Tin=8000., yrn=[0.4,300.], res=rstr4,/auto
plsinglesed, str1655r, 0, 53438.6, /plotrad, /ps, fname='53438.6.sed.eps', /opltsed, /fixind, Tin=8000., yrn=[0.4,300.], res=rstr5,/auto
plsinglesed, str1655r, 0, 53439.5, /plotrad, /ps, fname='53439.5.sed.eps', /opltsed, /fixind, Tin=8000., yrn=[0.4,300.], res=rstr6,/auto
plsinglesed, str1655r, 0, 53445.5, /plotrad, /ps, fname='53445.5.sed.eps', /opltsed, Tin=9000., yrn=[0.4,300.], res=rstr7,/auto
plsinglesed, str1655r, 0, 53447.5, /plotrad, /ps, fname='53447.5.sed.eps', /opltsed, Tin=9000., yrn=[0.4,300.], res=rstr8,/auto
plsinglesed, str1655r, 0, 53449.5, /plotrad, /ps, fname='53449.5.sed.eps', /opltsed, Tin=9400., yrn=[0.4,300.], res=rstr9,/auto
;plsinglesed, str1655r, 0, 53451.5, /plotrad, /ps, fname='53451.5.sed.eps', /opltsed, Tin=9400., yrn=[0.4,300.], res=rstr10,/auto
plsinglesed, str1655r, 0, 53453.5, /plotrad, /ps, fname='53453.5.sed.eps', /opltsed, Tin=9400., yrn=[0.4,300.], res=rstr10,/auto
plsinglesed, str1655r, 0, 53458.5, /ps, fname='53458.5.sed.eps', /opltsed, Tin=9400., yrn=[0.4,300.], res=rstr11,/auto
plsinglesed, str1655r, 0, 53460.5, /ps, fname='53460.5.sed.eps', /opltsed, Tin=10000., yrn=[0.4,300.], res=rstr12,/auto
plsinglesed, str1655r, 0, 53461.5, /ps, fname='53461.5.sed.eps', /opltsed, Tin=10000., yrn=[0.4,300.], res=rstr13,/auto
plsinglesed, str1655r, 0, 53462.5, /ps, fname='53462.5.sed.eps', /opltsed, Tin=10000., yrn=[0.4,300.], res=rstr14,/auto

nres=replicate(rstr1,14)
nres[0]=rstr1
nres[1]=rstr2
nres[2]=rstr3
nres[3]=rstr4
nres[4]=rstr5
nres[5]=rstr6
nres[6]=rstr7
nres[7]=rstr8
nres[8]=rstr9
nres[9]=rstr10
nres[10]=rstr11
nres[11]=rstr12
nres[12]=rstr13
nres[13]=rstr14
;nres[14]=rstr15

mjds=[53424.6,53428.5,53433.5,53435.5,53438.6,53439.5,53445.5,53447.5,53449.5,53451.5,53453.5,53458.5,53460.5,53461.5,53462.5]

plsinglesed, str1655r, 0, 53424.6, /plotrad, /ps, fname='53424.6.sed.eps', /opltsed,  Tin=8000., yrn=[0.4,300.], res=rstr1b,/auto
plsinglesed, str1655r, 0, 53428.5, /plotrad, /ps, fname='53428.5.sed.eps', /opltsed,  Tin=8000., yrn=[0.4,300.], res=rstr2b,/auto
plsinglesed, str1655r, 0, 53433.5, /plotrad, /ps, fname='53433.5.sed.eps', /opltsed,  Tin=8000., yrn=[0.4,300.], res=rstr3b,/auto
plsinglesed, str1655r, 0, 53435.5, /plotrad, /ps, fname='53435.5.sed.eps', /opltsed,  Tin=8000., yrn=[0.4,300.], res=rstr4b,/auto
plsinglesed, str1655r, 0, 53438.6, /plotrad, /ps, fname='53438.6.sed.eps', /opltsed,  Tin=8000., yrn=[0.4,300.], res=rstr5b,/auto
plsinglesed, str1655r, 0, 53439.5, /plotrad, /ps, fname='53439.5.sed.eps', /opltsed,  Tin=8000., yrn=[0.4,300.], res=rstr6b,/auto

mjdsb=[53424.6,53428.5,53433.5,53435.5,53438.6,53439.5]

nresb=replicate(rstr1,6)
nresb[0]=rstr1b
nresb[1]=rstr2b
nresb[2]=rstr3b
nresb[3]=rstr4b
nresb[4]=rstr5b
nresb[5]=rstr6b

!p.multi=[0,1,2]

plot,mjds,nres.T1,psym=4,yr=[7000.,11000.],/ystyle
oplot,mjdsb,nresb.T1,psym=5
plot,mjds,nres.chr1,psym=4,yr=[2E11,7E11],/ystyle
oplot,mjdsb,nresb.chr1,psym=5
END





