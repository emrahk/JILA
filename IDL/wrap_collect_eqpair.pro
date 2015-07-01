pro wrap_collect_eqpair, outstr

basedir='~/RXTE/DATA_AN/JILA/1655/JANUS_done/'
topdirhs='XRAY_hs_1'
topdirhims='XRAY_hims_eqp2'
topdirss='XRAY_ss_uf1'
topdirus3='XRAY_us_eqpair_uf3n'
topdirus4='XRAY_us_eqpair_uf4n'

collect_eqpair, basedir+topdirhs, ouths
outstr=ouths
xx=where(ouths.state eq 0)
hsi=n_elements(xx)
collect_eqpair, basedir+topdirhims, outstr, stind=hsi, /update, /hims
xx=where(outstr.state ne -1)
himsi=n_elements(xx)
collect_eqpair, basedir+topdirss, outstr, stind=himsi, /update, /ss
xx=where(outstr.state ne -1)
us3i=n_elements(xx)
collect_eqpair, basedir+topdirus3, outstr, stind=us3i, /update, /us3
xx=where(outstr.state ne -1)
us4i=n_elements(xx)
collect_eqpair, basedir+topdirus4, outstr, stind=us4i, /update, /us4

states=[0,0,0,0,$
0,0,0,0,$
0,0,0,0,$
0,0,0,0,$
1,1,1,$
1,1,1,1,$
2,4,4,4,$
4,4,4,4,$
3,3,3,3,$
3,3,3,3,$
3,3,3,3,$
3,3,3]

outstr.state[0:45]=states

END
