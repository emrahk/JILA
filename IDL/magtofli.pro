function magtofli,mag

Irr_i = 2.416e-23
FluxI = 10^(-0.4*mag + alog10(Irr_i) ) *1e29

return,fluxI

end
