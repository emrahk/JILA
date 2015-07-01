function magtoflv,mag

Irr_v = 3.636e-23
FluxV = 10^(-0.4*mag + alog10(Irr_v) ) *1e29

return,fluxV

end
