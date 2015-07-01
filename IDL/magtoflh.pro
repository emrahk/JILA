function magtoflh,mag

Irr_h = 0.98e-23
FluxH = 10^(-0.4*mag + alog10(Irr_h) ) *1e29

return,fluxH

end
