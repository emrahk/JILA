function magtoflb,mag

Irr_b = 4.27e-23
FluxB = 10^(-0.4*mag + alog10(Irr_b) ) *1e29

return,fluxB

end
