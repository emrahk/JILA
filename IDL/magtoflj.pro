function magtoflj,mag

Irr_j = 1.67e-23
FluxJ = 10^(-0.4*mag + alog10(Irr_j) ) *1e29

return,fluxJ

end
