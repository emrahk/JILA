function magtoflk,mag

Irr_k = 0.67e-23
FluxK = 10^(-0.4*mag + alog10(Irr_k) ) *1e29

return,fluxK

end
