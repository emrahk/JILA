function incpro, inc, beta

gamma=sqrt(1./(1-beta^2.))
out=1./(gamma*(1-beta*cos(inc*!pi/180.)))

return, out

end
