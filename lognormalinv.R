"Esta función computa la inversad de la cdf de una varia
ble aleatoria lognormal con parametros mu y sigma estan
theta."

lognormalinv<-function(x,theta){
  y<-qlnorm(x,theta[1],theta[2])
    return(y)
}
