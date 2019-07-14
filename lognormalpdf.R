"Esta función es una modificación lognormal con 
parametros mu y sigma estan theta."

lognormalpdf<-function(x,theta){
  y<-dlnorm(x,theta[1],theta[2])
  return(y)
}
