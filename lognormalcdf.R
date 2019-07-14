"Esta función es una modificación de logncdf con paramé
tros mu y sigma son almacenados en la primera y segunda
entrada del vector theta."
lognormalcdf<-function(x,theta){
  y<-plnorm(x,theta[1],theta[2])
  return(y)  
}
