"Esta función calcula la función de densidad de una 
distribución loggamma estos son almacenados en la primer
y segunda entrada de theta"

loggammapdf<-function(x,theta){
  alpha<-theta[1]
  beta<-1/theta[2]
  
  f<-beta^alpha*log(x+1)^(alpha-1)/(gamma(alpha)*(1+x)^(beta+1))
  
  return(f)
}
