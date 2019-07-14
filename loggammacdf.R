"Esta función es una modificación de la logncdf con los 
parametros mu y sigma son almacenados en la primera y 
segunda entrada del vector theta."

loggammacdf<-function(x,theta){
  alpha<-theta[1]
  beta<-1/theta[2]
  x<-log(x+1)
  "Existe una duda con beta en matlab"
  f<-pgamma(x,alpha,beta) 
  return(f)
}
  