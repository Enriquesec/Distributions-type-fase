"Esta función es una modificacion de rlnorm con parametros
almacenados en la primera y segunda entrada de el vector
theta."

lognormalrnd<-function(theta,M,N){
  y<-matrix(1:(N*M),M)
  for(i in 1:M){
  y[i,]<-rlnorm(N,theta[1],theta[2])
  }
  return(y)
}

