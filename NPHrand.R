library(arules)
library(pracma)

JSUinv<-function(x,theta){
  y<-qJSU(x,theta[1],theta[2],theta[3],theta[4])
  return(y)
}

phcdf<-function(x,alpha,T0,u){
  E<-t(ones(dim(alpha)[1],dim(alpha)[2]))
  if(x>0){
      a<-u-(alpha%*%expm(T0*as.numeric(x)))%*%E
      a<-as.numeric(a)
    }else{
      a<-Inf
    }
  print(a)
  return(a)
}


nphrand<-function(modelad,parametros,M,N){
  SCALINGCDF<-modelad$SCALINGCDF
  S<-modelad$S
  
  alpha<-parametros$alpha
  T0<-parametros$T0
  theta<-parametros$theta
  
  y<-zeros(M,N)
  
  for (i in 1:M){
    for (j in 1:N){
      s<-eval(parse(text=SCALINGCDF))(rand(),theta)
      s<-sum(rep(s,4)>S)+1
      print(s)
      u<-runif(n=1)
      print(j)
      aux<-fsolve(phcdf,x0=1,alpha=alpha,T0=T0/s,u=u)
      print(aux)
      y[i,j]<-as.numeric(aux$x)
      print("PASO 2")
      print(y[i,j])
      aux<-fsolve(phcdf,alpha=alpha,T0=T0/s,x0=y[i,j],u=u)
      print(aux)
      y[i,j]<-as.numeric(aux$x)
    }
  }
  return(y)
}

MODEL$SCALINGCDF<-"JSUinv"
set.seed(2)
ra<-
ra

