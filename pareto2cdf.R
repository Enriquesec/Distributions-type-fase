"Esta función retorna la cdf de una Pareto con beta y 
soporte sobre [o,infinity)."


pareto2cdf<-function(x,beta){
  aux<-x
  aux[x<1]<-0
  
  if (beta>1){
    FF<-(1-x^(-beta))*aux
  }
  else{
    FF<-Inf*x
  }
  "FALTA"
  dF<-(x)^(-beta)*log(x)*
  d2F<-- -(x)^(-beta)*(log(x)^2)*
  my_list<-list("F"=FF,"dF"=dF,"d2F"=d2F)
  
    return(my_list)
}

