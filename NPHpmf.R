"Esta función contruye el vector de probabilidades 
discretas de una función de distribución SCALINGCDF y 
soporte S

Las entradas son

% SCALINGCDF Nombre de la función de distribución donde 
             se extraerá la escala
% S          Soporte de la distribución de escala 
             (reales positivos)
% theta      Parámetros de la función de escalado"

nphpmf<-function(SCALINGCDF,S,theta){
  S<-sort(S)
  P2<-eval(parse(text=SCALINGCDF))(c(S,Inf),theta)
  P1<-eval(parse(text=SCALINGCDF))(c(-Inf,S),theta)
  
  PI<-P2-P1
  m<-length(PI)
  PI[m-1]<-PI[m]+PI[m-1]
  PI<-PI[-m]
  #Revisar!
  S[PI==0]<-NaN
  S<-S[is.nan(S)==FALSE]
  PI[PI==0]<-NaN
  PI<-PI[is.nan(PI)==FALSE]
  my_list <- list("PI"=PI,"S"=S)
  return(my_list)
}
