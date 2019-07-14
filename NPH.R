"y es el vector de puntos

counter el número máxicmo de iteraciones de el EM

modelad Array que contiene las caraterísticas del modelo
%       phases       Dimensión de la distribución clásica de PH
%       SCALINGCDF   Escalado de la distribución CDF
%       S            Vector de escalar argumentos
%       range        Rango de valores para theta
%       BinSize      Longitud de binnig

tol Array que contiene los parametros de tolerancia
%       epsilon      Cambio en la maximaverosimilitud
%       counter      número de iteraciones

inicial Arry que continee  los parametros iniciales del modelo
%       alpha0       Distribución inicial
%       T0           Matrix de transición
%       theta0      Parametro de la variable aleatoria de escala
"
nph<-function(y,modelad,tol,inicial){
"  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  %%%%%%%%%%%%%%%%%%%      FORMATING OF THE DATA   %%%%%%%%%%%%%%%%%%%%%%%%%%
  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%"
library(pracma)
library(arules)
  
dimey<-dim(y)  
  if (dimey[1]>dimey[2]) {
    y<-t(y)
  }

"%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 %%%%%%%%%%%%%%%%%%%      CHARACTERISTICS OF THE MODEL   %%%%%%%%%%%%%%%%%%%
 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%"

phases      <- modelad$phases
SCALINGCDF  <- modelad$SCALINGCDF
S           <- modelad$S
rango <- modelad$RangeTheta
BinSize     <- modelad$Binsize
"%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 %%%%%%%%%%%%%%%%%%%      DATA REDUCTION     %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%"
difa<-function(y){
  max(y)-min(y)
}
yy<-NaN

  if (BinSize>0) {
  MM<-round(difa(y)/BinSize)
  a<-hist(y,nclass=MM,plot = F)
  weights<-a$counts
  edges<-a$breaks
  WW<-discretize(y,method = "fixed",breaks = a$breaks)
  
  for(i in 1:length(a$counts)){
    yy[i]<-mean(y[WW==levels(WW)[i]])
  }
  y<-yy[a$counts>0]
  weights<-weights[weights>0]
  }
  else {
    weigths<-matrix(rep(1,dimey[1]*dimey[2]),dimey[1])
  }
print("DATA REDUCTION")

"%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 %%%%%%%%%%%%%%%%%%%%%%      INITIAL PARAMETERS   %%%%%%%%%%%%%%%%%%%%%%%%%%
 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#% The initial values can be given in two different forms:
#%   1. The initials values of alpha0, T0, theta0 are contained in INITIAL
#%   2. If no INITIAL input is given, then a new set is randomly generated

#% Initialization of some values used to stop the iterative process"

epsilonGlobal <-tol[1]
Counter       <-tol[2]
tolerance     <-NaN
contar        <-1

alpha0  <-inicial$alpha0
T0      <-inicial$T0
theta0  <-inicial$theta0
L0      <-inicial$L0
phases  <-length(alpha0)


"%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 %%%%%%%%%%%%%%%%%%%%%%   RECURSIVE EM ALGORITHM   %%%%%%%%%%%%%%%%%%%%%%%%%
 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%"

estimaciones<-nphstep(y,weights,alpha0,T0,theta=theta0,SCALINGCDF,S,rango)
  alpha0<-estimaciones$alpha1
  T0<-estimaciones$T1
  theta0<-estimaciones$theta1
  L1<-estimaciones$L1
  
  while (contar<Counter){
    estimaciones<-nphstep(y,weights,alpha0,T0,theta0,SCALINGCDF,S,rango)
      alpha0<-estimaciones$alpha1
      T0<-estimaciones$T1
      theta0<-estimaciones$theta1
      L1<-estimaciones$L1
    
    tolerance<-(L1-L0)/abs(L1)
    contar<-contar+1
    LO<-L1
    print(contar)
  }
  my_list<-list("alpha0"=alpha0,"T0"=T0,"theta0"=theta0,"L1"=L1)
return(my_list)
}
