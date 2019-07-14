
library(pracma)

"%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%"
mletheta<-function(theta0){
  auxa<-nphpmf(SCALINGCDF,S,theta0)
  PI<-auxa$PI
  aux<--w*log(PI)
  aux<-aux[abs(PI)>0] 
  MLE<-sum(aux)
  return(MLE)
} 
"%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%"
"Este programa produce una ITERACIÓN DEL ALGORITMO EM para estimar la 
siguiente salida:

% alpha1         Distribución inicial del pH.
% T1             Matriz de subintensidad del pH.
% theta1         Los parámetros de la variable aleatoria de escala seleccionada
% L1             El loglikelihood del modelo con los valores iniciales

Las variables de entrada son:

% y              Vector de puntos de datos
% alpha          Distribución inicial alfa del proceso de salto de Markov
% T              Matriz de transición inicial del proceso de salto de Markov
% SCALINGCDF     Scaling CDF (función de theta)
% S              Vector de valores de escala (soporte de la escala rv)
% rango          Matriz de parámetros de tamaño x 2 con el rango de valores de theta
% theta          parámetro (s) inicial (es) de la distribución de escala
% pesos          Pesos de la densidad teórica a ajustar."

nphstep<-function(y,weights,alpha0,T0,theta0,SCALINGCDF,S,rango){
  library(pracma)
"%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%  VECTOR OF SCALING PROBABILITIES (FINITE)   %%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%"
iniset<-nphpmf(SCALINGCDF,S,theta0)
PI<-iniset$PI
S<-iniset$S
"%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%  CALCULATION OF THE DENSITY FUNCTION   %%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%"
"Regresamos la función densidad. La verosimilitud L es computada acontinuación."

Ny<-length(y)                         #Ny es el numero de datos
NPI<-length(PI)                       #NPI numero de niveles   
Nphases<-length(alpha0)                #Numero de fases

t<-apply(-T0,2,sum)

U<-matrix(rep(0,Nphases*Nphases),Nphases) #Inicialización de matrices 
R<-matrix(rep(0,Nphases*Nphases),Nphases) #auxiliares.
W<-matrix(rep(0,NPI*Ny),NPI)

f<-rep(0,Ny)    
  
" %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  %%%%%%%%%%%%%%%%%%%%  CALCULATION OF THE DENSITY %%%%%%%%%%%%%%%%%%%%%%%%%%
  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%"
ta<-t(t%*%alpha0)

    for (j in 1:Ny){
      JAux<-matrix(rep(0,Nphases^2),Nphases)
      UAux<-matrix(rep(0,Nphases^2),Nphases)
        for (i in 1:NPI){
          Aux<-(expm(rbind(cbind(T0/S[i],t(ta)/S[i]),cbind(zeros(Nphases),T0/S[i]))*y[j]))*PI[i]/S[i]
          JAux<-JAux+Aux[1:Nphases,seq(Nphases+1,2*Nphases)]
          WAux<-ta%*%(expm(T0/S[i]*y[j]))*PI[i]/S[i]
          UAux<-UAux+WAux
          W[i,j]<-sum(WAux)
        }
      JAux<-t(JAux)
      f[j]<-sum(UAux)
      R<-R+JAux*weights[j]/f[j]
      U<-U+UAux*weights[j]/f[j]
      W[,j]<-W[,j]*weights[j]/f[j]
    }
alpha1<-apply(U,2,sum)/sum(weights)
alpha1<-t(alpha1)*(t(alpha1>0))
alpha1<-alpha1/sum(alpha1)


T1<-(diag(Nphases)/R)%*%(R*T0)
T1<-T1*(T1>0) 
t1<-apply(U,1,sum)/t(diag(R))
t1<-t1*(t1>0)
T1<-T1-diag(diag(T1))
T1<-T1-diag(t(t1)+apply(T1,2,sum))
print(T1)
print(alpha1)
print(theta0)
w<-t(apply(W,1,sum))
    if(SCALINGCDF=="pareto2cdf"){
      SS=log(S/S[1])
      c<-SS[2]
      "DUDA"
        if (SS==seq(from=0,to=2*(length(SS)-1),by=2)){
          theta1<--(1/c)*log(1-sum(w)/sum(w*t(seq(1:length(w)))))
        } else{
        theta1<-fminsearch(fn = mletheta,x0=theta,SCALINGCDF=SCALINGCDF,S=S,w=w,rango=rango,lower = 0,method = "Hooke-Jeeves")
        theta1<-fminsearch(fn = mletheta,SCALINGCDF=SCALINGCDF,S=S,w=w,rango=rango,x0 = theta1$xmin,lower = 0,method = "Hooke-Jeeves")
      }
    } else {
        #theta1<-fminsearch(fn = mletheta,SCALINGCDF=SCALINGCDF,S=S,w=w,range=range,x0 = theta0,lower = c(-Inf,0),method = "Hooke-Jeeves")
        #theta1<-fminsearch(fn = mletheta,SCALINGCDF=SCALINGCDF,S=S,w=w,range=range,x0 = theta1$xmin,lower = c(-Inf,0),method = "Hooke-Jeeves")
        theta1<-nlminb(start=theta0, mletheta ,lower=c(-Inf,0.001))
        #theta1<-fminsearch(fn = mletheta,SCALINGCDF=SCALINGCDF,S=S,w=w,range=range,x0 = theta0,lower = 0)
        #theta1<-fminsearch(fn = mletheta,SCALINGCDF=SCALINGCDF,S=S,w=w,range=range,x0 = theta1$xmin)
      }

  L1<-sum(log(f)%*%weights)
  print(theta1$par)
  my_lis<-list("alpha1"=alpha1,"T1"=T1,"theta1"=theta1$par,"L1"=L1)
  return(my_lis)
  }

finala<-nph(y,modelad,tol,inicial)

