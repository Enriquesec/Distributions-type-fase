
"Función auxiliar usada para simular una clasica PH"
inphcdf<-function(x,modelad,parameters,u){
  if ((u<0) || (u>1)){
    print("El valor esta entre 0 y 1")
  }else{
    y<-u-nphcdf(x,modelad,parameters)
  }
  return(y)
}

"ESta función calcular la cdf de una distribución NPH.
La variable de salida es un valor de la cdf evualuada
en y

f          densidad
Abajo se definen las variables del modelo
y          es un vector de puntos datos
alpha      es la distribución inicial de la PH
T0         es una matrix subintensidad de la PH
theta      contiene los parametros de escalade la distribución
SCALINGCDF contiene el nombre de la distribución escala

El soporte discreto sobre el que se discretizará la distribución de escala."

nphinv<-function(y,modelad,parameters){
"%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 %%%%%%% Technical stuff (size of vectors, initializing variables) %%%%%%%%%
 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%"
  
  Ny<-dim(y)[1]
  My<-dim(y)[2]
  
  f<-zeros(Ny,My)
"%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 %%%%%%%%%%%%%%%%%%%%  CALCULATION OF THE DENSITY %%%%%%%%%%%%%%%%%%%%%%%%%%
 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%"
  for (i in 1:Ny){
    for(j in 1:My){
      aux<-fsolve(inphcdf,x0=.1,modelad=modelad,parameters=parameters,u=y[i,j])
      f[i,j]<-as.numeric(aux$x)
      aux<-fsolve(inphcdf, x0=f[i,j],modelad=modelad,parameters=parameters,u=y[i,j])
      f[i,j]<-as.numeric(aux$x)
    }
  }
  return(f)
}







