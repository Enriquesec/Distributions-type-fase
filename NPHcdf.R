"Esta función calcula la cdf de una distribución 
NPH. Con las variables de salidad la densidad en si.
f       densidad

Abajo se definen las variables del modelo

y          es un vector de puntos de datos
alpha      es le valor inicial de la distribución PH
T0         es la matrix subintensidad de la PH
theta      contiene los parametros de la distribución de 
escala.
SCANLIGCDF contiene el nombre de la distribución de
escala.
El soporte discreto sobre el que se discretizará la 
distribución a escala."


nphcdf<-function(y,MODEL,PARAMETERS){
  SCALINGCDF  <- MODEL$SCALINGCDF
  S           <- MODEL$S
  
  alpha <- PARAMETERS$alpha
  T0    <- PARAMETERS$T0
  theta <- PARAMETERS$theta
  
  PI<-nphpmf(SCALINGCDF,S,theta)
  PI<-PI$PI
  "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  %%%%%%% Technical stuff (size of vectors, initializing variables) %%%%%%%%%
  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%"
  Ny=length(y)            #  Ny = number of data points
  NPi=length(PI)         #  Npi = number of levels
  f=matrix(rep(0,Ny),1)   #  Initializing vector of densities
  t=apply(T0,1,sum)*(-1)            #  Absorbtion rates
  "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 %%%%%%%%%%%%%%%%%%%%  CALCULATION OF THE DENSITY %%%%%%%%%%%%%%%%%%%%%%%%%%
 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  % The index i corresponds to the levels (rows of the matrix fy)
  % while the index j corresponds to data points (columns)"
  
  for(j in 1:Ny){
    for(i in 1:NPi){
      f[j]<-f[j]+PI[i]*(1-sum(alpha%*%(expm(T0*y[j]/S[i]))))
    }
  }
  return(f)
}



