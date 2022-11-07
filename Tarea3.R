#Pregunta 1

ns=c(25,50,80) #vector de valores de n
lambdas=c(1,0.5,2) # vector de valores de lambda


for (n in ns){ #recorremos cada valor de n
  print(sprintf("Resultados para n=%s ",n))
  print(" ")
  for (lambda in lambdas){ #recorremos cada valor de lambda para cada valor de n
    print(sprintf("Y lambda =%s",lambda))
    x=rpois(n, lambda) #muestra de Poisson(lambda) de tamaño n 
    xS=sum(x) #suma de todos los valores de x
    lgorro=xS/n #lambda gorro/ media muestral
    y=c() #inicializamos Y como vector vacio
    cont=0
    for (el in x){#generamos Y ber(exp(-y))
      if(cont==0){
        y1=el
      }
      if(el==0){
        y=c(y,1)
      }
      else{
        y=c(y,0)
      }
    }
    
    #==========================================
    
    #p0 gorro = exp(-lambdaEMV)
    
    #pEMV=c(pEMV,exp(-lgorro))
    pEMV=exp(-lgorro)
    EpEMV=exp(n*lambda*(exp(-1/n)-1))
    varpEMV=((n-1)/n)**(2*(n*lambda))-(1-(1-exp(-lambda)))
    print(varpEMV)
    eepEMV=sqrt(varpEMV)/sqrt(n)
    print(sprintf("pEMV=%s, ee(pEMV)=%s",pEMV,eepEMV))
    
    #==========================================
    
    #p0 gorro invertido = media muestral de Y
    
    #pY=c(pY,sum(y)/n)
    pY=sum(y)/n
    varpY=(exp(-lambda)*(1-exp(-lambda)))/n
    eepY=sqrt(varpY)/sqrt(n)
    print(sprintf("pY=%s, ee(pY)=%s",pY,eepY))
    
    #==========================================
    
    #p0 guion ondulado = E[Y1|T]
    
    #pUMVUE=c(pUMVUE,((n-1)/n)**xS)
    pUMVUE=((n-1)/n)**xS
    #varpUMVUE=exp(lambda*(1-2*n)/n)-exp(-2*lambda)
    varpUMVUE=-exp(-lambda)+((n-1)/n)**(2*xS)
    eepUMVUE=sqrt(varpUMVUE)/sqrt(n)
    print(sprintf("pUMVUE=%s, ee(pUMVUE)=%s",pUMVUE,eepUMVUE))
    print("=========================")
    print(" ")
    
  }
  
}

#==============================================================
#==============================================================
#==============================================================

#Pregunta 2

#Normal con colas del mismo peso
sigmas<-c(1,2,5) # vector con valores de desviacion estandar
alphas<-c(0.1,0.05,0.01) # vector de valores de alpha
print("Normal con colas del mismo peso \n")

for(sigma in sigmas){ # recorremos las sgimas
  print(sprintf("   Sigma = %s",sigma))
  for (alpha in alphas){ # recorremos los valores de alpha para cada sigma
    qi=qnorm(alpha/2,0,sigma) # calculamos quantiles para cada valor de alpha y sigma
    qs=-qi # por simetría de la normal centrada en 0
    w=qs-qi
    print(sprintf("Alpha=%s, qs=%.3f, qi=%.3f, w=%.3f",alpha,qs,qi,w))
  }
}
#==========================================================
#==========================================================

#Normal con colas del mismo peso
print("Normal con amplitud mínima \n")

#Se creara un vector (con la función seq) con valores de 0 a alpha incrementando 0.00001 y se
#probara los quantiles con cada valor y su complemento respecto a alpha 
#para buscar el menor

for(sigma in sigmas){
  print(sprintf("   Sigma = %s",sigma))
  for (alpha in alphas){
    wmin=1000000 # inicializamos el valor minimo de amplitud en un número grande
    for (parte in seq(0,alpha,0.00001)){# recorremos vector que recorre los valores entre alhpa y cero 
      qi=qnorm(parte,0,sigma)  # calculamos quantiles para cada valor de la parte de alpha y sigma
      qs=qnorm(1-(alpha-parte),0,sigma) # calculamos quantiles para cada valor del complemento de la parte de alpha y sigma
      w=qs-qi
      if(w<wmin){ # guardamos los valores minimos si la nueva amplitud es la menor
        qimin=qi
        qsmin=qs
        wmin=qs-qi
      }
    }
    print(sprintf("Alpha=%s, qs=%.3f, qi=%.3f, w mínima=%.3f",alpha,qsmin,qimin,wmin))
  }
}

#==========================================================
#==========================================================

#Xi con colas del mismo peso
print("Xi con colas del mismo peso \n")
ns=c(5,10,20) # vector con valores de grados de libertad
for(n in ns){
  print(sprintf("   n = %s",n))
  for (alpha in alphas){
    qi=qchisq(alpha/2,n) # quantiles de dsitribución chis  
    qs=qchisq(1-alpha/2,n)
    w=qs-qi
    print(sprintf("Alpha=%s, qs=%.3f, qi=%.3f, w=%.3f",alpha,qs,qi,w))
  }
}

#==========================================================
#==========================================================

#Xi con amplitud mínima
print("Xi con amplitud mínima \n")

#seguimos el mismo proceso que con la amplitud mínima de la normal pero en este
#caso el incremento en el vector será más chico para probar más valores (0.0000001)
#tardará más en correr

for(n in ns){
  print(sprintf("   n = %s",n))
  for (alpha in alphas){
    wmin=1000000
    for (parte in seq(0,alpha,0.0000001)){
      qi=qchisq(parte/2,n)
      qs=qchisq(1-(alpha-parte),n)
      w=qs-qi
      if(w<wmin){
        qimin=qi
        qsmin=qs
        wmin=qs-qi
      }
    }
    print(sprintf("Alpha=%s, qs=%.3f, qi=%.3f, w mínima=%.3f",alpha,qsmin,qimin,wmin))
  }
}

#==============================================================
#==============================================================
#==============================================================

#Pregunta 3



n=25 # tamaño de las muestras
alg1=c(10.53, 12.22, 9.01, 11.31, 9.90, 8.17, 9.92, 9.61,
       9.68, 8.86, 10.85, 7.97, 8.77, 11.12, 9.84, 11.47,
       10.89, 10.53, 9.54, 11.03, 10.43, 10.93, 11.70,
       10.58, 9.96) # muestra con el algoritmo 1
alg2=c(11.24, 11.53, 8.82, 11.20, 10.00, 9.07, 9.21, 9.88,
       9.00, 9.84, 11.40, 8.41, 9.70, 11.85, 10.58, 11.23,
       8.99, 12.50, 8.02, 13.87, 13.18, 12.31, 12.61, 12.19, 
       10.50)  #muestra con el algoritmo 2

Xn1=sum(alg1)/n # media muestral del algoritmo 1
Xn2=sum(alg2)/n # media muestral del algoritmo 1
#Inciso 1
#Intervalo de confianza para la media del algoritmo 1 con varianza 1
alpha=0.10
sigma=1
limInfMedia1=Xn1-qnorm(1-alpha/2)*sigma/sqrt(n)
limSupMedia1=Xn1+qnorm(1-alpha/2)*sigma/sqrt(n)
print(sprintf("Intervalo de confianza para Mediad del Alg. 1 con desviacion estandar 1 = (%.4f, %.4f)"
              ,limInfMedia1,limSupMedia1))

#==============================================================
#==============================================================

#Inciso 2
#Intervalo de confianza para la media del algoritmo 2 con varianza desconocida
SnCuad2=sum((alg2-Xn2)**2)/(n-1)
Sn2=sqrt(SnCuad2)
alpha=0.05
limInfMedia2=Xn1-qt(1-alpha/2,n-1)*Sn2/sqrt(n)
limSupMedia2=Xn1+qt(1-alpha/2,n-1)*Sn2/sqrt(n)
print(sprintf("Intervalo de confianza para Media del Alg. 2 con desviacion estandar desconocida = (%.4f, %.4f)"
              ,limInfMedia2,limSupMedia2))

#==============================================================
#==============================================================

#Inciso 3
#Intervalo de confianza para la desviacion estandar del Alg. 1
SnCuad1=sum((alg1-Xn1)**2)/(n-1)
alpha=0.05
limInfSD1=sqrt((n-1)*SnCuad1/qchisq(1-alpha/2,n-1))
limSupSD1=sqrt((n-1)*SnCuad1/qchisq(alpha/2,n-1))
print(sprintf("Intervalo de confianza para la SD del Alg. 1 = (%.4f, %.4f)"
              ,limInfSD1,limSupSD1))

#Intervalo de confianza para la desviacion estandar del Alg. 2

limInfSD2=sqrt((n-1)*SnCuad2/qchisq(1-alpha/2,n-1))
limSupSD2=sqrt((n-1)*SnCuad2/qchisq(alpha/2,n-1))
print(sprintf("Intervalo de confianza para la SD del Alg. 2 = (%.4f, %.4f)"
              ,limInfSD2,limSupSD2))

#Inciso 4
#Comparación de varianzas de ambos algoritmos
alpha=0.05
limInfVars=(SnCuad1/SnCuad2)/qf(1-alpha/2,n-1,n-1)
limSupVars=(SnCuad1/SnCuad2)/qf(alpha/2,n-1,n-1)
print(sprintf("Intervalo de confianza para el cociente de varianzas = (%.4f, %.4f)"
              ,limInfVars,limSupVars))
