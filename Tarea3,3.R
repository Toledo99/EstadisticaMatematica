#Pregunta 3



n=25
alg1=c(10.53, 12.22, 9.01, 11.31, 9.90, 8.17, 9.92, 9.61,
       9.68, 8.86, 10.85, 7.97, 8.77, 11.12, 9.84, 11.47,
       10.89, 10.53, 9.54, 11.03, 10.43, 10.93, 11.70,
       10.58, 9.96)
alg2=c(11.24, 11.53, 8.82, 11.20, 10.00, 9.07, 9.21, 9.88,
       9.00, 9.84, 11.40, 8.41, 9.70, 11.85, 10.58, 11.23,
       8.99, 12.50, 8.02, 13.87, 13.18, 12.31, 12.61, 12.19, 
       10.50)

Xn1=sum(alg1)/n
Xn2=sum(alg2)/n
#Inciso 1
#Intervalo de confianza para la media del algoritmo 1 con varianza 1
alpha=0.10
sigma=1
limInfMedia1=Xn1-qnorm(1-alpha/2)*sigma/sqrt(n)
limSupMedia1=Xn1+qnorm(1-alpha/2)*sigma/sqrt(n)
print(sprintf("Intervalo de confianza para Mediad del Alg. 1 con desviacion estandar 1 = (%.4f, %.4f)"
              ,limInfMedia1,limSupMedia1))

#Inciso 2
#Intervalo de confianza para la media del algoritmo 2 con varianza desconocida
SnCuad2=sum((alg2-Xn2)**2)/(n-1)
Sn2=sqrt(SnCuad2)
alpha=0.05
limInfMedia2=Xn1-qt(1-alpha/2,n-1)*Sn2/sqrt(n)
limSupMedia2=Xn1+qt(1-alpha/2,n-1)*Sn2/sqrt(n)
print(sprintf("Intervalo de confianza para Media del Alg. 2 con desviacion estandar desconocida = (%.4f, %.4f)"
              ,limInfMedia2,limSupMedia2))

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
