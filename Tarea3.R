#Pregunta 1

ns=c(25,50,80)
lambdas=c(1,0.5,2)
pEMV=c()
pY=c()
pUMVUE=c()
res=matrix()
vectorMedias=c()

for (n in ns){
  print(sprintf("Resultados para n=%s ",n))
  print(" ")
  for (lambda in lambdas){
    print(sprintf("Y lambda =%s",lambda))
    x=rpois(n, lambda)
    xS=sum(x)
    lgorro=xS/n
    vectorMedias=c(vectorMedias,lgorro)
    y=c()
    cont=0
    for (el in x){
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
    
    #pEMV=c(pEMV,exp(-lgorro))
    pEMV=exp(-lgorro)
    EpEMV=exp(n*lambda*(exp(-1/n)-1))
    varpEMV=((n-1)/n)**(2*(n*lambda))-(1-(1-exp(-lambda)))
    print(varpEMV)
    eepEMV=sqrt(varpEMV)/sqrt(n)
    print(sprintf("pEMV=%s, ee(pEMV)=%s",pEMV,eepEMV))
    #pY=c(pY,sum(y)/n)
    pY=sum(y)/n
    varpY=(exp(-lambda)*(1-exp(-lambda)))/n
    eepY=sqrt(varpY)/sqrt(n)
    print(sprintf("pY=%s, ee(pY)=%s",pY,eepY))
    #pUMVUE=c(pUMVUE,((n-1)/n)**xS)
    pUMVUE=((n-1)/n)**xS
    varpUMVUE=exp(lambda*(1-2*n)/n)-exp(-2*lambda)
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
sigmas<-c(1,2,5)
alphas<-c(0.1,0.05,0.01)
print("Normal con colas del mismo peso \n")

for(sigma in sigmas){
  print(sprintf("   Sigma = %s",sigma))
  for (alpha in alphas){
    qi=qnorm(alpha/2,0,sigma)
    qs=-qi
    w=qs-qi
    print(sprintf("Alpha=%s, qs=%.3f, qi=%.3f, w=%.3f",alpha,qs,qi,w))
  }
}
#==========================================================
#==========================================================

#Normal con colas del mismo peso
print("Normal con amplitud mínima \n")

for(sigma in sigmas){
  print(sprintf("   Sigma = %s",sigma))
  for (alpha in alphas){
    wmin=1000000
    for (parte in seq(0,alpha,0.00001)){
      qi=qnorm(parte,0,sigma)
      qs=qnorm(1-(alpha-parte),0,sigma)
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

#==========================================================
#==========================================================

#Xi con colas del mismo peso
print("Xi con colas del mismo peso \n")
ns=c(5,10,20)
for(n in ns){
  print(sprintf("   n = %s",n))
  for (alpha in alphas){
    qi=qchisq(alpha/2,n)
    qs=qchisq(1-alpha/2,n)
    w=qs-qi
    print(sprintf("Alpha=%s, qs=%.3f, qi=%.3f, w=%.3f",alpha,qs,qi,w))
  }
}

#==========================================================
#==========================================================

#Xi con amplitud mínima
print("Xi con amplitud mínima \n")

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