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
    varpEMV=exp(-lambda)*(1-exp(-lambda)) - ((n-1)/n)**xS*(1- ((n-1)/n)**2)
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
