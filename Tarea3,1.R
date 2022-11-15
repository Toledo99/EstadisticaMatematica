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
    xS2=xS-x[1]
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
    pthetaEMV=exp(-lgorro)
    EpthetaEMV=exp(n*lambda*(exp(-1/n)-1))
    varpthetaEMV=exp(n*lambda)*(exp(exp(-2/n)-1)-exp(2*exp(-1/n)-1))
    eepthetaEMV=sqrt(varpEMV)
    print(sprintf("pthetaEMV=%s, ee(pthetaEMV)=%s",pthetaEMV,eepthetaEMV))
    #pY=c(pY,sum(y)/n)
    pY=sum(y)/n
    varpY=(exp(-lambda)*(1-exp(-lambda)))/n
    eepY=sqrt(varpY)
    print(sprintf("pY=%s, ee(pY)=%s",pY,eepY))
    #pUMVUE=c(pUMVUE,((n-1)/n)**xS)
    pUMVUE=((n-1)/n)**xS
    cons=((n-1)/n)
    varpUMVUE=exp(-lambda)*(1-exp(-lambda))-((cons**(2*xS2))*(1-cons**2)*(1+(cons**2)*lambda))
    eepUMVUE=sqrt(varpUMVUE)
    print(sprintf("pUMVUE=%s, ee(pUMVUE)=%s",pUMVUE,eepUMVUE))
    print("=========================")
    print(" ")
  }
  
}
