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
    print(sprintf("Alpha=%s, qi=%.3f, qs=%.3f, w=%.3f",alpha,qi,qs,w))
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
    print(sprintf("Alpha=%s, qi=%.3f, qs=%.3f, w mínima=%.3f",alpha,qimin,qsmin,wmin))
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
    print(sprintf("Alpha=%s, qi=%.3f, qs=%.3f, w=%.3f",alpha,qi,qs,w))
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
    print(sprintf("Alpha=%s, qi=%.3f, qs=%.3f, w mínima=%.3f",alpha,qimin,qsmin,wmin))
  }
}