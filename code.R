library(HistogramTools)
f<-function(x){
  a=1/gamma(alp)
  b=exp(-x)
  c=x^(alp-1)
  return(a*b*c)
}

F<-function(x){
  return(pgamma(x,alp,1))
}

x1<-array(dim = 1000)
h=.01
alp=3.2
for(i in 1:1000){
  u<-runif(1)
  m=0
  while (u>F(m*h)) {
    m=m+1  
  }
  b=(f((m+1)*h)-f(m*h))/h
  a=((F(m*h+h)-F(m*h))/h)-b*((2*m+1)*h)/2
  x1[i]=(-a+(sqrt(a^2-4*(b/2)*(F(m*h)-a*m*h-(b/2)*(m*h)^2-u))))/b
}

PlotRelativeFrequency(hist(x1,breaks= 15))
x=c(0:15)
lines(x,y=f(x),type = "l",col="cornflowerblue",lwd=2)
