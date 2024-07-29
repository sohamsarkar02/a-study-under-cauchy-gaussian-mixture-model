m<-100000
n<-200
g<-0.94
mu<-0
s<-1
T<-double(n)
r<-runif(n)
sum(r<=g)
x<-c()
for(i in 1:m)
{
  for(j in 1:n)
  {
    if(r[j]<=g)
      x[j]<-rnorm(1,mu,s)
    else
      x[j]<-rcauchy(1,mu,s)
  }
  
  T[i]<-(sqrt(n)*(median(x)))/(IQR(x)/2)
}
l<-(T-mean(T))/sd(T)
qqplot(l,rnorm(m),xlab='Statistic',ylab='Normal')
