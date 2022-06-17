rm(list = ls())

histR<-function(x,GRID = 0,...){
hist(x,...)
if(GRID == 1){
grid()
}
axis(side = 1,lwd = 2)
axis(side = 2,lwd = 2)
box(lwd=2) 
}

plotR<-function(x,y,GRID = 1,...){
plot(x,y,...)
if(GRID == 1){
grid()
}
axis(side = 1,lwd = 2)
axis(side = 2,lwd = 2)
box(lwd=2) 
}

#### sims p = 2
p<-2 
set.seed(123)
n.s<-100 
R<-sapply(1:p,function(i)runif(n.s)) 
ap1<-sapply(1:n.s,function(x)sapply(1:n.s,function(y)sum((R[x,] - R[y,])^2)^(1/2)))
ap1<- as.numeric(ap1[ap1 != 0])
ap1<-unique(ap1)
#### sims p = 10

p<-10 
set.seed(123)
n.s<-100 
R<-sapply(1:p,function(i)runif(n.s)) 
ap2<-sapply(1:n.s,function(x)sapply(1:n.s,function(y)sum((R[x,] - R[y,])^2)^(1/2)))
ap2<- as.numeric(ap2[ap2 != 0])
ap2<-unique(ap2)
#### sims p = 100
p<-100
set.seed(123)
n.s<-100 
R<-sapply(1:p,function(i)runif(n.s)) 
ap3<-sapply(1:n.s,function(x)sapply(1:n.s,function(y)sum((R[x,] - R[y,])^2)^(1/2)))
ap3<- as.numeric(ap3[ap3 != 0])
ap3<-unique(ap3)
#### sims p = 1000
p<-1000
set.seed(123)
n.s<-100 
R<-sapply(1:p,function(i)runif(n.s)) 
ap4<-sapply(1:n.s,function(x)sapply(1:n.s,function(y)sum((R[x,] - R[y,])^2)^(1/2)))
ap4<- as.numeric(ap4[ap4 != 0])
ap4<-unique(ap4)
####minimas distancias
c(length(ap1),length(ap2),length(ap3),length(ap4))
range(ap1)
range(ap2)
range(ap3)
range(ap4)

par(mfrow = c(2,2))
histR(ap1, main = "dimension = 2", xlab = "distance between points"
      ,xlim = c(0, max(ap1)+1e-1))
histR(ap2, main = "dimension = 10", xlab = "distance between points"
      ,xlim = c(0, max(ap2)+1e-1))
histR(ap3,  main = "dimension=100", xlab = "distance between points"
      ,xlim = c(0, max(ap3)+1e-1))
histR(ap4, main = "dimension = 1000", xlab = "distance between points"
      ,xlim = c(0, max(ap4)+1e-1))

###Función hiperbola 
Vp<-function(p)pi^(p/2)/gamma(p/2 + 1)  

par(mfrow = c(1,1))
p<-seq(1e-6,100,length.out = 10000)
P<-Vp(p)
plotR(p,P,GRID = 0, type = "l",lwd = 2,main = "volume Vp(1)"
      ,ylab = "volume", xlab = "p")


CpV<-function(p) 1 -0.99^p
p<-seq(1e-6,1000,length.out = 10000)
P<-CpV(p)
plotR(p,P,GRID = 0, type = "l",lwd = 2,main = "fraction in the crust"
      ,ylab = "fraction", xlab = "p")




