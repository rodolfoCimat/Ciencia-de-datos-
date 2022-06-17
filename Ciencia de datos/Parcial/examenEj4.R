rm(list = ls())
library(MASS)
###Cool Plots
plotR<-function(x,y,GRID = 1,...){
plot(x,y,...)
if(GRID == 1){
grid()
}
axis(side = 1,lwd = 2)
axis(side = 2,lwd = 2)
box(lwd=2) 
}

##############Graficación de frontera eficiente
(p1<-c(0.1,0.2,0.5,0.8,0.9))
(R<-log((p1/(1- p1))*6) - (1/2)*(299/36) + ((17)^2)/24 + 49/144)
ev<-500
X<-matrix(,nc=500,nr=5)
Y1<-matrix(,nc=500,nr=5)
Y2<-matrix(,nc =500, nr = 5)

for(i in 1:5){
   X[i,]<-seq(17/3 - sqrt((8*R[i])/3)+1e-6,17/3 + sqrt((8*R[i])/3)-1e-6,length = ev)
   Y1[i,]<-7/8 + (3/2)*sqrt(R[i] - (3/8)*(X[i,] -17/3)^2)
   Y2[i,]<-7/8 - (3/2)*sqrt(R[i] - (3/8)*(X[i,] -17/3)^2)
}

###Fronteras óptimas para varios niveles:
plotR(X[1,],Y1[1,], col = 1,type ="l",ylim=c(-6,7)
      ,xlim=c(-1,15),lwd = 2,
      main = "Fronteras Optimas para Distintas a Priori",
      xlab ="x1", ylab = "x2")
lines(X[1,],Y2[1,], col = 1 ,lwd = 2)
for(i in 2:5){
lines(X[i,],Y1[i,], col = i,lwd = 2)
lines(X[i,],Y2[i,], col = i,lwd = 2)
}

##############################
m1<-c(8,1)
(S1<-rbind(c(1,0),c(0,1)))
m2<-c(15,2)
(S2<-rbind(c(4,0),c(0,9)))
n.sims<-5000 
clase <- numeric()
X<-matrix( ,nr = n.sims,nc = 2)
p1<-.5
p2<-1 - p1

##Sims
set.seed(123)
for(i in 1:n.sims){
    clase<-c(clase,sample(c(1,2),size = 1,prob=c(p1,p2)))
    ifelse(clase[i] == 1,X[i,]<-mvrnorm(1,mu = m1,Sigma = S1),X[i,]<-mvrnorm(1,mu = m2,Sigma = S2))
}

cl<-numeric(n.sims)

for(i in 1:n.sims){
   a<-(3/8)*X[i,1]^2 + (4/9)*X[i,2]^2 -(17/4)*X[i,1] - (7/9)*X[i,2]
   b<- log((p1/(1-p1))*6) - (1/2)*(299/36)
   ifelse( a > b,cl[i]<-2,cl[i]<-1) 
}
summary(cl)

(Perr1 <- mean(cl != clase))
(R<-log((p1/(1- p1))*6) - (1/2)*(299/36) + ((17)^2)/24 + 49/144)
ev<-500


###Params Elipse
(aa<-sqrt(8*R/3))
(bb<-sqrt(9*R/4)) 

x1<-seq(17/3 - sqrt((8*R)/3)+1e-6,17/3 + sqrt((8*R)/3)-1e-6,length = ev)
y1<-7/8 + (3/2)*sqrt(R - (3/8)*(x1 -17/3)^2)
y2<-7/8 - (3/2)*sqrt(R - (3/8)*(x1 -17/3)^2)

##Gráficas
plotR(x1,y1,col = 3,type ="l",ylim=c(-6,7),xlim=c(-1,15),lwd = 2,
      main = "Frontera Óptima con observaciones",
      xlab = "x1",ylab ="x2")
lines(x1,y2,col = 3,lwd = 2)
points(X[,1],X[,2], col = clase,pch = 20)
#############################
n.sims<-5000 
clase <- numeric()
X<-matrix( ,nr = n.sims,nc = 2)
p1<-.2
p2<-1 - p1

##Sims
set.seed(123)
for(i in 1:n.sims){
    clase<-c(clase,sample(c(1,2),size = 1,prob=c(p1,p2)))
    ifelse(clase[i] == 1,X[i,]<-mvrnorm(1,mu = m1,Sigma = S1),X[i,]<-mvrnorm(1,mu = m2,Sigma = S2))
}

cl<-numeric(n.sims)
for(i in 1:n.sims){
   a<-(3/8)*X[i,1]^2 + (4/9)*X[i,2]^2 -(17/4)*X[i,1] - (7/9)*X[i,2]
   b<- log((p1/(1-p1))*6) - (1/2)*(299/36)
   ifelse( a > b,cl[i]<-2,cl[i]<-1) 
}
summary(cl)

###Proba Error
(Perr2 <- mean(cl != clase))

(R<-log((p1/(1- p1))*6) - (1/2)*(299/36) + ((17)^2)/24 + 49/144)
ev<-500

###Params Elipse
(aa<-sqrt(8*R/3))
(bb<-sqrt(9*R/4)) 


x1<-seq(17/3 - sqrt((8*R)/3)+1e-6,17/3 + sqrt((8*R)/3)-1e-6,length = ev)
y1<-7/8 + (3/2)*sqrt(R - (3/8)*(x1 -17/3)^2)
y2<-7/8 - (3/2)*sqrt(R - (3/8)*(x1 -17/3)^2)

##Gráficas
plotR(x1,y1,col = 3,type ="l",ylim=c(-6,7),xlim=c(-1,15),lwd = 2,
      main = "Frontera Óptima con observaciones",
      xlab = "x1",ylab ="x2")
lines(x1,y2,col = 3,lwd = 2)
points(X[,1],X[,2], col = clase,pch = 20)
################################

n.sims<-5000 
clase <- numeric()
X<-matrix( ,nr = n.sims,nc = 2)
p1<-.8
p2<-1 - p1

#sims
set.seed(123)
for(i in 1:n.sims){
    clase<-c(clase,sample(c(1,2),size = 1,prob=c(p1,p2)))
    ifelse(clase[i] == 1,X[i,]<-mvrnorm(1,mu = m1,Sigma = S1),X[i,]<-mvrnorm(1,mu = m2,Sigma = S2))
}

cl<-numeric(n.sims)
for(i in 1:n.sims){
   a<-(3/8)*X[i,1]^2 + (4/9)*X[i,2]^2 -(17/4)*X[i,1] - (7/9)*X[i,2]
   b<- log((p1/(1-p1))*6) - (1/2)*(299/36)
   ifelse( a > b,cl[i]<-2,cl[i]<-1) 
}
summary(cl)

##Proba Error
(Perr3 <- mean(cl != clase))
(R<-log((p1/(1- p1))*6) - (1/2)*(299/36) + ((17)^2)/24 + 49/144)
ev<-500

###Params Elipse
(aa<-sqrt(8*R/3))
(bb<-sqrt(9*R/4)) 

x1<-seq(17/3 - sqrt((8*R)/3)+1e-6,17/3 + sqrt((8*R)/3)-1e-6,length = ev)
y1<-7/8 + (3/2)*sqrt(R - (3/8)*(x1 -17/3)^2)
y2<-7/8 - (3/2)*sqrt(R - (3/8)*(x1 -17/3)^2)

##Gráficas
plotR(x1,y1,col = 3,type ="l",ylim=c(-6,7),xlim=c(-1,15),lwd = 2,
      main = "Frontera Óptima con observaciones",
      xlab = "x1",ylab ="x2")
lines(x1,y2,col = 3,lwd = 2)
points(X[,1],X[,2], col = clase,pch = 20)