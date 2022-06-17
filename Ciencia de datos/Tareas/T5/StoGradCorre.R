rm(list=ls())

library(MASS)
library(Matrix)
#####################        Cool Plots        ######################
plotR<-function(x,y,GRID = 1,...){
plot(x,y,...)
if(GRID == 1){
grid()
}
axis(side = 1,lwd = 2)
axis(side = 2,lwd = 2)
box(lwd=2) 
}
#####################################################################


data<-read.csv(file.choose())
head(data)
summary(data)
sum(is.na(data))
###Summary of the dataset
X<-cbind(as.matrix(data[,-c(1,24)]))
head(X)
rowSums(X)
rankMatrix(X)
y<-as.matrix(data[,24])
dim(X);dim(y)

##Using OLS to solve the problem 
##Beta estimate
(b<-solve(t(X)%*%X)%*%t(X)%*%y)
##Sum of squares 
(ss<-sum((y-X%*%b)^2))
###Check results
coef(lm( y ~ X-1))
s<-summary(lm( y ~ X-1))$sigma
df<-summary(lm( y ~ X-1))$df
(s*sqrt(df[2]))^2

######################Implementation with gamma = 1e-1
##Choose a first observation. 

set.seed(123)
"%notin%" <- Negate("%in%")

(h<-1:nrow(X))
j<-sample(h,1)
h<-h[h %notin% j]
Xm<-matrix(X[j,],nc = length(X[j,]))
ym<-y[j,]
dim(Xm);dim(ym)
dim(beta)

beta<-t(Xm)*(ym)
(B<-beta)
gam <- 1e-10
SST <- sum((y - X%*%beta)^2)
err<-SST
i<-0
###### Iterations.
while((abs(ss- SST) > 1e-2)&(i <= 1500)){
     i<-i+1
     j<-sample(h,1)
     h<-h[h %notin% j]
     Xm<-matrix(X[j,],nc = length(X[j,]))
     ym<-y[j,]
     beta <- beta - 2*gam*(-t(Xm)*(ym) + t(Xm)%*%Xm%*%beta)  
     SST <- sum((y - X%*%beta)^2)
     err<-c(err, SST)
}

plotR(1:(length(err)-1),err[-1], type = "l", 
      lwd = 2,xlab = "Iteración", ylab ="SCR", 
       main = "SGD") 
####number of iter
i
##Last estimated beta
beta
##First estimated beta
B
##SST with first vs with last beta 
sum((y-X%*%beta)^2)
sum((y-X%*%B)^2)
ss
##Diference between estimated beta with SGD and with Moore-Penrose
max(abs(as.numeric(b)-beta))
################################################################

######################Implementation with gamma = 5
##Choose a first observation. 

set.seed(123)
"%notin%" <- Negate("%in%")

(h<-1:nrow(X))
j<-sample(h,1)
h<-h[h %notin% j]
Xm<-matrix(X[j,],nc = length(X[j,]))
ym<-y[j,]
dim(Xm);dim(ym)
dim(beta)

beta<-t(Xm)*(ym)
(B<-beta)
gam <- 5
SST <- sum((y - X%*%beta)^2)
err<-SST
i<-0
###### Iterations.
while((abs(ss- SST) > 1e-2)&(i <= 1500)){
     i<-i+1
     j<-sample(h,1)
     h<-h[h %notin% j]
     Xm<-matrix(X[j,],nc = length(X[j,]))
     ym<-y[j,]
     beta <- beta - 2*gam*(-t(Xm)*(ym) + t(Xm)%*%Xm%*%beta)  
     SST <- sum((y - X%*%beta)^2)
     err<-c(err, SST)
}

plotR(1:(length(err)-1),err[-1], type = "l", 
      lwd = 2,xlab = "Iteración", ylab ="SCR", 
       main = "SGD") 
####number of iter
i
##Last estimated beta
beta
##First estimated beta
B
##SST with first vs with last beta 
sum((y-X%*%beta)^2)
sum((y-X%*%B)^2)
ss
##Diference between estimated beta with SGD and with Moore-Penrose
max(abs(as.numeric(b)-beta))
################################################################

######################Implementation with gamma increasing to 1e-1
##Choose a first observation. 

set.seed(123)
"%notin%" <- Negate("%in%")

(h<-1:nrow(X))
j<-sample(h,1)
h<-h[h %notin% j]
Xm<-matrix(X[j,],nc = length(X[j,]))
ym<-y[j,]
dim(Xm);dim(ym)
dim(beta)

beta11<-t(Xm)*(ym)
(B<-beta11)
SST <- sum((y - X%*%beta11)^2)
err11<-SST
i<-0
###### Iterations.
while((abs(ss- SST) > 1e-2)&(i <= 1500)){
     i<-i+1
     j<-sample(h,1)
     h<-h[h %notin% j]
     Xm<-matrix(X[j,],nc = length(X[j,]))
     ym<-y[j,]
     beta11 <- beta11 - 2*abs(0.1 - 1/i)*(-t(Xm)*(ym) + t(Xm)%*%Xm%*%beta11)  
     SST <- sum((y - X%*%beta11)^2)
     err11<-c(err11, SST)
}

plotR(1:(length(err11)-1),err11[-1], type = "l", 
      lwd = 2,xlab = "Iteración", ylab ="SCR", 
       main = "SGD") 
####number of iter
i
##Last estimated beta
beta
##First estimated beta
B
##SST with first vs with last beta 
sum((y-X%*%beta11)^2)
sum((y-X%*%B)^2)
ss
##Diference between estimated beta with SGD and with Moore-Penrose
max(abs(as.numeric(b)-beta))
################################################################


######################Implementation with gamma = 2*1e-1
##Choose a first observation. 

set.seed(123)
"%notin%" <- Negate("%in%")

(h<-1:nrow(X))
j<-sample(h,1)
h<-h[h %notin% j]
Xm<-matrix(X[j,],nc = length(X[j,]))
ym<-y[j,]
dim(Xm);dim(ym)
dim(beta)

beta<-t(Xm)*(ym)
(B<-beta)
gam <- 2*1e-1
SST <- sum((y - X%*%beta)^2)
err<-SST
i<-0
###### Iterations.
while((abs(ss- SST) > 1e-2)&(i <= 1500)){
     i<-i+1
     j<-sample(h,1)
     h<-h[h %notin% j]
     Xm<-matrix(X[j,],nc = length(X[j,]))
     ym<-y[j,]
     beta <- beta - 2*gam*(-t(Xm)*(ym) + t(Xm)%*%Xm%*%beta)  
     SST <- sum((y - X%*%beta)^2)
     err<-c(err, SST)
}

plotR(1:(length(err)-1),err[-1], type = "l", 
      lwd = 2,xlab = "Iteración", ylab ="SCR", 
       main = "SGD") 
####number of iter
i
##Last estimated beta
beta
##First estimated beta
B
##SST with first vs with last beta 
sum((y-X%*%beta)^2)
sum((y-X%*%B)^2)
ss
##Diference between estimated beta with SGD and with Moore-Penrose
max(abs(as.numeric(b)-beta))
#####Comparative with incresing sequence

N<-min(length(err11),length(err))
ylim <- c(0,max(err11,err))
plotR(1:N,err11[1:N],type = "l", ylim = ylim,
      lwd = 2,xlab = "Iteración", ylab ="SCR", 
      main = "SGD's") 
lines(1:N,err[1:N],col = "red")
abline(h = ss, col = "green") 
dim(data)


################################################################


###Momentum.
set.seed(123)
mt <- 0
(h<-1:nrow(X))
j<-sample(h,1)
h<-h[h %notin% j]
Xm<-matrix(X[j,],nc = length(X[j,]))
ym<-y[j,]
dim(Xm);dim(ym)

beta1<-t(Xm)*(ym)
(B<-beta1)
SST <- sum((y - X%*%beta1)^2)
err1<-SST
b1<-0.2 ; gam<-5*1e-1
r<-0

while((abs(ss- SST) > 1e-2)&(r <= 1500)){
     r<-r+1
     ##GetGradiant
     j<-sample(h,1)
     h<-h[h %notin% j]
     Xm<-matrix(X[j,],nc = length(X[j,]))
     ym<-y[j,]
     gt<-2*(-t(Xm)*(ym) + t(Xm)%*%Xm%*%beta1) 
     mt<- b1*mt + (1-b1)*gt  
     beta1 <- beta1 - gam*mt
     SST <- sum((y - X%*%beta1)^2)
     err1<-c(err1, SST)
}
r
SST

##Last estimated beta
beta1
##First estimated beta
B
##Diference between estimated beta with SGD and with Moore-Penrose
max(abs(as.numeric(b)-beta1))

##SST with first vs with last beta 
sum((y-X%*%beta1)^2)
sum((y-X%*%B)^2)
######
plotR(1:length(err1),err1, type = "l", 
      lwd = 2,xlab = "Iteración", ylab ="SCR", 
      main = "SGDM") 
N<-min(length(err1),length(err))
ylim <- c(0,max(err1,err))

plotR(1:N,err1[1:N],type = "l", ylim = ylim,
      lwd = 2,xlab = "Iteración", ylab ="SCR", 
      main = "SGD vs SGDM") 
lines(1:N,err[1:N],col = "red")
abline(h = ss, col = "green") 
dim(data)


###ADAM.
set.seed(123)

mt <- 0
vt <- 1
j<-sample(1:nrow(X),1)
Xm<-as.matrix(X[j,])
ym<-y[j,]
dim(Xm);dim(ym)

beta<-t(Xm)*(ym)
(B<-beta)
SST <- sum((ym - Xm%*%beta)^2)
err2<-SST
b1<-0.5 ; b2<-0.999 ; e<-1e-8; gam<-100 ;t<-0

sapply(1:1e2,function(i){
     t<<-t+1
     ##GetGradiant
     j<<-sample(1:nrow(X),1)
     Xm<<-as.matrix(X[j,])
     ym<<-y[j,]
     gt<<-2*(-t(Xm)*(ym) + t(Xm)%*%Xm%*%beta) 
     mt<<- b1*mt + (1-b1)*gt  
     vt<<- b2*vt + (1-b2)*(gt%*%t(gt))
     hmt<<- mt/(1 - b1^t)
     hvt<<- vt/(1 - b2^t)
     beta <<- beta - gam*hmt/(sqrt(hvt) + e)
     SST <<- sum((y - X%*%t(beta))^2)
     err2<<-c(err2, SST)     
}
)

t(gt)%*%gt
beta
B
plot(err2, type ="l")
##SST with first vs with last beta 
sum((y-X%*%t(beta))^2)