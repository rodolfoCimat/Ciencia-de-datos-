rm(list=ls())
require(lasso2); require(dplyr);require(glmnet);require(latex2exp)
data(Prostate)
#########  Cool Plots  ###########
plotR<-function(x,y=FALSE,GRID = 1,...){
if(class(y) != "logical"){
plot(x,y,...)
if(GRID == 1){
grid()
}
axis(side = 1,lwd = 2)
axis(side = 2,lwd = 2)
box(lwd=2) 
}
else{
plot(x,...)
if(GRID == 1){
grid()
}
axis(side = 1,lwd = 2)
axis(side = 2,lwd = 2)
box(lwd=2) 
}
}

#################################

slam = function(x,lambda){ sign(x)*(abs(x)-lambda)*(abs(x)>lambda) }
dat<-rev(Prostate)
head(dat)
round(head(dat), 3)
round(tail(dat), 3)
(y = dat[,1])
N = length(y)
(y = y-mean(y))
X = dat[,2:9]
X = scale(X,center=TRUE,scale=FALSE)
X = t(t(X)/apply(X,2,function(x) sqrt(sum(x^2)/N)))

colnames(X) = names(dat)[2:9]
out = lm( y ~ -1+X ) # Ajuste LS de p.12 (sin normalizar cols)
beta = out$coeff
n = 100
n.iter = 50
m = 8
betan = rep(0,m)
betas = matrix(0,n,m)
lams = seq(1,0.15,length=n) # selecci´on de lambdas (note en reversa)


for(i in 1:n){
beta = matrix(out$coeff) 
for(k in 1:n.iter){ # 10 iteraciones (?)
res = y-X%*%beta
for(j in 1:m){ betan[j] = slam( beta[j,]+mean(X[,j]*res), lams[i])}
beta = matrix(betan) }
betas[i,] = beta }
tail(betas)


L1n = rowSums( abs(betas) )
aa = apply(betas,1,function(x){sum(abs(x)>0)})
bb = rep(0,m)
par(mfrow = c(1,1))
for(i in 1:m){ bb[i] = which(aa==i)[1] }
plotR(L1n,betas[,1],type="l", lwd=2, col="blue",
     xaxt="n",xlab="Norma L1",ylab="Estimadores",
     main="Modelo Prostata",GRID=0,ylim = c(-0.1,0.6))
for(i in 2:m){ lines(L1n, betas[,i], lwd=2, col=i) }
abline(v=L1n[bb],col=gray(.8))
axis(3, at=L1n[bb], labels=1:m, mgp=c(1.5,.5,0), cex.axis=.8)


####Lo anterior fue hecho manualmente, para lo siguiente se usará Glmnet 

dat<-rev(Prostate)
y = dat[,1]
(N = length(y))
(y = y-mean(y))
X = dat[,2:9]
X = scale(X,center=TRUE,scale=FALSE)
X = t(t(X)/apply(X,2,function(x) sqrt(sum(x^2)/N)))

##lambdaencontrado en el inciso a) 
(lambda.max = (1/N)*max(sapply(1:ncol(X),function(j)sum(abs(X[,j]*y)))))
##lambdamuuuy cercano a cero
lambda.min= 1e-2

##Rango de valores de lambda para buscar 
lambda = seq(lambda.min,lambda.max, length.out = 100)

out = glmnet(X,y, intercept = FALSE, lambda = lambda)
par(mfrow = c(1,1))
plotR(x = out,main = TeX("Estim. $\\beta$ variando $\\lambda$")
     ,lwd = 2)

cvcv10 = cv.glmnet(X, y,intercept=FALSE, lambda = lambda)
cvcvloo = cv.glmnet(X, y,intercept=FALSE, nfolds = length(y), lambda = lambda)

par(mfrow = c(1,2))
plotR(x = cvcv10, main ="10-Fold CV")
plotR(x = cvcvloo, main  = "Leave-One-Out CV")

##Parámetro lambda por CV
(lambda1<-cvcv10$lambda.min)
(lambda2<- cvcvloo$lambda.min)

fit1 = glmnet(X,y, intercept = FALSE, lambda = lambda1)
fit2 = glmnet(X,y, intercept = FALSE, lambda = lambda2)

###Coeficiente estimados con Lasso, haciendo uso del parámetro 
###lambda estimado por CV. 
round(coef(fit1),4)


##stepwize hacia atrás
dat<-rev(Prostate)
y = dat[,1]
(N = length(y))
(y = y-mean(y))
X = dat[,2:9]
X = scale(X,center=TRUE,scale=FALSE)
X = t(t(X)/apply(X,2,function(x) sqrt(sum(x^2)/N)))

dat1<-data.frame(cbind(y,X))
head(dat1)
out.max = lm( y ~ -1 +.,data = dat1)
summary(out.max)
out.min = lm( y ~ -1 ,data = dat1)
summary(out.min)
auto.backward<-step(out.max,direction = "backward", 
                    scope = list(lower = out.min,upper = out.max) )
fit3 = lm( y ~ svi + lbph + age + lweight + lcavol-1,data = dat1)
summary(fit3)



##Without sugesting a sequence of lambdas just for comparing
dat<-rev(Prostate)
y = dat[,1]
(N = length(y))
(y = y-mean(y))
X = dat[,2:9]
X = scale(X,center=TRUE,scale=FALSE)
X = t(t(X)/apply(X,2,function(x) sqrt(sum(x^2)/N)))


out = glmnet(X,y, intercept = FALSE)
par(mfrow = c(1,1))
plot(out)

cvcv10 = cv.glmnet(X, y,intercept=FALSE)
cvcvloo = cv.glmnet(X, y,intercept=FALSE, nfolds = length(y))
names(cvcv10)
par(mfrow = c(1,2))
plot(cvcv10)
(lambda1<-cvcv10$lambda.min)
plot(cvcvloo)
(lambda2<- cvcvloo$lambda.min)

fit1 = glmnet(X,y, intercept = FALSE, lambda = lambda1)
fit2 = glmnet(X,y, intercept = FALSE, lambda = lambda2)

coef(fit1)
coef(fit2)

