rm(list = ls())

A <- data.frame(
 rbind(c(0 ,1 ,1 ,1, 1),
 c(0, 1, 0, 1, 1),
 c(1, 0, 1, 0, 1),
 c(0, 0, 0, 1, 0),
 c(0, 0, 0, 0, 0),
 c(0, 1 ,1, 0, 0)))

names(A) <-  c(sapply(1:4,function(i)paste("V",i,sep = "")),"Y") 

## A priori probabilities 
(p <- mean(A$Y == 1))
(q <- 1 - p )
Pr <- c(q,p)
##

P<-list()
M<-table(A[,5],A[,1])

sapply(1:4, function(i){ 
      M <<- table(A[,5],A[,i])
      P[[i]] <<- t(t(M)/colSums(M))
})
##Probabilities
P
##

ClasierA<-function(y){
      y<-unname(as.numeric(y))
	Ppos<-sapply(1:4,function(j){
          sapply(1:2,function(r){
             a <<-  log(P[[j]][r,y[j]+1]/Pr[r] + 1e-2) 
             return(a)
          }
       )
      })
	Res <- apply(Ppos,1,sum) + log(Pr)
      class<- which.max(Res)-1
      data.frame(class = class, LI = Res[1], LD = Res[2]) 
}



unname(as.numeric(A[1,-5]))

pred0<-sapply(1:6,function(j)ClasierA(A[j,-5]))

##LI, LD y predicciones para el conjunto de datos orinales
pred0
##Errores de predicción
table(A[,5],unlist(pred0[1,]))

newData<- rbind(c(1,1,0,0),c(1,0,0,1),c(0,0,1,1))
(pred1<-sapply(1:3,function(j)ClasierA(newData[j,])))

##
newData2<-rbind(c(0,0,0,0),
  c(1,0,0,0),c(0,1,0,0),c(0,0,1,0),c(0,0,0,1),
  c(1,1,0,0),c(1,0,1,0),c(1,0,0,1),c(0,1,0,1),
  c(0,1,1,0),c(0,0,1,1),c(1,1,1,0),c(1,0,1,1),
  c(1,1,0,1),c(0,1,1,1),c(1,1,1,1)
)


(pred1<-sapply(1:nrow(newData2),function(j)ClasierA(newData2[j,])))





