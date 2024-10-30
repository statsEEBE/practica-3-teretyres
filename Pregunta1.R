#Ensayo de BERNOULLI
x<-c(0,1) #0 fracaso, 1 éxito
f<-c(0.68, 0.32) #probabilidad de cada una (p y 1-p)

n<-43 #repeticiones del ensayo
muestra<-sample(x, n, f, replace=TRUE) #generar simulación  
muestra
sum(muestra)

bar<-barplot(table(muestra)/n, ylim=c(0,1))
lines(bar,f,type="h", col="red")
points(bar,f, pch=16, col="red")

Y<-function(i){sum(sample(x, n, f, replace=TRUE))}

set.seed(123)
m<-400000
encuestas<-sapply(1:m, Y)
fr<-table(encuestas)/m

fr["13"]

#apartado 1, 13 éxitos
dbinom(13,43,0.32) #solució directa

#apartado 2, menos de  17 éxitos
pbinom(16,44,0.32) #P(X<17)=P(X<=16)=P(X=16)

#apartado 3
n<-24
valesp<-n*0.68 #valor esperado E=n*p
valesp

n*0.68*0.32 #variància V=n*p*(1-p)

qbinom(0.25,24,0.68) #quantil de un cuarto

