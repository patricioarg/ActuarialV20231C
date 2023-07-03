#Cálculo de matriz de varianzas y covarianzas

library(tidyverse)
library(quantmod)
library(ggplot2)

#Cargo los datos correspondientes a cada activo

infelem<-data.frame(
  Cantidad=c(300,200,250),
  Precio=c(10,40,20),
  Vol.anual=c(.25,.15,.2),
  row.names = LETTERS[1:3]
)

#Cargo la matriz de correlaciones

rho<-data.frame(
  A=c(1,.7,.5),
  B=c(.7,1,.6),
  C=c(.5,.6,1),
  row.names=LETTERS[1:3]
)

#Creo una matriz diagonal utilizando la columna de desvíos. Que sea matriz diagonal evita luego problemas de transposición

desv<-diag(infelem[,"Vol.anual"])

#Realizo el producto matricial (con el símbolo %*%) premultiplicando y postmultiplicando la matriz de correlaciones por la matriz diagonal con los desvíos

varcv<-as.matrix(desv) %*% as.matrix(rho) %*% as.matrix(desv)

View(varcv)

#Calculamos el valor de la cartera hoy

W0<-sum(infelem[,"Cantidad"]*infelem[,"Precio"]) #Calculo el valor al momento 0 de la cartera

W<-infelem[,"Cantidad"]*infelem[,"Precio"]/W0 #Calculo el vector de ponderaciones

#Calculamos la varianza de la cartera

vzacart<-t(as.matrix(W)) %*% as.matrix(varcv) %*% as.matrix(W)

desvcart<-sqrt(vzacart)