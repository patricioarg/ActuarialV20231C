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

#Defino el nivel de confianza con el que voy a trabajar

conf=.99

alfa=1-conf

#Defino el período (en días) con el que voy a trabajar

per=10

#Creo una matriz diagonal utilizando la columna de desvíos. Que sea matriz diagonal evita luego problemas de transposición

desv<-diag(infelem[,"Vol.anual"])

#Realizo el producto matricial (con el símbolo %*%) premultiplicando y postmultiplicando la matriz de correlaciones por la matriz diagonal con los desvíos

varcv<-as.matrix(desv) %*% as.matrix(rho) %*% as.matrix(desv)

View(varcv)

#Calculamos el valor de la cartera hoy

W0<-sum(infelem[,"Cantidad"]*infelem[,"Precio"]) #Calculo el valor al momento 0 de la cartera

Wabs<-infelem[,"Cantidad"]*infelem[,"Precio"] #Calculo el vector de mis asignaciones de capital

W<-Wabs/W0 #Calculo el vector de ponderaciones

#Calculamos la varianza de la cartera

vzacart<-t(as.matrix(W)) %*% as.matrix(varcv) %*% as.matrix(W)

desvcart<-sqrt(vzacart)

vzaper<-vzacart*per/252 #Calculamos la varianza para el período dado

desvper<-sqrt(vzaper) #Ídem con el desvío

vectordesvper<-sqrt(desv^2*per/252) #Tenemos los desvíos para todos los elementos de la cartera

VaRind<-(-1)*qnorm(alfa)*Wabs*vectordesvper #Calculamos el VaR de cada activo

View(varind)

VaRdiv<-(-1)*qnorm(alfa)*sum(Wabs)*desvper

covconportafolio<-varcv%*%W #En este punto extrajimos las covarianzas anuales.

covconportafolioper<-covconportafolio*(per/252)

betas<-covconportafolio*(per/252)/as.double(vzaper) #Ahora calculamos los betas entre los activos y la cartera para el período dado. No sé por qué tengo que forzar que lo lea como "de doble precisión".

VaRmarg<-betas%*%(VaRdiv/W0) #Aquí hemos extraido el vector con los valores de VaR marginal

porcentajesVaRdescompuesto<-betas*W #VaRdescompuesto es la castellanización más coherente que he lucubrado para "Component VaR"

VaRdescompuesto<-porcentajesVaRdescompuesto*as.double(VaRdiv) #No entiendo por qué R me arma problemas, pongo typeof(VaRdiv) y me dice que es "double"

#Ahora hay que armar la tabla para que todo quede prolijo

tabula<-data.frame(
 # Acciones=LETTERS(1:3),
  Beta=betas,
  VaRMg=VaRmarg,
  CVaR=VaRdescompuesto,
  PorcVaR=porcentajesVaRdescompuesto,
  row.names=LETTERS[1:3]
)

View(tabula)