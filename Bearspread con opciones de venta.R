#b) Diseño de un bearspread con opciones de venta.
#Tendremos que estar en posición deudora en el put con menor k (k1) y en posición acreedora en el put con el k más alto. Eso significará que, de caer demasiado el precio de la acción, el put en k1 amortiguará las ganancias del put en k2

library(ggplot2)
library(tidyverse)

precios <- seq(20, 50) #Definimos el rango de precios con el que vamos a trabajar (es posible asignar cualquier otro intervalo, siempre que sea entre números positivos)
k1 = 30  #Valor del menor strike
k2 = 35 #Valor del mayor strike 
prima1put = 1.75 #Prima correspondiente al menor strike
prima2put = 2.5 #Prima correspondiente al mayor strike

#Valor intrínseco y payoff del put en k1
valintrínsecoput1 <- k1 - prima1put - precios
payoffput1 <- pmin(prima1put,-valintrínsecoput1)

#Valor intrínseco y payoff del put en k2
valintrínsecoput2 <- k2 - prima2put - precios
payoffput2 <- pmax(-prima2put,valintrínsecoput2)

#Payoff de la estrategia
payoff <- rowSums(cbind(payoffput1,payoffput2))

#Ahora creamos una tabla con los datos de los resultados

resultados <- data.frame(cbind(payoffput1,payoffput2,payoff))

#Procedemos a crear el gráfico correspondiente

ggplot(resultados, aes(x=precios)) + 
  geom_line(aes(y = payoffput1, color = "put acreedor")) + 
  geom_line(aes(y = payoffput2, color="put deudor"))+
  geom_line(aes(y=payoff, color = "Payoff")) +
  scale_colour_manual("", 
                      breaks = c("put acreedor", "put deudor", "Payoff"),
                      values = c("darkred", "darkblue", "darkgreen")) + ylab("Payoff")+
  ggtitle("Payoff neto - Bearspread con opc. de venta")

El presente código se realizó tomando como base el de https://financetrain.com/options-strategy-create-bull-call-spread-with-r-language