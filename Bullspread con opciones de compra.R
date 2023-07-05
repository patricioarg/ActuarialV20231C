#Ejercicio 2 - Estrategia con opciones y límites de las primas
library(ggplot2)
library(tidyverse)
#a) Bullspread con opciones de venta
#Para diseñar el mismo debemos comprar un call con precio de ejercicio bajo y vender un call con precio de ejercicio alto. Si construyéramos un bullspread con opciones de venta las posiciones deberían ser las opuestas.

precios <- seq(20, 50) #Definimos el rango de precios con el que vamos a trabajar (es posible asignar cualquier otro intervalo, siempre que sea entre números positivos)
k1 = 30  #Valor del menor strike
k2 = 35 #Valor del mayor strike 
prima1 = 3 #Prima correspondiente al menor strike
prima2 = 1.5 #Prima correspondiente al mayor strike

#Valor intrínseco y payoff del call en posic. acreedora
valintrínsecocall1 <- precios - k1 - prima1
payoffcall1 <- pmax(-prima1,valintrínsecocall1)

#Valor intrínseco y payoff del call en posic. deudora
valintrínsecocall2 <- precios - k2 - prima2
payoffcall2 <- pmin(prima2,-valintrínsecocall2)

#Payoff de la estrategia
payoff <- rowSums(cbind(payoffcall1,payoffcall2))

#Ahora creamos una tabla con los datos de los resultados

resultados <- data.frame(cbind(payoffcall1,payoffcall2,payoff))

#Procedemos a crear el gráfico correspondiente

ggplot(resultados, aes(x=precios)) + 
  geom_line(aes(y = payoffcall1, color = "Call acreedor")) + 
  geom_line(aes(y = payoffcall2, color="Call deudor"))+
  geom_line(aes(y=payoff, color = "Payoff")) +
  scale_colour_manual("", 
                      breaks = c("Call acreedor", "Call deudor", "Payoff"),
                      values = c("darkred", "darkblue", "darkgreen")) + ylab("Payoff")+
  ggtitle("Payoff neto - Bullspread con opc. de compra")

#b) Diseño de un bearspread con opciones de venta.
#Tendremos que estar en posición deudora en el put con menor k (k1) y en posición acreedora en el put con el k más alto. Eso significará que, de caer demasiado el precio de la acción, el put en k1 amortiguará las ganancias del put en k2

precios <- seq(20, 50) #Definimos el rango de precios con el que vamos a trabajar (es posible asignar cualquier otro intervalo, siempre que sea entre números positivos)
k1 = 30  #Valor del menor strike
k2 = 35 #Valor del mayor strike 
prima1put = 1.75 #Prima correspondiente al menor strike
prima2put = 2.5 #Prima correspondiente al mayor strike

#Valor intrínseco y payoff del call en posic. acreedora
valintrínsecocall1 <- precios - k1 - prima1
payoffcall1 <- pmax(-prima1,valintrínsecocall1)

#Valor intrínseco y payoff del call en posic. deudora
valintrínsecocall2 <- precios - k2 - prima2
payoffcall2 <- pmin(prima2,-valintrínsecocall2)

#Payoff de la estrategia
payoff <- rowSums(cbind(payoffcall1,payoffcall2))
# Generate a dataframe with the payoffcall1, payoffcall1 and payoff vectors
# in order to plot the strategy payoffs using ggplot
resultados <- data.frame(cbind(payoffcall1,payoffcall2,payoff))

ggplot(resultados, aes(x=precios)) + 
  geom_line(aes(y = payoffcall1, color = "Call acreedor")) + 
  geom_line(aes(y = payoffcall2, color="Call deudor"))+
  geom_line(aes(y=payoff, color = "Payoff")) +
  scale_colour_manual("", 
                      breaks = c("Call acreedor", "Call deudor", "Payoff"),
                      values = c("darkred", "darkblue", "darkgreen")) + ylab("Payoff")+
  ggtitle("Payoff neto - Bullspread con opc. de compra")

#El presente código se realizó tomando como base el de https://financetrain.com/options-strategy-create-bull-call-spread-with-r-language