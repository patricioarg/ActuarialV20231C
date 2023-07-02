library(tidyverse)
library(quantmod)
library(ggplot2)
library(lubridate)

rm(list=ls())
graphics.off()

s1="AAPL" #Modificando los símbolos (tickers) podemos obtener datos y generar los gráficos de otras compañías
s2="IBM"

fechainic="2023-01-01" #La fecha, en formato año-mes-día, también puede ser modificada sin problemas
fechafin="2023-06-30"

#Utilizamos el símbolo presente en Yahoo! Finance para buscar datos
getSymbols(c(s1,s2), from = fechainic, to = fechafin)

#Creo conjunto de datos con los precios de cierre
#Uso la herramienta nativa de R porque con tibble tenía problemas
corrob<-sum(!index(eval(parse(text=s1)))==index(eval(parse(text=s2)))) #Corroboro que las fechas coincidan, sumo otros controles porque habría problemas con el índice

if(corrob==0){
  print("Las fechas de las cotizaciones son iguales")
}else
{print("Existe alguna diferencia en las fechas de las cotizaciones")}

dacc<-data.frame(index(eval(parse(text=s1))),
                 eval(parse(text = paste(s1,"[,4]"))),
                 eval(parse(text = paste(s2,"[,4]"))))
colnames(dacc)<-c("fechas",s1,s2)

#agrego columna con las fechas (que mi computadora no interpreta como índice, sino como nombres de las filas)

fechas<-rownames(dacc)

#Convertimos los datos en fechas usando el paquete lubridate

fechas<-as.Date(fechas)

#Unimos las fechas a la base de datos

dacc<-data.frame(fechas,dacc)

#Creo gráfico simple

ggplot(dacc)+
  geom_line(aes(x=fechas,y=!!ensym(s1),color=s1))+
  geom_line(aes(x=fechas,y=!!ensym(s2),color=s2))+
  labs(title="Precios de cierre de las acciones",
       x="Fecha",
       y="Precio de cierre",
       color="Acciones")+
  scale_x_date(date_breaks="1 month", date_labels="%b-%y")+
  theme(axis.text.x=element_text(angle=90))