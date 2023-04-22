#En la primera parte crearemos la tabla con tasas spot (continuas y efectivas)

Periodo<-c(1,2,3,4)
Precio<-c(945,890,835,785)
tabla<-data.frame(Periodo,Precio)
tabla<-data.frame(tabla,Precio/1000)
names(tabla)[3]="FD"
tabla<-data.frame(tabla,tabla[3]^(-1))
names(tabla)[4]="FC"
tabla<-data.frame(tabla,log(1/tabla$FD)/Periodo)
tabla<-data.frame(tabla,tabla$FC^(1/Periodo)-1)
names(tabla)[5]="Tasa cont. spot"
names(tabla)[6]="Tasa efva. spot"
View(tabla)

#En la segunda parte crearemos una tabla en base a la anterior para calcular las tasas forward

tabla1=NA
tablafwd<-data.frame(tabla,tabla1,tabla1)
names(tablafwd)[7]="tcontfwd"
names(tablafwd)[8]="tefvafwd"

#Llenamos los datos de las tasas efectivas

tablafwd$tefvafwd[1]=tablafwd$FC[1]/1-1
for(x in 2:4){tablafwd$tefvafwd[x]<-tablafwd$FC[x]/tablafwd$FC[x-1]-1}

#Llenamos los datos de las tasas continuas

tablafwd$tcontfwd<-log(tablafwd$tefvafwd+1)

#Ahora eliminamos las columnas redundantes

tablafwd<-data.frame(tablafwd[1],tablafwd[7],tablafwd[8])

#Ahora cambiamos los nombres de los intervalos y de las variables

names(tablafwd)[1]="Intervalo"
names(tablafwd)[2]="Tasa cont. fwd."
names(tablafwd)[3]="Tasa efva. fwd."
for(x in 1:4){tablafwd$Intervalo[x]=paste(x-1,"-",x)}
View(tablafwd)
