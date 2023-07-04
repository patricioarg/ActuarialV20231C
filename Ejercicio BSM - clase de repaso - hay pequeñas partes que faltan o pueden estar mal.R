library(tidyverse)
rm(list=ls())

#Parámetros
r=.08
s0=75
mu=.2
sigma=.3

k=78
v0=2/12
qs=1000

#Descomponemos la fórmula de Black-Scholes-Merton

d1=(log(s0/k)+(r+.5*sigma^2)*v0)/(sigma*sqrt(v0)) 
d2=d1-sigma*sqrt(v0)
primacall=s0*pnorm(d1)-k*exp(-r*v0)*pnorm(d2)
contratocall=primacall*qs #Mi contrato (o, visto de otra manera, mi posición) es el precio (prima) por cantidad de unidades adquiridas

nsim=1000
simulación=tibble(
  epsilonvto=rnorm(nsim),
  st=s0*exp((r-.5*sigma^2)*v0+sigma*sqrt(v0)*ALGOCORROBORARENLABIBLIOGRAFÍA),
  Payoutt=map(st,function(x) max(x-k,0)) %*% unlist()
  )

mean(simulación$st)
s0*exp(r*v0)

primacallsim=mean(simulación$payoutt)
contratocallsim=primacallsim*qs

#c)
delta0=pnorm(d1)
posic0=round(delta0*qs,0)
compra0=posic0
deuda0=compra0*s0
paste("Debo comprar",compra0,"acciones a $",s0,"cada una para ACÁ SIGUE ALGO", "deuda al",r*100,"%anual (con cap. continua) por $",deuda0)

#d)
s1=72
dt=1/48 #Supongo que todos los meses son de 4 semanas. ¡Tanto nos repitieron el mnemotécnico de "30 días tiene noviembre junto a abril, junio y septiembre; de 28 sólo hay uno; los demás, de 31" para después simplificar todo!
v1=v0-dt
d11=(log(s1/k)+(r+.5*sigma^2)*v1)/(sigma*sqrt(v1))
delta1=pnorm(d11)
posic1=round(delta1*qs,0)
compra1=posic1-posic0 #Estoy calculando el delta de la posición. Lo que compro entre 1 y 0 es lo que tengo en 1 menos lo que tenía en 0. Tiene un parecido con lo que en contabilidad sería una diferencia de inventario. Por supuesto que si lo que tengo en 1 es menos de lo que tenía en 0 eso significa que no compré, sino que vendí (que es la operación comercial opuesta a vender).
deuda1nueva=compra1*s1 #La deuda nueva es la compra (en posiciones, o sea en q) por el precio (s1)
deuda0capitaliz=round(deuda0*exp(r*dt),2) #Capitalizo (por interés continuo) por un espacio de tiempo dt, EN ESTE CASO 1/48
deuda1=deuda1nueva+deuda0capitaliz
paste("Debo tener", posic1,"acciones en cartera. dado que tenía ACÁ SIGUE ALGO",", debo vender",-compra1,"acciones a $",s1,"cada una, SIGUE ALGO","un ingreso de $",-deuda1nueva)
paste("Mi deuda acumulada es $",deuda1,"correspondientes a $","SIGUE ALGO")
paste("capitalizados al",r*100,"%anual (cap. cont.) por un plazo SIGUE ALGO")
paste(round(dt,4),"años (es decir, $",deuda0capitalizada,"), de los cuales se cancelaron $",-deuda1nueva,"obtenidos de la venta de acciones.")
#e)
scamino=c(75,72,70,72,74,75,77,79,80)
sem=seq(0,8)
nsem=lenght(sem)
dt=1/48
t=seq(0,v0,by=dt)
vto=v0-t

d1camino=(log(scamino/k)+(r+.5*sigma^2)*vto)/(sigma*sqrt(vto))
deltacamino=pnorm(d1camino)
posiccamino=round(deltacamino*qs,0)

compracamino=c(posiccamino[1],diff(posiccamino))
ctocamino=compracamino*scamino
valcamino=posiccamino*scamino

deudacamino=rep(NA,length(sem))
intcamino=rep(NA,lenght(sem))
deudacamino[1]=ctocamino[1]
for(i in 2:length(sem)){
  intcamino[i]=deudacamino[i-1]*(exp(r*dt)-1)
  deudacamino[i]=deudacamino[i-1]+intcamino[i]#+Costo_camino y no se lee el resto
}

Tabla=tibble(sem,t,vto,d1camino,deltacamino,posiccamino,compracamino,ctocamino,
             deudacamino,intcamino,valcamino)
Tabla #La imprimo

#Liq. final
(callliqcamino=ifelse(scamino[length(sem)]>k, k*qs,0))
(deudaliqcamino=-deudacamino[length(sem)])
(flujofinalcamino=callliqcamino+deudaliqcamino)
(vpctocamino=flujofinalcamino*exp(-r*v0))

#Comparación
vpctocamino+contratocall

#f) Estrategia delta neutral simulada muchas veces

callentrega=rep(NA,nsim)
liquidodeuda=rep(NA,nsim)
flujofinal=req(NA,nsim)
vpcto=rep(NA,nsim)

tendencia<-(mu-.5*sigma^2)*dt
volat<-sigma*sqrt(dt)

for(j in 1:nsim){
  #j=2
  #Precios simulados
  S=rep(NA,nsem)
  s[1]=s0
  for(i in 2:nsem){
    s[i]=s[i-1]*exp(tendencia+volat*rnorm(1))
  }
  #Estrategia dinám. delta
  d1=(log(s/k)+(r+.5*sigma^2)*vto)/(sigma*sqrt(vto))
  delt=pnorm(d1)
  posic=round(delta,3)*qs
  compraventa=c(posic[1],diff(posic))
  ctos=comrpaventa*s
  vals=posic*s
  
  deuda=rep(NA,nsem)
  int=rep(NA,nsem)
  deuda[1]=ctos[1]
  for(i in 2:nsem){
    int[i]=deuda[i-1]*(exp(r*dt)-1)
    deuda[i]=deuda[i-1]+int[i]+ctos[i]
  }
  #Liq final
  callentrega[j]=ifelse(s[nsem]>k,k*qs,0)
  liquidodeuda[j]=-deuda(nsem)
  flujofinal[j]=callentrega[j]+liquidodeuda[j]
  
}

mean(vpcto)

#Análisis de resultados

primacobrada=contratocall #Costo teórico
resultado=vpcto*primacobrada
(promedio=mean(resultado))
(desvío=sd(resultado))
(desempeño=sd(resultado)/contratocall)
sum(resultado<0)/nsim
hist(resultado) #Crea un histograma
hist(s) #NO ENTIENDO DE DÓNDE SALE
#Intervalo de confianza
conf=c(.025,.975)
orden=nsim*conf
(IC=sort(resultado)[orden])

#g) Cuánto debo cobrar
primacobrada2=2809 #Contrato call - costo teórico - hallado por iteraciones "a mano"
resultado2=vpcto*primacobrada2
(promedio=mean(resultado2))
sum(resultado2>0)/nsim
hist(resultado2)
#h)
k=c(70,72,74,76,78)
primas=c(8,7,4,3.5,2.809)

kusado=k[s] #Yo creo ver un sigo $, pero RStudio me da error en seguida
primausada=prima[s] #Ídem
f=function(x){
  d1=(log(s/kusado)+(r+.5*x^2)*v0)/(x*sqrt(v0))
  d2=d1-x*sqrt(v0)
  return(s0*pnorm(d1)-k*exp(-r*v0)*pnorm(d2))
}
f(0.3) #No sé qué evalúa en 0.3

f1=function(x){
  d1=(log(s0/k)+(r+.5*x^2)*v0)/(x*sqrt(v0))
  return(s0*sqrt(v0))
}

sig0=.3
niter=20
sig=matrix(rep(NA,niter))
sig[1]=sig0
for(i in 2:niter){
  sig[i]=sig[i-1]-(f(sig[i-1])-4.175)/f1(sig[i-1])
}

f(.3) #No sé por qué lo evalúa
sig[niter]
f(sig[niter])#le resta "pri" pero no sé nada más
