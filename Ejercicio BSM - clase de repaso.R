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
paste0("Debo comprar ",compra0,"acciones a $",s0," cada una, para ACÁ SIGUE ALGO deuda al ",r*100, "%anual (cap. continua) por $", deuda0)

#d)
s1=72
dt=1/48 #Supongo que todos los meses son de 4 semanas. ¡Tanto nos repitieron el mnemotécnico de "30 días tiene noviembre junto a abril, junio y septiembre; de 28 sólo hay uno; los demás, de 31" para después simplificar todo!
v1=v0-dt
d11=(log(s1/k)+(r+.5*sigma^2)*v1)/(sigma*sqrt(v1))
delta1=pnorm(d11)
posic1=round(delta1*qs,0)
compra1=posic1-posic0 #Estoy calculando el delta de la posición. Lo que compro entre 1 y 0 es lo que tengo en 1 menos lo que tenía en 0. Tiene un parecido con lo que en contabilidad sería una diferencia de inventario. Por supuesto que si lo que tengo en 1 es menos de lo que tenía en 0 eso significa que no compré, sino que vendí (que es la operación comercial opuesta a vender).
deuda1nueva=compra1*s1
deuda0capitaliz=round(deuda0*exp(r*dt),2)
deuda1=deuda1nueva+deuda0capitaliz
