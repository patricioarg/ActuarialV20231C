#Ejercicio 1 del modelo del primer parcial
#Árbol binomial con una opción de compra

#Primero calculamos u y d

sigma=0.2
plazo=0.25
tlr=0.05
u=exp(sigma*sqrt(plazo))
d=exp(sigma*(-1)*sqrt(plazo))
textou=paste("El valor de u es ",u)
textod=paste("El valor de d es ",d)
print(textou)
print(textod)

#Hasta acá sacamos cómo suben (o bajan) las ramas. Ahora sacamos los valores del árbol

suby=70
su=suby*u
sd=suby*d
suu=su*u
sud=su*d
sdu=sd*u
sdd=sd*d

textoárbol=paste("Los valores de los nodos son: su=",su,", sd=",sd,",suu=",suu," sud=",sud,", sdu=",sdu,", sdd=",sdd,".")
print(textoárbol)

#Ahora armamos el "árbol" (entre comillas, porque en todos los escenarios vale lo mismo) del activo libre de riesgo

bu=bd=exp(tlr*plazo)
buu=bud=bdu=bdd=exp(tlr*2*plazo)

print(paste("El valor del activo libre de riesgo es de ",bu,"en todos los escenarios en el primer período y de ",buu,"en todos los escenarios del segundo período"))

#Ahora vamos por el árbol de la opción de compra

precioejerc=68

if(suu>precioejerc){
  cuu=suu-68
} else {
  cuu=0
}

if(sud>precioejerc){
  cud=sud-68
} else {
  cud=0
}

if(sdu>precioejerc){
  cdu=sdu-68
} else {
  cdu=0
}

if(sdd>precioejerc){
  cdd=sdd-68
} else {
  cdd=0
}

print(paste("los valores de la opción de compra en cada escenario son cuu=",cuu,", cud=",cud,", cdu=",cdu,", cdd=",cdd))

#Sí, ya sé. Alguien que sepa más de programación le encuentra la vuelta y automatiza los 4 últimos nodos del árbol.

#Ahora vamos por las ramas intermedias por eliminación de oportunidades de arbitraje

fiu=(cuu-cud)/(suu-sud)
psiu=(cud-fiu*sud)/buu
cu=fiu*su+psiu*bu
print(paste("Los valores de la rama superior son fiu=",fiu,", psiu=",psiu,", cu=",cu))

#Ahora es cuestión de repetir los cálculos con el nodo bajista

fid=(cud-cdd)/(sud-sdd)
psid=(cud-fid*sud)/bdd
cd=fid*sd+psid*bd
print(paste("Los valores de la rama inferior son fid=",fid,", psid=",psid,", cd=",cd))

#Luego, por retroinducción, tendremos el valor de la opción de compra en el momento cero

fi=(cu-cd)/(su-sd)
psi=(cu-fi*su)/bu
c0=suby*fi+1*psi
print(paste("Los valores del primer nodo son fi=",fi,", psi=",psi,", c0=",c0))