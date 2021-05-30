##Practica 2 
#Autocorrelacion y Estacionaridad 
install.packages("tseries")
library(tseries)
library(data.table)

rm(list = ls())
dev.off()

set.seed(608)
#definimos un ruido blanco 
WN  =rnorm(1000,0,1) #debe tener esperanza 0 y varianza homoscedastica
plot.ts(WN, main = "WN")
#propiedades
mean(WN)
var(WN)
#hasta ahorita sabemos que es una serie estacionaria, porque nosotros la definimos asi, pero con eso bastaria si fuera otra serie?
#la varianza que tenemos es la de toda la serie, pero el problema que tenemos aqui  
#es que no podemos garantizar que cada variable tenga la misma varianza. 

mWN = 0 #definimos una caminata aleatoria. #suma de choques aleatorios. 
#definimos un nuevo vector. 
#en el for lo que estamos haciendo es que: 
# MWN es la media del ruido blanco, 
#nos fijamos desde el 1 hasta 1000 y le agregaremos una nueva observacion al vector de medias, 
#siendo este el promedio desde 1 hasta el i en el que vayamos. 
#esto para ver si la media se estabiliza, que es lo que deberia de pasar
for (i in 1:length(WN)) {mWN[i] = mean(WN[1:i])}
plot(mWN, type = "l")

vWN<-0
for(i in 1:length(WN)){vWN[i]<-var(WN[1:i])}
plot(vWN, type = "l") #al principio la varianza fluctua mucho porque son pocos datos. 
#pero conforme se van haciendo mas datos, la varianza parece estabilizarse. 
#esto ya nos da la pista de que si se podria tratar de una prueba estacionaria. 


GRWT <- par(mfrow = c(1, 3))
plot.ts(WN)
acf(WN, main="ACF RWT", lag.max = 100) #enemos la ACF,
acf(WN, type = "partial", main="PACF RWT", lag.max = 100) #tenemos la PACF.
#¿Como interpretamos esas graficas? 
# nos damos cuenta que tanto aquellos que destacan en ambas graficas 
#destacan de una manera muy irregular, esperariamos que destacaramos siguiendo cierto patron
#el que destaque de una manera muy irregular, destacaron por pura aleatoridad, 
#solamente porque habia muchos aleatorios y por ello, muchos podrian tener cierta correlacion. 

#el unico que si no podemos negar que destaca es el del 0. 
#se trata de un rudio blanco. 
#¿Que pasa con la caminata aleatoria? 
RW = WN
for(t in 2:length(WN)){RW[t] = RW[t-1]+WN[t]}
#propiedades
mean(RW)
var(RW)
#¿Basta con eso? 
dev.off()
#vemos que ambas graficas explotan, en el anterior con el paso del tiempo s3e estabilizaba, 
#mientras mas observaciones agregamos mas grande se hacen los valores
mRW = 0
for (i in 1:length(RW)) {mRW[i] = mean(RW[1:i])}
plot(mRW, type =  "l")
vRW = 0
for (i in 1:length(RW)) {mRW[i] = var(RW[1:i])}
plot(mRW, type =  "l")

#funciones de autocorrelacion 
GRW =  par(mfrow = c(1,3))
plot.ts(RW, main = "RW")
acf(RW, main = "ACF RW", log.max = 100)
acf(RW, type = "partial", main = "PACF RW", log.max=  100)
#que podemos ver de la grafica, que es un modelo autoregresivo de orden 1

#podemos definir tambien una caminata aleatoria con tendencia  
RWT = WN
for (t in 2:length(WN)) { RWT[t] = RW[t-1]+WN[t]+0.5*t}
#vemos que se vuelve a ver obvio, la ACF ya se ve homogenea, la PACF ya solo destaca de una. 
#estamos definiendo al ruido blanco, como lo que paso en el momento anterior + 
#el choque aleatorio presente + la tendencia. 

GRWT = par(mfrow = c(1,3))
plot.ts(RWT)
acf(RWT, main = "ACF RWT", log.max = 100)
acf(RWT, type = "partial", main = "PACF RWT", log.max=  100)

#recordemos que la tendencia, la ACF y la PACF aplican para procesos que ya son extacionarios, 
#la tendencia hace que la serie no sea estacionaria, es decir estamos interpretando 
# atraves de al ACF y la PACF una serie que todavia no es interpretable, sigue estando contaminada. 
#lo que debimos haber hecho fue haberle quitado esa contamiancion





#ADF Test
#¿que pasa cuando le metemos el ruido blanco?
adf.test(WN) #con esto probamos estacionariedad, pruebas adecuadas
#nos dice que el p-value es de 0.01 entonces rechazamos la hipotesis nula, 
#entonces, la alternativa es que el proceso es estacionario. 
#aqui si estariamos probando estacionariedad, y tiene toda la logica porque esta
#es ruido blanco. 

adf.test(WN, k=0) #el k es para ver el orden de rezago, que queremos hacer. 
adf.test(RW, k=0) #este nos dice que no es estacionaria, la caminata aleatoria no lo es. 
adf.test(RWT, k=0)

#definimos vectores
adf_pv_WN<-0
adf_pv_RW<-NA
adf_pv_RWT<-NULL

#aqui obtendremos que lo que vamos a guardar en ese vector, seran los p values 
#que resultan e la spruebas ADF y donde estaremos variando van a ser en los rezagos. 
#compararemos a pedazos, el momento actual con el pasado, con hace dos rezagos, etc
#para ver como se comportan los pvalues de cada uno de esos casos. 
for(i in 1:100){adf_pv_WN[i]<-adf.test(WN, k=i)$p.value}
for(i in 1:100){adf_pv_RW[i]<-adf.test(RW, k=i)$p.value}
for(i in 1:100){adf_pv_RWT[i]<-adf.test(RWT, k=i)$p.value}

adf_pv_WN #vemos los p-values del ruido blanco, todos se rechazarian. 
adf_pv_RW #con la caminata aleatoria, pasa todo lo contrario.
adf_pv_RWT #consistentemente la prueba nos dice que no es estacionario.


#Ljnug-Box Test
#probar si lo que le estabamos dando era ruido blanco.

Box.test(WN) #no rechazamos la hipotesis nula, por ello si es ruido blanco. 

Box.test(WN, type = 'Ljung-Box')
Box.test(RW, type = 'Ljung-Box') #esto no es ruido blanco.
Box.test(RWT, type = 'Ljung-Box') #tampoco es ruido blanco, la caminata con tendencia. 

lb_pv_WN<-0
lb_pv_RW<-NA
lb_pv_RWT<-NULL

for(i in 1:100){lb_pv_WN[i]<-as.numeric(Box.test(WN, type = 'Ljung-Box', lag = i)$p.value)}
for(i in 1:100){lb_pv_RW[i]<-as.numeric(Box.test(RW, type = 'Ljung-Box', lag = i)$p.value)}
for(i in 1:100){lb_pv_RWT[i]<-as.numeric(Box.test(RWT, type = 'Ljung-Box', lag = i)$p.value)}

lb_pv_WN
lb_pv_RW
lb_pv_RWT

#Epílogo

cs = 2*cos(2*pi*1:500/50 + .6*pi) 
w = rnorm(1000,0,1)
dev.off()
plot(cs, type = "l", main=expression(2*cos(2*pi*t/50+.6*pi)))
plot(cs+w, type = "l", main=expression(2*cos(2*pi*t/50+.6*pi) + N(0,1)))
plot(cs+5*w, type = "l", main=expression(2*cos(2*pi*t/50+.6*pi) + N(0,25)))

par(mfrow = c(1,2))
plot.ts(WN, main="WN")
plot(cs+5*w, type = "l", main=expression(2*cos(2*pi*t/50+.6*pi) + N(0,25)))

cs<-cs+5*w

acf(cs, lag.max = 100, main="ACF cs")
acf(cs, type = "partial", main="PACF cs", lag.max = 100)

acf(WN, lag.max = 100, main="ACF WN")
acf(cs, lag.max = 100, main="ACF cs")

acf(WN, type = "partial", main="PACF WN", lag.max = 100)
acf(cs, type = "partial", main="PACF cs", lag.max = 100)

adf_pv_cs<-0
for(i in 1:100){adf_pv_cs[i]<-adf.test(cs, k=i)$p.value}

lb_pv_cs<-0
for(i in 1:100){lb_pv_cs[i]<-as.numeric(Box.test(cs, type = 'Ljung-Box', lag = i)$p.value)}

adf_pv_cs
lb_pv_cs

bp_pv_cs<-0
for(i in 1:100){bp_pv_cs[i]<-as.numeric(Box.test(cs, lag = i)$p.value)}
bp_pv_cs


