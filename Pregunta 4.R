#EXAMEN 
library(tseries)
library(data.table)
library(zoo)
library(readxl)
library(lmtest)
library(stats4)
library(strucchange)
library(FinTS)
Serie = read.csv("Series.csv")
#Vemos que de la serie original, todos los datos estan en formato numerico. 
Serie1 = Serie$Y5
ts1 = as.ts(Serie1)
plot(ts1,type="l", xlab="X", ylab="Y")
#vemos que tiene una varianza entre -2 y 5, eso nos habla de una establizacion 
#en la varianza 
#¿Podria haber una tendencia? 
#posiblemente si, 
#para trabajar con ello, usaremos diferencias. 
tsdl = diff(ts1, 1)
plot(tsdl, type = "l", xlab = "X", ylab = "Y") 
#con esa grafica, vemos que ya se ve centrada en 0, con esto quitamos ya la tendencia
#dejando todo  al parecer estacionario, aunque aun no podemos asegurarlo. 
mean(Serie1)
var(Serie1)

mWN = 0 #definimos una caminata aleatoria
for (i in 1:length(Serie1)) {mWN[i] = mean(Serie1[1:i])}
plot(mWN, type = "l", main = "Media") 
#podemos observar la media con el paso del tiempo se estabiliza. 

vWN = 0
for(i in 1:length(Serie1)){vWN[i]<-var(Serie1[1:i])}
plot(vWN, type = "l") 
#podemos observar que la varianza con el paso del tiempo tiende a estabilizarse. 
#esto ya nos da pista que podría tratarse de una prueba estacionaria. 


#Usamos ADF test, para checar si es o no estacionario. 
adf.test(tsdl) 
#nos dice que el p-value es de 0.01 entonces rechazamos la hipotesis nula, 
# aceptando entonces la alternativa  que es,  el proceso sea estacionario.

#checamos su respetiva funcion de autocorrelacion. 
par(mfrow = c(1,2))
acf(tsdl, main = "ACF tsdl", log.max = 100)
acf(tsdl, type = "partial", main = "PACF tsdl", log.max=  100)

#MODELOS PROPUESTOS 
# (1) vemos como una primera observacion que ambos tienen como cierto 
#comportamiento infinitamente decreciente, aunque no se ajuste demasiado. 
#como primero modelo, sugerimos un 
ARIMA_1_1_1 <- arima(tsdl, order = c(1,1,1))
ARIMA_1_1_1 

coeftest(ARIMA_1_1_1)
#con  esto podemos checar que ambos parametros no son 0, es correcto 
#estimar esos parametros, son significativos

#con esto podemos ver que si es un buen modelo. 

#validacion de este supuesto 
resid<-ARIMA_1_1_1$residuals ## Obtenemos los residuales
resid
#¿como deberias de ser los residuales? 
#deberian de ser muy pequeños, el supuesto es que los residuales son ruido blanco
#y que no estan correlacionados, no hay correlacion entre ellos,
#los residuales pueden ser ruido blanco sin ser residuales. 
#vemos que hay algunos residuos que no son nada significativos, por ende, 
#no es un buen modelo este. 

#(2) AMA 
MA_4 = arima(tsdl, order = c(0,0,4))
MA_4
coeftest(MA_4)

#Con esto vemos que los primeros todos los rezagos son significativos, 
#esto nos da buena señal para modelar con dicha propuesta. 

#criterios de seleleccion 
AIC(MA_4)
BIC(MA_4)
#Validacion del modelo 

resid1<-MA_4$residuals 
resid1

plot(resid1)
hist(resid1)

#vemos que es buena idea modelar con dicha propuesta. 
#hacemos el pronostico 
fore<-predict(MA_4, n.ahead = 24) 
ts.plot(tsdl, exp(fore$pred), main="Pronóstico")
U <- exp(fore$pred)+exp(fore$se) 
L <- exp(fore$pred)-exp(fore$se)
xx <- c(time(U), rev(time(U))) 
yy <- c(L, rev(U))
polygon(xx, yy, border = 8, col = gray(.6, alpha = .2))

