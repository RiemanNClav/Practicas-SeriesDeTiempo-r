##Universidad Nacional Autónoma de México
##Facultad de Estudios Superiores Acatlán
##Licenciatura en Actuaría
##Series de Tiempo
##Semestre 2020-II

##Practica 3

##Metodología de Box-Jenkins

##Retomando lo hecho en la práctica 1

#Importar datos
install.packages("zoo")
install.packages("strucchange")
install.packages("sandwich")
install.packages("FinTS")
library(zoo)
library(tseries)
library(readxl)
library(lmtest)
library(stats4)
library(strucchange)
library(FinTS)

rm(list=ls())
dev.off()

setwd("/Users/Arturo1/Dropbox/UNAM/2021/2021-II/Series de Tiempo/Practicas/P1 20210313")

WN <- rnorm(1000,0,1)
base <- read.csv("Consulta_20210309-000212894.csv", skip = 18,  col.names = c("fecha","nivel"))
base$nivel <- as.numeric(base$nivel)
base$fecha <- as.Date(base$fecha,"%d/%m/%Y")

ts1 <- ts(base$nivel,start = c(1970,1), frequency = 12)

#Nivel

plot.ts(ts1, xlab="Fecha", ylab="INPC", main = "Inflación anual en México", sub = "Enero 1970-Enero 2021")

##¿Basta con eso?

G1 <- par(mfrow = c(1, 3))
plot.ts(ts1, xlab="Fecha", ylab="INPC", main = "Inflación anual en México", sub = "Enero 1970-Enero 2021")
acf(ts1, lag.max = 36, main="ACF INPC", xlab="Rezago")
acf(ts1, type = "partial", lag.max = 36, main="PACF INPC", xlab="Rezago")

##¿Es estacionario, tiene algún tipo de tendencia?

##Dado que el rango en el que varian los datos es grande, la varianza podría ocultar una tendencia

##Paso 1: Estabilizar la varianza a través de una transformación
##Logaritmo

tsl <- log(ts1)
dev.off()
plot.ts(log(ts1), xlab="Fecha", ylab="INPC", main = "Inflación anual en México (ln)", sub = "Enero 1970-Enero 2021")

##¿Basta con eso?

dev.off()
G2 <- par(mfrow = c(1, 3))
plot.ts(log(ts1), xlab="Fecha", ylab="lnINPC", main = "Inflación anual en México (ln)", sub = "Enero 1970-Enero 2021")
acf(log(ts1), lag.max = 36, main="ACF lnINPC", xlab="Rezago")
acf(log(ts1), type = "partial", lag.max = 36, main="PACF lnINPC", xlab="Rezago")

##Se observa una posible tenedencia, por lo que se aplicará una 

#Logaritmo con primeras diferencias

tsdl <- diff(log(ts1),1)
dev.off()
plot.ts(tsdl, xlab="Fecha", ylab="INPC", main = "Inflación anual en México (ln)", sub = "Enero 1970-Enero 2021")

##¿Basta con eso?

dev.off()
G3 <- par(mfrow = c(1, 3))
plot.ts(tsdl, xlab="Fecha", ylab="dif(lnINPC)", main = "Inflación anual en México (dif ln)", sub = "Enero 1970-Enero 2021")
acf(tsdl, lag.max = 36, main="ACF dif(lnINPC)", xlab="Rezago")
acf(tsdl, type = "partial", lag.max = 36, main="PACF dif(lnINPC)", xlab="Rezago")

##Modelos Propuestos

##Tanto la ACF como la PACF se vuelven infinitamente decrecientes después del primer rezago, por lo que un primer modelo sugerido es un

##ARIMA(1,1,1)

ARIMA_1_1_1 <- arima(log(ts1), order = c(1,1,1)) #el segundo parametro es la cantidad de diferencias.
#esto dice estimame una serie ARIMA 
#sobre la serie log(S1) 

ARIMA_1_1_1 

coeftest(ARIMA_1_1_1) #con esto vemos que parametros son significativos, 
#¿como podemos interpretarla?, la hacemos sobre el p-value, la prueba T - student. 
#prueba de significancia estadistica, si los parametros son significativamente distintos 
#de cero, que el parametro bien podría ser cero, 
#el primero nos dice que no es 0, que esta bien que estimemos ese parametro. 
#vemos que el MA(1) no es significativo. 


##¿Es un buen modelo? (Si, No, ¿Por qué?)
##¿Propuestas para mejorarlo?...

##-------

##En la ACF y PACF se observan también rezagos significativos cada 12 periodos, lo que nos hace pensar en un modelo estacional

##SARIMA(1,1,1)_(12,0,12)

SARIMA_1_1_1_12_0_12 <- arima(log(ts1), order = c(1,1,1), seasonal = list(order = c(1,0,1), period = 12))

SARIMA_1_1_1_12_0_12

coeftest(SARIMA_1_1_1_12_0_12)

##¿Es un buen modelo? (Si, No, ¿Por qué?)
##¿Propuestas para mejorarlo?...

##-------

##Un último SARIMA(2,1,1)_(0,0,12)

SARIMA_2_1_1_0_0_12 <- arima(log(ts1), order = c(2,1,1), seasonal = list(order = c(0,0,1), period = 12))

SARIMA_2_1_1_0_0_12

coeftest(SARIMA_2_1_1_0_0_12)

##¿Es un buen modelo? (Si, No, ¿Por qué?)
##¿Mejor o peor que los anteriores?...

##Criterios de selección

AIC(SARIMA_2_1_1_0_0_12)
BIC(SARIMA_2_1_1_0_0_12)

##Validación de supuestos

resid<-SARIMA_2_1_1_0_0_12$residuals ## Obtenemos los residuales
#¿como deberias de ser los residuales? 
#deberian de ser muy pequeños, el supuesto es que los residuales son ruido blanco
#y que no estan correlacionados, no hay correlacion entre ellos, si son normales, 
#entonces se cumple, pero la normalidad no es exactamente lo que se busca. 
#los residuales pueden ser ruido blanco sin ser residuales. 

dev.off()

plot.ts(resid)

hist(resid)
hist(resid, freq = FALSE)
lines(density(resid), col = "red")

dev.off()
#vemos que en esas graficas, se ve como ruido blanco, 
#hay algunos que destacan, 
RP <- par(mfrow = c(1, 3))
plot.ts(resid, main="Residuales")
acf(resid, lag.max = 100, main="ACF Residuales")
acf(resid, type = "partial", main="PACF Residuales", lag.max = 100)


##ADF Test
#
adf_pv_resid<-NULL
for(i in 1:100){adf_pv_resid[i]<-adf.test(resid, k=i)$p.value}
adf_pv_resid

##Ljnug-Box Test

lb_pv_resid<-NULL
for(i in 1:100){lb_pv_resid[i]<-as.numeric(Box.test(resid, type = 'Ljung-Box', lag = i)$p.value)}
lb_pv_resid

##Pronóstico

fore<-predict(SARIMA_2_1_1_0_0_12, n.ahead = 24) 

dev.off()
ts.plot(ts1, exp(fore$pred), xlim=c(2018,2022), ylim=c(0,8), main="INPC - Pronóstico")
U <- exp(fore$pred)+exp(fore$se) 
L <- exp(fore$pred)-exp(fore$se)
xx <- c(time(U), rev(time(U))) 
yy <- c(L, rev(U))
polygon(xx, yy, border = 8, col = gray(.6, alpha = .2))

## Puntos de quiebre y cambio estructural

Chow <- Fstats(ts1~1)
tsC <- Chow$Fstats
sctest(Chow)

tsbp <- breakpoints(ts1~1)
tsdt <- breakdates(tsbp)
icbp <- confint(tsbp)

plot.ts(ts1, xlab="Fecha", ylab="INPC", main = "Inflación anual en México", sub = "Enero 1970-Enero 2021")
lines(tsbp)
lines(icbp)

sc <- efp(ts1 ~ 1, data = ts1, type = "Score-CUSUM")
plot(sc, functional = NULL, xlab="Fecha")

ts2 <- window(ts1, 2015, c(2019,12))
sc2 <- efp(ts2 ~ 1, data = ts2, type = "Score-CUSUM")
plot(sc2, functional = NULL, xlab="Fecha")

## Hetroscedásticidad

resid2 <- resid^2
hist(resid2, freq = F)
lines(density(resid2))

dev.off()
RP2 <- par(mfrow = c(1, 3))
plot.ts(resid2, main="Residuales 2")
acf(resid2, lag.max = 100, main="ACF Residuales2")
acf(resid2, type = "partial", main="PACF Residuales2", lag.max = 100)

lb_pv_resid2<-NULL
for(i in 1:100){lb_pv_resid2[i]<-as.numeric(Box.test(resid2, type = 'Ljung-Box', lag = i)$p.value)}
lb_pv_resid2

Atest <- ArchTest(resid2)

ARCHm <- garch(ts1, order = c(0,1))

summary(ARCHm)

