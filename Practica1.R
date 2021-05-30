#Universidad Nacional Autonoma de Mexico
##Facultad de Estudios Superiores Acatlan 
###Licenciatura en Actuaria 
##Series de Tiempo 
##Semestre 2021-II 

##Practica 1 

##Introduccion a R. 
#primer acercamiento 
cs = 2*cos(2*pi*1:500/50 + .6*pi)
w = rnorm(500,0,1)
plot(cs, type = "l", main = expression(2*cos(2*pi*t/50+.6*pi)))

#A esta grafica ya le metimos ruido a la serie, aunque 
#se siga percibiendo la onda del coseno.
plot(+w, type = "l", main = expression(2*cos(2*pi*t/50+.6*pi)+N(0,1)))
 
#Ya no se ve tanto ese comportamiento ondulatorio                                                                       
plot(cs+5*w, type = "l", main = expression(2*cos(2*pi*t/50+.6*pi)+N(0,25)))
#hay veces que no sabemos que hay dentro de una serie de tiempo, 
#por eso es muy importante saber aislar el efecto de los choques aleatorios. 

#importar datos 
library(readxl)
#Datos relacionados a la inflación en Mexico. 
base = read.csv("Consulta_20210309-000212894.csv", skip = 19,
col.names = c("fecha", "nivel"))

#Le vamos a cambiar el tipo de dato. 
#veamos los datos 
str(base)
base$nivel= as.numeric(base$nivel)
base$fecha = as.Date(base$fecha, format = "%d/%m/%Y")


#Crear un objeto de tipo series de tiempo 
#ventaja trabajar de esta forma la base de  datos.
#si queremos convertir toda la base a formato series de tiempo. 
ts1 = as.ts(base) #la fecha las esta marcando mal, eso no sirve tanto. 
ts1 = ts(base$nivel) #aqui solo queremos definir el nivel a ese formato. 
ts1 = ts(base$nivel, start = 1970) #empieza en 1970, pero no los reconoce por años.
ts1 = ts(base$nivel, start = c(1970,1), frequency = 12) #empezaremos en 1970 en el mes1 con frecuencia de 12 meses. 
#lo que obtendremos es lo que queremos, ya tenemos la estructura temporal que queremos. 

#lo que podemos ver con la grafica, es que tiene una varianza muy grande, 
#varianza muy dispar, lo primero en lo que se debe de trabajar es con la varianza. 
#¿Habra una tendencia?, a primera vista, no se ve, pero en muchas 
#veces la varianza encubre una tendencia.
#una buena opcion es primero trabajar con la varianza, para que nos revele 
#si tiene alguna tendencia. 
plot(ts1, type="l", xlab="Fecha", ylab="INPC", main = "Inflacion anual en Mexico", sub = "Enero 1970-Enero 2021")
plot.ts(ts1, xlab="Fecha", ylab="INPC", main = "Inflacion anual en Mexico", sub = "Enero 1970-Enero 2021")

#aplicamos el logaritmo, para analizar la varianza 
#Logaritmo

base$ln <- log(base$nivel) #esto nos genera, a valores grandes los hace pequeños y viceversa
tsl <- log(ts1)
plot.ts(tsl, xlab="Fecha", ylab="INPC", main = "Inflacion anual en Mexico (ln)", sub = "Enero 1970-Enero 2021")
plot.ts(log(ts1), xlab="Fecha", ylab="INPC", main = "Inflacion anual en Mexico (ln)", sub = "Enero 1970-Enero 2021")
#si vemos la comparacion, ahorita la grafica estan entre 1 y 5, eso ya nos habla 
#de una estabilizacion de la varianza. 
#¿Pareciera que hay una tendencia? 
#el punto es que si podria existir una tendencia. 
#¿Como trabajamos con las tendencias? 
#con las diferencias, para eso usaremos la funcion diff. 
tsdl = diff(log(ts1), 1) #diferencia de orden 1, 
#con esta grafica, podemos ver que ya se ve centrado sobre el 0, con eso quitamos la tendencia, dejando todo centrando en 0. 
#de esta manera, esta serie ya se ve estacionaria, aunque aun no lo podemos asegurar.
plot.ts(tsdl, xlab="Fecha", ylab="INPC", main = "Inflacion anual en Mexico (ln)", sub = "Enero 1970-Enero 2021")

par(mfrow=c(3,1))
plot.ts(ts1, xlab="Fecha", ylab="INPC", main = "Inflacion anual en Mexico", sub = "Enero 1970-Enero 2021")
plot.ts(tsl, xlab="Fecha", ylab="INPC", main = "Inflacion anual en Mexico (ln)", sub = "Enero 1970-Enero 2021")
plot.ts(tsdl, xlab="Fecha", ylab="INPC", main = "Inflacion anual en Mexico d(ln)", sub = "Enero 1970-Enero 2021")


