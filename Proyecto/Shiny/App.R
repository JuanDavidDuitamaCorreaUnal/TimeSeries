#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::#
#             App - proyecto series de tiempo                #
#             Desarrollado por: Juan Duitama                 #
#                          2024-1                            #
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::#

rm(list =ls())


# Librerías ---------------------------------------------------------------

library(shiny)
library(bslib)
library(bsicons)
library(shinyWidgets)
library(shinydashboard)
library(leaflet)
library(tidyverse)
library(readxl)
library(zoo)
library(dplyr)
library(lubridate)
library(plotly)
library(ggsci)
library(scales)
library(viridis)
library(plotly)
library(forecast)
library(MASS)
library(tidyverse)
library(lubridate)
library(timetk)
library(tibble)
library(zoo)
library(tsibble)
library(feasts)
library(fable)
library(cowplot)
library(astsa)
library(TSstudio)
library(fabletools)
library(TSA)
library(parsnip)
library(rsample)
library(modeltime)
library(tidymodels)
library(lmtest)
library(tseries)
library(urca)
library(uroot)
library(fUnitRoots)
library(aTSA)
library(sarima)
library(tsoutliers)
library(fpp)


Ruta<-"D:/Estadística/Semestres/Semestre 9/Series de Tiempo Univariadas/Shiny/"


# Desempleo ---------------------------------------------------------------


Desempleo <- read_excel(paste0(Ruta,"Datos/Desempleo.xlsx"), skip = 5, n_max = 276)
Desempleo <- subset(Desempleo, select = -`Tasa de ocupación (%)`)
colnames(Desempleo)<-c("AnioMes","TasaDesempleo")
Desempleo$AnioMes<-paste0(Desempleo$AnioMes,"-01")
Desempleo$AnioMes<-as.Date(Desempleo$AnioMes,format = "%Y-%m-%d")
#Objeto ts
DesempleoTS <- ts(rev(Desempleo$TasaDesempleo), start = c(2001, 1), end = c(2023, 12), frequency = 12)


DiferenciaOrd<-diff(DesempleoTS)

DiferTSibl<-as_tsibble(DiferenciaOrd)

#Estimación de la estacionalidad
ajuste_final_models<-DiferTSibl%>%model(
  `Fourier1DesempleoDiff`=ARIMA(value~fourier(K=1)+pdq(0, 0, 0) + PDQ(0, 0, 0)),#1 componente de Fourier
  `Fourier2DesempleoDiff`=ARIMA(value~fourier(K=2)+pdq(0, 0, 0) + PDQ(0, 0, 0)),#2 componente de Fourier
  `Fourier3DesempleoDiff`=ARIMA(value~fourier(K=3)+pdq(0, 0, 0) + PDQ(0, 0, 0)),#3 componente de Fourier
  `Fourier4DesempleoDiff`=ARIMA(value~fourier(K=4)+pdq(0, 0, 0) + PDQ(0, 0, 0)),#4 componente de Fourier
  `DummyDesempleoDiff`=ARIMA(value~season()+pdq(0, 0, 0) + PDQ(0, 0, 0))#Ajuste dummy
)

glance(ajuste_final_models)

Modelo_serie_diff_models<-DiferTSibl%>%left_join(fitted(ajuste_final_models)|>group_by(.model)%>%
                                                   pivot_wider(names_from = .model, values_from = .fitted))


### Suavizamiento exponencial -----------------------------------------------


Train=ts(DesempleoTS[1:221], start = c(2001, 1),frequency = 12)
Test=ts(DesempleoTS[222:276],start=c(2019,6),frequency = 12)

h=1

lserie=length(DesempleoTS)
ntrain=trunc(length(DesempleoTS)*0.80)+1 ##% del datos en el conjunto de entrenamiento es del 80%.
ntrain
time(DesempleoTS)
time(DesempleoTS)[ntrain]###Me entrega la ultima fecha de la posición ntrain
#Partiendo la serie en entrenamiento y test
train=window(DesempleoTS,end=time(DesempleoTS)[ntrain])
test=window(DesempleoTS,start=time(DesempleoTS)[ntrain]+1/12)##1/12 porque es la fracción que corresponde a un mes
length(train)
ntest=length(test)
ntest ##Me define el valor de origins, o de ventanas de rolling.
lserie ### Comparar los valores
fchstepahe=matrix(0,nrow=ntest,ncol=h) ##Crea una Columna para los h-pasos adelante
### verval contiene los verdaderos valores de la serie en el conjunto de prueba con los que se compararán los pronósticos.
verval=cbind(test[1:ntest])
###Observación: Note que que esos son las estimaciones de los parámetros de suavizamiento. Se puede también hacer una grilla de valores para explorar si hay unos valores mejores.
# por ejemplo como sigue:
require(utils)


### ARMA --------------------------------------------------------------------

DesempNSDiff <- DiferenciaOrd-Modelo_serie_diff_models$DummyDesempleoDiff
ARPURODesem=Arima(DesempNSDiff,order=c(1,0,0),include.mean = FALSE)
Arima_automatico_desempleo=forecast::auto.arima(DesempNSDiff,d=0,D=0,max.p=5,max.q=5,start.p=0, start.q=0,seasonal=FALSE,max.order=10,stationary=TRUE,ic="bic",stepwise=FALSE,allowmean = TRUE) 

# Análisis de residuales
residualesARPURO=ARPURODesem$residuals


###Estad?sticas CUSUM
resARPURO=residualesARPURO
cum=cumsum(resARPURO)/sd(resARPURO)
N=length(resARPURO)
cumq=cumsum(resARPURO^2)/sum(resARPURO^2)
Af=0.948 ###Cuantil del 95% para la estad?stica cusum
co=0.09821####Valor del cuantil aproximado para cusumsq para n/2 aprox 130
LS=Af*sqrt(N)+2*Af*c(1:length(resARPURO))/sqrt(N)
LI=-LS
LQS=co+(1:length(resARPURO))/N
LQI=-co+(1:length(resARPURO))/N



### ARIMA -------------------------------------------------------------------



# PIB ---------------------------------------------------------------------


PIB3 <- read_excel(paste0(Ruta,"Datos/PIB.xlsx"), range = "AS18:AS93", col_names = FALSE)
PIB3 <- data.frame('Fecha'=seq.Date(from=as.Date("2005-03-01"),to=as.Date("2023-12-01"),by="quarter"),'PIBtrimestral'=PIB3$...1)
#Objeto ts
PIB3TS <- ts(PIB3, start = c(2005, 1), end = c(2023, 4), frequency = 4)



source(paste0(Ruta,"/ui.R"))
source(paste0(Ruta,"/server.R"))
shinyApp(ui=ui,server = server)
