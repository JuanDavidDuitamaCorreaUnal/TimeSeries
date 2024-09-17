server <- function(input,output,session){
  

# Panel principal ---------------------------------------------------------

  # Gráfica del desempleo
  
  output[["GraficaDesempleo"]]<-renderPlotly({
    plot_ly( x = Desempleo$AnioMes, y = Desempleo$TasaDesempleo, type = 'scatter', mode = 'lines',color=I("red3")) %>%
      layout(title = 'Tasa de Desempleo mensual en Colombia',
             xaxis = list(title = 'Mes'))
  })
  # Gráfica del PIB
  output[["GraficaPIB"]]<-renderPlotly({
    plot_ly( x = PIB3$Fecha, y = PIB3$PIBtrimestral, type = 'scatter', mode = 'lines',color=I("red3")) %>%
      layout(title = 'PIB Trimestral en Colombia',
             xaxis = list(title = 'Trimestre'))
  })
  

# Tasa de desempleo -------------------------------------------------------


## 1. Análisis descriptivo -------------------------------------------------
  
  #Muestro de nuevo la serie
  output[["GraficaDesempleox"]]<-renderPlotly({
    plot_ly( x = Desempleo$AnioMes, y = Desempleo$TasaDesempleo, type = 'scatter', mode = 'lines',color=I("red3")) %>%
      layout(title = 'Tasa de Desempleo mensual en Colombia',
             xaxis = list(title = 'Mes'))
  })

### 1.1 Transformación de Box-Cox -----------------------------------------------
  output[["Desempleo_BoxCox"]]<-renderPlotly({
    DesempleoTSBox<-BoxCox(DesempleoTS,lambda = input$Lambda_Desemp)
    plot_ly( x = rev(Desempleo$AnioMes), y = DesempleoTSBox, type = 'scatter', mode = 'lines',color=I("red3")) %>%
      layout(title = 'Tasa de Desempleo en Colombia Transformada',
             xaxis = list(title = 'Mes'))
  })


### 1.2 Extracción de tendencia ---------------------------------------------

  #Regresión lineal
  output[["Tend_Lineal_Desemp"]]<-renderPlotly({
    fitLM <- lm(DesempleoTS~time(DesempleoTS), na.action=NULL)
    DesempNoLM=DesempleoTS-predict(fitLM)#Eliminando la tendencia
    plot_ly( x = seq.Date(from=as.Date("2001-01-01"),to=as.Date("2023-12-01"),by="month"), y = DesempNoLM, type = 'scatter', mode = 'lines',color=I("red3")) %>%
      layout(title = 'Tasa de Desempleo en Colombia con tendencia lineal extraída',
             xaxis = list(title = 'Mes'))
  })
  
  #No paramétrica
  output[["Tend_No_Param_Desemp"]]<-renderPlotly({
    STLextra<-DesempleoTS-smooth_vec(DesempleoTS,span = 0.2, degree = 2)
    plot_ly( x = seq.Date(from=as.Date("2001-01-01"),to=as.Date("2023-12-01"),by="month"), y = STLextra, type = 'scatter', mode = 'lines',color=I("red3")) %>%
      layout(title = 'Tasa de Desempleo con tendencia STL extraída',
             xaxis = list(title = 'Mes'))
  })
  
  #Diferencia ordinaria
  output[["Tend_Diff_ord_Desemp"]]<-renderPlotly({
    DiferenciaOrd<-diff(DesempleoTS)
    plot_ly( x = seq.Date(from=as.Date("2001-02-01"),to=as.Date("2023-12-01"),by="month"), y = DiferenciaOrd, type = 'scatter', mode = 'lines',color=I("red3")) %>%
      layout(title = 'Tasa de Desempleo sin tendencia(Diferencia Ordinaria, lag=1)',
             xaxis = list(title = 'Mes'))
  })
  
  #Promedio móvil
  output[["Tend_Prom_mov_Desemp"]]<-renderPlotly({
    DescomProm=decompose(DesempleoTS)
    ExtraProm=DesempleoTS-DescomProm$trend
    plot_ly( x = seq.Date(from=as.Date("2001-01-01"),to=as.Date("2023-12-01"),by="month"), y = ExtraProm, type = 'scatter', mode = 'lines',color=I("red3")) %>%
      layout(title = 'Tasa de Desempleo con tendencia extraída (Promedio móvil)',
             xaxis = list(title = 'Mes'))
  })
  

### 1.3 Gráfico de retardos -----------------------------------------------------
  output[["Retardos_Desemp"]]<-renderPlot({
    lag1.plot(diff(DesempleoTS), 12,corr=T)
  })

### 1.4 Gráfico de correlación parcial ----------------------------------------
  output[["pacf_Desemp"]]<-renderPlot({
    pacf(diff(DesempleoTS), 12)
  })
  

### 1.5 Detección de estacionalidad ---------------------------------------------
  #Mapa de calor
  output[["Detec_Est_Desem_heatmap"]]<-renderPlotly({
    TSstudio::ts_heatmap(diff(DesempleoTS),title = "Mapa de Calor - Tasa de Desempleo en Colombia(Diferencia Ordinaria)")
  })
  #Subseries
  output[["Detec_Est_Desem_subser"]]<-renderPlot({
    monthplot(diff(DesempleoTS),main="Subseries(Diferencia Ordinaria)")
  })
  
  #Boxplots
  output[["Detec_Est_Desem_boxplo"]]<-renderPlotly({
    tsibbleDesem<-as_tsibble(DesempleoTS)
    tsibbleDesem <- tsibbleDesem %>%
      mutate(index = as.Date(index))
    tsibbleDesem<-tsibbleDesem%>%mutate(Diferencia=value-lag(value))
    ggplotly( tsibbleDesem %>%
      na.omit() %>%
      plot_seasonal_diagnostics(.date_var = index, .value = Diferencia, .feature_set = c("month.lbl"), .geom = "boxplot"))
  })

### 1.6 Estimación de la estacionalidad -----------------------------------------
  output[["Varias_Estacional_Desemp"]]<-renderPlotly({
    
    plot_ly(x = seq.Date(from=as.Date("2001-02-01"),to=as.Date("2023-12-01"),by="month"), y = DiferenciaOrd, type = 'scatter', mode = 'lines', color = I("red3"), name = "SerieDifOrd") %>%
      add_trace(y = Modelo_serie_diff_models$Fourier1DesempleoDiff, mode = 'lines', line = list(color = 'blue'), name = "Fourier1") %>%
      add_trace(y = Modelo_serie_diff_models$Fourier2DesempleoDiff, mode = 'lines', line = list(color = 'green'), name = "Fourier2") %>%
      add_trace(y = Modelo_serie_diff_models$Fourier3DesempleoDiff, mode = 'lines', line = list(color = 'yellow'), name = "Fourier3") %>%
      add_trace(y = Modelo_serie_diff_models$Fourier4DesempleoDiff, mode = 'lines', line = list(color = '#00CD66'), name = "Fourier4") %>%
      add_trace(y = Modelo_serie_diff_models$DummyDesempleoDiff, mode = 'lines', line = list(color = 'purple'), name = "Dummy") %>%  
      layout(title = 'Estimación de la estacionalidad (Diferencia Ordinaria, lag=1)',
             xaxis = list(title = 'Mes'))
  })
 
  #Extracción de la tendencia
  output[["Extrac_tendencia_Desemp"]]<-renderPlotly({
    plot_ly(x = seq.Date(from=as.Date("2001-02-01"),to=as.Date("2023-12-01"),by="month"), y = DiferenciaOrd, type = 'scatter', mode = 'lines', color = I("red3"), name = "SerieDifOrd") %>%
      add_trace(y = DiferenciaOrd-Modelo_serie_diff_models$Fourier1DesempleoDiff, mode = 'lines', line = list(color = 'blue'), name = "Fourier1") %>%
      add_trace(y = DiferenciaOrd-Modelo_serie_diff_models$Fourier2DesempleoDiff, mode = 'lines', line = list(color = 'green'), name = "Fourier2") %>%
      add_trace(y = DiferenciaOrd-Modelo_serie_diff_models$Fourier3DesempleoDiff, mode = 'lines', line = list(color = 'yellow'), name = "Fourier3") %>%
      add_trace(y = DiferenciaOrd-Modelo_serie_diff_models$Fourier4DesempleoDiff, mode = 'lines', line = list(color = '#00CD66'), name = "Fourier4") %>%
      add_trace(y = DiferenciaOrd-Modelo_serie_diff_models$DummyDesempleoDiff, mode = 'lines', line = list(color = 'purple'), name = "Dummy") %>%  
      layout(title = 'Tasa de Desempleo sin tendencia y estacionalidad\n(Diferencia Ordinaria, lag=1)',
             xaxis = list(title = 'Mes'))
  })
  
  #ACF final
  output[["ACF_Desemp"]]<-renderPlot({
    par(mfrow = c(2, 2))
    acf(DiferenciaOrd-Modelo_serie_diff_models$Fourier2DesempleoDiff,lag.max =100,main="Autocorrelación serie sin componentes\n(F2)")
    acf(DiferenciaOrd-Modelo_serie_diff_models$Fourier3DesempleoDiff,lag.max =100,main="Autocorrelación serie sin componentes\n(F3)")
    acf(DiferenciaOrd-Modelo_serie_diff_models$Fourier4DesempleoDiff,lag.max =100,main="Autocorrelación serie sin componentes\n(F4)")
    acf( DiferenciaOrd-Modelo_serie_diff_models$DummyDesempleoDiff,lag.max =100,main="Autocorrelación serie sin componentes\n(Dummy)")
  })
  

### 1.7 Suavizamiento exponencial -----------------------------------------------
  
  output[["Division_Desem"]]<-renderPlotly({
    plot_ly(x = seq.Date(from=as.Date("2001-01-01"),to=as.Date("2019-05-01"),by="month"), y = Train, type = 'scatter', mode = 'lines', color = I("red3"), name = "Entrenamiento") %>%
      add_trace(x =seq.Date(from=as.Date("2019-06-01"),to=as.Date("2023-12-01"),by="month"),y = Test, mode = 'lines', line = list(color = 'blue'), name = "Prueba") %>%
      layout(title = 'División entrenamiento y prueba',
             xaxis = list(title = 'Mes'))
  })
#Estimando el modelo y haciendo el rolling
  Predict_ETS_Desem <- reactive({
    
    if(input$ETS_Default){
      
      for(i in 1:(ntest)){
        x=window(DesempleoTS,end=time(DesempleoTS)[ntrain]+(i-1)/12)
        #print(length(x))
        refit=stats::HoltWinters(x,seasonal="additive",alpha=input$Alpha_ETS_Desem,beta=input$Beta_ETS_Desem,gamma=input$Gamma_ETS_Desem)
        fchstepahe[i,]=as.numeric(forecast::forecast(refit,h=h)$mean)
      }
      errores_pred=verval -fchstepahe ##Observación: debo devolver los pronósticos y los verdaderos valores a la escala original si es necesario.
      ECM=apply(errores_pred^2,MARGIN = 2,mean,na.rm=TRUE) ##Acá se computa la medida de precisión del pronóstico(en este caso ECM).
      RECM=sqrt(ECM) ##Se le saca raíz 
      
      lista_ETS<-list("Predicciones"=fchstepahe,"Errores_Pred"=errores_pred,"MSE"=ECM,"RMSE"=RECM)
      
    }else{
    HWAP_train=stats::HoltWinters(train,seasonal="additive")  
    for(i in 1:(ntest)){
      x=window(DesempleoTS,end=time(DesempleoTS)[ntrain]+(i-1)/12)
      #print(length(x))
      refit=stats::HoltWinters(x,seasonal="additive",alpha=HWAP_train$alpha,beta=HWAP_train$beta,gamma=HWAP_train$gamma)
      fchstepahe[i,]=as.numeric(forecast::forecast(refit,h=h)$mean)
    }
    errores_pred=verval -fchstepahe ##Observación: debo devolver los pronósticos y los verdaderos valores a la escala original si es necesario.
    ECM=apply(errores_pred^2,MARGIN = 2,mean,na.rm=TRUE) ##Acá se computa la medida de precisión del pronóstico(en este caso ECM).
    RECM=sqrt(ECM) ##Se le saca raíz 
    
    lista_ETS<-list("Predicciones"=fchstepahe,"Errores_Pred"=errores_pred,"MSE"=ECM,"RMSE"=RECM)
    
    return(lista_ETS)
    }
  })
  
  #Gráfico de las predicciones
  output[["TestSuavExp_Desem"]]<-renderPlotly({
    plot_ly(x = seq.Date(from=as.Date("2019-06-01"),to=as.Date("2023-12-01"),by="month"), y = Test, type = 'scatter', mode = 'lines', color = I("red3"), name = "TasaDesempleo") %>%
      add_trace(y = Predict_ETS_Desem()[["Predicciones"]], mode = 'lines', line = list(color = 'blue'), name = "Predicciones") %>%
      layout(title = 'Tasa de Desempleo vs Predicciones (Junio 2019-Diciembre 2023)',
             xaxis = list(title = 'Mes'))
  })

### 1.8 ARMA --------------------------------------------------------------------

  output[["ARMA_Est_Desem"]]<-renderPlotly({
    plot_ly(x = seq.Date(from=as.Date("2001-02-01"),to=as.Date("2023-12-01"),by="month"), y = DesempNSDiff, type = 'scatter', mode = 'lines', color = I("red3"), name = "Serie") %>%
      layout(title = 'Serie sin tendencia y estacionalidad\n(Diferencia ordinaria y Dummy)',
             xaxis = list(title = 'Mes'))
  })
  
  output[["ACF_ARMA_Desem"]]<-renderPlot({
    acf(DesempNSDiff,ci.type='ma',lag.max = length(DesempNSDiff)/4)
  })
  
  output[["PACF_ARMA_Desem"]]<-renderPlot({
    pacf(DesempNSDiff,lag.max = length(DesempNSDiff)/4)
  })
  
  output[["AR(1)_Desem"]]<-renderPrint({
    Arima_automatico_desempleo  # El objeto que contiene el modelo ARIMA
  })
  
  output[["AR1_Resid_Desem"]]<-renderPlotly({
    plot_ly(x = seq.Date(from=as.Date("2001-02-01"),to=as.Date("2023-12-01"),by="month"), y = residualesARPURO, type = 'scatter', mode = 'lines', color = I("red3"), name = "Residuales") %>%
      layout(title = 'Residuales Modelo AR(1)',
             xaxis = list(title = 'Mes'))
  })
  
  output[["ACF_AR1_Desem"]]<-renderPlot({
    acf(residualesARPURO)
  })
  
  output[["PACF_AR1_Desem"]]<-renderPlot({
    pacf(residualesARPURO)
  })
  
  output[["TestsAR1_Desem"]]<-renderPrint({
    cat("Test de autocorrelación:\n")
    print(Box.test(residualesARPURO, lag =30 , type = "Ljung-Box", fitdf = 1))
    
    cat("Test de normalidad:\n")
    print(tseries::jarque.bera.test(residualesARPURO) )
  })
  
  output[["Cusum_AR1_Desem"]]<-renderPlot({
    plot(cum,type="l",ylim=c(min(LI),max(LS)),xlab="t",ylab="",main="CUSUM")
    lines(LS,type="S",col="red3")
    lines(LI,type="S",col="red3")
  })
  
  output[["Cusumq_AR1_Desem"]]<-renderPlot({
    plot(cumq,type="l",xlab="t",ylab="",main="CUSUMSQ")                      
    lines(LQS,type="S",col="red3")                                                                           
    lines(LQI,type="S",col="red3")
    
  })
  
  
}