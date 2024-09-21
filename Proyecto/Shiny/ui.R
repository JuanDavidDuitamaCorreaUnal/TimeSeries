ui <- page_navbar(title="Series de tiempo univariadas 2024-1",
                  underline=T,
                  id='nav',
                  theme = bs_theme(version = 5,
                                   preset = 'zephyr',
                                   bg = "#fff",
                                   fg = "#000",
                                   primary = "#FF0000",
                                   secondary = "#00008B",
                                   success = "#EEC900"),
                  #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
                  # 0.Introducción ------------------------------------------------------------
                  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
                  
                  nav_panel(
                    title = "Series",
                    br(),
                    br(),
                    br(),
                    
                    # Fila de gráficos
                    fluidRow(
                      column(6, plotlyOutput("GraficaDesempleo")),
                      column(6, plotlyOutput("GraficaPIB"))
                    ),
                    # Pie de página
                    div(
                      strong("Realizado por: John Anderson Guarín - Ander Steven Cristancho - Juan David Duitama"),
                      style = "
                            position: fixed;
                            bottom: 0;
                            width: 100%;
                            background-color: #f8f9fa;
                            text-align: center;
                            padding: 10px;
                            font-size: 14px;"
                    )
                  ),

                  #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
                  # 1. Tasa mensual de desempleo ------------------------------------------------------------
                  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
                  nav_menu(title="Tasa de desempleo mensual",
                           #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
                           ## 1.1 Análisis descriptivo ------------------------------------------------------------
                           #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
                           nav_panel(title="Análisis descriptivo",
                                     
                                       fluidRow(
                                         column(12,card(full_screen = T,
                                                        plotlyOutput("GraficaDesempleox"))),
                                         column(6,card(full_screen = T,
                                                       card_header("Transformación de Box-Cox",
                                                                   popover(
                                                                     bsicons::bs_icon("bricks",class = "ms-auto"),
                                                                     sliderInput("Lambda_Desemp",label = "Seleccione un Lambda",min = -3,max=3,value=-1.3,step=0.1),
                                                                   ),class = 'd-flex align-items-center gap-1'),
                                                       plotlyOutput("Desempleo_BoxCox")
                                                       )),
                                         column(6,navset_card_tab(
                                           full_screen = TRUE,
                                           title = "Extracción de tendencia",
                                           nav_panel("Diferencia Ordinaria",plotlyOutput("Tend_Diff_ord_Desemp")),
                                           nav_panel("Regresión lineal",plotlyOutput("Tend_Lineal_Desemp")),
                                           nav_panel("No paramétrico",plotlyOutput("Tend_No_Param_Desemp")),
                                           nav_panel("Promedio móvil",plotlyOutput("Tend_Prom_mov_Desemp"))
                                         )),
                                         column(6,card(card_header("Gráfico de retardos (Diferencia Ordinaria)"),full_screen = T,plotOutput("Retardos_Desemp"))),
                                         column(6,card(card_header("Autocorrelación parcial (Diferencia Ordinaria)"),full_screen = T,plotOutput("pacf_Desemp"))),
                                         column(6,navset_card_tab(
                                           full_screen = TRUE,
                                           title = "Detección de estacionalidad",
                                           nav_panel("Mapa de calor",plotlyOutput("Detec_Est_Desem_heatmap")),
                                           nav_panel("Subseries mensuales",plotOutput("Detec_Est_Desem_subser")),
                                           nav_panel("Boxplots",plotlyOutput("Detec_Est_Desem_boxplo"))
                                         )),
                                         column(6,card(full_screen=T,card_header("Estimación de la estacionalidad"),plotlyOutput("Varias_Estacional_Desemp"))),
                                         column(6,card(full_screen=T,card_header("Estacionalidad extraída"),plotlyOutput("Extrac_tendencia_Desemp"))),
                                         column(6,card(full_screen = T,card_header("ACF"),plotOutput("ACF_Desemp"))),
                                         column(12,card(full_screen = T,card_header( "División entrenamiento y prueba"),plotlyOutput("Division_Desem")))
                                       )
                                       
                                     ),#Fin nav_panel Análisis descriptivo
                           
                           #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
                           ## 1.2 Aprendizaje automático ------------------------------------------------------------
                           #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
                           nav_panel(title = "Aprendizaje automático",
                                       fluidRow(
                                         column(6,card(full_screen = F,card_header("División"),plotlyOutput("Division_Desem2"))),
                                         column(6,card(full_screen = F,card_header("Árboles"),img(src = "ArbolesDesempleo.png", height = "400px", width = "100%"))),
                                         column(6,card(full_screen = F,card_header("Red Multicapa"),img(src = "MLPDesempleo.png", height = "400px", width = "100%"))),
                                         column(6,card(full_screen = F,card_header("Red Recurrente"),img(src = "RNNDesempleo.png", height = "400px", width = "100%")))
                                       )
                                       
                                     
                             
                           ),#Fin nav_panel Aprendizaje automático
                           
                           #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
                           ## 1.3 Suavizamiento exponencial ------------------------------------------------------------
                           #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
                           nav_panel(title = "Suavizamiento exponencial",
                                         card(full_screen = T,
                                             card_header("Suavizamiento Exponencial",
                                                         popover(
                                                           bsicons::bs_icon("bricks",class = "ms-auto"),
                                                           bslib::input_switch("ETS_Default","Modificar parámetros",F),
                                                           shiny::conditionalPanel(
                                                             condition = "input.ETS_Default == true",
                                                             sliderInput("Alpha_ETS_Desem",label = "Alpha (Nivel)",min = 0,max=1,value=0.3702011,step=0.01),
                                                             sliderInput("Beta_ETS_Desem",label = "Beta (Tendencia)",min=0,max=1,value=0.03144839,step=0.01),
                                                             sliderInput("Gamma_ETS_Desem",label = "Gamma (Estacionalidad)",min=0,max=1,value=0.3471283,step=0.01)
                                                           ),title = "Parámetros",
                                                           placement = "bottom"
                                                         ),class = 'd-flex align-items-center gap-1'),
                                              plotlyOutput("TestSuavExp_Desem"))
                                     

                                     ),#Fin nav_panel Modelos estadísticos
                           #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
                           ## 1.4 Modelos ARMA ------------------------------------------------------------
                           #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
                           nav_panel(title = "ARMA",
                                     fluidRow(
                                       column(12,card(full_screen = T,card_header("Serie"),plotlyOutput("ARMA_Est_Desem"))),
                                       column(6,navset_card_tab(
                                         full_screen = T,
                                         title = "Identificación del modelo",
                                         nav_panel("ACF",plotOutput("ACF_ARMA_Desem")),
                                         nav_panel("PACF",plotOutput("PACF_ARMA_Desem"))
                                       )),
                                       column(6,card(full_screen = F,card_header("Estimación"),verbatimTextOutput("AR(1)_Desem"))),
                                       column(6,card(full_screen = T,card_header("Residuos"),plotlyOutput("AR1_Resid_Desem"))),
                                       column(6,navset_card_tab(
                                         full_screen = T,
                                         title = "Análisis de residuales",
                                         nav_panel("ACF residuos",plotOutput("ACF_AR1_Desem")),
                                         nav_panel("PACF residuos",plotOutput("PACF_AR1_Desem"))
                                       )),
                                       column(6,card(full_screen = F,card_header("Tests"),verbatimTextOutput("TestsAR1_Desem"))),
                                       column(6,navset_card_tab(
                                         full_screen = T,
                                         title = "Estadísticas",
                                         nav_panel("CUSUM",plotOutput("Cusum_AR1_Desem")),
                                         nav_panel("CUSUMSQ",plotOutput("Cusumq_AR1_Desem"))
                                       ))
                                     )
                                     ),#Fin nav_panel ARMA
                           #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
                           ## 1.5 Modelos ARIMA ------------------------------------------------------------
                           #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
                           nav_panel(title = "ARIMA",
                                     fluidRow(
                                       column(12,card(full_screen = T,card_header("Serie"),plotlyOutput("Serie_inicial_ARIMA"))),
                                       column(6,navset_card_tab(
                                         full_screen = T,
                                         title = "Diferenciación",
                                         nav_panel("Primera Prueba", div(style = "height: 300px; overflow-y: scroll;", verbatimTextOutput("Pruebas_Raiz1_Desem"))),
                                         nav_panel("Serie diferenciada",plotlyOutput("ARIMA_Diferenciada_Desem")),
                                         nav_panel("Segunda Prueba", div(style = "height: 300px; overflow-y: scroll;", verbatimTextOutput("Pruebas_Raiz2_Desem")))
                                       )),
                                       column(6,navset_card_tab(
                                         full_screen = T,
                                         title = "Identificación del modelo",
                                         nav_panel("ACF",plotOutput("ACF_serie_difer_ARIMA_Desem")),
                                         nav_panel("PACF",plotOutput("PACF_serie_difer_ARIMA_Desem")),
                                         nav_panel("Ajuste",div(style = "height: 300px; overflow-y: scroll;", verbatimTextOutput("Ajuste_ARIMA_Desemp")))
                                       )),
                                       column(6,navset_card_tab(
                                         full_screen = T,
                                         title = "Análisis de residuos",
                                         nav_panel("Residuos",plotlyOutput("Residuos_ARIMA_Desem")),
                                         nav_panel("ACF",plotOutput("ACF_ARIMA_Desem")),
                                         nav_panel("PACF",plotOutput("PACF_ARIMA_Desem")),
                                         nav_panel("Tests",div(style = "height: 300px; overflow-y: scroll;", verbatimTextOutput("TestsARIMA_Desem"))),
                                         nav_panel("CUSUM",plotOutput("Cusum_ARIMA_Desem")),
                                         nav_panel("CUSUMSQ",plotOutput("Cusumq_ARIMA_Desem"))
                                       )),
                                       column(6,card(full_screen = T,card_header("Pronósticos"),plotOutput("Pronosticos_ARIMA_desem")))
                                     )
                                     ),#Fin nav_panel ARIMA
                           #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
                           ## 1.6 Modelos SARIMA ------------------------------------------------------------
                           #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
                           nav_panel(title = "SARIMA",
                                     fluidRow(
                                       column(12,card(full_screen = T,card_header("Serie"),plotlyOutput("Serie_inicial_SARIMA"))),
                                       column(6,navset_card_tab(
                                         full_screen = T,
                                         title = "Identificación del modelo",
                                         nav_panel("Diferencia ordinaria",plotlyOutput("SARIMA_Diferenciada_Desem")),
                                         nav_panel("Diferencia estacional",plotlyOutput("SARIMA_Diferenciada_est_Desem")),
                                         nav_panel("Subseries",plotOutput("Subseries_SARIMA_Desem")),
                                         nav_panel("ACF",plotOutput("ACF_serie_difest_SARIMA_Desem")),
                                         nav_panel("PACF",plotOutput("PACF_serie_difest_SARIMA_Desem"))
                                       )),
                                       column(6,navset_card_tab(
                                         full_screen = T,
                                         title = "Estimación del modelo SARIMA(0,1,0)(5,1,1)",
                                         nav_panel("Estimación",div(style = "height: 300px; overflow-y: scroll;", verbatimTextOutput("Primer_Estim_SARIMA_Desem"))),
                                         nav_panel("Residuales",plotlyOutput("Primer_Residuos_SARIMA_Desem")),
                                         nav_panel("ACF",plotOutput("ACF_Resid1_SARIMA_Desem")),
                                         nav_panel("PACF",plotOutput("PACF_Resid1_SARIMA_Desem")),
                                         nav_panel("Tests",div(style = "height: 300px; overflow-y: scroll;", verbatimTextOutput("Test_Resid1_SARIMA_Desem1"))),
                                         nav_panel("CUSUM",plotOutput("Cusum_SARIMA1_Desem")),
                                         nav_panel("CUSUMSQ",plotOutput("Cusumq_SARIMA1_Desem"))
                                       )),
                                       column(6,navset_card_tab(
                                         full_screen = T,
                                         title = "Ajuste del nuevo modelo SARIMA(1,1,2)(2,1,1)",
                                         nav_panel("Outliers",div(style = "height: 300px; overflow-y: scroll;", verbatimTextOutput("Outliers_SARIMA_Desem"))),
                                         nav_panel("Estimación",div(style = "height: 300px; overflow-y: scroll;", verbatimTextOutput("Coef_defini_SARIMA_Desem"))),
                                         nav_panel("Residuales",plotlyOutput("Segund_Residuos_SARIMA_Desem")),
                                         nav_panel("ACF",plotOutput("ACF_supremo_Desem")),
                                         nav_panel("PACF",plotOutput("PACF_supremo_Desem")),
                                         nav_panel("Tests",div(style = "height: 300px; overflow-y: scroll;", verbatimTextOutput("Tests_Definitivos_Desem"))),
                                         nav_panel("CUSUM",plotOutput("CUSUM_SARIMA_definitivo_Desem")),
                                         nav_panel("CUSUMSQ",plotOutput("CUSUMSQ_SARIMA_definitivo_Desem"))
                                       )),
                                       column(6,card(full_screen = T,card_header("Predicciones"),plotlyOutput("Rolling_definitivo_SARIMA_Desem")))
                                     )),#Fin nav_panel SARIMA
                           #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
                           ## 1.7 Resumen con todos los MSE ------------------------------------------------------------
                           #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
                           nav_panel(title = "Resumen MSE",
                                     fluidPage(
                                       tags$style(type = "text/css", "
    #model_table {
      height: 100vh;   /* Altura total de la ventana */
      overflow-y: auto; /* Habilitar scroll vertical si es necesario */
    }
  "),
                                       DTOutput("model_table")
                                     ))#Fin nav_panel Resumen de los MSE
                    
                  ),#Fin nav_menu Tasa del desempleo
                  
                #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
                # 2. PIB trimestral ------------------------------------------------------------
                #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
                nav_menu(title="PIB trimestral",
                         #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
                         ## 2.1 Análisis descriptivo ------------------------------------------------------------
                         #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
                         nav_panel(title="Análisis descriptivo",
                                   layout_sidebar(
                                     sidebar = sidebar(
                                       selectInput(inputId = "AnioPIB_Desc",
                                                   label = "Seleccione el año desde donde desea visualizar las series",
                                                   choices = 2001:2023,
                                                   selected = 2001,
                                                   multiple = FALSE),
                                       selectInput(inputId = "MesPIB_Desc",
                                                   label = "Mes:",
                                                   choices = c("Enero","Febrero","Marzo","Abril","Mayo","Junio","Julio","Agosto","Septiembre","Octubre","Noviembre","Diciembre"),
                                                   selected = "Enero",
                                                   multiple = FALSE),
                                       width = 500,
                                       open = TRUE,
                                       fluid =T
                                     ),
                                     position = "left",
                                     fillable = T,
                                     fluidRow(
                                       column(6,),
                                       column(6,),
                                       column(6,),
                                       column(6,)
                                     )
                                     
                                   )),#Fin nav_panel Análisis descriptivo
                         
                         #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
                         ## 2.2 Aprendizaje automático ------------------------------------------------------------
                         #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
                         nav_panel(title = "Aprendizaje automático",
                                   layout_sidebar(
                                     position = "left",
                                     fillable = F,
                                     fluidRow(
                                       column(6,),
                                       column(6,),
                                       column(6,),
                                       column(6,)
                                     )
                                     
                                   )
                                   
                         ),#Fin nav_panel Aprendizaje automático
                         
                         #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
                         ## 2.3 Modelos estadísticos ------------------------------------------------------------
                         #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
                         nav_panel(title = "Modelos estadísticos",
                                   layout_sidebar(
                                     sidebar = sidebar(
                                       selectInput(inputId = "AnioPIB_Mod",
                                                   label = "Seleccione el año desde donde desea visualizar las series",
                                                   choices = 2001:2023,
                                                   selected = 2001,
                                                   multiple = FALSE),
                                       selectInput(inputId = "MesPIB_Mod",
                                                   label = "Mes:",
                                                   choices = c("Enero","Febrero","Marzo","Abril","Mayo","Junio","Julio","Agosto","Septiembre","Octubre","Noviembre","Diciembre"),
                                                   selected = "Enero",
                                                   multiple = FALSE),
                                       width = 500,
                                       open = TRUE,
                                       fluid =T
                                     ),
                                     position = "left",
                                     fillable = T,
                                     fluidRow(
                                       column(6,),
                                       column(6,),
                                       column(6,),
                                       column(6,)
                                     )
                                     
                                   )
                         )#Fin nav_panel Modelos estadísticos
                         
                )
                  

                  
                  )#Fin del UI