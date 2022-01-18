
# Define UI for application that draws a histogram
shinyUI(fluidPage(navbarPage(title =div(tags$a(img(src="Logo.png", height=40,width=223), href= "https://www.facebook.com/Grupo-de-Energ%C3%ADas-Alternativas-y-Ambiente-Geaa-Espoch-215676138448500/?ref=page_internal"),
                                        style = "position: relative; top: -5px;"), # Navigation bar
                             #windowTitle = "ScotPHO profiles", #title for browser tab
                             theme = shinytheme("darkly"), #Theme of the app (blue navbar)
                             collapsible = TRUE,
                       
                 
                   # Application title
                   #titlePanel("ANALISIS EXPLORATORIO"),
                   
                   # Sidebar with a slider input for number of bins 
                   tabPanel("AED",icon = icon("area-chart"), 
                            setBackgroundColor(
                              color = c("black", "#2171B5"),
                              gradient = "radial",
                              direction = c("top", "left")
                            ),
                            sidebarPanel(
                                tags$style(".well {background-color:#4682B4;}"),
                                p("NAVEGACIÓN"),
                                actionButton("ayuda1",label="Ayuda", icon= icon('question-circle'), class ="down"),
                                #helpText("Cargar una base de datos de acuerdo
                                #al formato estandar: https://example.com"),
                                #fileInput("Carga", h3("Cargar Datos")),
                                selectInput("var_sel","Variable:",
                                            var_names,selected = "Temperatura Ambiental"),
                                #selectInput("st_sel","Estaciones:",
                                #            st_names,selected = "Alao"),
                                selectizeInput("st_sel", "Estaciones", choices =  c("Seleccione las Estaciones" = "", paste(st_names)),
                                               multiple=TRUE, selected = "Alao"),
                                dateRangeInput("fechas",
                                               "Fechas", start = "2019-01-01",
                                               end = "2019-01-07",weekstart = 1,
                                               min = "2014-01-01", max = "2019-12-31",
                                               startview = 'year', language = 'es'),
                                helpText("Intervalo de horas por día"),
                                sliderInput("hora1", "Horas", min = 0, 
                                            max = 23, value = c(0, 23)),
                                checkboxInput("act_avg", label = "Promediar Serie", value = FALSE),
                                conditionalPanel(
                                  condition = "input.act_avg==1",
                                  #h4("Opciones Graficas"),
                                  radioButtons("timeavg", label = "Promedio por",
                                               choices = c("día" = "day", "Semana" = "week", "Mes" = "month",
                                                           "Trimestre" = "quarter", "Año" = "year")),
                                  helpText("Aumente el intervalo de Fechas"),
                                ),
                                
                                #radioButtons("Agrupar1", label = "Promedios Globales",
                                #             choices = list("Hour" = 1, "Week Day" = 2, "Month" = 3,
                                #                            "Year" = 4, "Year Month" = 5, "Year day" = 6), 
                                #             selected = 1)
                                
                                
                            ),
                            
                            mainPanel(
                                tabsetPanel(
                                    tabPanel("Series", 
                                             br(),
                                             p("Series de tiempo de la variable seleccionada registrada por hora y filtradas por fechas"),
                                             #withSpinner(plotOutput("plot")),#grafica ggplotnormal
                                             withSpinner(plotlyOutput("plot")),#plot interactiva
                                             #br(),
                                             #p("Valores Globales"),
                                             #plotOutput("plot2")
                                             #plotlyOutput("plot2")
                                              ), 
                                    tabPanel("Histograma", 
                                             withSpinner(plotlyOutput("Histo1"))
                                             ),
                                    tabPanel("BoxPlot", 
                                             withSpinner(plotlyOutput("boxplot"))
                                             ),
                                    #tabPanel("Correlograma",
                                    #         plotOutput("acf1"),
                                    #         verbatimTextOutput("ljungbox1")),
                                    #tabPanel("Seasonal Decomposition", 
                                    #         plotOutput("desc_ts")
                                    #),
                                    tabPanel("Resumen", 
                                             p("Estadisticos Descriptivos de la variable Seleccionada"),
                                             downloadButton("downloadtable1", "Descargar"),
                                             br(),
                                             withSpinner(dataTableOutput("table1"))
                                             ), 
                                    tabPanel("Datos", 
                                             p("Datos Seleccionados"),
                                             downloadButton("downloadtable2", "Descargar"),
                                             br(),
                                             withSpinner(dataTableOutput("table2"))
                                             ),
                                    id="tabsAED"
                                )
                            )
                   ),
                   #ANALISIS EXPLORATORIO DE DATOS ESPACIALES
                   tabPanel("AEDE", icon = icon("globe"), 
                            sidebarPanel(
                                br(),
                                p("NAVEGACIÓN"),
                                actionButton("ayuda2",label="Ayuda", icon= icon('question-circle'), class ="down"),
                                #helpText("Cargar una base de datos de acuerdo
                                #al formato estandar: https://example.com"),
                                #fileInput("Carga", h3("Cargar Datos")),
                                br(),
                                selectInput("var_sel2","Variable:",
                                            var_names,selected = "Temperatura Ambiental"),
                                #selectInput("st_sel2","Estaciones:",
                                #            st_names,selected = "Alao"),
                                #selectizeInput("st_sel", "Estaciones", choices =  c("Seleccione las Estaciones" = "", paste(st_names)),
                                #               multiple=TRUE, selected = "Alao"),
                                br(),
                                dateRangeInput("fechas2",
                                               "Rango de Fechas", start = "2019-01-01",
                                               end = "2019-01-07",weekstart = 1,
                                               min = "2014-01-01", max = "2019-12-31",
                                               startview = 'year', language = 'es'),
                                br(),
                                helpText("Filtro de horas por día"),
                                sliderInput("hora2", "Horas", min = 0, 
                                            max = 23, value = c(0, 23)),
                                #selectInput("colors", "Paleta de Colores",
                                #            rownames(subset(brewer.pal.info, category %in% c("seq", "div")))
                                #),
                                #checkboxInput("legend", "Mostrar Leyenda", TRUE)
                                conditionalPanel(
                                  condition = "input.tabsAEDE=='Variograma' || input.tabsAEDE=='Interp. Kriging'",
                                  #h4("Opciones Graficas"),
                                  selectInput("mod_sel1","Modelo:",
                                              c("Automático",as.character(vgm()[c(2:4,6,9),1])),#agregar lo mismo en variograma
                                              selected = "Automático")
                                ),
                               
                            ),
                            
                            mainPanel(
                                tabsetPanel(
                                    #los mapas bases son bastantes interactivos a nivel general
                                    #pero no son limitados a  nivel particular 
                                    #tabPanel("Mapa Inicial", 
                                    #         p("Ubicacion geografica de las Estaciones Metereologicas"),
                                    #         br(),
                                    #         leafletOutput("map")
                                    #),
                                    #tabPanel("Coropleta", 
                                    #         p("Ubicacion por parroquias de las Estaciones Metereologicas"),
                                    #         br(),
                                    #         plotOutput("SpPlot1")),
                                    tabPanel("Bubble Map", 
                                             p("Intensidades de cada variable en su respectiva Estacion Metereologica"),
                                             br(),
                                             withSpinner(plotOutput("SpPlot2"))
                                             ),
                                    #tabPanel("Mapa de Ubicaciones", plotOutput("SpPlot")),
                                    tabPanel("Datos", 
                                             withSpinner(tableOutput("table3"))
                                             ),
                                    #tabPanel("Histograma", plotOutput("hist2")),
                                    #tabPanel("Correlograma",
                                    #         plotOutput("acf"),
                                    #         verbatimTextOutput("ljungbox")),
                                    tabPanel("QQ Plot",
                                             withSpinner(plotOutput("normqqplot")),
                                             verbatimTextOutput("shapiro")),
                                    #tabPanel("semi-Var-control", plotOutput("semivar")),
                                    #tabPanel("control-error", verbatimTextOutput("modelres")),
                                    tabPanel("Variograma", 
                                             p(),
                                             withSpinner(plotOutput("empvar")),
                                             p(),
                                             verbatimTextOutput("modelres")),
                                    tabPanel("Interp. Thiessen",
                                             withSpinner(plotOutput("thiessenPlot"))
                                             ),
                                    tabPanel("Interp. IDW",
                                             withSpinner(plotOutput("IDWPlot"))
                                             ),
                                    tabPanel("Interp. Kriging",
                                             withSpinner(plotOutput("OKPlot"))
                                             ),
                                    tabPanel("Correlación ",
                                             withSpinner(plotOutput("SpaCorPlot"))
                                             ),
                                    tabPanel("BoxPlot ", 
                                             withSpinner(plotOutput("SpaBoxPlot"))
                                             ),
                                    tabPanel("V. Cruzada", 
                                             br(),
                                             downloadButton("downloadtable3", "Descargar"),
                                             br(),
                                             withSpinner(tableOutput("table4"))
                                             ),
                                    id="tabsAEDE"
                                )
                            ),
                            #absolutePanel(top = 10, right = 10,
                            #sliderInput("range", "Magnitudes", min(tprom1$varprom), max(tprom1$varprom),
                            #            value = range(tprom1$varprom), step = 0.1
                            #),
                            #selectInput("colors", "Color Scheme",
                            #            rownames(subset(brewer.pal.info, category %in% c("seq", "div")))
                            #)
                            #)
                   ),
                   tabPanel("INFORMACIÓN",icon = icon("info-circle"),
                            #<div style="text-align: justify">
                            p("Los datos de cada variable usados en esta aplicación corresponden a series de tiempo promediadas por hora."),
                            p("Se cuenta con información registrada desde 01/01/2014 hasta 01/01/2019."),
                            p("A la base de datos se le ha aplicado previamente técnicas de relleno de datos faltantes usando el paquete R climatol."),
                            strong("Variables"),
                            p("Se cuenta con datos de 7 variables meteorológicas: Temperatura Ambiental, Humedad Relativa, Radiación Solar Difusa, 
                              Radiación Solar Global, Temperatura de Suelo a nivel 1, Dirección del Viento y Velocidad del viento."),
                            strong("Estaciones"),
                            p("Los datos registrados son de 11 estaciones meteorológicas ubicadas en la provincia de Chimborazo que miden todas 
                              las variables anteriores en cada estación. 
                              Las estaciones son: Alao, Atillo, Cumandá, Espoch, Matus, Multitud, Quimiag, San Juan, Tixán, Tunshi y Urbina."),
                            br(),
                            strong("ANÁLISIS EXPLORATORIO DE DATOS (AED)"),
                            p("En la parte izquierda muestra un panel para filtrar los datos para cada una de las 7 variables de estudio y también para cada una de 
                              las estaciones. Según la variable y tiempo filtrado las gráficas se actualizan permitiendo analizar cualquier intervalo de tiempo requerido 
                              por el usuario. Esta sección puede realizar gráficos de: líneas para series, histogramas, boxplots y cálculo de algunos estadísticos principales 
                              que permiten describir la información de manera general, además de poder realizar una descarga de los datos de interés."),
                            br(),
                            #p("A new p() command starts a new paragraph.", style = "font-family: 'times'; font-si16pt"),
                            strong("ANÁLISIS EXPLORATORIO DE DATOS ESPACIALES (AEDE)"),
                            #em("em() creates italicized (i.e, emphasized) text."),
                            p("De igual forma, esta cuenta con un panel izquierdo para filtrar los datos de cada variable. Así mismo en el panel central se visualiza los 
                              resultados espaciales de los datos filtrados. El valor de cada estación es el promedio de los datos en el intervalo de tiempo filtrado por 
                              el usuario. Adicionalmente se cuenta con las coordenadas geográficas de cada estación."),
                            p("Aquí no existe el filtro por estación ya que en AEDE trabaja con todas las estaciones a la vez. Como se cuenta con información de pocos 
                              puntos espaciales(estaciones) se ha añadido 3 técnicas de interpolación para aproximar posibles valores en los lugares donde no se han 
                              registrado mediciones. Los 3 métodos de interpolación son: Thiessen, Inverse Distance Weight y Ordinary Kriging."),
                            br(),
                            #strong("NOTA:"),
                            #("Hasta el momento las faltas ortograficas con tilde o ~n son inevitables en el entorno Shiny, debido a que genera errores cuando se usan.
                            #   Pedimos disculpas por este inconveniente, seguimos trabajando para mejorar este aspecto"),
                            br(),
                            strong("Version 4.9"),
                            #<div/>
                            #em("Version 4.3"),
                            br()
                   )
)
))
