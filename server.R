



# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    #source(file.path("Graficos.R"),  local = TRUE)$value # Summary tab
    output$HelpBox = renderUI({
      if (input$action %% 2){
        helpText("Here is some help for you")
      } else {
        return()
      }
    })
    observeEvent(input$ayuda1, {
      showModal(modalDialog(
        title = "Analisis Exploratorio de Datos (AED)",
        p(column(6, p("Seleccione la variable de interes y la/s estacion/es para mostrar sus resultados graficos."),
                 p("Cada una de las variables estan registradas por hora y las puede filtrar segun el intervalo de tiempo que requiera."),
                 p("Use la opcion 'Promediar Datos' para reordenar las series de tiempo y filtrar los datos por dia, mes, trimestre o años."),
                 p("Recuerde que para promediar los datos debe aumentar el intervalo de tiempo para poder tener resultados correctos")),
          column(6, img(src="help1.JPG"),
                 br(), br(),
                 img(src="help2.JPG"),
                 br()), br()),
        size = "l", easyClose = TRUE, fade=FALSE, footer = modalButton("Close (Esc)")
      ))
    })
    observeEvent(input$ayuda2, {
      showModal(modalDialog(
        title = "Analisis Exploratorio de Datos Espaciales (AEDE)",
        p(column(6, p("Seleccione la variable de interes y la/s estacion/es para mostrar sus resultados graficos."),
                 p("Cada una de las variables estan registradas por hora y las puede filtrar segun el intervalo de tiempo que requiera."),
                 p("Esta seccion cuenta con tres metodos de interpolacion, importantes para dar una aproximacion de los posibles valores
                    en lugares donde aun no se ha recolectado datos de las respectivas variables."),
                 p("Para el metodo de interpolacion Kriging se puede seleccionar tipo de modelo con el que se desea trabajar por defecto tiene ajuste autometico")),
          column(6, img(src="help1.JPG"),
                 br(), br(),
                 img(src="help3.JPG"),
                 br()), br()),
        size = "l", easyClose = TRUE, fade=FALSE, footer = modalButton("Close (Esc)")
      ))
    })
    selectedData <- reactive({
        switch(input$var_sel,
               "Temperatura Ambiental"=dataX1,
               "Humedad Relativa"=dataX2,
               "Radiacion Solar Difusa"=dataX3,
               "Radiacion Solar Global"=dataX4,
               "Temperatura de Suelo a nivel 1"=dataX5,
               "Direccion del Viento"=dataX6,
               "Velocidad del viento"=dataX7)
    })
    select_st <- reactive({
        sel <- c()
        for (i in 1:length(input$st_sel)) {
            sel[i] <- switch(input$st_sel[i],
                             "Alao"=1, "Atillo"=2,
                             "Cumanda"=3, "Espoch"=4,
                             "Matus"=5, "Multitud"=6,
                             "Quimiag"=7,"San Juan"=8,
                             "Tixan"=9,"Tunshi"=10,
                             "Urbina"=11)
        }
        sel
    })
    
    
    fil_alltime <- reactive({
        data <- selectedData()
        st <- select_st()
        #fechas <- c("2014-01-01","2014-01-02")
        horas <- c("00:00:00","23:00:00")
        date_time <- paste(input$fechas,horas)
        datafil <-  data %>% 
          dplyr::filter(Fecha >= as.POSIXct(date_time[1], tz="UTC") & 
                       Fecha <= as.POSIXct(date_time[2], tz="UTC"))
        
        
    })

    #fil_date <- reactive...
    sel_hour <- reactive({
      datafil <- fil_alltime()
      #horas_dia <- c(strftime(input$hora1, "%T"),strftime(input$hora2, "%T"))
      #fechas <- c("2014-01-01","2014-01-03")
      fecha_fil <- as.Date(input$fechas)
      fechas <- as.character(seq.Date( fecha_fil[1],fecha_fil[2],1))

      #h_ini <- as.numeric(substring(horas_dia[1], first=1,last =2))
      #h_fin <- as.numeric(substring(horas_dia[2], first=1,last =2))
      h <- seq(input$hora1[1],input$hora1[2],1)
      h2 <- parse_date_time(h, "HMS", truncated = 3)
      h3 <- as_hms(h2)
      fd <- expand.grid(fecha=fechas,hora=h3)
      fd2 <- map_df(arrange(fd,fecha),as.character)
      fd2 <- as.data.frame(fd2)
      u_date <- as.POSIXct(paste(fd2[,1], fd2[,2]), format="%Y-%m-%d %H:%M:%S",tz = "UTC")
      fil_day <- data.frame(Fecha=u_date)
      fil_day <- as.tibble(fil_day)
      data_f <- as.tibble(merge(fil_day,datafil,"Fecha"))
      data_f
    })
    
    
    fil_date <- reactive({
      datasel <- sel_hour()
      datasel <- rename(datasel, date = Fecha)
      #tavg <- c("day", "Week", "Month", "quarter", "year")[input$timeavg]
      if (input$act_avg==TRUE) {
        datasel <- timeAverage(datasel, avg.time = input$timeavg)
      }
      datasel <- rename(datasel , Fecha = date)
      datasel
    })
    
    EstDes <- reactive({
        datafil <- fil_date()
        #numvar <- sapply(datafil,is.numeric)
        #actualizacion: se calcula desde la col 8 ya q desde aqui empiezan las estaciones
        numvar <- 8:length(datafil)
        ADT <- fun_df(datafil[,numvar],AD)
        ED <- data.frame(Estacion=st_names,ADT)
        colnames(ED) <- c("Estacion","Media","Mediana","Moda","Desv. Est.","Asimetria","Curtosis","Min","Max","Rango","1_Cuantil1","3_Cuantil")
        ED
        #ED <- profiling_num(datafil)
        #dataset <- ED[,c(1:4,7:10,12:16)]
        #data.frame(dataset)
        #colnames(dataset) <- c("Estacion","Media","Desv. Est.","Coef. Var.","Percentil (25%)","Percentil (50%)","Percentil (75%)","Percentil (95%)","Asimetria","Curtosis","Rango Inter.","Rango 98%","Rango 80%")
        #dataset
    })
    
    Agrupar_cal <- reactive({
      st <- select_st()
      data <- selectedData()
      #el filtro de datos con var no toma en cuenta la usada en group by
      GHora <- data %>% group_by(Hora) %>% summarise_at(vars(st+6),mean,na.rm=T)
      GDiaSem <- data %>% group_by(Dia.Semanal) %>% summarise_at(vars(st+6),mean,na.rm=T)
      #GDiaMen <- data %>% group_by(Dia.Mensual) %>% summarise_at(vars(st+6),mean,na.rm=T)
      GMes <- data %>% group_by(Mes) %>% summarise_at(vars(st+6),mean,na.rm=T)
      #GDiaAn <- data %>% group_by(Dia.Anual) %>% summarise_at(vars(st+6),mean,na.rm=T)
      GYear <- data %>% group_by(Year) %>% summarise_at(vars(st+6),mean,na.rm=T)
      #promedios mensuales por year
      GYM <- data %>% group_by(Year,Mes) %>% summarise_at(vars(st+5),mean,na.rm=T)
      #promedios diarios anual
      GYD <- data %>% group_by(Year,Dia.Anual) %>% summarise_at(vars(st+5),mean,na.rm=T)
      
      switch(input$Agrupar1,"1"=GHora,"2"=GDiaSem,"3"=GMes,
             "4"=GYear,"5"=GYM,"6"=GYD)
    })
    
    Sel_agrup <- reactive({
      st <- select_st()
      dta <- Agrupar_cal()
      #aqui se usa st+1 debido a que las estaciones empiezan en col2
      switch(input$Agrupar1,"1"={
        #sem <- seq(from = as.POSIXct("2018-03-01 00:00"),
        #           to  = as.POSIXct("2018-03-01 23:00"),
        #           by = "hour")
        h <- seq(0,23,1)
        h2 <- parse_date_time(h, "HMS", truncated = 2)
        h3 <- as_hms(h2)
        dta$Hora <- h3
        dta$Hora <- h3
        df <- dta[,c(1:length(dta))] %>%
          gather(key = "Estacion", value = Variable, -1)
        df
      },"2"={
        sem <- c("2020-03-01","2020-03-07")#para dias de la semana ordenados
        sem <- as.Date(sem)
        sem <- seq.Date(sem[1],sem[2],1)
        dta$Dia.Semanal <- sem
        df <- dta[,c(1:length(dta))] %>%
          gather(key = "Estacion", value = Variable, -1)
        df
      },"3"={
        sem <- c("2017-01-02","2017-12-31")#para meses ordenados
        sem <- as.Date(sem)
        sem <- seq.Date(sem[1],sem[2],32)
        dta$Mes <- sem
        df <- dta[,c(1:length(dta))] %>%
          gather(key = "Estacion", value = Variable, -1)
        df
      },"4"={
        sem <- c("2014-01-02")#para years ordenados
        sem <- as.Date(sem)
        sem <- seq.Date(sem[1], by = "year", length.out = dim(dta)[1])
        dta$Year <- sem
        df <- dta[,c(1:length(dta))] %>%
          gather(key = "Estacion", value = Variable, -1)

      },"5"={
        sem <- c("2014-01-01")#para year mes ordenados
        sem <- as.Date(sem)
        sem <- seq.Date(sem[1], by = "month", length.out = dim(dta)[1])
        sem <- as.POSIXct(sem, format="%Y-%m-%d %H:%M:%S",tz = "UTC")
        dta$Year <- sem
        df <- dta[,c(1,3:length(dta))] %>%
          gather(key = "Estacion", value = Variable, -1)
        df
      },"6"={
        sem <- c("2014-01-01")#para year dia
        sem <- as.Date(sem)
        sem <- seq.Date(sem[1], by = "day", length.out = dim(dta)[1])
        sem <- as.POSIXct(sem, format="%Y-%m-%d %H:%M:%S",tz = "UTC")
        dta$Year <- sem
        df <- dta[,c(1,3:length(dta))] %>%
          gather(key = "Estacion", value = Variable, -1)
        df
      })
      
   
    })
    
    un_med1 <- reactive({
      
      pos <- switch(input$var_sel,
                    "Temperatura Ambiental"=1,
                    "Humedad Relativa"=2,
                    "Radiacion Solar Difusa"=3,
                    "Radiacion Solar Global"=4,
                    "Temperatura de Suelo a nivel 1"=5,
                    "Direccion del Viento"=6,
                    "Velocidad del viento"=7)
      unidades <- c("°C","%","W/m^2","W/m^2","°C","°","m/s")
      unidades[pos]
    })
  
    output$plot <- renderPlotly({
        #data <- selectedData()
        #yname <- input$st_sel
        
        st <- select_st()
        datafil <- fil_date()
        medida <- un_med1()
        df <- datafil[,c(1,st+7)] %>%
            gather(key = "Estacion", value = Valor, -1)
        ag1 <- " ("
        name <- input$var_sel
        ag2 <- ")"
        ti <- "Serie por Hora"
        if (input$act_avg==TRUE) {
          ti <- c("Serie por ","input$timeavg")
        }
        
        p <- ggplot(df, aes(x = Fecha, y = Valor)) + 
            geom_line(aes(color = Estacion), size = 0.8) +
            #facet_wrap(~Estacion)+
            ylab(paste0(name,ag1,medida,ag2)) + xlab("Fecha") +
            #scale_x_datetime(guide = guide_axis(angle = 90))+
          theme_minimal() +
          labs(title= ti)+
          theme(text = element_text(size=12),
                #legend.position="bottom", 
                #legend.box = "horizontal",
                axis.text.x = element_text(size=rel(1.3)),
                axis.text.y = element_text(size=rel(1.3)),
                axis.title.x = element_text(face="bold",size=rel(1.2)),
                axis.title.y = element_text(face="bold",size=rel(1.1)),
                #strip.text.x = element_text(size=rel(2)),
                #strip.text.y = element_text(size=rel(2)),
                title = element_text(face="bold",size=rel(1.1)),
                legend.title = element_text(size=rel(1)),
                legend.text = element_text(size=rel(1)),
                plot.margin = unit(c(1,1,1,1), "cm")
                )
       p <- ggplotly(p)
       p
       
    })
    
    #graficas de serie agrupada
    output$plot2 <- renderPlotly({
      df <- Sel_agrup()
      x <- names(df)[1]
      xt <- str_replace_all(names(df)[1], "[._]", " ")
      medida <- un_med1()
      ag1 <- " ("
      name <- input$var_sel
      ag2 <- ")"
      yt <- paste0(name,ag1,medida,ag2)
      #condicion para graficos con fechas y hora
      if (is.Date(df[[1]])==TRUE) {
        #caso explicito de texto si se agrupa por dia,mes year 
        if (df[[1]][1]== "2020-03-01") {
          style <- "%a"
        }else{
          if (df[[1]][1]== "2017-01-02") {
            style <- "%b"
          }else{
            style <- "%Y"
          }
        }
        p <- ggplot(df, aes(x = !!ensym(x), y = Variable)) + 
          geom_line(aes(color = Estacion), size = 1) +
          scale_x_date(breaks = df[[1]], date_labels = style) +
          theme_minimal() + geom_point(aes(colour = factor(Estacion)))+
          ylab(yt) + xlab(xt) +
          theme(text = element_text(size=14),
                axis.text.x = element_text(size=rel(1.3)),
                axis.text.y = element_text(size=rel(1.3)),
                strip.text.x = element_text(size=rel(2)),
                strip.text.y = element_text(size=rel(2)))
      }else{#en el caso de que el eje x solo sea por hora
        #%H&M&S
        p <- ggplot(df, aes(x = !!ensym(x), y = Variable)) + 
          geom_line(aes(color = Estacion), size = 1) +
          #scale_x_datetime(breaks = df[[1]], date_labels = "%H") +
          theme_minimal() + geom_point(aes(colour = factor(Estacion)))+
          ylab(yt) + xlab(xt) +
          theme(text = element_text(size=14),
                axis.text.x = element_text(size=rel(1)),
                axis.text.y = element_text(size=rel(1)),
                strip.text.x = element_text(size=rel(2)),
                strip.text.y = element_text(size=rel(2)),
                plot.margin = unit(c(1,1,1,1), "cm")
                )
      }
      p <- ggplotly(p)
      p
    })
   
    output$Histo1 <- renderPlotly({
      #yname <- et_st()
      st <- select_st()
      datafil <- fil_date()
      df <- datafil[,c(1,st+7)] %>%
        gather(key = "Estacion", value = Variable, -1)
      
      p <- ggplot(df, aes(x = Variable,color=Estacion, fill=Estacion)) + 
        geom_histogram(alpha=0.6, binwidth = 5) + scale_fill_viridis(discrete=TRUE) +
        scale_color_viridis(discrete=TRUE) + 
        theme(
          legend.position="none",
          #panel.spacing = unit(0.1, "lines"),
          #strip.text.x = element_text(size = 8)
        ) +
        xlab("") +
        ylab("Frecuencia") +
        labs(title= input$var_sel)+
        facet_wrap(~Estacion)+
        theme_minimal() +
        theme(text = element_text(size=12),
              axis.text.x = element_text(size=rel(1.2)),
              axis.text.y = element_text(size=rel(1.2)),
              axis.title.x = element_text(size=rel(1.1)),
              axis.title.y = element_text(size=rel(1.1)),
              strip.text.x = element_text(size=rel(2)),
              strip.text.y = element_text(size=rel(2)),
              title = element_text(face="bold",size=rel(1.2)),
              plot.margin = unit(c(1,1,1,1), "cm")
              )
      p <- ggplotly(p)
      hide_legend(p)
    })
    
    output$boxplot <- renderPlotly({
        #yname <- et_st()
        st <- select_st()
        datafil <- fil_date()
        df <- datafil[,c(1,st+7)] %>%
            gather(key = "Estacion", value = Variable, -1)
        
        p <- ggplot(df, aes(x = Estacion, y = Variable)) + 
            geom_boxplot() +
            #facet_wrap(~ Estacion, scales="free",ncol = 4)+
            ylab(input$var_sel) + 
            #xlab("Division por Estacion")+
          theme_minimal() +
          theme(text = element_text(size=14),
                axis.text.x = element_text(size=rel(1.3)),
                axis.text.y = element_text(size=rel(1.3)),
                axis.title.x = element_text(face="bold",size=rel(1.2)),
                axis.title.y = element_text(face="bold",size=rel(1.1)),
                strip.text.x = element_text(size=rel(2)),
                strip.text.y = element_text(size=rel(2)),
                plot.margin = unit(c(1,1,1,1), "cm")
                )
        p <- ggplotly(p)
        p
    })
    
    
    output$acf1 <- renderPlot({
      #correlograma
      
      #dbbp <- point_names()
      name <- input$var_sel1
      st <- select_st()
      datafil <- fil_date()
      #df <- datafil[,c(1,st+7)] %>%
      #  gather(key = "Estacion", value = Variable, -1)
      #ggplot(df, aes(x = Estacion, y = Variable)) + 
      #  geom_boxplot() +
      #  facet_wrap(~ Estacion, scales="free",ncol = 4)+
      #  ylab(input$var_sel) + xlab("Division por Estacion")+
        
      ggplot_acf_pacf(res= acf(datafil[,c(st+7)], plot= F),
                      lag=1, label= "ACF",name)
      
      #facet_wrap(~Estacion)+
    })
    
    output$ljungbox1 <- renderPrint({
      #prueba de estacionariedad
      
      name <- input$var_sel1
      st <- select_st()
      datafil <- fil_date()
      s <- Box.test(datafil[,c(st+7)], lag=1, type="Ljung-Box")
      s$data.name <- name
      s
      
    })
    
    output$desc_ts <- renderPlot({
      #data <- selectedData()
      #yname <- input$st_sel
      
      st <- select_st()
      datafil <- fil_date()
      df <- datafil[,c(1,st+7)] %>%
        gather(key = "Estacion", value = Variable, -1)
      
      freq <- 24

      if (input$act_avg==TRUE) {
        #freq <- c(365,52,12,4,1)[input$timeavg]
        freq <- switch(input$timeavg,
               "day"=7, "week"=4, "month"=12,
               "quarter"=4, "year"=365)
      }
      ggsdc(df , aes(x = Fecha, y = Variable,colour = Estacion),
            frequency = freq, s.window = 7) +
        geom_line() + labs(x = "", y = input$var_sel) +
        #scale_x_datetime(guide = guide_axis(angle = 90))+
        theme_minimal() +
        theme(text = element_text(size=14),
              axis.text.x = element_text(size=rel(1.3)),
              axis.text.y = element_text(size=rel(1.3)),
              strip.text.x = element_text(size=rel(2)),
              strip.text.y = element_text(size=rel(2)))
    })
    
    output$table1 <- renderDataTable({
        dataED<- EstDes()
        dataED
        #dataset <- dataED[,c(1:4,7:10,12:16)]
        #data.frame(dataset)
        #(dataset) <- c("Estacion","Media","Desv. Est.","Coef. Var.","Percentil (25%)","Percentil (50%)","Percentil (75%)","Percentil (95%)","Asimetria","Curtosis","Rango Inter.","Rango 98%","Rango 80%")
        #head(dataset,n = 20)
    })
    output$table2 <- renderDataTable({
        dt <- fil_date()
        dtv <- data.frame(dt[,c(1,8:length(dt))])
        dtv$Fecha <- as.character(dtv$Fecha)
        dtv
        #head(dtv,n = 30)
    })
    
    Fil_date_exp <- reactive({
      dt <- fil_date()
      dtv <- data.frame(dt)
      dtv$Fecha <- as.character(dtv$Fecha)
      dtv
    })

    #||||||||||  AEDE ||||||||||||||||||
    selectedData2 <- reactive({
        switch(input$var_sel2,
               "Temperatura Ambiental"=dataX1,
               "Humedad Relativa"=dataX2,
               "Radiacion Solar Difusa"=dataX3,
               "Radiacion Solar Global"=dataX4,
               "Temperatura de Suelo a nivel 1"=dataX5,
               "Direccion del Viento"=dataX6,
               "Velocidad del viento"=dataX7)
    })
  
    
    fil_alltime2 <- reactive({
        data <- selectedData2()
        #fechas <- c("2014-01-01","2014-01-02")
        horas <- c("00:00:00","23:00:00")
        date_time <- paste(input$fechas2,horas)
        datafil <-  data %>% 
          dplyr::filter(Fecha >= as.POSIXct(date_time[1], tz="UTC") & 
                       Fecha <= as.POSIXct(date_time[2], tz="UTC"))
        datafil
        
    })
    
    fil_date2 <- reactive({
      datafil <- fil_alltime2()
      #horas_dia <- c(strftime(input$hora1, "%T"),strftime(input$hora2, "%T"))
      #fechas <- c("2014-01-01","2014-01-03")
      fecha_fil <- as.Date(input$fechas2)
      fechas <- as.character(seq.Date( fecha_fil[1],fecha_fil[2],1))
      
      #h_ini <- as.numeric(substring(horas_dia[1], first=1,last =2))
      #h_fin <- as.numeric(substring(horas_dia[2], first=1,last =2))
      h <- seq(input$hora2[1],input$hora2[2],1)
      h2 <- parse_date_time(h, "HMS", truncated = 3)
      h3 <- as_hms(h2)
      fd <- expand.grid(fecha=fechas,hora=h3)
      fd2 <- map_df(arrange(fd,fecha),as.character)
      fd2 <- as.data.frame(fd2)
      u_date <- as.POSIXct(paste(fd2[,1], fd2[,2]), format="%Y-%m-%d %H:%M:%S",tz = "UTC")
      fil_day <- data.frame(Fecha=u_date)
      fil_day <- as.tibble(fil_day)
      data_f <- as.tibble(merge(fil_day,datafil,"Fecha"))
      data_f
    })
    
    EstDes2 <- reactive({
        datafil <- fil_date2()
        #numvar <- sapply(datafil,is.numeric)
        #actualizacion: se usa desde la col 8 debido a q desde ahi empiezan las estaciones
        numvar <- 8:length(datafil)
        ADT <- fun_df(datafil[,numvar],AD)
        ED <- data.frame(Estacion=st_names,ADT)
        ED
        #ED <- profiling_num(datafil)
        #ED
    })
    
    sel_coord <- reactive({
        tab <- EstDes2()
        tprom <- as.tibble(cbind(coord[,c(1,6:7)],round(tab[2],2)))
        tprom1 <- tprom %>% rename(valor=Mean)
        tprom1
    })
    
    colorpal <- reactive({
        datafil <- sel_coord()
        colorNumeric(input$colors, datafil$valor)
    })
    
    output$table3 <- renderTable({
      #dt <- sel_coord()
      #dtv <- data.frame(dt)
      #dtv
      
      ED <- EstDes2()
      #sptdf$varprom <- round(ED[,2],2)
      #dt <- sptdf@data
      dat <- pcd
      dat$Valor <- round(ED[,2],2)
      dat <- as_tibble(dat)
      dtv <- dat %>% select(-LON,-LAT)
      dtv
      
      
      
    })
    
    #output$map <- renderLeaflet({
    
    #    data <- sel_coord()
    #    leaflet(data) %>% addTiles() %>%
    #        fitBounds(~min(Lon), ~min(Lat), ~max(Lon), ~max(Lat))
    #})
    

    #observe({
    #    dat <- sel_coord()
    #    pal <- colorpal()
    #    #data=filtro estaciones
    #    leafletProxy("map", data = dat) %>%
    #        clearShapes() %>%
    #        addCircles(radius = ~valor/sum(valor)*30000, weight = 1, color = "black",
    #                   fillColor = ~pal(valor), fillOpacity = 0.7, popup = ~paste(Estacion,valor)
    #        )
    #})
    
    #observe({
    #    data <- sel_coord()
    #    proxy <- leafletProxy("map", data = data)
        
    #    # Remove any existing legend, and only if the legend is
    #    # enabled, create a new one.
    #    proxy %>% clearControls()
    #    if (input$legend) {
    #        pal <- colorpal()
    #        proxy %>% addLegend(position = "bottomright",
    #                            pal = pal, values = ~valor
    #        )
    #    }
    #})
    

    #calcular matriz base de datos espaciales
    
    sptdfMean <- reactive({
        #puntos espaciales de promedio de un periodo de cada estacion
        ED <- EstDes2()
        sptdf$varprom <- round(ED[,2],2)
        sptdf
    })
    
   
    output$hist2 <- renderPlot({
        sptdf <- sptdfMean()
        #grafico de burbujas(cartograma)
        #hist(sptdf$varprom)
        hist(log1p(sptdf$varprom))         
    })
    
    map_base <- reactive({
      sptdf <-  sptdfMean()
      #convertir Spatialdataframe en en tibble con id codigo parroquias
      #no funciona con fortify ni con tidy para la pagina de shinyapssio
      spdf_fortified <- fortify(parro_chimb, region = "DPA_DESPAR")
      #unir la tibble anterior con una base de datos con valores o mediciones de la variable
      spdf_fortified = spdf_fortified %>%
        left_join(. , sptdf@data, by=c("id"="PARROQUIA"))
      spdf_fortified <- left_join(spdf_fortified, sptdf@data,
                                  by=c("id"="PARROQUIA"))
      #gg <- spdf_fortified #actualizado no es necesario la base polygon_ch_fil.xlsx
    })
    
    est_base <- reactive({
      sptdf <-  sptdfMean()
      #filtar nombres y centroides de parroquias
      dtstparr <- merge(parro_chimb,sptdf@data,by.x="DPA_DESPAR",by.y="PARROQUIA")
      dtstparr <-dtstparr[is.na(dtstparr$ESTACION)==FALSE,]
      dt <- dtstparr@data[,c(1,8:length(dtstparr@data))]
      dt <- cbind(dt,coordinates(dtstparr))
      colnames(dt)[8:9] <- c("X","Y")
      dt
    })
    
    bubble_names <- reactive({
      dt <- est_base()
      dbbp <- dt
      #input <- "Temperatura"
      dbbp <- dbbp %>%
        arrange(varprom) %>%
        mutate( name=factor(ESTACION, unique(ESTACION))) %>%
        mutate( mytext=paste(
          "Estacion: ", ESTACION, "\n", 
          input$var_sel2,": ", varprom, sep="")
        )
    })
    
    point_names <- reactive({
      dt <- est_base()
      dbbp <- dt
      dbbp <- dbbp%>%
        mutate( name=factor(ESTACION, unique(ESTACION))) %>%
        #mutate( mytext=paste(ESTACION, "\n",varprom, sep="")
        mutate( mytext=paste(ESTACION, "\n",sep="")
                        
        )
    })
    
    un_med <- reactive({
      pos <- switch(input$var_sel2,
             "Temperatura Ambiental"=1,
             "Humedad Relativa"=2,
             "Radiacion Solar Difusa"=3,
             "Radiacion Solar Global"=4,
             "Temperatura de Suelo a nivel 1"=5,
             "Direccion del Viento"=6,
             "Velocidad del viento"=7)
      unidades <- c("°C","%","W/m^2","W/m^2","°C","°","m/s")
      unidades[pos]
    })
    
   # output$SpPlot3 <- renderPlot({
      #grafico dummy
    #  spdf_fortified <- map_base() 
      #dt <- est_base()
      
      #ggplot(spdf_fortified, aes(x = long , y =  lat )) + 
      #  geom_line() 
    #  plot(spdf_fortified[,1:2])
    #})
    
    output$SpPlot <- renderPlot({
      #Mapa de ubicacion
      spdf_fortified <- map_base() 
      unidades <- un_med()
      dt <- est_base()
      dbbp <- point_names()
      scpts <- seq(min(dbbp$varprom), max(dbbp$varprom), length.out = 5)
      scpts <- as.numeric(round(scpts,2))
      titulo <- "PROVINCIA DE CHIMBORAZO\nUBICACION DE ESTACIONES"
      ag1 <- "Predicciones de\n"
      input <- input$var_sel2
      ag2 <- "\n(en "
      #unidades <- c("°C","%","°C","W/m^2","W/m^2","°","m/s")
      ag3 <- ")"
      ag4 <- "Valores de\n"
      ag5 <- "Intensidad de\n"
      
      ggplot() +
        geom_polygon(data = spdf_fortified, aes(x=long, y = lat, group=group), color="grey",fill="grey", alpha=0.3) +
        geom_point(data= dbbp,aes(x=X, y=Y),size=3,stroke=FALSE) +
        scale_size_continuous(breaks=scpts) +
        scale_alpha_continuous(breaks=scpts) +
        scale_color_gradient(breaks=scpts,low = "red", high = "blue") +
        guides( colour = guide_legend()) +
        geom_shadowtext(data= dbbp,aes(x=X, y=Y, label=mytext),
                        fontface = "bold",size=3.2) +
        #scale_color_viridis(option="inferno") +
        #scale_color_viridis(option=sample(LETTERS[2:5],1)) +
        theme_bw() +  
        labs(title= titulo, x="Coordenada X", y="Coordenada Y",
             color=paste0(ag4,input,ag2,unidades,ag3),
             size=paste0(ag4,input,ag2,unidades,ag3),
             fill=paste0(ag1,input,ag2,unidades,ag3)) +
        theme(text = element_text(size=10),
              title = element_text(face="bold",size=rel(1.2)),
              axis.title.x = element_text(face="bold",size=rel(1.2)),
              axis.title.y = element_text(face="bold",size=rel(1.2)),
              axis.text.x = element_text(size=rel(1.3)),
              axis.text.y = element_text(size=rel(1.3)))
      #theme(text = element_text(size=14),
      #      axis.text.x = element_text(size=rel(1.3)),
      #      axis.text.y = element_text(size=rel(1.3)),
      #      strip.text.x = element_text(size=rel(2)),
      #      strip.text.y = element_text(size=rel(2)))
    })
    
    output$SpPlot1 <- renderPlot({
      #grafico de coropletas
      spdf_fortified <- map_base() 
      unidades <- un_med()
      dt <- est_base()
      dbbp <- point_names()
      scpts <- seq(min(dbbp$varprom), max(dbbp$varprom), length.out = 5)
      scpts <- as.numeric(round(scpts,2))
      titulo <- "PROVINCIA DE CHIMBORAZO\nMAPA DE COROPLETAS"
      ag1 <- "Predicciones de\n"
      input <- input$var_sel2
      ag2 <- "\n(en "
      #unidades <- c("°C","%","°C","W/m^2","W/m^2","°","m/s")
      ag3 <- ")"
      ag4 <- "Valores de\n"
      ag5 <- "Intensidad de\n"
      
      ggplot() +
        geom_polygon(data = spdf_fortified, aes(fill = varprom, x = long, y = lat, group=group),color="grey" , alpha=0.9) +
        scale_fill_continuous(low="thistle2", high="darkred", guide="colorbar",na.value="white") +
        geom_shadowtext(data= dt,aes(x=X, y=Y, label=DPA_DESPAR),
                        fontface = "bold", check_overlap = FALSE)+
        theme_bw() +  
        labs(title= titulo, x="Coordenada X", y="Coordenada Y",
             #color=paste0(ag4,input,ag2,unidades,ag3),
             #size=paste0(ag4,input,ag2,unidades,ag3),
             fill=paste0(ag4,input,ag2,unidades,ag3)) +
        theme(text = element_text(size=10),
              title = element_text(face="bold",size=rel(1.2)),
              axis.title.x = element_text(face="bold",size=rel(1.2)),
              axis.title.y = element_text(face="bold",size=rel(1.2)),
              axis.text.x = element_text(size=rel(1.3)),
              axis.text.y = element_text(size=rel(1.3)))
      
    })
    
    output$SpPlot2 <- renderPlot({
      #mapa de burbujas
      spdf_fortified <- map_base()
      dbbp <- point_names()
      unidades <- un_med()
      scpts <- seq(min(dbbp$varprom), max(dbbp$varprom), length.out = 5)
      scpts <- as.numeric(round(scpts,2))
      titulo <- "PROVINCIA DE CHIMBORAZO\nMAPA DE BURBUJAS"
      ag1 <- "Predicciones de\n"
      input <- input$var_sel2
      ag2 <- "\n(en "
      #unidades <- c("°C","%","°C","W/m^2","W/m^2","°","m/s")
      ag3 <- ")"
      ag4 <- "Valor"
      ag5 <- "Intensidad de\n"
      
      ggplot() +
        geom_polygon(data = spdf_fortified, aes(x=long, y = lat, group=group), color="grey",fill="grey", alpha=0.3) +
        geom_point(data= dbbp,aes(x=X, y=Y, size=varprom),color="brown",stroke=FALSE) +
        scale_size_continuous(breaks=scpts) +
        scale_alpha_continuous(breaks=scpts) +
        scale_color_gradient(breaks=scpts,low = "red", high = "blue") +
        guides( colour = guide_legend()) +
        geom_shadowtext(data= dbbp,aes(x=X, y=Y, label=mytext),
                        fontface = "bold",size=3.2) +
        #scale_color_viridis(option="inferno") +
        #scale_color_viridis(option=sample(LETTERS[2:5],1)) +
        theme_bw() +  
        labs(title= titulo, x="Coordenada X", y="Coordenada Y",
             color=paste0(ag4,ag2,unidades,ag3),
             size=paste0(ag4,ag2,unidades,ag3),
             fill=paste0(ag1,input,ag2,unidades,ag3)) +
        theme(text = element_text(size=10),
              title = element_text(face="bold",size=rel(1.2)),
              axis.title.x = element_text(face="bold",size=rel(1.2)),
              axis.title.y = element_text(face="bold",size=rel(1.2)),
              legend.title = element_text(size=rel(1.3)),
              legend.text = element_text(size=rel(1.3)),
              axis.text.x = element_text(size=rel(1.3)),
              axis.text.y = element_text(size=rel(1.3)))

      #forma renderplotly
      #Se aclara que ahora se usa renderPlotly en vez de renderPlot
      #p <- ggplot() +
      #  geom_polygon(data = spdf_fortified, aes(x=long, y = lat, group=group), color="grey",fill="grey", alpha=0.3) +
      #  geom_point(data= dbbp,aes(x=X, y=Y, size=varprom, color=varprom, text=mytext, alpha=varprom),stroke=FALSE) +
      #  scale_size_continuous() +
        #scale_color_viridis(option="inferno", trans="log") +
      #  scale_color_viridis(option=sample(LETTERS[2:5],1)) +
      #  scale_alpha_continuous(trans="log") +
      #  theme_void() 
      #p <- ggplotly(p, tooltip="text")
      #p
    })
    
   
     output$normqqplot <- renderPlot({
      #grafico de  normalidad
     
      dt <- est_base()
      dbbp <- point_names()
      name <- input$var_sel2
      ggqqplot(dbbp, x = "varprom", 
               ggtheme = theme_pubclean(),title = paste0("Q-Q Plot\n",name))+
        theme(text = element_text(size=14),
              title = element_text(face="bold",size=rel(1)),
              axis.title.x = element_text(face="bold",size=rel(1)),
              axis.title.y = element_text(face="bold",size=rel(1)),
              strip.text.x = element_text(size=rel(3)),
              strip.text.y = element_text(size=rel(3)))
      
      
    })
    
     output$acf <- renderPlot({
       #correlograma
      
       dbbp <- point_names()
       name <- input$var_sel2
 
       ggplot_acf_pacf(res= acf(dbbp$varprom, plot= F),
                       lag=1, label= "ACF",name)

       
     })
     
     output$ljungbox <- renderPrint({
       #prueba de estacionariedad
       
       dbbp <- point_names()
       name <- input$var_sel2
       s <- Box.test(dbbp$varprom, lag=1, type="Ljung-Box")
       s$data.name <- name
       s
       
     })
     
     output$shapiro <- renderPrint({
       #prueba de normalidad shapiro
       
       dbbp <- point_names()
       name <- input$var_sel2
       t1 <- shapiro.test(dbbp$varprom)
       t2 <- jarque.test(dbbp$varprom)
       t3 <- ad.test(dbbp$varprom)
       #t1$data.name <- name
       m <- c(t1$method,t2$method,t3$method)
       p <- c(t1$p.value,t2$p.value,t3$p.value)
       dat <- data.frame(test=m,p.value=p)
       dat
       
     })
     
     
    #||||||||||||modelo a base de variogramas|||||||||||||||||||||
    
    var_ev <- reactive({
        sptdf <- sptdfMean()
        #Variograma empirico
        ev <- variogram(log1p(varprom)~1, sptdf) 
        ev
    })
    
    var_iv <- reactive({
        sptdf <- sptdfMean()
        # variograma inicial
        #modsel <- as.character(vgm()[c(2:15,17:19),1])
        if (input$mod_sel1=="Automatico") {
          modsel <- as.character(vgm()[c(2:4,6,9),1])
        }else{
          modsel <- input$mod_sel1
        }
        iv <- vgm(nugget=0, 
                  #model=c("Exp", "Sph", "Gau", "Mat"), 
                  model=modsel, 
                         range=sqrt(diff(sptdf@bbox["X",])^2 +
                                        diff(sptdf@bbox["Y",])^2)/4, 
                         psill=var(log1p(sptdf$varprom)))
        iv
    })
    
    var_tv <- reactive({
        #Ajuste del variograma (teorico)
        ev <- var_ev()
        iv <- var_iv()
        tv <- fit.variogram(ev, model=iv)
        tv
    })
    
    output$empvar <- renderPlot({
        ev <- var_ev()
        tv <- var_tv()
        nam <- input$var_sel2
        plot_variogram(ev, tv,nam) 
        
    })
    output$semivar <- renderPlot({
      ev <- var_ev()
      #tv <- var_tv()
      #plot(ev, pl = T, model = tv)
      plot(ev, pl = T)
    })
    output$modelres <- renderPrint({
      res <- var_tv()
      #tv <- var_tv()
      #plot(ev, pl = T, model = tv)
      res
    })
    
    #|||||||||||||||  Interpolaciones |||||||||||||||||||||||
    grid_pred <- reactive({
        sptdf <- sptdfMean()
        # grid para prediccion
        #sptdf_grid = spsample(parro_chimb, type = "regular", cellsize = c(1000,1000))
        sptdf_grid  <- spsample(parro_chimb, "regular", n=50000)
        gridded(sptdf_grid) = TRUE
        proj4string(sptdf_grid) <- proj4string(sptdf)
        sptdf_grid
    })
    
    Thiessen <- reactive({
        sptdf <- sptdfMean()
        sptdf_grid <- grid_pred()
        thiessen_p = krige(varprom ~ 1, sptdf, sptdf_grid, nmax = 1)
        thiessen_p
    })
    
    output$thiessenPlot <- renderPlot({
        spdf_fortified <- map_base()
        unidades <- un_med()
        dbbp <- point_names()
        thiessen.d<- Thiessen()
        df1 <- data.frame(thiessen.d)
        predaprox <- round(df1$var1.pred)
        df1$predaprox <- predaprox

        scpts <- seq(min(dbbp$varprom), max(dbbp$varprom), length.out = 5)
        scpts <- as.numeric(round(scpts,2))
        scrtr <- seq(min(df1$var1.pred), max(df1$var1.pred), length.out = 5)
        scrtr <- as.numeric(round(scrtr,2))
        titulo <- "PROVINCIA DE CHIMBORAZO\nINTERPOLACION THIESSEN"
        #ag1 <- "Predicciones de\n"
        ag1 <- "Valor"
        input <- input$var_sel2
        ag2 <- "\n(en "
        #unidades <- c("C","%","KJ","Kj","C","cm/h","grados")
        ag3 <- ")"
        ag4 <- "Valores de\n"
        ag5 <- "Intensidad de\n"
        
        ggplot() + geom_raster(data=df1, aes(x = x1, y = x2, fill = var1.pred)) +
          scale_fill_continuous(breaks=scrtr, guide="colorbar",na.value="white") +
          #scale_alpha_continuous(breaks=scrtr) +
          geom_polygon(data = spdf_fortified, aes(x=long, y = lat, group=group), color="grey",fill="grey", alpha=0) +
          geom_point(data= dbbp,aes(x=X, y=Y),size=3,color="yellow",stroke=FALSE) +
          scale_size_continuous(breaks=scpts) +
          scale_alpha_continuous(breaks=scpts) +
          scale_color_gradient(breaks=scpts,low = "red", high = "green") +
          guides( colour = guide_legend()) +
          geom_shadowtext(data= dbbp,aes(x=X, y=Y, label=mytext),
                          fontface = "bold",size=3.2)+
          #scale_color_viridis(option=sample(LETTERS[2:5],1)) +
          theme_bw() + 
          labs(title= titulo, x="Coordenada X", y="Coordenada Y",
               color=paste0(ag4,input,ag2,unidades,ag3),
               size=paste0(ag4,input,ag2,unidades,ag3),
               fill=paste0(ag1,ag2,unidades,ag3)) +
          theme(text = element_text(size=10),
                title = element_text(face="bold",size=rel(1.2)),
                axis.title.x = element_text(face="bold",size=rel(1.2)),
                axis.title.y = element_text(face="bold",size=rel(1.2)),
                legend.title = element_text(size=rel(1.3)),
                legend.text = element_text(size=rel(1.3)),
                axis.text.x = element_text(size=rel(1.3)),
                axis.text.y = element_text(size=rel(1.3)))
       
    })
    
    IDW <- reactive({
        sptdf <- sptdfMean()
        sptdf_grid <- grid_pred()
        idw_p <- idw(varprom ~ 1, sptdf, sptdf_grid)
        idw_p
    })
    
    output$IDWPlot <- renderPlot({
      spdf_fortified <- map_base()
      unidades <- un_med()
      dbbp <- point_names()
      idw.d<- IDW()
      df1 <- data.frame(idw.d)
      predaprox <- round(df1$var1.pred)
      df1$predaprox <- predaprox
      
      scpts <- seq(min(dbbp$varprom), max(dbbp$varprom), length.out = 5)
      scpts <- as.numeric(round(scpts,2))
      scrtr <- seq(min(df1$var1.pred), max(df1$var1.pred), length.out = 5)
      scrtr <- as.numeric(round(scrtr,2))
      titulo <- "PROVINCIA DE CHIMBORAZO\nINTERPOLACION IDW"
      #ag1 <- "Predicciones de\n"
      ag1 <- "Valor"
      input <- input$var_sel2
      ag2 <- "\n(en "
      #unidades <- c("C","%","KJ","Kj","C","cm/h","grados")
      ag3 <- ")"
      ag4 <- "Valores de\n"
      ag5 <- "Intensidad de\n"
      
      ggplot() + geom_raster(data=df1, aes(x = x1, y = x2, fill = predaprox)) +
        scale_fill_continuous(low = "black", high = "yellow",breaks=scrtr, guide="colorbar",na.value="white") +
        scale_alpha_continuous(breaks=scrtr) +
        geom_polygon(data = spdf_fortified, aes(x=long, y = lat, group=group), color="grey",fill="grey", alpha=0) +
        geom_point(data= dbbp,aes(x=X, y=Y),size=3,color="orange",stroke=FALSE) +
        scale_size_continuous(breaks=scpts) +
        #scale_alpha_continuous(breaks=scpts) +
        scale_color_gradient(breaks=scpts,low = "green", high = "red") +
        guides( colour = guide_legend()) +
        geom_shadowtext(data= dbbp,aes(x=X, y=Y, label=mytext),
                        fontface = "bold",size=3.2)+
        #scale_fill_gradient(high = "yellow", low = "white")+
        #scale_color_viridis(option=sample(LETTERS[2:5],1)) +
        theme_bw() + 
        #theme_void()+  
        labs(title= titulo, x="Coordenada X", y="Coordenada Y",
             color=paste0(ag4,input,ag2,unidades,ag3),
             size=paste0(ag4,input,ag2,unidades,ag3),
             fill=paste0(ag1,ag2,unidades,ag3),
             alpha=paste0(ag1,input,ag2,unidades,ag3)) +
        theme(text = element_text(size=10),
              title = element_text(face="bold",size=rel(1.2)),
              axis.title.x = element_text(face="bold",size=rel(1.2)),
              axis.title.y = element_text(face="bold",size=rel(1.2)),
              legend.title = element_text(size=rel(1.3)),
              legend.text = element_text(size=rel(1.3)),
              axis.text.x = element_text(size=rel(1.3)),
              axis.text.y = element_text(size=rel(1.3)))

    })
    
    OK <- reactive({
        sptdf <- sptdfMean()
        sptdf_grid <- grid_pred()
        tv <- var_tv()
        ok_p <- krige(log1p(varprom) ~ 1, locations = sptdf, 
                      newdata = sptdf_grid, model = tv) 
        ok_p$var1.pred <- expm1(ok_p$var1.pred)
        ok_p$var1.var <- expm1(ok_p$var1.var)
        ok_p
    })
    
    output$OKPlot <- renderPlot({
      spdf_fortified <- map_base()
      unidades <- un_med()
      dbbp <- point_names()
      ok.d<- OK()
      df1 <- data.frame(ok.d)
      predaprox <- round(df1$var1.pred)
      varaprox <- round(df1$var1.var)
      df1$predaprox <- predaprox
      df1$varaprox <- varaprox
      
      scpts <- seq(min(dbbp$varprom), max(dbbp$varprom), length.out = 5)
      scpts <- as.numeric(round(scpts,2))
      scrtr <- seq(min(df1$var1.pred), max(df1$var1.pred), length.out = 5)
      scrtr <- as.numeric(round(scrtr,2))
      titulo <- "PROVINCIA DE CHIMBORAZO\nINTERPOLACION KRIGING"
      #ag1 <- "Predicciones de\n"
      ag1 <- "Valor"
      input <- input$var_sel2
      ag2 <- "\n(en "
      #unidades <- c("C","%","KJ","Kj","C","cm/h","grados")
      ag3 <- ")"
      ag4 <- "Valores de\n"
      ag5 <- "Intensidad de\n"
      
      ggplot() + geom_raster(data=df1, aes(x = x1, y = x2, fill = predaprox)) +
        scale_fill_continuous(breaks=scrtr, low = "darkred", high = "yellow",guide="colorbar",na.value="white") +
        #scale_alpha_continuous(breaks=scrtr) +
        geom_polygon(data = spdf_fortified, aes(x=long, y = lat, group=group), color="grey",fill="grey", alpha=0) +
        geom_point(data= dbbp,aes(x=X, y=Y),size=3,stroke=FALSE) +
        scale_size_continuous(breaks=scpts) +
        scale_alpha_continuous(breaks=scpts) +
        scale_color_gradient(breaks=scpts,low = "blue", high = "green") +
        guides( colour = guide_legend()) +
        geom_shadowtext(data= dbbp,aes(x=X, y=Y, label=mytext),
                        fontface = "bold",size=3.2)+
        #scale_fill_gradient(high = "yellow", low = "white")+
        #scale_color_viridis(option=sample(LETTERS[2:5],1)) +
        theme_bw() + 
        #theme_void()+  
        labs(title= titulo, x="Coordenada X", y="Coordenada Y",
             color=paste0(ag4,input,ag2,unidades,ag3),
             size=paste0(ag4,input,ag2,unidades,ag3),
             fill=paste0(ag1,ag2,unidades,ag3)) +
        theme(text = element_text(size=10),
              title = element_text(face="bold",size=rel(1.2)),
              axis.title.x = element_text(face="bold",size=rel(1.2)),
              axis.title.y = element_text(face="bold",size=rel(1.2)),
              legend.title = element_text(size=rel(1.3)),
              legend.text = element_text(size=rel(1.3)),
              axis.text.x = element_text(size=rel(1.3)),
              axis.text.y = element_text(size=rel(1.3)))

    })
    
    cv_thiessen <- reactive({
        sptdf <- sptdfMean()
        cv.th <- krige.cv(varprom ~ 1, sptdf,nmax = 1)
        cv.th
    })
    
    cv_idw <- reactive({
        sptdf <- sptdfMean()
        cv.idw <- krige.cv(varprom ~ 1, sptdf)
        cv.idw
    })
    
    cv_ok <- reactive({
        sptdf <- sptdfMean()
        tv <- var_tv()
        cv.ok <- krige.cv(log1p(varprom) ~ 1, locations = sptdf, model = tv)
        cv.ok$var1.pred <- expm1(cv.ok$var1.pred)
        cv.ok$var1.var <- expm1(cv.ok$var1.pred)
        cv.ok$observed <- sptdf$varprom
        cv.ok$residual <- cv.ok$var1.pred-cv.ok$observed
        cv.ok
    })
    
    #output$SpaCorPlot <- renderPlot({
    #    cv.th <- cv_thiessen()
    #    cv.idw <- cv_idw()
    #    cv.ok <- cv_ok()
    #    par(mfrow=c(1,3))
    #    print(plot(var1.pred~observed,cv.th, main="Thiessen"), split=c(1,1,3,1), more=TRUE)
    #    print(plot(var1.pred~observed,cv.idw, main="IDW"), split=c(2,1,3,1), more=TRUE)
    #    print(plot(var1.pred~observed,cv.ok, main="Kriging"), split=c(3,1,2,1), more=FALSE)
     #   
    #})
    
    output$SpaCorPlot <- renderPlot({
      cv.th <- cv_thiessen()
      cv.idw <- cv_idw()
      cv.ok <- cv_ok()
      pred <- data.frame(Thiessen=cv.th$var1.pred, IDW=cv.idw$var1.pred,Kriging=cv.ok$var1.pred)
      obs <- data.frame(Thiessen=cv.th$observed, IDW=cv.idw$observed,Kriging=cv.ok$observed)
      
      df1 <- obs [,1:3] %>%
        gather(key = "Metodo", value = Observado)
      df2 <- pred[,1:3] %>%
        gather(key = "Metodo", value = Estimado)
      df <- bind_cols(df1,df2[2])
   
      ggplot(df, aes(x = Observado, y = Estimado)) + 
        geom_point() + 
        ggtitle(paste0("Dispersion Observados vs Estimados\n",input$var_sel2))+
        facet_wrap(~ Metodo, scales="free",ncol = 4)+
        theme_minimal()+
        theme(text = element_text(size=16),
              axis.text.x = element_text(size=rel(1.3)),
              axis.text.y = element_text(size=rel(1.3)),
              strip.text.x = element_text(size=rel(2)),
              strip.text.y = element_text(size=rel(2)))
      
    })
    #output$SpaBoxPlot <- renderPlot({
    #    cv.th <- cv_thiessen()
     #   cv.idw <- cv_idw()
     #   cv.ok <- cv_ok()
     #   boxplot(cv.th$residual, cv.idw$residual, cv.ok$residual, main="Thiessen, IDW, Kriging")
        
    #})
    
    output$SpaBoxPlot <- renderPlot({
      cv.th <- cv_thiessen()
      cv.idw <- cv_idw()
      cv.ok <- cv_ok()
      datafil <- data.frame(Thiessen=cv.th$residual, IDW=cv.idw$residual,Kriging=cv.ok$residual)
      df <- datafil[,1:3] %>%
        gather(key = "Metodo", value = Residuos)
      
      ggplot(df, aes(x = Metodo, y = Residuos)) + 
        geom_boxplot() +
        facet_wrap(~ Metodo, scales="free",ncol = 4)+
        ylab(input$var_sel2) + xlab("Distribucion de los Residuos")+
        theme_minimal()+
        theme(text = element_text(size=16),
              axis.text.x = element_text(size=rel(1.3)),
              axis.text.y = element_text(size=rel(1.3)),
              strip.text.x = element_text(size=rel(2)),
              strip.text.y = element_text(size=rel(2)))
      
    })
    
 
    val_cruz <- reactive({
        sptdf <- sptdfMean()
        cv.th <- cv_thiessen()
        cv.idw <- cv_idw()
        cv.ok <- cv_ok()
        met <- c("Thiessen","IDW","Kriging")
        #correlacion, idealmente 1
        c1 <- cor(cv.th$var1.pred,cv.th$observed)
        c2 <- cor(cv.idw$var1.pred,cv.idw$observed)
        c3 <- cor(cv.ok$var1.pred,cv.ok$observed)
        ct <- c(c1,c2,c3)
        #correlacion, idealmente 1
        c11 <- cor(cv.th$var1.pred,cv.th$residual)
        c12 <- cor(cv.idw$var1.pred,cv.idw$residual)
        c13 <- cor(cv.ok$var1.pred,cv.ok$residual)
        ct2 <- c(c11,c12,c13)
        #media de residuos, idealmente 0
        m1 <- mean(cv.th$residual)
        m2 <- mean(cv.idw$residual)
        m3 <- mean(cv.ok$residual)
        mt <- c(m1,m2,m3)
        #Desviaciones estandar del error de interpolacion (residuos). Idealmente pequeno
        de1 <- sd(cv.th$residual)
        de2 <- sd(cv.idw$residual)
        de3 <- sd(cv.ok$residual)
        det <- c(de1,de2,de3)
        #MAE
        MAE1 <- mae(cv.th$observed,cv.th$var1.pred)
        MAE2 <- mae(cv.idw$observed,cv.idw$var1.pred)
        MAE3 <- mae(cv.ok$observed,cv.ok$var1.pred)
        MAET <- c(MAE1,MAE2,MAE3)
        #MSE
        MSE1 <- mse(cv.th$observed,cv.th$var1.pred)
        MSE2 <- mse(cv.idw$observed,cv.idw$var1.pred)
        MSE3 <- mse(cv.ok$observed,cv.ok$var1.pred)
        MSET <- c(MSE1,MSE2,MSE3)
        # MSPE (mean square predictor error), idealmente pequeno
        MSPE1 <- mean(cv.th$residual^2)
        MSPE2 <- mean(cv.idw$residual^2)
        MSPE3 <- mean(cv.ok$residual^2)
        MSPET <- c(MSPE1,MSPE2,MSPE3)
        #Error medio cuadratico (RMSE) es una medida general. Idealmente pequeno
        RMSE1 <- sqrt(sum(cv.th$residual^2)/length(cv.th$residual))
        RMSE2 <- sqrt(sum(cv.idw$residual^2)/length(cv.idw$residual))
        RMSE3 <- sqrt(sum(cv.ok$residual^2)/length(cv.ok$residual))
        RMSET <- c(RMSE1,RMSE2,RMSE3)
        #Varianza de residuos, idealmente cercanos a cero
        var1 <- var(cv.th$residual, na.rm=T)
        var2 <- var(cv.idw$residual, na.rm=T)
        var3 <- var(cv.ok$residual, na.rm=T)
        vart <- c(var1 ,var2 ,var3)
        # Cantidad de variacion explicada por el modelo en cada metodo, idealmente cercano a 100
        VE1 <- (1-var(cv.th$residual, na.rm=T)/var(sptdf$varprom))*100
        VE2 <- (1-var(cv.idw$residual, na.rm=T)/var(sptdf$varprom))*100
        VE3 <- (1-var(cv.ok$residual, na.rm=T)/var(sptdf$varprom))*100
        VET <- c(VE1,VE2,VE3)
        #tabla
        #tbl <- data.frame(Metodo=met,Cor.Obs_Pred=ct,Cor.Pred_Res=ct2,Media_Res=mt,Var_Res=vart,Desv_Res=det,MSPE=MSPET,MAE=MAET,MSE=MSET,RMSE=RMSET,Var.Expl=VET)
        tbl <- data.frame(Metodo=met,Cor.Obs_Pred=ct,Cor.Pred_Res=ct2,Media_Res=mt,Var_Res=vart,Desv_Res=det,MSPE=MSPET,MAE=MAET,MSE=MSET,RMSE=RMSET)
        
        tbl
    })
    output$table4 <- renderTable({
        dt <- val_cruz()
        dt
    })
    
    output$downloadtable1 <- downloadHandler(
      filename = function() {
        paste("Est_Desc-", Sys.Date(), ".xlsx")
      },
      content = function(file) {
        write.xlsx(EstDes(), file)
      }
    )
    output$downloadtable2 <- downloadHandler(
      filename = function() {
        paste("Datos_Filtrados-", Sys.Date(), ".xlsx")
      },
      content = function(file) {
        write.xlsx(Fil_date_exp(), file)
      }
    )
    output$downloadtable3 <- downloadHandler(
      filename = function() {
        paste("Val_Cruzada-", Sys.Date(), ".xlsx")
      },
      content = function(file) {
        write.xlsx(val_cruz(), file)
      }
    )
    
    #vc_kg <- reactive({
    #  sptdf <- sptdfMean()
    #  cv.th <- cv_thiessen()
    #  cv.idw <- cv_idw()
    #  cv.ok <- cv_ok()
    #  compare.cv(cv.th <- cv_thiessen()
    #             cv.idw <- cv_idw()
    #             cv.ok <- cv_ok())
    #})
    
    output$downloadvckg <- downloadHandler(
      filename = function() {
        paste("Val_Cruzada-", Sys.Date(), ".xlsx")
      },
      content = function(file) {
        write.xlsx(vc_kg(), file)
      }
    )
    
})
