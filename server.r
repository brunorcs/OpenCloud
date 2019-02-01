#server G16_PE/Band
#source('E:/TP/g16_graphs.r')
source('helper.r')

server <- function(input, output,session){

  options(stringsAsFactors = FALSE)

  #####################################################################
  #NOWCAST

  #UI con link de la dinámico para abrir la imgen del G16
  output$imgUI <- renderUI({
  
    lista1 <- list.files('www/G16/', pattern = paste0(input$loop,input$banda1))
	
    link <- ifelse(input$loop == 'c', lista1[input$numima], lista1)
	
    tags$a(
      imageOutput('img', height = '825px'), 
      href = paste0('G16/', link),
      target = '_blank'
    )
	
  })

  #UI de la imagen G16
  output$img <- renderImage({
  
    lista2 <- list.files('www/G16/', pattern = paste0(input$loop,input$banda1), full.name = T)

	outfile <- ifelse(input$loop == 'c', lista2[input$numima], lista2)
	
	list(src = outfile,
	  #contentType = 'image/png',
	  width = 880,
	  heigth = 825,
	  alt = 'Generando imagen'
	)
  
  }, deleteFile = FALSE)
  
   output$relojillo <- renderValueBox({
   
    valueBox(invalidateLater(60000, session), 
	  icon = icon("time", lib = "glyphicon"),
      color = "light-blue",
	 paste("Hora local: ",format(Sys.time(),format='%H:%M'),"Lima,Peru")
	  )
	  
  }) 

   output$reloj_utc <- renderValueBox({
	valueBox( invalidateLater(60000, session), 
	  icon = icon("time", lib = "glyphicon"),
      color = "lime",
	  paste("Hora UTC: " ,format(as.POSIXlt(Sys.time(),format="%H:%M:%S",tz="UTC"),"%H:%M"),"LSRGM,UNALM"))
  })                                                

  #pal<- colorNumeric(c('grey99', 'black'), values(marcia),na.color='transparent')
  
  #####################################################################
  #ANALISIS
  
  #UI del Leaflet
  output$g16<-renderLeaflet({
  
    # lista3 <- list.files('Data/', full.name = T)
	# ruta <- paste0('D:/G16_PE/Band', input$banda2)
	# lista3 <- list.files(ruta, pattern = 'M3', full.name = T)
	lista3 <- list.files('Data/', pattern = input$banda2, full.name = T)
    rtr <- raster(tail(lista3, 1))
	
	rtr <-fact(rtr,as.integer(input$banda2)) 
	
	if(as.integer(input$banda2)==2){
	rtr[rtr<30] <- NA	
	}
	
	else if(as.integer(input$banda2)==7){
	rtr [rtr>0] <- NA
	}
    
	else{
	rtr[rtr>-40] <- NA
	}
	
      if(as.integer(input$banda2)==2){
        titan=" (%)"
      } 
	  else{
	  titan='   (ºC)'
	  }
    
	
	pal <- colorNumeric(c('grey99', 'black'), values(rtr), na.color='transparent')
	
    leaflet()%>% 
	addTiles()%>% 
	setView(lng = -76, lat = -12, zoom = 7)%>%
	addRasterImage(rtr, colors = pal, input$shingeky)%>%
	addLegend(pal = pal, values = values(rtr), title = paste0('Banda',input$banda2,titan))
	
  })
  
  # output$test <- renderPrint({
  
    # lista3 <- list.files('Data/', full.name = T)
	# lista3[as.integer(input$banda2)]
  
  # })
  
  #####################################################################
  # SINOPTICA
  
  # Dimensiones de las imagenes
  ancho <- 1200
  alto <- 926

  # Cargando las imágenes
  output$sinopUI1 <- renderImage({
  
    # Tamaño del box que tendrá la imagen
    width  <- session$clientData$output_sinopUI1_width
    height <- round(alto*width/ancho, 0)
  
    # Ruta de imagen
    sinim1 <- paste0('www/GFS/', input$sinop_1)
    
    # Parámetros de la imagen
    list(
      src = sinim1,
      width = width,
      height = height,
      alt = 'No hay imagen'
    )
  
  }, deleteFile = FALSE)
  
  output$sinopUI2 <- renderImage({
  
    # Tamaño del box que tendrá la imagen
    width  <- session$clientData$output_sinopUI2_width
    height <- round(alto*width/ancho, 0)
  
    # Ruta de imagen
    sinim2 <- paste0('www/GFS/', input$sinop_2)
    
    # Parámetros de la imagen
    list(
      src = sinim2,
      width = width,
      height = height,
      alt = 'No hay imagen'
    )
  
  }, deleteFile = FALSE)
  
  #####################################################################
  # DO REPORTE

	# Datos para el reporte
	rep_arch = c(
	"Tmax_Norte.csv",
	"Tmin_Norte.csv",
	"Prec_Norte.csv",
	"Tmax_Centro.csv",
	"Tmin_Centro.csv",
	"Prec_Centro.csv",
	"Tmax_Sur.csv",
	"Tmin_Sur.csv",
	"Prec_Sur.csv"
	)
	
	rep_dat <- list()
	
	for(i in 1:9){
	
	rep_dat[[i]] <- read.csv(paste0("www/Reporte/Tablas/", rep_arch[i]))
	
	}
	
	names(rep_dat) <- rep_arch
  
  # Seleccion de datos segun region 
  sel <- eventReactive(input$region, {
  
    if(input$region == 'Norte'){ # Tmax
      1:3
    }else if(input$region == 'Centro'){ # Tmin
      4:6
    }else{ # Prec
      7:9
    }
  
  })
  
  # Tablas 
  output$tabtmax <- renderDT(rep_dat[[ sel()[1] ]], selection = 'none', editable = TRUE, class = 'compact display')
  output$tabtmin <- renderDT(rep_dat[[ sel()[2] ]], selection = 'none', editable = TRUE, class = 'compact display')
  output$tabprec <- renderDT(rep_dat[[ sel()[3] ]], selection = 'none', editable = TRUE, class = 'compact display')
  
  # Variables proxy para edicion en tabla
  proxy_tmax <- dataTableProxy('tabtmax')
  proxy_tmin <- dataTableProxy('tabtmin')
  proxy_prec <- dataTableProxy('tabprec')
  
  # Proceso de edicion en tabla
  observeEvent(input$tabtmax_cell_edit, {
  
    info = input$tabtmax_cell_edit
    # str(info)
    i = info$row
    j = info$col
    v = info$value
    rep_dat[[ sel()[1] ]][i,j] <<- DT::coerceValue(v, rep_dat[[ sel()[1] ]][i,j])
    replaceData(proxy_tmax, rep_dat[[ sel()[1] ]], resetPaging = FALSE)
  
  })
  
  observeEvent(input$tabtmin_cell_edit,{
  
    info = input$tabtmin_cell_edit
    # str(info)
    i = info$row
    j = info$col
    v = info$value
    rep_dat[[ sel()[2] ]][i,j] <<- DT::coerceValue(v, rep_dat[[ sel()[2] ]][i,j])
    replaceData(proxy_tmin, rep_dat[[ sel()[2] ]], resetPaging = FALSE)
  
  })
  
  observeEvent(input$tabprec_cell_edit,{
  
    info = input$tabprec_cell_edit
    # str(info)
    i = info$row
    j = info$col
    v = info$value
    rep_dat[[ sel()[3] ]][i,j] <<- DT::coerceValue(v, rep_dat[[ sel()[3] ]][i,j])
    replaceData(proxy_prec, rep_dat[[ sel()[3] ]], resetPaging = FALSE)
  
  })
  
  # Boton para guardar cambios
  observeEvent(input$save_rep, {
  
    write.csv(rep_dat[[ sel()[1] ]], paste0("www/Reporte/Tablas/Tmax_", input$region, ".csv"), row.names = FALSE)
    write.csv(rep_dat[[ sel()[2] ]], paste0("www/Reporte/Tablas/Tmin_", input$region, ".csv"), row.names = FALSE)
    write.csv(rep_dat[[ sel()[3] ]], paste0("www/Reporte/Tablas/Prec_", input$region, ".csv"), row.names = FALSE)
  
  })
  
  # Boton para generar reporte
  observeEvent(input$rmark, {
  
    cat(paste0(input$region, "\n"), file = paste0("www/Reporte/log_", input$region, ".txt"), append = FALSE)
    cat(paste0(input$pronosticador1, "\n"), file = paste0("www/Reporte/log_", input$region, ".txt"), append = TRUE)
    
    rmarkdown::render(paste0("www/Reporte/reporte_", input$region, ".Rmd"))
  
  })
	
	output$desrep <- downloadHandler(
	
		filename = function(){
			rut <- paste0("reporte_", input$region, ".html")
		},
		content = function(file){
			file.copy(paste0("www/Reporte/reporte_", input$region, ".html"), file, overwrite = T)
		}
	
	)
	
   #####################################################################
  # REPORTE
  
  output$img_rep <- renderImage({
  
    ancho <- 1536
    alto <- 576
  
    ruta <- paste0("www/Reporte/Figuras/", input$ciudad)
    
    width  <- session$clientData$output_img_rep_width
    height <- round(alto*width/ancho, 0)
    
    list(
      src = ruta,
      width = width,
      height = height,
      alt = 'No hay imagen'
    )
  
  }, deleteFile = FALSE)
	
     ####################################################################
     #Climatologia
	output$distPlot <- renderPlotly({
    if(input$radio == "Temperatura"){
      x1 = filter(cristina, Departamento == input$sel)
      x1$average <- rowMeans(x1[,c("High", "Low")])  
   p <- plot_ly(x1, x = x1$Ch_Mes, y = x1$High,type = 'scatter', mode = 'lines',
                line = list(color = 'transparent'),
                showlegend = FALSE, name = 'High') %>%
     add_trace(y = x1$Low, type = 'scatter', mode = 'lines',
               fill = 'tonexty', fillcolor='rgba(0,100,80,0.2)', line = list(color = 'transparent'),
               showlegend = FALSE, name = 'Low') %>%
     add_trace(x = x1$Ch_Mes, y = x1$Tnclim, type = 'scatter', mode = 'lines',
               line = list(color='rgb(0,100,80)'),
               name = 'average') %>%
    add_trace(x = x1$Ch_Mes, y = x1$alto,type = 'scatter', mode = 'lines',
              line = list(color = 'transparent'),
              showlegend = FALSE, name = 'High') %>%
    add_trace(y = x1$bajo, type = 'scatter', mode = 'lines',
              fill = 'tonexty', fillcolor='rgba(0,100,80,0.2)', line = list(color = 'transparent'),
              showlegend = FALSE, name = 'Low') %>%
    add_trace(x = x1$Ch_Mes, y = x1$average, type = 'scatter', mode = 'lines',
              line = list(color='rgb(0,100,80)'),
              name = 'average') %>%
     layout(paper_bgcolor='rgb(255,255,255)', plot_bgcolor='rgb(229,229,229)',
           xaxis = list(title = "Meses",
                        gridcolor = 'rgb(255,255,255)',
                         showgrid = TRUE,
                         showline = FALSE,
                         showticklabels = TRUE,
                         tickcolor = 'rgb(127,127,127)',
                         ticks = 'outside',
                         zeroline = FALSE),
            yaxis = list(title = "Temperatura (grados Celsius)",
                         gridcolor = 'rgb(255,255,255)',
                         showgrid = TRUE,
                         showline = FALSE,
                         showticklabels = TRUE,
                         tickcolor = 'rgb(127,127,127)',
                         ticks = 'outside',
                         zeroline = FALSE))
          p$elementId <- NULL
          p
    }else if(input$radio == "Precipitacion"){
      x2 = filter(prueba, Departamento == input$sel)
      p1 <- ggplot(x2, aes(x = Ch_Mes, y = Precs)) + geom_boxplot()
      p1 + xlab('Meses') + ylab('Precipitacion')
    }else{
       print("Seleccione un tipo de grafica")
        
    }
   })
    
    output$brun <- renderTable({
    and <- rbind(filter(var_fe, Departamento == input$sel),filter(var_fi,
    Departamento == input$sel),filter(var_vi, Departamento == input$sel))
    and = select(and, Variable,Media, Max, Min, Mediana, Desv.est, Q1, Q3)
  })
    output$dista <- renderText({
if (input$sel == 'Lima'){
  'Lima'}else if(input$sel == 'Ucayali'){ 'Pucallpa'}else if(input$sel == 'Junin'){
    'Huancayo'}else if(input$sel == 'Huanuco'){'Huanuco'}else if(input$sel == 'Ancash'){
      'Huaraz'
    }
      
     })
    output$holo <- renderText({
      if (input$sel == 'Lima'){
        'Nana'}else if(input$sel == 'Ucayali'){ 'El Maronal'}else if(input$sel == 'Junin'){
          'Santa Ana'}else if(input$sel == 'Huanuco'){'Huanuco'}else if(input$sel == 'Ancash'){
            'Recuay'
          }
    })
    output$mymap <- renderLeaflet({
      m <- leaflet()
      m <- setView(m,lng = -76, lat = -10, zoom = 4.8)
      m <- addTiles(m)
      m
    })
    observe({
      adicta = filter(adriana, Nombre == input$sel)
      proxy <- leafletProxy("mymap", data = adicta)
      proxy <- setView(proxy, lng = adicta$Lon, lat = adicta$Lat, zoom = 9)
      proxy <- addMarkers(proxy, lng= adicta$Lon, lat=adicta$Lat,popup= adicta$Estacion)
    })
  
 }
