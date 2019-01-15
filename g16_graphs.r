#Función para crear la gráfica
g16.graph <- function(nomb, dat, lon, lat, band, shp, puntos, limites = FALSE){

  par(mar = c(3,6,1,3))
  
  breaks <- g16.brk(band)
  pal.goes <- g16.pal(band)
  
  #Gráfica de contornos
  filled.contour2(
  	lon,
  	lat,
  	dat[ , ],
  	xlim = c(min(lon), max(lon)),
  	ylim = c(min(lat), max(lat)),
  	zlim = range(breaks),
  	levels = breaks,
  	axes = TRUE,
  	col = colorRampPalette(pal.goes, space='Lab')(length(breaks)-1)
  )
  
  #Contorno de los países
  map(shp, add=TRUE, lwd= 0.9,
      col = ifelse(band <= 7, 'yellow', 'black'))
  if(band %in% c(13, 14, 15)){
  map (puntos, add=TRUE, lwd= 2.5, col = 'black')
  map (puntos, add=TRUE, lwd= 1.5, col = 'red')
  map (puntos, add=TRUE, lwd= 0.7, col = 'yellow')	
  }else{
  map(puntos, add=TRUE, lwd=1, col = pointcolor(band)) 
  }
 
  if(limites){limit(band)}
  
  #Poniendo texto en imagen (lili)
  showlat <- (max(lat) - min(lat))*0.05 + min(lat)
  showlon <- (max(lon) - min(lon))*0.25 + min(lon)
  text(showlon, showlat, g16.not(nomb), 
       col = ifelse(band <= 7, 'yellow', 'black'))
  
  #Leyenda de la gráfica
  par(mar = c(3,4,1,32))
  par(new = TRUE)
  image.scale(
  	dat,
  	zlim = range(dat),
  	breaks = breaks,
  	col = colorRampPalette(pal.goes, space='Lab')(length(breaks)-1),
  	horiz = FALSE,
  	xlab = '',
  	ylab = titley(band),
  	yaxt = 'n',
  	las = 2
  )
  
  #Parámetros de la leyenda
  # axis(2, at = breaks[seq(1, length(breaks), 5)], las = 2)
  g16.ley(band, breaks)
  
  box()

  
}

#Función con los valores de escala y desfase de las bandas del G16
fact <- function(dat, band){

  if(band == 1){dat <- dat*0.0003175*100}
  else if(band == 2){dat <- dat*0.0002442*100}
  else if(band == 7){dat <- dat*0.0130962 + 197.31 - 273.15}
  else if(band == 8){dat <- dat*0.0422499 + 138.05 - 273.15}
  else if(band == 9){dat <- dat*0.0423391 + 137.70 - 273.15}
  else if(band == 10){dat <- dat*0.0498892 + 126.91 - 273.15}
  else if(band == 12){dat <- dat*0.0472703 + 117.49 - 273.15}
  else if(band == 13){dat <- dat*0.0614533 + 89.62 - 273.15}
  else if(band == 14){dat <- dat*0.0598507 + 96.19 - 273.15}
  else if(band == 15){dat <- dat*0.0595608 + 97.38 - 273.15}

  return(dat)
  
}

#Función para la selección de una paleta de colores
g16.pal <- function(band){

  if(band <= 6){
  
    # plo5 <-grey.colors(40,star=0.8,end=1)
    plo5 <-grey.colors(40,star=0.75,end=1)
	# plo4 <-grey.colors(40,star=0.6,end=0.8)
	plo4 <-grey.colors(40,star=0.5,end=0.75)
	# plo3 <-grey.colors(60,star=0.4,end=0.6)
	plo3 <-grey.colors(60,star=0.3,end=0.5)
	# plo2 <-grey.colors(50,star=0.2,end=0.4)
	plo2 <-grey.colors(50,star=0.15,end=0.3)
	# plo1 <-grey.colors(10,star=0,end=0.2)
	plo1 <-grey.colors(10,star=0,end=0.15)
	pal.goes <- c(plo1,plo2,plo3,plo4,plo5)
  
  }else if(band == 7){
  
    cya <- colorRampPalette(c('darkslategray4','darkcyan'))(10)
	cyam <- colorRampPalette (c('darkcyan','white'))(80)
	gr2 <- colorRampPalette(c('white', 'black'))(80)
	pal.goes <- c(cya,cyam,gr2)
  
  }else if(band %in% 8:10){
  
    tae <- colorRampPalette(c('#02D170','#129057'))(14) #-87 -80
    aqu <- colorRampPalette(c('#004A13','#00eb3b'))(26) #-80 -67
    gre <- colorRampPalette(c('#00f03d','#84FFA3'))(10) #-67 -62
    blu <- colorRampPalette(c('#0000B2','#ffffff'))(40) #-62 -42
	whi <- colorRampPalette(c('#ffffff','#ffffff'))(8)  #-42 -38
    gry <- colorRampPalette(c('#ffffff','#adadab'))(25) #-38 -25.5
    mar <- colorRampPalette(c('#8c8475','#7a4e00'))(5)  #-25.5 -23
    moz <- colorRampPalette(c('#855500','#f7b51c'))(10) #-23 -18
    ama <- colorRampPalette(c('#f8b018','#ff8900'))(12) #-18 -12
    roj <- colorRampPalette(c('#FD7F00','#fd0b00'))(8)  #-12 -8
    neg <- colorRampPalette(c('#f90b00','#000000'))(46) #-8 -15
    pal.goes <- c(tae,aqu,gre,blu,whi,gry,mar,moz,ama,roj,neg)
  
  }else if(band %in% 11:16){
  
    mor <- colorRampPalette(c('darkmagenta','mediumorchid1'))(20)
    gr1 <- colorRampPalette(c('white','black'))(20)
    red <- colorRampPalette(c('darkred','red'))(10)
    org <- colorRampPalette(c('red','yellow'))(20)
    gre <- colorRampPalette(c('yellow','green'))(20)
    tae <- colorRampPalette(c('green','navyblue'))(20)
    blu <- colorRampPalette(c('navyblue','cyan'))(28)
    grr <- grey.colors(22,star=1,end=0.9)
	grrr <- grey.colors(22,star=0.9,end=0.80)
	gr2 <- grey.colors(32,star=0.80,end=0.5)
	gr3 <- grey.colors(40,star=0.5,end=0.3)
	gr4 <- grey.colors(40,star=0.3,end=0)
    pal.goes <- c(mor,gr1,red,org,gre,tae,blu,grr,grrr,gr2,gr3,gr4)
  
  }else if(band %in% 18 ){
   
    mor <- colorRampPalette(c('#BA55D3','#663399'))(8)   #0.25-2
    blu <- colorRampPalette(c('#0000CD','#00BFFF'))(32)  #2-10
    gre <- colorRampPalette(c('#00CED1','#00FFFF'))(40)  #10-20
    yel <- colorRampPalette(c('#90EE90','#7CFC00'))(40)  #20-30
    ora <- colorRampPalette(c('#FFD700','#DAA520'))(80)  #30-50
    red <- colorRampPalette(c('#FF8C00','#DC143C'))(150) #50-75  
	fux <- colorRampPalette(c('#DC143C','#FF1493'))(50) #75-100
    
	
    pal.goes <- c(mor,blu,gre,yel,ora,red,fux)
  }
  

  return(pal.goes)
  
}

#Función para selección de rangos
g16.brk <- function(band){

  if(band %in% 1:6){breaks <- seq(0,100,0.5)}
  else if(band %in% 7){breaks <- -110:60}
  else if(band %in% 8:10){breaks <- seq(-87,15,0.5)}
  else if(band %in% 11:15){breaks <- seq(-87,60,0.5)}
  else if(band %in% 18){breaks <- seq(0,100,0.25)}
  return(breaks)
  
}

#Función para modificar la leyenda
g16.ley <- function(band, brk){

  if(band %in% 1:6){axis(2, at = brk[seq(1, length(brk), 20)], las = 2)}
  else if(band %in% 7){axis(2, at = brk[seq(1, length(brk), 30)], las = 2)}
  else if(band %in% 8:10){axis(2, at = brk[seq(15, length(brk), 20)], las = 2)}
  else if(band %in% 11:15){axis(2, at = brk[seq(15, length(brk), 40)], las = 2)}
  else if(band %in% 18){axis(2, at = brk[seq(1,length(brk),40)], las=2)}
}

#Función para generar el texto de las imágenes
g16.not <- function(nomb){

  Anio <- substr(nomb, 31, 34)
  Diaj <- substr(nomb, 35, 37)
  Hora <- substr(nomb, 38, 39)
  Minuto <- substr(nomb, 40, 41)
  Banda <- substr(nomb, 23, 24)
  
  Anio <- as.integer(Anio)
  
  Fecha <- as.Date(as.integer(Diaj), origin = paste0(Anio-1,'-12-31'))
  
  fechita <- paste0(Fecha, ' ', Hora, ':', Minuto, ' UTC Banda ', Banda)
  
  return(fechita)

}

#Función para agregar el contorno de los demás países del continente
limit <- function(band){

  # cont1 <- readOGR(dsn = 'E:/Shapes/Ecuador', layer = 'Ecuador')
  # cont2 <- readOGR(dsn = 'E:/Shapes/Brasil', layer = 'Brasil')
  # cont3 <- readOGR(dsn = 'E:/Shapes/Chile', layer = 'Chile')
  
  # map(cont1, add = T, lwd = 0.7, col = ifelse(band <= 7, 'yellow', 'black'))
  # map(cont2, add = T, lwd = 0.7, col = ifelse(band <= 7, 'yellow', 'black'))
  # map(cont3, add = T, lwd = 0.7, col = ifelse(band <= 7, 'yellow', 'black'))
  
  cont <- readOGR(dsn = 'E:/Shapes/SA', layer = 'paises')
  cont <- cont[c(30,43,61), ]
  map(cont, add = T, lwd = 0.9, col = ifelse(band <= 7, 'yellow', 'black'))

}

# Función para generar el título de las leyendas
titley <- function(band){

  if(band == 2){
	titulo <- 'Reflectancia (%)'
  }else if(band %in% c(7,8,9,13,14,15)){ 
    titulo <- expression('Temperatura de brillo ('*degree*'C)')
  }else{
	titulo <- 'Precipitacion (mm/h)'
  }

}

# Funciòn pada dar color a los puntos
pointcolor <- function(band){
	if (band %in% c(2,7)){
		col = 'red'
	}else if(band %in% c(8,9)){
		col = 'deeppink'
	}else{
		col = 'black'
	}	
}	