# Helper

# Lista de imágenes sinópticas
sinls <- c(
  'Agua precipitable' = 'Agua_precipitable.png',
  'Presi\u00f3n reducida nivel del mar' = 'presionsuperficial.png',
  'L\u00edneas de corriente 200 hPa' = 'streamlines.png',
  'Divergencia 850 hPa' = 'div_850.png',
  'Divergencia 200 hPa' = 'div_200.png',
  'Velocidad vertical 800 hPa' = 'Vel_vert_800.png',
  'Velocidad vertical 500 hPa' = 'Vel_vert_500.png',
  'Velocidad vertical 300 hPa' = 'Vel_vert_300.png',
  '\u00cdndice CAPE' = 'cape.png',
  '\u00cdndice GDI' = 'GDI.png',
  '\u00cdndice K' = 'IndiceK.png'
)

# Función para convertir bits a temperatura de brillo
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

# Calcular alto de ventana
alto_box <- function(){

  ancho <- 1200
  alto <- 926
  window_width <- as.integer(unlist(strsplit(JS('window.innerWidth'), 'p'))[1])
  window_width <- (window_width - 150)/2
  box_height <- round(alto*window_width/ancho, 0)

}

cap <- c(
    "Huaraz" = "Centro_grafica-1.png",
    "Lima" = "Centro_grafica-2.png",
    "Ica" = "Centro_grafica-3.png",
    "Hu\u00e1nuco" = "Centro_grafica-4.png",
    "Cerro de Pasco" = "Centro_grafica-5.png",
    "Huancayo" = "Centro_grafica-6.png",
    "Huancavelica" = "Centro_grafica-7.png",
    "Pucallpa" = "Centro_grafica-8.png",
    "Tumbes" = "Norte_grafica-1.png",
    "Piura" = "Norte_grafica-2.png",
    "Chiclayo" = "Norte_grafica-3.png",
    "Trujillo" = "Norte_grafica-4.png",
    "Cajamarca" = "Norte_grafica-5.png",
    "Chachapoyas" = "Norte_grafica-6.png",
    "Tarapoto" = "Norte_grafica-7.png",
    "Iquitos" = "Norte_grafica-8.png",
    "Arequipa" = "Sur_grafica-1.png",
    "Ilo" = "Sur_grafica-2.png",
    "Tacna" = "Sur_grafica-3.png",
    "Ayacucho" = "Sur_grafica-4.png",
    "Abancay" = "Sur_grafica-5.png",
    "Cusco" = "Sur_grafica-6.png",
    "Puno" = "Sur_grafica-7.png",
    "Puerto Maldonado" = "Sur_grafica-8.png"
)

sel_implot <- c(
	"Banda 2" = "PE_OR_ABI-L2-CMIPF-M3C02_G16_s20183251615368_e20183251626135_c20183251626212-114300_0.nc.tiff",
	"Banda 7" = "PE_OR_ABI-L2-CMIPF-M3C07_G16_s20180102000410_e20180102011188_c20180102011243.nc.tiff",
	"Banda 8" = "PE_OR_ABI-L2-CMIPF-M3C08_G16_s20180101930410_e20180101941177_c20180101941249.nc.tiff",
	"Banda 9" = "PE_OR_ABI-L2-CMIPF-M3C09_G16_s20183251615368_e20183251626140_c20183251626217.nc.tiff",
	"Banda 13" = "PE_OR_ABI-L2-CMIPF-M3C13_G16_s20183251615368_e20183251626146_c20183251626219.nc.tiff",
	"Banda 14" = "PE_OR_ABI-L2-CMIPF-M3C14_G16_s20180101930410_e20180101941177_c20180101941267.nc.tiff",
	"Banda 15" = "PE_OR_ABI-L2-CMIPF-M3C15_G16_s20180102000410_e20180102011183_c20180102011270.nc.tiff"
)
	  
#Climatologia
	  
	  #Para temperatura maxima
meteo = read.csv("www/para_app.csv")
prueba <- mutate(meteo, Fecha = paste(Anio, Mes, 15, sep = '-'))
prueba = mutate(prueba, Fecha = as.Date(Fecha))
prueba$Ch_Mes <- factor(prueba$Mes) 
levels(prueba$Ch_Mes) <- c("ENE", "FEB", "MAR", "ABR", "MAY", "JUN", "JUL", "AGO", "SEP", "OCT", "NOV", "DEC")
prueba$char = as.character(prueba$Ch_Mes)
cristina = group_by(prueba,Departamento ,Mes)
cristina = summarise(cristina, Txclim = mean(Tmaxp, na.rm = T), Ds = sd(Tmaxp, na.rm = T), Tnclim = mean(Tminp, na.rm = T), de = sd(Tminp, na.rm = T))
cristina = mutate(cristina, High = Txclim + Ds, Low = Txclim - Ds, alto = Tnclim + de , bajo = Tnclim - de)
cristina$Ch_Mes <- factor(cristina$Mes) 
levels(cristina$Ch_Mes) <- c("ENE", "FEB", "MAR", "ABR", "MAY", "JUN", "JUL", "AGO", "SEP", "OCT", "NOV", "DEC")
cristina$char = as.character(cristina$Ch_Mes)
var_ko = group_by(prueba,Departamento)
var_fe = summarise(var_ko, 
                   Media = mean(Tmaxp, na.rm = T),
                   Max = max(Tmaxp, na.rm = T), 
                   Min = min(Tmaxp, na.rm = T),
                   Mediana = median(Tmaxp, na.rm = T),
                   Desv.est = sd(Tmaxp,na.rm = T), 
                   Q1 = quantile(Tmaxp,0.25, na.rm = T),
                   Q3 = quantile(Tmaxp, 0.75, na.rm = T),
                   numtotal = length(Tmaxp),
                   numNA = sum(is.na(Tmaxp)),
                   Porc_na = (numNA/numtotal)*100)
din = rep('Temperatura maxima',each = 5)
var_fe$Variable = paste0(din)
var_fe = select(var_fe, Variable,Departamento,Media, Max, Min, Mediana, Desv.est, Q1, Q3)

var_fi = summarise(var_ko, 
                   Media = mean(Tminp, na.rm = T),
                   Max = max(Tminp, na.rm = T), 
                   Min = min(Tminp, na.rm = T),
                   Mediana = median(Tminp, na.rm = T),
                   Desv.est = sd(Tminp,na.rm = T), 
                   Q1 = quantile(Tminp,0.25, na.rm = T),
                   Q3 = quantile(Tminp, 0.75, na.rm = T),
                   numtotal = length(Tminp),
                   numNA = sum(is.na(Tminp)),
                   Porc_na = (numNA/numtotal)*100)
dina = rep('Temperatura minima',each = 5)
var_fi$Variable = paste0(dina)
var_fi = select(var_fi, Variable,Departamento,Media, Max, Min, Mediana, Desv.est, Q1, Q3)

var_vi = summarise(var_ko, 
                   Media = mean(Precs, na.rm = T),
                   Max = max(Precs, na.rm = T), 
                   Min = min(Precs, na.rm = T),
                   Mediana = median(Precs, na.rm = T),
                   Desv.est = sd(Precs,na.rm = T), 
                   Q1 = quantile(Precs,0.25, na.rm = T),
                   Q3 = quantile(Precs, 0.75, na.rm = T),
                   numtotal = length(Precs),
                   numNA = sum(is.na(Precs)),
                   Porc_na = (numNA/numtotal)*100)
dinm = rep('Precipitacion',each = 5)
var_vi$Variable = paste0(dinm)
var_vi = select(var_vi,Variable,Departamento, Media, Max, Min, Mediana, Desv.est, Q1, Q3)
stn = c("Junin", "Huanuco", "Ancash", "Lima" , "Ucayali")
Graf = c("Temperatura", "Precipitacion")
adriana = read.csv("www/metadata.csv")
