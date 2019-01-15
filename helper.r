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