#UI G16
source('helper.r')

library(shiny)
library(shinydashboard)
library(raster)
library(leaflet)
library(DT)

ui <-dashboardPage(

  #Cabecera el dashboard
  dashboardHeader(
  
    title = 'OpenCloud',
    titleWidth = 150,
  
    #Menu de notificaciones
    dropdownMenu(type = 'notifications',
	
      notificationItem(
	  
        text = 'Alertas del G16',
        icon = icon('exclamation-triangle'),
        status = 'warning'
	  
      )
	
    )
  
  ),
  
  #Menu lateral
  dashboardSidebar(
  
    width = 150,
  
    sidebarMenu(
  
      menuItem('Nowcast', tabName = 'nowcast', icon = icon('dashboard')),
      menuItem('Analisis', tabName = 'analisis', icon = icon('th')),
      menuItem('Sinoptica', tabName = 'sinoptica', icon = icon('cloud')),
      menuItem('Reporte', tabName = 'doreporte', icon = icon('file')),
      menuItem('Pronostico', tabName = 'pronos', icon = icon('list-alt')),
      menuItem('Links', tabName = 'links' , icon = icon('fab fa-chrome')),
      menuItem('Directivas', tabName = 'directivas', icon = icon('fas fa-book')),
      menuItem('Glosario', tabName = 'glosario', icon = icon('book open'))
      
    )
  
  ),
  
  dashboardBody(
  
    tabItems(
    
      ####################################################
      # Sección Nowcast del menú lateral
      tabItem(tabName = 'nowcast',
	  
        fluidRow(
		
          #Imagen GOES 16
          column(width = 8,
		  
            box(width = NULL,
			
              uiOutput('imgUI')
			  
            ) 
		  
          ),
		  
          #Parámetros de selección de imagen
          column(width = 4,
		  
            box(width = NULL,
			
              radioButtons('loop', label = 'Animación',
                choices = list('Si' = 'B', 'No' = 'c'),
                selected = 'c'
              )
			
            ),
		  
            box(width = NULL,
		  
              radioButtons('banda1', label = 'Selección de bandas', 
                choices = list('Banda 02' = '02', 'Banda 07' = '07', 'Banda 08' = '08',
                  'Banda 09' = '09', 'Banda 13' = 13, 'Banda 14' = 14, 
                  'Banda 15' = 15, 'Autoestimador'=18),
                selected = '02'
              )
			  
            ),
			
            box(width = NULL,
			  
              sliderInput('numima', label = 'Número de imagen', 
                min = 1, max = 10, value = 10 )
              ),
			  
            valueBoxOutput("relojillo",width=4),
            valueBoxOutput("reloj_utc",width=4)
		  
          )
		
        )
	  
      ),
	  
      ####################################################
      #Sección de analisis de imágenes
      tabItem(tabName = 'analisis',
	  
        fluidRow(
		
          #Gráfica Leaflet
          box(width = 8,
		  
            leafletOutput('g16', height = 700)
		  
          ),
		  
          #Parámetros de selección de imágenes
          box(title = 'Selección de imagen', width = 2, heigth = 800,
		  
            radioButtons('banda2', label = 'Bandas', 
              choices = list('Banda 02' = '02', 'Banda 07' = '07', 
                'Banda 08' = '08', 'Banda 09' = '09', 'Banda 13' = '13', 
                'Banda 14' = '14', 'Banda 15' = '15'),
              selected = '13'
            )
			
          ),
		  
          box(width=4, 
		  
            sliderInput("shingeky",label='Opacidad de la Imagen',
              min = 0.0, max = 1.0, value = 1.0
            )
		  
          )

        )
		
      ),
     
      ####################################################
      # Sección de sinoptica, solo selecciona gráficas
      tabItem(tabName = 'sinoptica',
      
        box(width = 6,
        
          selectInput('sinop_1',
          
            label = h4('Seleccionar una gráfica'),
            choices = sinls,
            selected = head(sinls,1)
          
          ),
          
          imageOutput('sinopUI1', height = alto_box())
        
        ),
        
        box(width = 6,
        
          selectInput('sinop_2',
          
            label = h4('Seleccionar otra gráfica'),
            choices = sinls,
            selected = tail(sinls,1)
          
          ),
          
          imageOutput('sinopUI2', height = alto_box())
        
        )
      
      ),
      
      ####################################################
      # Sección para hacer el reporte automatizado
      
      tabItem(tabName = 'doreporte',
      
        fluidRow(
        
          column(width = 8,
          
            box(title = 'Temperatura máxima', width = NULL, collapsible = TRUE, status = "danger",
            
              DTOutput("tabtmax")
            
            ),
            
            box(title = 'Temperatura mínima', width = NULL, collapsible = TRUE, status = "primary",
            
              DTOutput("tabtmin")
            
            ),
            
            box(title = 'Precipitación', width = NULL, collapsible = TRUE, status = "success",
            
              DTOutput("tabprec")
            
            )
          
          ),
          
          column(width = 4,
        
            box(
          
              selectInput('region',
            
                label = h4('Seleccionar region'),
                choices = c('Norte', 'Centro', 'Sur'),
                selected = 'Norte'
            
              ),
              
              hr(),
              h4("Datos de reporte"),
              textInput("pronosticador1", label = "Pronosticador", value = ""),

              hr(),
              actionButton('save_rep', label = 'Guardar cambios'),
							
							hr(),
              actionButton('rmark', label = 'Generar reporte'),
							
							hr(),
							downloadButton("desrep", label = "Descargar Reporte")
            
            )
          
          )
        
        )
      
      ),
      
      # Sección donde se muestran los reportes
      tabItem(tabName = 'pronos',
      
        fluidRow(
        
          box(width = 6,
          
            selectInput("ciudad",
            
                label = "Reporte diario",
                choices = cap,
                selected = cap[2]
            
            )
            
          ),
          
          box(width = 8,
          
						img(src = "Reporte/Figuras/leyenda.png", heigh = "50%", width ="50%")
          
          ),
          
          box(width = 8,
          
            imageOutput("img_rep")
          
          )
          
        )
      
      ),
      
      ####################################################
      # Sección de links
      tabItem(tabName = 'links',
      
        fluidRow(
        
          box(title = 'Windy',
          
            'Muestra el pronostico del tiempo para toda la tierra.',
            br(),
            a("Acceder a esta pagina",
              href="https://www.windy.com",
              target="_blank"
            ),
            img(src="pqr/Captura.PNG",heigh = "100%", width ="100%")
          
          ),
          
          box(title = 'CORPAC',
          
            'Muestra los METAR de los aeródromos del Peru administrados por CORPAC.',
            br(),
            a("Acceder a esta pagina",
              href="http://www.corpac.gob.pe/app/Meteorologia/tiempo/reportemetar.php",
              target="_blank"
            ),
            img(src="pqr/Captura1.PNG",heigh = "100%", width ="100%")
          
          ),
          
          box(title = 'ANA',
          
            'Plataforma web de monitoreo de variables hidrometerologicas.',
            br(),
            a("Acceder a esta pagina",
              href="https://snirh.ana.gob.pe/consultassnirh/oObservatorios.aspx",
              target="_blank"
            ),
            img(src="pqr/Captura2.PNG",heigh = "100%", width ="100%")
            
          ),
          
          box(title = 'OGIMET',
          
            'Servicio de informacion meteorologica.',
            br(),
            a("Acceder a esta pagina",
              href="https://www.ogimet.com/",
              target="_blank"
            ),
            img(src="pqr/Captura3.PNG",heigh = "100%", width ="100%")
          
          ),
          
          box(title = 'SENAMHI',
          
            'Servicio nacional de meteorologia e hidrologia del Peru.',
            br(),
            a("Acceder a esta pagina",
              href="http://www.senamhi.gob.pe/",
              target="_blank"
            ),
            img(src="pqr/Captura4.PNG",heigh = "100%", width ="100%")
          
          ),
          
          box(title = 'CPTEC',
          
            'Imagenes satelitales.',
            br(),
            a("Acceder a esta pagina",
              href="http://satelite.cptec.inpe.br/acervo/goes16.formulario.logic",
              target="_blank"
            ),
            img(src="pqr/Captura5.PNG",heigh = "100%",width = "100%")
          
          )
        
        )
      
      ),
      
      ####################################################
      # Sección de directivas
      tabItem(tabName = 'directivas',
      
        fluidRow(
        
          box(title = 'Restricciones',
          
            background = "orange",
            
            '- Esta prohibido el ingreso de personal ajeno al laboratorio al lugar de trabajo.',
            br(),
            '- Esta prohibido el ingreso de personal que no este en la lista de programadores.',
            br(),
            '- El aforo maximo de pronosticadores es de cuatro.',
            br(),
            '- Esta prohibido ingerir alimentos en el laboratorio.',
            br(),
            '- De no encontrarse el profesor encargado en el laboratorio, se solicitara el permiso para trabajar al docente que se encuentre en el laboratorio previa coordinacion con el docente encargado.',
            br(),
            '- De no encontrarse ningun docente en el laboratorio, se solicitara permiso al profesor encargado o el jefe del departamento.',
            br(),
            '- Esta prohibido apagar la computadora receptora de imagenes satelitales.',
            br(),
            '- Toda comunicacion que no sea con el cliente, debera realizarse fuera del laboratorio.'
            
          ),
          
          box(title = 'Responsabilidades dentro del laboratorio',
          
            background = "blue",
            
            '- El primer pronosticador(P1) se encargara de realizar los pronosticos, el briefing a los relevos y el vocero a los medios de comunicacion; sera el responsable de los pronosticos.El segundo pronosticador(P2) se encargara de la redaccion de los reportes, vigilancia del tiempo presente y atencion al publico; sera el responsable del grupo.',
            br(),
            '- P1 y P2 analizaran las anomalias de la sinoptica mensual.',
            br(),
            'P1 y P2 analizaran la dinamica basica de la atmosfera para los Now-Casting.',
            br(),
            '- P1 y P2 identificaran y codificaran los complejos convectivos(CC).',
            br(),
            '- P1 hara seguimiento a los CC.',
            br(),
            '- P1 actualizara la informacion de los caudales(1 pm).',
            br(),
            '- P2 realizara el mapeo de las condiciones atmosfericas de los domicilios.',
            br(),
            '- P2 realizara la descarga de datos a las 12 y 00 UTC.'
            
          ),
          
          box(title = 'Pronosticos y reportes',
            
            background = "light-blue",
            
            '- Se realizaran reportes regulares y obligatorios cada una hora.',
            br(),
            '- Los pronosticos se emitiran a horas exactas(Ejm. 9:00,10:00)',
            br(),
            '- Los reportes contienen informacion del tiempo actual, pronostico para las siguientes 2 a 3 horas y pronosticos a 6 horas.',
            br(),
            '- Los reportes estaran enfocados en la region central del Peru.',
            br(),
            '- Los pronosticos de los reportes estaran listos al menos 15 minutos antes de su emision, mientras que el reporte estara listo al menos 5 minutos antes de su emision.',
            br(),
            '- De existir un cambio drastico en el pronostico o condiciones del tiempo, redactar la enmienda correspondiente.'
            
          ),
          
          box(title = 'Responsabilidades fuera del laboratorio',
          
            background = "purple",
            
            '- Todos los pronosticadores deberan indicar las condiciones de nubosidad, visibilidad y fenomenos meteorologicos en su vivienda a las horas sinoptica principales y secundarias. Ante cualquier cambio repentino, emitir la notificacion lo antes posible.',
            br(),
            '- Todos los reportes emitidos por un pronosticador deberan ser validados mediante tablas de contingencia. De presentarse algun error, explicar las posibles causas del mismo.',
            br(),
            '- La validacion de los reportes constituye parte de la calificacion del pronosticador, asi como la validacion de las horas trabajadas por el pronosticador.',
            br(),
            '- Los reportes y validacion de los mismos seran incluidos en el informe final para la calificacion de las practicas pre profesionales.'
            
          ),
          
          box(title = 'Turnos y personal',
            
            background = "green",
            
            '- Los turnos y horas asignadas estaran a cargo del alumno coordinador o, en su defecto,del alterno.',
            br(),
            '- El horario preliminar de trabajo sera de 8 am y 1pm.',
            br(),
            '- Por turno se requiere un minimo de dos pronosticadores.',
            br(),
            '- Cualquier falta a la siguientes directivas tendra una penalidad(por el momento,impuesta por el docente a cargo).'
            
          ),
          
          box(title = 'Ingreso y salida',
          
            background = "teal",
            
            '- Todo personal pronosticador debera marcar su hora de ingreso y salida cumpliendo la cantidad de horas asignadas a su turno.',
            br(),
            '- Todo pronosticador debera ingresar 15 minutos antes de que empiece su turno para el respectivo relevo o instalacion.',
            br(),
            'En caso alguienno pueda presentarse a su turno, debera informar con tiempo el nombre del pronosticador que lo reemplazara. Luego se justificara la falta con el profesor encargado.'
          
          )

        )
      
      ),
      
      ####################################################
      # Sección de glosario
      tabItem(tabName = 'glosario',
      
        fluidRow(
        
          tabBox(title = 'Bandas G16', height = 800, width = 6,
          
            tabPanel(title = 'B-2',
            
              # fluidRow(
              
                # column(width = 5,
              
                  'Canal Rojo',
                  br(), 'Tipo: Visible',
                  br(), 'Rango (µm): 0.59- 0.69',
                  br(), 'Resolución: 0.5 km (1 km en nuestro caso)',
                  br(), 'Uso: Niebla diurna, insolación y vientos',
              
                # ),
                
                # column(width = 7,
                  br(),
                  img(src = 'Ejm/Ejm_2.png', width = 500, height = 500)
                
                # )
            
              # )
            
            ),
            
            tabPanel(title = 'B-7',
            
              # fluidRow(
              
                # column(width = 5,
              
                  'Canal infrarojo de onda corta',
                  br(), 'Tipo: IR cercano',
                  br(), 'Rango (µm): 3.8 – 4.0',
                  br(), 'Resolución: 2 km',
                  br(), 'Uso: Nubes y superficies, niebla en la noche, fuego y vientos',
              
                # ),
                
                # column(width = 7,
                  br(),
                  img(src = 'Ejm/Ejm_7.png', width = 500, height = 500)
                
                # )
            
              # )
            
            ),
            
            tabPanel(title = 'B-8',
            
              # fluidRow(
              
                # column(width = 5,
              
                  'Vapor de agua en tropósfera alta',
                  br(), 'Tipo: IR',
                  br(), 'Rango (µm): 5.8 - 6.6',
                  br(), 'Resolución: 2 km',
                  br(), 'Uso: Vapor de agua a niveles altos, vientos y lluvia',
              
                # ),
                
                # column(width = 7,
                  br(),
                  img(src = 'Ejm/Ejm_8.png', width = 500, height = 500)
                
                # )
            
              # )
            
            ),
            
            tabPanel(title = 'B-9',
            
              # fluidRow(
              
                # column(width = 5,
              
                  'Vapor de agua de tropósfera media',
                  br(), 'Tipo: IR',
                  br(), 'Rango (µm): 6.75 – 7.15',
                  br(), 'Resolución: 2 km',
                  br(), 'Uso: Vapor de agua a niveles medios, vientos y lluvia',
              
                # ),
                
                # column(width = 7,
                  br(),
                  img(src = 'Ejm/Ejm_9.png', width = 500, height = 500)
                
                # )
            
              # )
            
            ),
            
            tabPanel(title = 'B-13',
            
              # fluidRow(
              
                # column(width = 5,
              
                  'Ventana de onda larga limpia',
                  br(), 'Tipo: Visible',
                  br(), 'Rango (µm): 10.1 – 10.6',
                  br(), 'Resolución: 2 km',
                  br(), 'Uso: Superficie y nubes',
              
                # ),
                
                # column(width = 7,
                  br(),
                  img(src = 'Ejm/Ejm_13.png', width = 500, height = 500)
                
                # )
            
              # )
            
            ),
            
            tabPanel(title = 'B-14',
            
              # fluidRow(
              
                # column(width = 5,
              
                  'Ventana de onda larga',
                  br(), 'Tipo: IR',
                  br(), 'Rango (µm): 10.8 – 11.6',
                  br(), 'Resolución: 2 km',
                  br(), 'Uso: TSM, nubes y lluvia',
              
                # ),
                
                # column(width = 7,
                  br(),
                  img(src = 'Ejm/Ejm_14.png', width = 500, height = 500)
                
                # )
            
              # )
            
            ),
            
            tabPanel(title = 'B-15',
            
              # fluidRow(
              
                # column(width = 5,
              
                  'Ventana de onda larga sucia',
                  br(), 'Tipo: IR',
                  br(), 'Rango (µm): 11.8 – 12.8',
                  br(), 'Resolución: 2 km',
                  br(), 'Uso: Agua total, cenizas y TSM',
              
                # ),
                
                # column(width = 7,
                  br(),
                  img(src = 'Ejm/Ejm_15.png', width = 500, height = 500)
                
                # )
            
              # )
            
            )
          
          ),
          
          box(title = 'Hidroestimador (HE)', status = 'warning', width = 6, height = 800,
          
            'El Estimador Hidraúlico (HE) utiliza datos infrarrojos (IR) de los satélites geoestacionarios operacionales del medio ambiente de la NOAA (GOES) para estimar las tasas de lluvia.',
            br(),
            'Las estimaciones de las precipitaciones de los satélites pueden proporcionar información crítica sobre las precipitaciones en las regiones donde los datos de los medidores o el radar no están disponibles o no son confiables, como sobre los océanos o las regiones escasamente pobladas.',
            br(),
            img(src = 'Ejm/HE.jpg', width = 450, height = 600)
          
          ),
          
          box(title = 'FORTRACC', status = 'success', width = 6, height = 500,
          
            'El algoritmo Forecasting and Tracking the Evolution of Cloud Clusters (ForTraCC), se ha empleado operativamente en Brasil desde 2005 para rastrear y pronosticar el desarrollo de nubes convectivas.',
            br(),
            'Esta técnica describe las principales características morfológicas de los sistemas de nubes y lo más importante, reconstruye todo su ciclo de vida.',
            br(),
            'En base a esta informacióse emplean varias relaciones que usan la expansión del área y la fracción convectiva y estratiforme para predecir la duración del tiempo de vida y el área de la nube.',
            br(),
            img(src = 'EJM/fortracc.png', width = 400, heigth = 300)
          
          )
        
        )
      
      )
      
    )
	
  )
  
)

