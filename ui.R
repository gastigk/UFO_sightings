# Define User Interface --------------------------------------------------------
secure_app(
  
  dashboardPage(
    
    dashboardHeader(title = "UFO Sightings", titleWidth = 300),

    dashboardSidebar(
      width = 300,
      conditionalPanel(
        condition = "input.tabs == 'Resume'",
        hr(),
        helpText("La base de datos de UFO Sightings tiene relatos de 
                 avistamientos de ovnis en todo el mundo. Al analizar la 
                 información detectamos que el volumen más imporntante de 
                 registros son de Estados Unidos, por lo cual nos enfocamos en 
                 este país para el análisis."),
        hr(),
        helpText("Aquí podrán encontrar un resumen de los datos más relevantes")
      ),
      conditionalPanel(
        condition = "input.tabs == 'Avistamientos en EEUU'",
        selectInput("shape", "Selecciona una forma:", 
                    choices = unique(UFO_sightings$shape)),
        helpText("Selecciona una forma para visualizar los datos.")
      ),
      conditionalPanel(
        condition = "input.tabs == 'Más sobre OVNIs en US'",
        selectInput("year", "Selecciona un año:", 
                    choices = unique(UFO_sightings$year)),
        helpText("Selecciona el año para visualizar los datos.")
      ),
      conditionalPanel(
        condition = "input.tabs == 'Análisis por año'",
        selectInput("year", "Selecciona un año:", 
                    choices = UFO_sightings$year),
        hr(),
        actionButton("change", "Visualizar"),
        hr(),
        helpText("Debe seleccionar un año y apretar el botón para visualizar 
                 los datos.")
      )
    ),
    
    dashboardBody(
      fluidPage(
        tabsetPanel(id = "tabs",
                    tabPanel("Resume",
                             br(),
                             fluidRow(    
                               box(
                                 title = "Top 10 países con mayor avistamientos",
                                 status = "primary",
                                 solidHeader = TRUE,
                                 collapsible = TRUE,
                                 width = 12,
                                 plotOutput("top_countries_plot", height = "500px")
                               ),
                               box(
                                 title = "Nube de palabras según registro de testigos",
                                 status = "primary",
                                 solidHeader = TRUE,
                                 collapsible = TRUE,
                                 width = 12,
                                 plotOutput("word_cloud_plot", height = "500px")
                               ),
                               hr(),
                               box(
                                 title = "Descargar imagen",
                                 status = "primary",
                                 downloadButton("download_pdf", "Descargar .pdf"),
                                 helpText("Extraer imagen del word cloud.")
                               )
                             ),
                             fluidRow(    
                               box(
                                 valueBoxOutput("total_words", width = 50),
                                 width = 4
                               ),
                               box(
                                 valueBoxOutput("unique_words", width = 50),
                                 width = 4
                               )
                             )
                    ),
                    tabPanel("Avistamientos en EEUU",
                             br(),
                             fluidRow( 
                               box(
                                 valueBoxOutput("sightings_US", width = 50),
                                 width = 4
                               ),
                               box(
                                 valueBoxOutput("state_most_sightings", 
                                                width = 50),
                                 width = 4
                               ),
                               box(
                                 valueBoxOutput("city_most_sightings", 
                                                width = 50),
                                 width = 4
                               )
                             ),
                             fluidRow(
                               box(
                                 title = "Formas más comunes registradas por año",
                                 status = "primary",
                                 solidHeader = TRUE,
                                 collapsible = TRUE,
                                 width = 12,
                                 plotOutput("sightings_by_year", 
                                            height = "500px")
                               )
                             ),
                             fluidRow( 
                               box(
                                 title = "Top 10 ciudades según formas",
                                 status = "primary",
                                 solidHeader = TRUE,
                                 collapsible = TRUE,
                                 width = 12,
                                 plotOutput("common_shapes_plot", 
                                            height = "500px")
                               )
                             )
                    ),
                    tabPanel("Más sobre OVNIs en US",
                             br(),
                             fluidRow(
                               box(
                                 title = "Duración del avistamiento según año",
                                 status = "primary",
                                 solidHeader = TRUE,
                                 collapsible = TRUE,
                                 width = 12,
                                 plotOutput("duration_seconds_plot", 
                                            height = "500px")
                               )
                             ),
                             fluidRow( 
                               box(
                                 title = "Avistamiento según momento del día",
                                 status = "primary",
                                 solidHeader = TRUE,
                                 collapsible = TRUE,
                                 width = 12,
                                 plotOutput("day_part_plot", 
                                            height = "500px")
                               )
                             )
                    ),
                    tabPanel("Análisis por año",
                             fluidRow(
                               box(                     
                                 title = "Avistamientos según el momento del día",
                                 status = "primary",
                                 solidHeader = TRUE,
                                 collapsible = TRUE,
                                 width = 12,
                                 plotOutput("timeSeriesPlot",
                                            height = "500px")
                               ),
                               fluidRow( 
                                 box(
                                   title = "Download Data",
                                   status = "primary",
                                   downloadButton("download_csv", 
                                                  "Descargar .csv"),
                                   helpText("Extraer datos según selección de año.")
                                 )
                               )
                             )
                    )
        )
      )
    )
  ),
  choose_language = TRUE)