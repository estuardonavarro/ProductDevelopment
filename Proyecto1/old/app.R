library(shiny)
library(leaflet)
library(readxl)
library(DT)
library(lubridate)
library(dplyr)
library(ggplot2)
library(forcats)

# ---- Cargar datos ----
data2<- data.frame(read_xlsx("Ubicacion plantas solares y eolicas.xlsx"))
df <- data.frame(read_xlsx("Limitaciones.xlsx"))
df$Fecha <- as.Date(as.character(df$Fecha), format = "%y/%m/%d")
df <- df[,3:ncol(df)]
minimo <- min(df$Energia, na.rm = TRUE)
maximo <- max(df$Energia, na.rm = TRUE)
df_group_init <- df %>% replace(is.na(.),0) %>% group_by(Planta) %>% summarise(Energia=sum(Energia))

# ---- Interfaz de usuario ----
ui <- fluidPage(
   sidebarLayout(
      sidebarPanel(
         titlePanel(title = "Filtros"),
         sliderInput(
            "slider1",
            "Seleccione nivel de Energia",
            min = minimo,
            max = maximo,
            value = c(minimo, maximo)
         ),
         selectInput(
            "Planta",
            "Seleccione una planta:",
            choices = c(unique(df$Planta)),
            multiple = TRUE
         ),
         dateRangeInput(
            "date_range",
            "Seleccione un rango de fechas:",
            min = min(df$Fecha),
            max = today(),
            start = min(df$Fecha),
            end = today(),
            language = "es",
            weekstart = 1,
            separator = "a"
         ),
         downloadButton("descargar_datos", "Descargar datos")
      ),
      mainPanel(
         tabsetPanel(
            tabPanel("Mapa",
            titlePanel (title="Ubicacion de plantas solares"),
            leafletOutput("map")
            ),
            tabPanel("Tablas",
            titlePanel (title="Tabla de datos"),
            DT::dataTableOutput("table")
            ),
            tabPanel("Graficas",
            titlePanel (title="Graficas de datos"),
            plotOutput("plot")
            ),
            tabPanel("Actualizacion de la base de datos",
            titlePanel (title="Actualizacion de la base de datos"),
            fileInput('file_input', 'upload file ( . pdf format only)', accept = c('.pdf')), 
            DT::dataTableOutput("newdata")
            )
         )
      )
   )
)

# ---- Servidor de aplicaciones ----
server <- function(input, output, session) {
   # ---- Variables globales ----
   data <- reactiveValues(df=df)
   df_filtered <- reactiveValues(df=data.frame())
   df_grouped <- reactiveValues(df= df_group_init)
 
   # ---- Eventos ----
   observeEvent(
      input$Planta,{
         df_filtered$df <- data$df %>%
            filter(Planta %in% input$Planta) %>%
            filter(Energia >= input$slider1[1] & Energia <= input$slider1[2]) %>%
            filter(Fecha >= input$date_range[1] & Fecha <= input$date_range[2])
            df_grouped$df <- df_filtered$df %>%
               group_by(Planta) %>%
               summarise(Energia=sum(Energia))
      }
   )
   observeEvent(
      input$date_range,{
         df_filtered$df <- data$df %>%
            filter(Planta %in% input$Planta) %>%
            filter(Energia >= input$slider1[1] & Energia <= input$slider1[2]) %>%
            filter(Fecha >= input$date_range[1] & Fecha <= input$date_range[2])
            df_grouped$df <- df_filtered$df %>%
               group_by(Planta) %>%
               summarise(Energia=sum(Energia))
      }
   )


   # ---- Renderizar mapa ----
   output$map <- renderLeaflet({
      df_merge <- merge(df_grouped$df, data2, all = TRUE)
      if(length(input$Planta)==0) {
         df_temp <- df_merge
      }
      else {
         df_temp <- df_merge %>%
         filter(Planta %in% input$Planta)

      }
      df_temp$Label <- with(df_temp, paste(Planta,"</b> </br>", Energia, " kWh", sep=""))

    leaflet(df_temp) %>%
      addTiles() %>%
      addCircles(lng =~Longitud, lat =~Latitud,
         radius = ~Energia/sum(Energia)*50000, color = "red", popup = ~Label
      )
  })
   # ---- Renderizar tablas ----
   output$table <- renderDataTable(
      df_filtered$df
   )
   # ---- Render graficas ----
   output$plot <- renderPlot({
      df_grouped$df %>%
      mutate(Planta = fct_reorder(Planta,desc(Energia))) %>%
      ggplot(aes(x=Energia,y=Planta)) +
      geom_bar(stat = "identity")
   })
   # ---- Descargar datos ----
   output$descargar_datos <- downloadHandler(
    filename = function(){
      paste('data-',Sys.Date(),'.csv',sep='')
      },
      
      content = function(file){
        readr::write_csv(df_filtered$df,file)
      }
    
  )
   

}

shinyApp(ui, server)

