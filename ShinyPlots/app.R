library(shiny)
library(dplyr)
library(stringr)

server <- shinyServer(function(input, output) {

  file <- reactive({
    content <- input$file_chooser
    if (is.null(content)) {
      return(NULL)
    } else if (str_detect(content$name, '.csv')) {
      out <- readr::read_csv(content$datapath)
      return(out)
    } else {
      out <- data_frame(nombre_archivo = content$name,
                        error = 'Extension de archivo no soportada')
      return(out)
    }

  })

  out_dataset <- reactive({
    if (is.null(file())) {
      return(NULL)
    }
    out <- file() %>%
      filter(Date >= input$dates[1],
             Date <= input$dates[2])
    return(out)
  })

  output$contenido_archivo <- DT::renderDataTable({
    out_dataset() %>% DT::datatable()
  })

  output$download_dataframe <- downloadHandler(
    filename = function() {
      paste0('data-', Sys.Date(), '.csv')
    },

    content = function(file) {
      readr::write_csv(out_dataset(), file)
    }

  )

})

ui <- shinyUI(fluidPage(
  titlePanel("Carga de archivos"),
  sidebarLayout(
    sidebarPanel(
      fileInput('file_chooser', 'Cargue un archivo CSV',
                accept = ".csv",
                buttonLabel = 'Seleccione archivo',
                placeholder = 'No hay archivo seleccionado'
      ),
      checkboxInput("header", "Incluir encabezado", TRUE),
      dateRangeInput('dates', 'Defina intervalo de fechas',
                     min = '1900-01-05',
                     max = '2007-09-30',
                     start = '1900-01-05',
                     end = '2007-09-30',
                     language = "es",
                     separator = " a ",
                     format = "dd-mm-yyyy"
      ),
      downloadButton('download_dataframe', 'Descargar')
    ),
    mainPanel(
      DT::dataTableOutput('content')
    )
  )
)
)
shinyApp(ui = ui, server = server)