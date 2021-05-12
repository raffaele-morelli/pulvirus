# Define UI for application that draws a histogram
shinyUI(fluidPage(
  # Header
  headerPanel(
    title = tags$a(
      href = 'https://pulvirus.isprambiente.it',
      tags$img(src = 'ispra_snpa.png', height = 90, width = 100 * 2.85 * 1.75 ), target = "_blank"),
    tags$head(
      tags$link(rel = "icon", type = "image/png", href = "dwq_logo_small.png"),
      windowTitle = "Pulvirus/GAM dashboard"
    )
  ),
  
  fluidRow(
    column(
      5,
      conditionalPanel(condition = "input.plot_tabs!='Guida'",
                       tabsetPanel(
                         id = "ui_tab",
                         tabPanel("Mappa", column(
                           12,
                           h4("Stazioni/serie valide per inquinante"),
                           h5("Selezionare una stazione con un clik"),
                           shinycssloaders::withSpinner(
                             leaflet::leafletOutput("aqMap", height = "700px"),
                             size = 2,
                             color = "#0080b7"
                           )
                         )),
                         tabPanel("Tabella", column(
                           12,
                           h4("Cerca una stazione e seleziona la riga"),
                           div(DT::dataTableOutput("table_input"), style = "font-size:90%")
                         ))
                       )),
      conditionalPanel(condition = "input.plot_tabs=='Guida'", column(12))
    ),
    column(
      7,
      tabsetPanel(
        id = "plot_tabs",
        tabPanel("Inquinante",
                 fluidRow(
                   column(12,
                          htmlOutput("nota_pltnt"),
                          selectInput("ts_pltnt", label = "Inquinante:", choices = c("NO2" = "no2", "PM10" = "pm10", "PM25" = "pm25", "NOx" = "nox")
                          ))
                 )),
        tabPanel("Plot",
                 fluidRow(
                   column(12,
                     # uiOutput("date_slider"),
                     radioButtons("ts_plot_type", "Tipo di plot:", choices = c("Serie", "Pollution rose", "Polar freq", "Boxplot"), inline = T),
                     conditionalPanel(condition = "input.ts_plot_type=='Boxplot'", plotOutput("boxplot", width = "98%", height = "700px")),
                     conditionalPanel(condition = "input.ts_plot_type=='Pollution rose'", plotOutput("pollution_rose")),
                     conditionalPanel(condition = "input.ts_plot_type=='Serie'", plotOutput("serie", width = "98%", height = "600px")),
                     conditionalPanel(condition = "input.ts_plot_type=='Polar freq'", plotOutput("polar"))
                   )
                 )),
        tabPanel("Descrittive",
                 fluidRow(column(12, div(DT::dataTableOutput("descrittive"), style = "font-size:90%"))),
                 fluidRow(column(12, plotOutput("diff_plot")))
                 ),
        tabPanel(
          "GAM",
          fluidRow(column(
            12,
            # selectInput("ts_pltnt", label = "Inquinante:", choices = c("NO2" = "no2", "PM10" = "pm10", "PM25" = "pm25")),
            htmlOutput("gam_output"),
            htmlOutput("gam_summary"),
            # plotOutput("jd_plot", width = "500px", height = "400px"),
            # plotOutput("jd_plot_last", width = "500px", height = "400px"),
            # plotOutput("residui", width = "600px", height = "600px")
          )),
          fluidRow(column(6, plotOutput("jd_plot")),
                   column(6, plotOutput("jd_plot_last"))),
          fluidRow(column(12, plotOutput("residui", width = "95%", height = "750px"))),
        ),
        tabPanel("Info stazione", fluidRow(column(8, htmlOutput("info_stazione")))),
        tabPanel("Guida", fluidRow(column(8, includeMarkdown('./guida/user_guide.rmd'))))
        
      )
    )
  ),
))
