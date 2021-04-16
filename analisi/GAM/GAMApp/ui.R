
# Define UI for application that draws a histogram
shinyUI(fluidPage(headerPanel(
    title = "Lorem ipsum",
),

fluidRow(
    column(5,
           conditionalPanel(condition="input.plot_tabs!='User guide'",
                            tabsetPanel(id = "ui_tab",
                                        tabPanel("Mappa",
                                                 column(
                                                     12,
                                                     h4("Click su una stazione"),
                                                     shinycssloaders::withSpinner(leaflet::leafletOutput("aqMap", height = "600px"),
                                                                                  size = 2,
                                                                                  color = "#0080b7"
                                                     )
                                                 )),
                                        tabPanel("Tabella",
                                                 column(
                                                     12,
                                                     h4("Click su una stazione"), div(DT::dataTableOutput("table_input"), style = "font-size:90%")
                                                 ))
                            )
           ),
           conditionalPanel(condition="input.plot_tabs=='User guide'", column(12)
           )
    ),
    column(7, tabsetPanel(
        id = "plot_tabs",
        tabPanel("Plot",
                 fluidRow(
                     column(12,
                            # uiOutput("date_slider"),
                            selectInput("ts_pltnt", label = "Inquinante:", choices = c("NO2" = "no2", "PM10" = "pm10", "PM25" = "pm25")),
                            radioButtons("ts_plot_type", "Tipo di plot:", choices = c("Serie", "Pollution rose", "Polar freq", "Boxplot"), inline = T),
                            conditionalPanel(condition = "input.ts_plot_type=='Boxplot'", plotOutput("boxplot", width = "90%", height = "800px")),
                            conditionalPanel(condition = "input.ts_plot_type=='Pollution rose'", plotOutput("pollution_rose")),
                            conditionalPanel(condition = "input.ts_plot_type=='Serie'", plotOutput("serie", width = "90%", height = "500px")),
                            conditionalPanel(condition = "input.ts_plot_type=='Polar freq'", plotOutput("polar"))
                     )
                 )),
        tabPanel("Descrittive",
                 fluidRow(column(12,
                                 div(DT::dataTableOutput("descrittive"), style = "font-size:90%")
                 ))),
        tabPanel("GAM",
                 fluidRow(column(12,
                                 # selectInput("ts_pltnt", label = "Inquinante:", choices = c("NO2" = "no2", "PM10" = "pm10", "PM25" = "pm25")),
                                 htmlOutput("gam_output"),
                                 htmlOutput("gam_summary"),
                                 plotOutput("jd_plot"),
                                 plotOutput("jd_plot_last")
                 ))
        ),
        tabPanel("User guide",
                 fluidRow(
                     column(8,
                            includeMarkdown('./guida/user_guide.rmd')
                     )
                 )
        )
        
    ))
),))