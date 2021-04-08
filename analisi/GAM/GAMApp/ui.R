#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(leaflet)
library(datiInquinanti)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
    headerPanel(
        title = tags$a(href = 'https://pulvirus.isprambiente.it/', "Pulvirus", target =
                           "_blank"),
    ),
    
    fluidRow(
        column(5,
        tabsetPanel(id = "ui_tab",
                    tabPanel(
                        "Map",
                        column(
                            12,
                            h4("Click a site"),
                            shinycssloaders::withSpinner(
                                leaflet::leafletOutput("mymap", height = "600px"),
                                size = 2,
                                color = "#0080b7"
                            )
                        )
                    ),
                    tabPanel("Table",
                             column(
                                 12,
                                 h4("Click a site"),
                                 div(DT::dataTableOutput("table_input"), style = "font-size:70%")
                             )))
    ),
    column(7, tabsetPanel(id = "plot_tabs",
                          tabPanel("Time series",
                                   fluidRow(column(5,
                                                   radioButtons("inquinante",
                                                                "pltnt:", choices=c("Heatmap", "Habitable width", "Water column exceedances"), inline=T),
                                                   # conditionalPanel(condition="input.ts_plot_type=='Heatmap'",
                                                   #                  selectInput("heatmap_param",label="Heatmap parameter:", choices=heatmap_param_choices)
                                                   # ),
                                                   # checkboxInput("show_dates", label="Show all profile dates", value=TRUE),
                                                   # conditionalPanel(condition="input.ts_plot_type=='Heatmap'",
                                                   #                  plotOutput("heatmap")
                                                   # ),
                                                   # conditionalPanel(condition="input.ts_plot_type=='Habitable width'",
                                                   #                  plotOutput("hab_width")
                                                   # ),
                                                   # conditionalPanel(condition="input.ts_plot_type=='Water column exceedances'",
                                                   #                  plotOutput("pct_exc")
                                                   # )
                                   ))
                          )
                          
    )
    )
    ),

    )
    )
