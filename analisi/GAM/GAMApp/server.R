library(shiny)
library(leaflet)
library(dplyr)
library(datiInquinanti)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
    reactive_objects=reactiveValues()

    map = createLeafletMap(session, 'mymap')
    
    session$onFlushed(once = T, function() {
        output$mymap <- renderLeaflet({
            leaflet(stazioniAria) %>% addTiles() %>%
                addMarkers(~ st_x, ~ st_y, layerId = stazioniAria$station_eu_code) 
            # addGeoJSON(geojson = no2staz, lng = no2staz$st_y, lat = no2staz$st_x)
            # addMarkers(data = no2staz)
        })
    })
    
    # Table interface
    output$table_input = DT::renderDataTable({
        DT::datatable(
            stazioniAria %>% dplyr::select(station_eu_code, regione, provincia, comune),
            selection = 'single',
            rownames = FALSE,
            filter = "top",
            options = list(scrollY = '600px', paging = FALSE, scrollX = TRUE, dom = "ltipr")
        )
    })
    
    observe({ 
        click <- input$mymap_marker_click
        
        if (is.null(click))
            return()
        
        text <- paste("Lattitude ", click$lat, "Longtitude ", click$lng)
        text2 <- paste("You've selected point ", click$id)
        
        reactive_objects$sel_station_eu_code=click$id
        
        leafletProxy(mapId = "mymap") %>%
            clearPopups() %>%
            addPopups(dat = click, lat = ~lat, lng = ~lng, popup = text)
    })
    
    # Table row click (to identify selected site & parameter)
    observe({
        req(input$table_input_rows_selected)
        row_click = input$table_input_rows_selected
        station_eu_code = stazioniAria[row_click, "station_eu_code"]
        # reactive_objects$sel_param = mlid_param_asmnts[row_click, "R3172ParameterName"]
        reactive_objects$sel_station_eu_code = station_eu_code
    })
    
    # Change map zoom on table click & update selected heatmap_param to selected row param
    map_proxy = leaflet::leafletProxy("mymap")

    observeEvent(input$table_input_rows_selected, {
        lat = stazioniAria[which(stazioniAria$station_eu_code == "IT2101A"), "st_y"]
        long = stazioniAria[which(stazioniAria$station_eu_code == "IT2101A"), "st_x"]
        map_proxy %>% leaflet::setView(lng = long, lat = lat, zoom = 12)
        # updateSelectInput(session, "heatmap_param", selected = reactive_objects$sel_param)
    })
    
    # Filter table to match clicked site from map
    input_table_proxy = DT::dataTableProxy('table_input')
    observeEvent(input$map_marker_click, {
        input_table_proxy %>% 
            DT::clearSearch() %>% 
            DT::updateSearch(keywords = list(global = "", columns = c("", paste(reactive_objects$sel_station_eu_code) )))
    })

})