


# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  # Loading modal to keep user out of trouble while map draws...
  showModal(modalDialog(
    title = "PLEASE WAIT...",
    "Please wait",
    size = "l",
    footer = NULL
  ))
  
  # Remove modal when app is ready
  observe({
    req(map)
    removeModal()
  })
  
  # Empty reactive values object
  reactive_objects = reactiveValues()
  
  # Resources for returning site info on click:
  ## https://stackoverflow.com/questions/28938642/marker-mouse-click-event-in-r-leaflet-for-shiny
  ## https://stackoverflow.com/questions/42613984/how-to-implement-inputmap-marker-click-correctly?noredirect=1&lq=1
  
  map = createLeafletMap(session, 'aqMap')
  
  session$onFlushed(once = T, function() {
    output$aqMap <- renderLeaflet({
      zoneclimatiche <- unique(stazioniAria$zona_climatica) %>% sort()
      pal <- colorRampPalette(RColorBrewer::brewer.pal(6, "Oranges"))
      pal = leaflet::colorFactor(pal(length(unique(stazioniAria$zona_climatica))), domain = zoneclimatiche)
      
      map = leaflet(stazioniAria) %>% addTiles()
      # addMarkers(~ st_x, ~ st_y, layerId = stazioniAria$station_eu_code)
      
      map = leaflet::addProviderTiles(
        map, "Esri.WorldImagery", group = "Satellite",
        options = providerTileOptions(updateWhenZooming = FALSE, updateWhenIdle = TRUE)
      )
      map = leaflet::addProviderTiles(
        map, "Esri.WorldTopoMap", group = "Topo",
        options = providerTileOptions(updateWhenZooming = FALSE, updateWhenIdle = TRUE)
      )
      
      ptsRadius <- 3
      # map = addMapPane(map, "markers", zIndex = 400)
      
      map = leaflet::addCircleMarkers(
        map,
        data = no2Staz, lat = no2Staz$st_y, lng = no2Staz$st_x, group = "NO2",
        radius = ptsRadius,
        # color = pal(no2Staz$zona_climatica),
        color = "black",
        layerId = paste("no2", no2Staz$station_eu_code, sep = "_"),
        popup = paste0("Location ID: ", no2Staz$station_eu_code,
          "<br> Name: ", no2Staz$nome_stazione,
          "<br> Type: ", no2Staz$zona_tipo,
          "<br> Lat: ", no2Staz$st_x,
          "<br> Long: ", no2Staz$st_y
        )
      )
      
      map = leaflet::addCircleMarkers(
        map,
        data = pm10Staz, lat = pm10Staz$st_y, lng = pm10Staz$st_x, group = "PM10",
        radius = ptsRadius,
        # color = pal(pm10Staz$zona_climatica),
        color = "green",
        layerId = paste("pm10", pm10Staz$station_eu_code, sep = "_"),
        popup = paste0(
          "Location ID: ", pm10Staz$station_eu_code,
          "<br> Name: ", pm10Staz$nome_stazione,
          "<br> Type: ", pm10Staz$zona_tipo,
          "<br> Lat: ", pm10Staz$st_x,
          "<br> Long: ", pm10Staz$st_y
        )
      )
      
      map = leaflet::addCircleMarkers(
        map,
        data = pm25Staz, lat = pm25Staz$st_y, lng = pm25Staz$st_x, group = "PM25",
        radius = ptsRadius,
        # color = pal(pm25Staz$zona_climatica),
        color = "red",
        layerId = paste("pm25", pm25Staz$station_eu_code, sep = "_"),
        popup = paste0(
          "Location ID: ", pm25Staz$station_eu_code,
          "<br> Name: ", pm25Staz$nome_stazione,
          "<br> Type: ", pm25Staz$zona_tipo,
          "<br> Lat: ", pm25Staz$st_x,
          "<br> Long: ", pm25Staz$st_y
        )
      )
      
      map = leaflet::addLayersControl(
        map,
        position = "topleft",
        baseGroups = c("Topo", "Satellite"),
        overlayGroups = c("NO2", "PM10", "PM25"),
        options = leaflet::layersControlOptions(collapsed = FALSE, autoZIndex =
                                                  TRUE)
      )
      
      # TODO riorganizzare il livelli per ordinare la zona climatica
      # map = leaflet::addLegend(
      #     map,
      #     position = 'topright',
      #     title = "Zona climatica",
      #     colors = unique(pal(stazioniAria$zona_climatica)),
      #     labels = unique(stazioniAria$zona_climatica)
      # )
      
      map = hideGroup(map, c("NO2", "PM25", "PM10"))
    })
  })
  
  # Table interface
  output$table_input = DT::renderDataTable({
    DT::datatable(
      stazioniAria %>% dplyr::select(station_eu_code, regione, provincia, comune, nome_stazione),
      selection = 'single',
      rownames = FALSE,
      filter = "top",
      options = list(
        scrollY = '600px',
        paging = FALSE,
        scrollX = TRUE,
        dom = "ltipr"
      )
    )
  })
  
  # Map marker click (to identify selected site)
  observe({
    click <- input$aqMap_marker_click
    
    if (is.null(click))
      return()
    
    # text <- paste("Lattitude ", click$lat, "Longtitude ", click$lng, "Stazione", click$id)
    # text2 <- paste("You've selected point ", click$id)
    #
    # leafletProxy(mapId = "aqMap") %>%
    #     clearPopups() 
    
    reactive_objects$sel_station_eu_code <-
      stringr::str_split(click, "_")[[1]][2]
    
  })
  
  # Table row click (to identify selected site & parameter)
  observe({
    req(input$table_input_rows_selected)
    row_click = input$table_input_rows_selected
    
    station_eu_code = stazioniAria[row_click, "station_eu_code"] %>% as.character()
    reactive_objects$sel_station_eu_code = station_eu_code
  })
  
  # scelta inquinante
  observe({
    req(reactive_objects$sel_station_eu_code)
    
    if (is.null(input$ts_pltnt))
      return()
    
    reactive_objects$ts_pltnt = input$ts_pltnt
    
    stazioniAria %>% filter(station_eu_code == reactive_objects$sel_station_eu_code) %>% select(region_id) -> rec
    f <-
      paste0(
        "/home/rmorelli/R/pulvirus/analisi/GAM/scelta/output/",
        rec$region_id,
        "/",
        reactive_objects$ts_pltnt,
        "/",
        reactive_objects$ts_pltnt,
        "_",
        rec$region_id,
        "_",
        reactive_objects$sel_station_eu_code,
        ".RData"
      )
    if (file.exists(f)) {
      load(f)
      reactive_objects$models <- models
    }
    
  })
  
  output$nota_pltnt <- renderUI({
    HTML(
      "<br /><p style='color: red; background-color: yellow;'><b style='color: black;'>Importante</b> Scegliere un inquinante di una serie valida (visualizzata sulla mappa) per mostrare
              i plot, le descrittive ed il modello GAM</p>"
    )
  })
  
  # Change map zoom on table click & update selected heatmap_param to selected row param
  map_proxy = leaflet::leafletProxy("aqMap")
  observeEvent(input$table_input_rows_selected, {
    lat = filter(stazioniAria,
                 station_eu_code == reactive_objects$sel_station_eu_code) %>% select(st_y) %>% as.numeric()
    long = filter(stazioniAria,
                  station_eu_code == reactive_objects$sel_station_eu_code) %>% select(st_x) %>% as.numeric()
    map_proxy %>% leaflet::setView(lng = long,
                                   lat = lat,
                                   zoom = 15)
    # updateSelectInput(session, "heatmap_param", selected = reactive_objects$sel_param)
  })
  
  
  # Filter table to match clicked site from map
  input_table_proxy = DT::dataTableProxy('table_input')
  observeEvent(input$aqMap_marker_click, {
    input_table_proxy %>%
      DT::clearSearch() %>%
      DT::updateSearch(keywords = list(global = "", columns = c(
        paste(reactive_objects$sel_station_eu_code), ""
      )))
  })
  
  # Serie storica
  output$serie = renderPlot({
    req(reactive_objects$sel_station_eu_code,
        reactive_objects$ts_pltnt)
    # showModal(modalDialog(title = "PLEASE WAIT...", "Please wait", size = "l", footer = NULL ) )
    
    filter(
      get(reactive_objects$ts_pltnt),
      station_eu_code == reactive_objects$sel_station_eu_code
    ) -> df
    
    if (nrow(df) > 0) {
      df %>% mutate(week = as.Date(date, '%Y-%V')) %>%
        group_by(reporting_year, station_eu_code, week) %>%
        summarise(media = mean(value, na.rm = TRUE)) %>%
        ggplot(aes(week, media)) + geom_line(color = "dodgerblue") +
        # facet_grid(vars(reporting_year), scales = "free") +
        xlab("Settimana") + ylab("Concentrazione media") +
        geom_smooth(method = "loess",
                    se = TRUE,
                    color = "#000080",
                    aes(group = 1)) +
        scale_x_date(labels = date_format("%Y")) +
        ggtitle(paste("Inquinante: ", reactive_objects$ts_pltnt)) +
        theme_pulvirus()
    } else{
      ggplot2::ggplot() + ggplot2::geom_blank() + annotate(
        "text",
        x = 4,
        y = 25,
        label = paste("Non ci sono dati per", reactive_objects$ts_pltnt, 
                      "Scegliere un inquinante diverso!", sep = "\n" ),
        size = 9
      ) + theme_void()
    }
    # removeModal()
    
  })
  
  # pollution rose
  output$pollution_rose = renderPlot({
    req(reactive_objects$sel_station_eu_code,
        reactive_objects$ts_pltnt)
    showModal(modalDialog(
      title = "PLEASE WAIT...",
      "Please wait",
      size = "l",
      footer = NULL
    ))
    
    filter(
      get(reactive_objects$ts_pltnt),
      station_eu_code == reactive_objects$sel_station_eu_code
    ) %>%
      inner_join(dati_meteo, by = c("station_eu_code", "date")) %>%
      select(reporting_year,
             date,
             value,
             wdir,
             wspeed,
             pbl00,
             pbl12,
             pblmin,
             pblmax) -> gr
    
    if (nrow(gr) > 0) {
      names(gr)[4:5] <- c("wd", "ws")
      gr$reporting_year <-
        as.character(as.numeric(gr$reporting_year))
      
      polarPlot(
        gr,
        pollutant = c("value"),
        type = "reporting_year",
        cols = "jet",
        key.header = "--",
        key.footer = "--",
        key.position = "right"
      )
    } else{
      ggplot2::ggplot() + ggplot2::geom_blank() + annotate(
        "text",
        x = 4,
        y = 25,
        label = paste("Non ci sono dati per", reactive_objects$ts_pltnt, 
                      "Scegliere un inquinante diverso!", sep = "\n" ),        size = 9
      ) + theme_void()
    }
    
    removeModal()
  })
  
  # Polar plot
  output$polar <- renderPlot({
    req(reactive_objects$sel_station_eu_code,
        reactive_objects$ts_pltnt)
    showModal(
      modalDialog(
        title = "PLEASE WAIT...",
        "Please wait for plot to draw",
        size = "l",
        footer = NULL
      )
    )
    
    filter(
      get(reactive_objects$ts_pltnt),
      station_eu_code == reactive_objects$sel_station_eu_code
    ) %>%
      inner_join(dati_meteo, by = c("station_eu_code", "date")) %>%
      select(reporting_year,
             date,
             value,
             wdir,
             wspeed,
             pbl00,
             pbl12,
             pblmin,
             pblmax) -> gr
    
    if (nrow(gr) > 0) {
      names(gr)[4:5] <- c("wd", "ws")
      gr$reporting_year <-
        as.character(as.numeric(gr$reporting_year))
      # print(gr)
      polarFreq(gr, cols = "jet", type = "year")
    } else{
      ggplot2::ggplot() + ggplot2::geom_blank() + annotate(
        "text",
        x = 4,
        y = 25,
        label = paste("Non ci sono dati per", reactive_objects$ts_pltnt, 
                      "Scegliere un inquinante diverso!", sep = "\n" ),        size = 9
      ) + theme_void()
    }
    removeModal()
    
  })
  
  # Boxplot
  output$boxplot <- renderPlot({
    req(reactive_objects$sel_station_eu_code,
        reactive_objects$ts_pltnt)
    showModal(modalDialog(
      title = "PLEASE WAIT...",
      "Please wait",
      size = "l",
      footer = NULL
    ))
    
    
    filter(
      get(reactive_objects$ts_pltnt),
      station_eu_code == reactive_objects$sel_station_eu_code
    ) %>%
      select(reporting_year, date, value) -> bp
    
    if (nrow(bp) > 0) {
      bp <- cutData(bp, type = c("month"))
      
      bp$reporting_year <-
        as.character(as.numeric(bp$reporting_year))
      
      # print(bp)
      
      bp %>% filter(month %in% c(
        "gennaio",
        "febbraio",
        "marzo",
        "aprile",
        "maggio",
        "giugno"
      )) %>%
        ggplot(aes(x = reporting_year, y = value)) +
        stat_boxplot(
          geom = "errorbar",
          position = position_dodge(width = 0.9),
          color = "#87CEFA",
          width = 0.5
        ) +
        geom_boxplot(position = position_dodge(width = 0.9), color = "#87CEFA") +
        xlab("Anno") + ylab("Concentrazione") +
        stat_summary(
          fun.y = mean,
          geom = "point",
          shape = 20,
          size = 3,
          color = "#000080",
          fill = "#B0E0E6"
        ) +
        geom_smooth(method = "loess",
                    se = TRUE,
                    color = "#000080",
                    aes(group = 1)) +
        facet_wrap( ~ month) +
        theme_pulvirus() -> p1
      
      filter(
        get(reactive_objects$ts_pltnt),
        station_eu_code == reactive_objects$sel_station_eu_code
      ) %>%
        mutate(month = month(date)) %>%
        ggplot(aes(factor(month), y = value)) +
        geom_boxplot() + xlab("Mese") + ylab("Concentrazione") +
        facet_grid(cols = vars(reporting_year)) + theme_pulvirus() -> p2
      
      grid.arrange(p1, p2)
      
    } else{
      ggplot2::ggplot() + ggplot2::geom_blank() + annotate(
        "text",
        x = 4,
        y = 25,
        label = paste("Non ci sono dati per", reactive_objects$ts_pltnt, "<br>Scegliere un inquinante diverso!" ),
        size = 9
      ) + theme_void()
    }
    removeModal()
    
  })
  
  # Descrittive
  output$descrittive = DT::renderDataTable({
    req(reactive_objects$sel_station_eu_code,
        reactive_objects$ts_pltnt)
    
    filter(
      get(reactive_objects$ts_pltnt),
      station_eu_code == reactive_objects$sel_station_eu_code
    ) %>%
      inner_join(dati_meteo, by = c("station_eu_code", "date")) -> df
    
    # statistiche descrittive annuali#####
    df <- cutData(df, type = c("month", "monthyear", "season"))
    
    stat_yy <- df %>%
      group_by(station_eu_code, reporting_year) %>%
      skim(value)
    
    # calcolo percentili ####
    p <- c(0.05, 0.904, 0.95, 0.98, 0.999)
    p_names <- map_chr(p, ~ paste0(.x * 100, "percentile"))
    
    p_funs <-
      map(p, ~ partial(
        quantile,
        probs = .x,
        type = 8,
        na.rm = TRUE
      )) %>%
      set_names(nm = p_names)
    
    perc <- df %>%
      group_by(station_eu_code, reporting_year) %>%
      summarize_at(vars(value), funs(!!!p_funs))
    
    perc$ID <- seq.int(nrow(perc))
    
    stat_yy <- stat_yy[, c(3:13)]
    names(stat_yy)[5:11] <-
      c("Valore medio annuo",
        "sd",
        "min",
        "25th",
        "mediana",
        "75th",
        "max")
    
    
    # conteggio dati disponibili per anno ####
    conteggio <-
      df %>% group_by(reporting_year) %>% count(station_eu_code)
    
    # unisco statistiche, percentili e conteggi
    # unione conteggi ####
    stat_yy1 <-
      inner_join(
        stat_yy,
        perc,
        by = c("station_eu_code", "reporting_year"),
        all = TRUE
      )
    stat_yy2 <-
      inner_join(
        stat_yy1,
        conteggio,
        by = c("station_eu_code", "reporting_year"),
        all.x = TRUE
      )
    
    # validi per anno ####
    stat_yy2 <-
      stat_yy2 %>% mutate(n_validi = n - n_missing,
                          percentuale_validi = n_validi / n)
    
    DT::datatable(
      stat_yy2 %>% select(-c(station_eu_code)),
      rownames = FALSE,
      filter = "top",
      options = list(
        scrollY = '600px',
        paging = FALSE,
        scrollX = TRUE,
        dom = "ltipr"
      )
    ) %>% DT::formatRound(c(2:19), 2)
  })
  
  output$gam_output <- renderUI({
    req(reactive_objects$models)
    
    modello <-
      paste("<h4>Modello</h4><b>",
            names(reactive_objects$models),
            "</b>")
    
    b <- mgcv:::anova.gam(reactive_objects$models[[1]][[1]])
    b$s.table %>%
      kable(format = "html") %>% kable_styling() -> pvalues
    
    reactive_objects$models %>%
      purrr::map( ~ purrr::map_dbl(.x, AIC)) %>%
      do.call(cbind, .) %>%
      as.data.frame() %>%
      magrittr::set_colnames(c('AIC')) %>%
      round(2) -> aic
    
    reactive_objects$models[[1]] %>%
      purrr::map(summary.gam) %>%
      purrr::map_dbl( ~ .$r.sq) %>%
      as.data.frame() %>%
      magrittr::set_colnames(c('R')) %>%
      round(3) -> rquadro
    
    bv <- getViz(reactive_objects$models[[1]][[1]])
    indx <- grep("jd", bv[["smooth"]])
    plot(sm(bv, indx), allTerms = TRUE, select = 4) +
      geom_hline(yintercept = 0) -> g
    
    aic <- paste("<b>AIC</b>:", aic)
    rquadro <- paste("<b>Rsquared</b>:", rquadro)
    
    HTML(paste(modello, pvalues, aic, rquadro, sep = '<br /><br />'))
  })
  
  output$gam_summary <- renderUI({
    req(reactive_objects$models)
    sommario <-
      mgcv::summary.gam(reactive_objects$models[[1]][[1]])
    
    sommario$p.table %>% kable(format = "html") %>% kable_styling() %>% HTML()
  })
  
  output$jd_plot <- renderPlot({
    req(reactive_objects$models)
    
    bv <- getViz(reactive_objects$models[[1]][[1]])
    indx <- grep("jd", bv[["smooth"]])
    
    plot(sm(bv, indx), allTerms = TRUE, select = 4) + geom_hline(yintercept = 0) + ggtitle("Julian day") + theme_pulvirus()
  })
  
  output$jd_plot_last <- renderPlot({
    req(reactive_objects$models)
    
    bv <- getViz(reactive_objects$models[[1]][[1]])
    indx <- grep("jd", bv[["smooth"]])
    
    o <-
      plot(sm(bv, indx)) + ggtitle("Ultimo miglio") +  coord_cartesian(xlim = c(2500, 2750))
    
    o + l_fitLine(colour = "red") + l_rug(mapping = aes(x = x, y = y), alpha = 0.8) +
      l_ciLine(mul = 5,
               colour = "blue",
               linetype = 2) +
      l_points(shape = 19,
               size = 1,
               alpha = 0.1) + theme_pulvirus() + ggtitle("Julian day ultimi 250gg")
    
  })
  
  output$residui <- renderPlot({
    req(reactive_objects$models)
    
    bv <- getViz(reactive_objects$models[[1]][[1]])
    
    check(
      bv,
      type = "deviance",
      a.qq = list(
        method = "tnorm",
        a.cipoly = list(fill = "light blue")
      ),
      a.respoi = list(size = 0.5),
      a.hist = list(bins = 25)
    )
  })
  
  output$date_slider <- renderUI({
    date_min = min(2016)
    date_max = max(2020)
    sliderInput(
      "date_slider",
      "Date range:",
      min = date_min,
      max = date_max,
      value = c(date_min, date_max),
      round = TRUE,
      sep = ""
    )
  })
  
  
})