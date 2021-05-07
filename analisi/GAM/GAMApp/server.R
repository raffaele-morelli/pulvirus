


# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  # Loading modal to keep user out of trouble while map draws...
  showModal(modalDialog(title = "PLEASE WAIT...", "Please wait", size = "l", footer = NULL))
  
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
      
      pal <- colorRampPalette(RColorBrewer::brewer.pal(4, "Spectral"))
      
      stazioniAria %>%
        mutate(tipo_s = case_when(
          zona_tipo == "FU" ~ "Fondo urbano/suburbano",
          zona_tipo == "FS" ~ "Fondo urbano/suburbano",
          zona_tipo == "TU" ~ "Traffico",
          zona_tipo == "TS" ~ "Traffico",
          tipo_zona == "R" ~ "Rurale",
          tipo_zona == "R-nearcity" ~ "Rurale",
          tipo_zona == "R-regional" ~ "Rurale",
          tipo_zona == "R-remote" ~ "Rurale",
          tipo_stazione == "I" ~ "Industriale",
          tipo_stazione == "F/I" ~ "Industriale",
        )) %>% filter(station_eu_code %in% stazUniche) -> stazioniAria
      
      pal = leaflet::colorFactor(pal(length(unique(stazioniAria$tipo_s))), domain = unique(stazioniAria$tipo_s))
      
      map = leaflet() %>% addTiles()

      # map = leaflet::addProviderTiles(
      #   map, "Esri.WorldImagery", group = "Satellite",
      #   options = providerTileOptions(updateWhenZooming = FALSE, updateWhenIdle = TRUE)
      # )
      
      map = leaflet::addProviderTiles(
        map, "Esri.WorldTopoMap", group = "Topo",
        options = providerTileOptions(updateWhenZooming = FALSE, updateWhenIdle = TRUE)
      )
      
      ptsRadius <- 5
      # map = addMapPane(map, "markers", zIndex = 400)
      
      map = leaflet::addCircleMarkers(
        map,
        data = no2Staz, lat = no2Staz$st_y, lng = no2Staz$st_x, group = "NO2",
        radius = ptsRadius,
        color = "black", # pal(no2Staz$tipo_s),
        opacity = 0.2,
        fillColor = pal(stazioniAria$tipo_s), #"black",
        fillOpacity = 1,
        layerId = paste("no2", no2Staz$station_eu_code, sep = "_"),
        popup = paste0("Location ID: ", no2Staz$station_eu_code,
          "<br> Name: ", no2Staz$nome_stazione,
          "<br> Type: ", no2Staz$zona_tipo,
          "<br> Lat: ", no2Staz$st_y,
          "<br> Long: ", no2Staz$st_x
        )
      )
      
      map = leaflet::addCircleMarkers(
        map,
        data = pm10Staz, lat = pm10Staz$st_y, lng = pm10Staz$st_x, group = "PM10",
        radius = ptsRadius,
        color = "black", # pal(no2Staz$tipo_s),
        opacity = 0.2,
        fillColor = pal(stazioniAria$tipo_s), #"black",
        fillOpacity = 1,
        layerId = paste("pm10", pm10Staz$station_eu_code, sep = "_"),
        popup = paste0(
          "Location ID: ", pm10Staz$station_eu_code,
          "<br> Name: ", pm10Staz$nome_stazione,
          "<br> Type: ", pm10Staz$zona_tipo,
          "<br> Lat: ", pm10Staz$st_y,
          "<br> Long: ", pm10Staz$st_x
        )
      )
      
      map = leaflet::addCircleMarkers(
        map,
        data = pm25Staz, lat = pm25Staz$st_y, lng = pm25Staz$st_x, group = "PM25",
        radius = ptsRadius,
        color = "black", # pal(no2Staz$tipo_s),
        opacity = 0.2,
        fillColor = pal(stazioniAria$tipo_s), #"black",
        fillOpacity = 1,
        layerId = paste("pm25", pm25Staz$station_eu_code, sep = "_"),
        popup = paste0(
          "Location ID: ", pm25Staz$station_eu_code,
          "<br> Name: ", pm25Staz$nome_stazione,
          "<br> Type: ", pm25Staz$zona_tipo,
          "<br> Lat: ", pm25Staz$st_y,
          "<br> Long: ", pm25Staz$st_x
        )
      )
      
      map = leaflet::addLayersControl(
        map,
        position = "topleft",
        # baseGroups = c("Topo", "Satellite"),
        # overlayGroups = c("NO2", "PM10", "PM25"),
        baseGroups = c("NO2", "PM10", "PM25"),
        options = leaflet::layersControlOptions(collapsed = FALSE, autoZIndex = TRUE)
      )
      
      # TODO riorganizzare il livelli per ordinare la zona climatica
      map = leaflet::addLegend(
          map,
          opacity = 1,
          position = 'topright',
          title = "Tipo stazione",
          colors = unique(pal(stazioniAria$tipo_s)),
          labels = unique(stazioniAria$tipo_s)
      )
      
      map = hideGroup(map, c("NO2", "PM25", "PM10"))
    })
  })
  
  # Table interface
  output$table_input = DT::renderDataTable({
    
    DT::datatable(
      stazioniAria %>% filter(station_eu_code %in% stazUniche) %>% 
        dplyr::select(station_eu_code, regione, provincia, comune, nome_stazione),
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
    
    station_eu_code = stazioniAria %>% filter(station_eu_code %in% stazUniche) %>% select(station_eu_code) %>% slice(row_click) %>% as.character()
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
    HTML("<br><div  style='color: red; background-color: yellow; font-size: 1.2em; padding: 0.5em;'><h4 style='color: black;'>IMPORTANTE</h4>
    <p> I layer disponibili sulla mappa mostrano <u>soltanto le serie valide</u>.</p>
<p>I plot, le statistiche descrittive e le risultanze del modello GAM per l'inquinante scelto dal menù a tendina/dropdown che segue
saranno visibili solo dopo aver selezionato la stazione di interesse dalla mappa o dalla tabella.</p></div>")
  }) 
  
  # Change map zoom on table click & update selected heatmap_param to selected row param
  map_proxy = leaflet::leafletProxy("aqMap")
  
  observeEvent(input$table_input_rows_selected, {
    lati = filter(stazioniAria, station_eu_code == reactive_objects$sel_station_eu_code) %>% 
      select(st_y) %>% as.numeric()
    long = filter(stazioniAria, station_eu_code == reactive_objects$sel_station_eu_code) %>% 
      select(st_x) %>% as.numeric()
    
    map_proxy %>% leaflet::setView(lng = long, lat = lati, zoom = 15)
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
      
      bp %>% filter(month %in% c("gennaio", "febbraio", "marzo", "aprile", "maggio", "giugno")) %>%
        ggplot(aes(x = reporting_year, y = value)) +
        stat_boxplot(geom = "errorbar", position = position_dodge(width = 0.9), color = "#87CEFA", width = 0.5 ) +
        geom_boxplot(position = position_dodge(width = 0.9), color = "#87CEFA") +
        xlab("Anno") + ylab("Concentrazione") +
        stat_summary(fun.y = mean, geom = "point", shape = 20, size = 3, color = "#000080",  fill = "#B0E0E6") +
        geom_smooth(method = "loess", se = TRUE, color = "#000080", aes(group = 1)) +
        facet_wrap( ~ month) +
        theme_pulvirus() -> p1
      
      # filter(get(reactive_objects$ts_pltnt), station_eu_code == reactive_objects$sel_station_eu_code) %>%
      #   mutate(month = month(date)) %>%
      #   ggplot(aes(factor(month), y = value)) +
      #   geom_boxplot() + xlab("Mese") + ylab("Concentrazione") +
      #   facet_grid(cols = vars(reporting_year)) + theme_pulvirus() -> p2
      
      grid.arrange(p1)
      
    } else{
      ggplot2::ggplot() + ggplot2::geom_blank() + 
        annotate("text", x = 4, y = 25, label = paste("Non ci sono dati per", reactive_objects$ts_pltnt, "<br>Scegliere un inquinante diverso!" ),
        size = 9
      ) + theme_void()
    }
    removeModal()
    
  })
  
  # Descrittive
  output$descrittive = DT::renderDataTable({
    req(reactive_objects$sel_station_eu_code, reactive_objects$ts_pltnt)
    
    filter(get(reactive_objects$ts_pltnt), station_eu_code == reactive_objects$sel_station_eu_code ) -> df
    
    # statistiche descrittive annuali#####
    df <- cutData(df, type = c("month", "monthyear", "season"))
    
    stat_yy <- df %>% group_by(station_eu_code, reporting_year) %>% skim(value)
    
    # calcolo percentili ####
    p <- c(0.05, 0.904, 0.95, 0.98, 0.999)
    p_names <- map_chr(p, ~ paste0(.x * 100, "percentile"))
    
    p_funs <- map(p, ~ partial(quantile, probs = .x, type = 8, na.rm = TRUE )) %>% set_names(nm = p_names)
    
    perc <- df %>% group_by(station_eu_code, reporting_year) %>% summarize_at(vars(value), funs(!!!p_funs))
    
    perc$ID <- seq.int(nrow(perc))
    
    stat_yy <- stat_yy[, c(3:13)]

    # conteggio dati disponibili per anno ####
    conteggio <- df %>% group_by(reporting_year) %>% count(station_eu_code)
    
    # unisco statistiche, percentili e conteggi
    # unione conteggi ####
    stat_yy1 <- inner_join(stat_yy, perc, by = c("station_eu_code", "reporting_year"), all = TRUE )
    stat_yy2 <- inner_join(stat_yy1, conteggio, by = c("station_eu_code", "reporting_year"), all.x = TRUE)
    
    # validi per anno ####
    stat_yy2 <-
      stat_yy2 %>% mutate(n_validi = n - n_missing, percentuale_validi = n_validi / n) %>% 
      select(-c(station_eu_code, ID, n_validi, n, "98percentile", "99.9percentile" ))
    
    # [1] "station_eu_code" "reporting_year"  "n_missing"       "complete_rate"   "numeric.mean"    "numeric.sd"     
    # [7] "numeric.p0"      "numeric.p25"     "numeric.p50"     "numeric.p75"     "numeric.p100"    "5percentile"    
    # [13] "90percentile"    "95percentile"    "98percentile"    "99.9percentile"  "ID"              "n"

    colnames(stat_yy2) <- c("Anno", "mancanti", "compl", "Media", "SD", "Min","25th", "Mediana", "75th", "Max", "5th", "90.4th", "95th", "validi")
    
    DT::datatable(stat_yy2 , rownames = FALSE, filter = c("none"),
      # filter = "top",
      options = list(
        # scrollY = '600px',
        paging = FALSE,
        scrollX = TRUE,
        dom = "ltipr"
      )) %>% DT::formatRound(c(2:14), 2)
  })

  # differenze percentuali
  output$diff_plot <- renderPlot({
    req(reactive_objects$sel_station_eu_code, reactive_objects$ts_pltnt)
    
    filter(get(reactive_objects$ts_pltnt), station_eu_code == reactive_objects$sel_station_eu_code ) -> df
    
    # calcolo medie mensili (per mesi con almeno il 75% di dati validi) ####
    mm <- timeAverage(df, pollutant = "value", type = "station_eu_code", avg.time = "month", data.thresh = 75)
    
    # creo tabella mese tipico periodo 2013-2019 ####
    mm <- cutData(mm, type = c("year", "month", "monthyear", "season"))
    
    mm  %>%
      group_by(station_eu_code, month) %>%
      summarise(
        n = n(),
        mean_pr = mean(value, na.rm = T)) -> mm.pre.mese
    
    names(mm.pre.mese)[c(2)] <- c("mese")
    names(mm.pre.mese)[c(4)] <- c("media_2013_2019")
    
    # creo tabella mese tipico periodo 2020 ####
    mm.post <- selectByDate(mm, year = 2020)
    
    mm.post  %>%
      group_by(station_eu_code,month) %>%
      summarise(
        n = n(),
        mean_post = mean(value, na.rm = T))->mm.post.mese
    names(mm.post.mese)[c(2)] <- c("mese")
    names(mm.post.mese)[c(4)] <- c("media_2020")
    
    
    # calcolo le differenze percentuali ####
    diff_mm <- merge(mm.post.mese, mm.pre.mese, by = c("station_eu_code","mese"), all = TRUE)
    diff_mm <- diff_mm %>%
      mutate(diff_perc = (((media_2020 - media_2013_2019)/(media_2013_2019)*100)) )
    
    ggplot(diff_mm) + geom_col(aes(mese, diff_perc), fill = "dodgerblue") + 
      xlab("Mese") + ylab("Diff %") + 
      theme_pulvirus() + ggtitle("Differenze percentuali 2013-19 Vs 2020")
  })
  
  # output GAM
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
  
  # info stazione
  output$info_stazione <- renderUI({
    req(reactive_objects$sel_station_eu_code)
    
    as.character( reactive_objects$sel_station_eu_code ) -> cod
    
    stazioniAria %>% 
      filter(station_eu_code == cod) %>% 
      select(nome_stazione, tipo_zona, tipo_stazione, zona_tipo, zone_name, descrizione, gradi_giorno, zona_climatica) %>% 
      as.data.frame() %>% t() %>% 
      set_rownames(c("Nome stazione", "Tipo", "Zona", "Tipo/zona", "Nome zona", "Descrizione", "GG", "Zona climatica")) %>% 
      kable() %>% 
      kable_styling() %>% 
      HTML()
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
      l_ciLine(mul = 5, colour = "blue", linetype = 2) + l_points(shape = 19, size = 1, alpha = 0.1) + theme_pulvirus() + ggtitle("Julian day ultimi 250gg")
    o
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