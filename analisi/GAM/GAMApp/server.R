
shinyServer(function(input, output, session) {
  showModal(modalDialog(title = "PLEASE WAIT...", "Please wait", size = "l", footer = NULL))
  
  observe({
    req(map)
    removeModal()
  })
  
  # Empty reactive values object
  reactive_objects = reactiveValues()

  map = createLeafletMap(session, 'aqMap')
  
  session$onFlushed(once = T, function() {
    
    # function per generare i popup
    ppup <- function(pltntDF) {
      return(
        paste0("EU Code: ", pltntDF$station_eu_code,
             "<br> Nome: ", pltntDF$nome_stazione,
             "<br> Lat: ", pltntDF$st_y,
             "<br> Long: ", pltntDF$st_x )
      )
    }
    
    output$aqMap <- renderLeaflet({
      zoneclimatiche <- unique(stazioniAria$zona_climatica) %>% sort()
      
      map = leaflet() %>% addTiles()
      
      map = leaflet::addProviderTiles(
        map, "Esri.WorldTopoMap", group = "Topo",
        options = providerTileOptions(updateWhenZooming = FALSE, updateWhenIdle = TRUE)
      )
      
      # grandezza dei marker
      ptsRadius <- 5

      map = leaflet::addCircleMarkers(
        map,
        data = no2Staz, lat = no2Staz$st_y, lng = no2Staz$st_x, group = "NO2",
        radius = ptsRadius,
        color = "black", 
        opacity = 0.2, fillColor = paletta(no2Staz$tipo_s), fillOpacity = 1,
        layerId = paste("no2", no2Staz$station_eu_code, sep = "_"),
        popup = ppup(no2Staz)
      )
      
      map = leaflet::addCircleMarkers(
        map,
        data = pm10Staz, lat = pm10Staz$st_y, lng = pm10Staz$st_x, group = "PM10",
        radius = ptsRadius,
        color = "black", 
        opacity = 0.2, fillColor = paletta(pm10Staz$tipo_s), fillOpacity = 1,
        layerId = paste("pm10", pm10Staz$station_eu_code, sep = "_"),
        popup = ppup(pm10Staz)
      )
      
      map = leaflet::addCircleMarkers(
        map,
        data = pm25Staz, lat = pm25Staz$st_y, lng = pm25Staz$st_x, group = "PM25",
        radius = ptsRadius,
        color = "black", 
        opacity = 0.2, fillColor = paletta(pm25Staz$tipo_s), fillOpacity = 1,
        layerId = paste("pm25", pm25Staz$station_eu_code, sep = "_"),
        popup = ppup(pm25Staz)
      )
      
      map = leaflet::addCircleMarkers(
        map,
        data = noxStaz, lat = noxStaz$st_y, lng = noxStaz$st_x, group = "NOX",
        radius = ptsRadius,
        color = "black", 
        opacity = 0.2, fillColor = paletta(noxStaz$tipo_s), fillOpacity = 1,
        layerId = paste("nox", noxStaz$station_eu_code, sep = "_"),
        popup = ppup(noxStaz)
      )
      
      map = leaflet::addLayersControl(
        map,
        position = "topleft",
        baseGroups = c("NO2", "PM10", "PM25", "NOX"),
        options = leaflet::layersControlOptions(collapsed = FALSE, autoZIndex = TRUE)
      )
      
      p <- stazioniAria %>% filter(station_eu_code %in% stazUniche) 
      
      map = leaflet::addLegend(
          map,
          opacity = 1,
          position = 'topright',
          title = "Tipo stazione",
          colors = unique(paletta(p$tipo_s)),
          labels = unique(p$tipo_s)
      )
      
      map = hideGroup(map, c("NO2", "PM25", "PM10", "NOX"))
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
    
    # leafletProxy(mapId = "aqMap") %>% clearPopups() 
    reactive_objects$sel_station_eu_code <- stringr::str_split(click, "_")[[1]][2]
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
        base_path, rec$region_id,
        "/", reactive_objects$ts_pltnt,
        "/", reactive_objects$ts_pltnt,
        "_", rec$region_id,
        "_", reactive_objects$sel_station_eu_code, ".RData"
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
    req(reactive_objects$sel_station_eu_code, reactive_objects$ts_pltnt)

    filter(
      get(reactive_objects$ts_pltnt),
      station_eu_code == reactive_objects$sel_station_eu_code
    ) -> df
    
    titolo <- case_when(
      reactive_objects$ts_pltnt == "no2" ~ bquote("Inquinante "~NO[2]),
      reactive_objects$ts_pltnt == "pm10" ~ bquote("Inquinante "~PM[10]),
      reactive_objects$ts_pltnt == "pm25" ~ bquote("Inquinante "~PM[25]),
      reactive_objects$ts_pltnt == "nox" ~ bquote("Inquinante "~NO[x])
    )
  
    if (nrow(df) > 0) {
      req(reactive_objects$models)
      
      # modello <-  names(reactive_objects$models)
      
      # b <- mgcv:::anova.gam(reactive_objects$models[[1]][[1]])
      # formGAM <- gsub("value", "media", b[["formula"]]) 
      
      df %>% mutate(week = as.Date(date, '%Y-%V')) %>%
        group_by(reporting_year, station_eu_code, week) %>%
        summarise(media = mean(value, na.rm = TRUE)) %>%
        ggplot(aes(week, media)) + geom_line(color = "dodgerblue") +
        facet_wrap(~reporting_year, scales = "free_x") +
        xlab("Settimana") + 
        ylab("Concentrazione media (\U003BCg/m³)") +
        geom_smooth(method = "gam", formula = y ~ s(x, k = 10) , se = TRUE, color = "#000080", aes(group = 1)) +
        scale_x_date(labels = date_format("%Y")) +
        ggtitle(titolo) +
        theme_pulvirus()
    } else{
      ggplot2::ggplot() + ggplot2::geom_blank() + 
        annotate( "text", x = 4, y = 25, label = paste("Non ci sono dati per", reactive_objects$ts_pltnt, 
                      "Scegliere un inquinante diverso!", sep = "\n" ), size = 9
      ) + theme_void()
    }

  })
  
  # pollution rose
  output$pollution_rose = renderPlot({
    req(reactive_objects$sel_station_eu_code, reactive_objects$ts_pltnt)
    showModal(modalDialog(title = "PLEASE WAIT...", "Please wait", size = "l", footer = NULL ))
    
    filter(
      get(reactive_objects$ts_pltnt),
      station_eu_code == reactive_objects$sel_station_eu_code
    ) %>%
      inner_join(dati_meteo, by = c("station_eu_code", "date")) %>%
      select(reporting_year, date, value, wdir, wspeed, pbl00, pbl12, pblmin, pblmax) -> gr
    
    if (nrow(gr) > 0) {
      pltnt <- as.character(reactive_objects$ts_pltnt)
      names(gr)[3:5] <- c(pltnt, "wd", "ws")
      gr$reporting_year <-
        as.character(as.numeric(gr$reporting_year))
      
      polarPlot(gr, pollutant = c(pltnt), type = "reporting_year", cols = "jet", key.position = "right")
    } else{
      ggplot2::ggplot() + ggplot2::geom_blank() + 
        annotate("text", x = 4, y = 25,
        label = paste("Non ci sono dati per", reactive_objects$ts_pltnt, 
                      "Scegliere un inquinante diverso!", sep = "\n" ), size = 9) + theme_void()
    }
    
    removeModal()
  })
  
  # Polar plot
  output$polar <- renderPlot({
    req(reactive_objects$sel_station_eu_code, reactive_objects$ts_pltnt)
    showModal(modalDialog(title = "PLEASE WAIT...", "Please wait for plot to draw", size = "l", footer = NULL ))
    
    filter(
      get(reactive_objects$ts_pltnt),
      station_eu_code == reactive_objects$sel_station_eu_code
    ) %>%
      inner_join(dati_meteo, by = c("station_eu_code", "date")) %>%
      select(reporting_year, date, value, wdir, wspeed, pbl00, pbl12, pblmin, pblmax) -> gr
    
    pltnt <- reactive_objects$ts_pltnt
    if (nrow(gr) > 0) {
      names(gr)[3:5] <- c(pltnt, "wd", "ws")
      
      gr$reporting_year <- as.character(as.numeric(gr$reporting_year))
      # print(gr)
      polarFreq(gr, pollutant = c(pltnt), cols = "jet", type = "year")
    } else{
      ggplot2::ggplot() + ggplot2::geom_blank() + 
        annotate("text", x = 4, y = 25,
        label = paste("Non ci sono dati per", reactive_objects$ts_pltnt, 
                      "Scegliere un inquinante diverso!", sep = "\n" ),        size = 9
      ) + theme_void()
    }
    removeModal()
    
  })
  
  # Boxplot
  output$boxplot <- renderPlot({
    req(reactive_objects$sel_station_eu_code, reactive_objects$ts_pltnt)
    showModal(modalDialog(title = "PLEASE WAIT...", "Please wait", size = "l", footer = NULL))
    
    filter(get(reactive_objects$ts_pltnt), station_eu_code == reactive_objects$sel_station_eu_code) %>%
      select(reporting_year, date, value) -> bp
    
    if (nrow(bp) > 0) {
      bp <- cutData(bp, type = c("month"))
      
      bp$reporting_year <- as.character(as.numeric(bp$reporting_year))
      
      # print(bp)
      titolo <- case_when(
        reactive_objects$ts_pltnt == "no2" ~ bquote("Inquinante "~NO[2]),
        reactive_objects$ts_pltnt == "pm10" ~ bquote("Inquinante "~PM[10]),
        reactive_objects$ts_pltnt == "pm25" ~ bquote("Inquinante "~PM[25]),
        reactive_objects$ts_pltnt == "nox" ~ bquote("Inquinante "~NO[x])
      )
      bp %>% filter(month %in% c("gennaio", "febbraio", "marzo", "aprile", "maggio", "giugno")) %>%
        ggplot(aes(x = reporting_year, y = value)) +
        stat_boxplot(geom = "errorbar", position = position_dodge(width = 0.9), color = "#87CEFA", width = 0.5 ) +
        geom_boxplot(position = position_dodge(width = 0.9), color = "#87CEFA") +
        xlab("Anno") + 
        ylab("Concentrazione media (\U003BCg/m³)") +
        stat_summary(fun.y = mean, geom = "point", shape = 20, size = 3, color = "#000080",  fill = "#B0E0E6") +
        geom_smooth(method = "loess", se = TRUE, color = "#000080", aes(group = 1)) +
        facet_wrap( ~ month) +
        ggtitle(titolo) +
        theme_pulvirus() -> p1

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
      paste("<h4>Modello</h4><b>", names(reactive_objects$models), "</b>")
    
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
    
    if(length(indx) != 0) {
      
    plot(sm(bv, indx), allTerms = TRUE, select = 4) +
      geom_hline(yintercept = 0) -> g
    }else{
      ggplot2::ggplot() + ggplot2::geom_blank() + 
        annotate("text", x = 4, y = 25, label = paste("Non c'è il JD" ),
                 size = 9
        ) + theme_void()
    }
    
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
    
    b <- getViz(reactive_objects$models[[1]][[1]])
    indx <- grep("jd", b[["smooth"]])
    if(length(indx) != 0) {
      
    plot(sm(b, indx)) + 
      # l_points(shape = 19, size = 1, color = "orange", alpha = 0.5) +
      l_fitLine(colour = "red", size = 1) + 
      # l_rug(mapping = aes(x = x, y = y), alpha = 1) +
      l_ciLine( colour = "blue", linetype = 2) +
      # geom_hline(yintercept = 0) +
      # coord_cartesian(ylim = c(-30, 30)) +
      ggtitle("Julian day")
    }else{
      ggplot2::ggplot() + ggplot2::geom_blank() + 
        annotate("text", x = 4, y = 25, label = paste("Non c'è il JD" ),
                 size = 9
        ) + theme_void()
    }
  })
  
  output$jd_plot_last <- renderPlot({
    req(reactive_objects$models)
    
    b <- getViz(reactive_objects$models[[1]][[1]])
    
    indx <- grep("jd", b[["smooth"]])
    if(length(indx) != 0) {
      
    plot( sm(b, indx) ) + 
      # l_points(shape = 19, size = 1, color = "orange", alpha = 0.5) +
      l_fitLine(colour = "red", size = 1) + 
      # l_rug(mapping = aes(x = x, y = y), alpha = 1) +
      l_ciLine(mul = 5, colour = "blue", linetype = 2) +
      coord_cartesian(xlim = c(2500, 2750)) +
      ggtitle("Julian day ultimi 250gg")
    }else{
      ggplot2::ggplot() + ggplot2::geom_blank() + 
        annotate("text", x = 4, y = 25, label = paste("Non c'è il JD" ),
                 size = 9
        ) + theme_void()
}
  })
  
  output$residui <- renderPlot({
    req(reactive_objects$models)
    
    b <- getViz(reactive_objects$models[[1]][[1]])
    
    check(
      b,
      type = "deviance",
      a.qq = list(method = "tnorm", a.cipoly = list(fill = "dodgerblue")),
      a.respoi = list(size = 0.5),
      a.hist = list(bins = 25, fill = "dodgerblue")
    )
  })
  
  # output$date_slider <- renderUI({
  #   date_min = min(2016)
  #   date_max = max(2020)
  #   sliderInput(
  #     "date_slider",
  #     "Date range:",
  #     min = date_min,
  #     max = date_max,
  #     value = c(date_min, date_max),
  #     round = TRUE,
  #     sep = ""
  #   )
  # })
  
  
})