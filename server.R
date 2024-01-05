# Define server logic ----------------------------------------------------------
function(input, output, session) {
  
  # call the server part of shinymanager ---------------------------------------
  res_auth <- secure_server(
    check_credentials = check_credentials(credentials)
  )
  
  # observe the result of the authentication -----------------------------------
  observe({
    if(!is.null(res_auth$user)){
      showModal(modalDialog(
        title = "Login successful",
        paste("Bienvenido", res_auth$user),
        size = "m",
        easyClose = T,
        footer = NULL
      ))
    }
  })
  
  # resumen section ------------------------------------------------------------
  
  # top 10 countries with the most UFO sightings
  output$top_countries_plot <- renderPlot({
    ggplot(top_countries, aes(x = "", y = count, fill = country_code)) +
      geom_bar(width = 1, stat = "identity") +
      coord_polar("y", start = 0) +
      theme_minimal() +
      labs(x = NULL, y = NULL, fill = "País") +
      theme(axis.line = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            plot.title = element_text(hjust = 0.5)) +
      geom_text(aes(label = country_code), 
                position = position_stack(vjust = 0.5))
  })
  
  # create a word cloud
    output$word_cloud_plot <- renderPlot({
      
      data_filtered <- tabla %>%
        filter(!words %in% c("sky", "lights", "light"))
      
      wordcloud(data_filtered$words, data_filtered$Freq, max.words = 100, 
                colors = brewer.pal(8, "Dark2"))
    })
  
  # download .pdf logic
    output$download_pdf <- downloadHandler(
    filename = function() {
      paste("wordcloud-", Sys.Date(), ".pdf", sep="")
    },
    content = function(file) {
      withProgress(message = 'Descarga en proceso', value = 0, {
        # filter data based on input year
        incProgress(1/3)
        Sys.sleep(1)
        data_filtered <- UFO_sightings %>%
          filter(year == input$year) %>%
          group_by(day_part) %>%
          summarise(count = n())
        
        # create the wordcloud and save it to a PDF file
        incProgress(1/3)
        Sys.sleep(1)
        pdf(file)
        wordcloud(words = data_filtered$word, freq = data_filtered$freq, 
                  max.words = 100, random.order = FALSE, rot.per = 0.35, 
                  colors = brewer.pal(8, "Dark2"), scale=c(5,0.5))
        dev.off()
        
        # complete the progress bar
        incProgress(1/3)
        Sys.sleep(1)
      })
    }
  )
  
  # create KPIs of total words and unique words
  output$total_words <- renderValueBox({
    valueBox(total_words, "Total palabras", icon = icon("list"),
             color = "purple")
  })
  
  output$unique_words <- renderValueBox({
    valueBox(unique_words, "Palabras únicas", icon = icon("thumbs-up", 
                                                       lib = "glyphicon"),
             color = "yellow")
  })

  # US data section ------------------------------------------------------------
  
  # create KPIs of US data
  output$sightings_US <- renderValueBox({
    sightings_US <- nrow(UFO_sightings[UFO_sightings$country_code == "US",])
    valueBox(sightings_US, "Avistamientos en EEUU", 
             icon = icon("eye"), color = "blue")
  })
  
  output$state_most_sightings <- renderValueBox({
    state_most_sightings <- 
      UFO_sightings[UFO_sightings$country_code == "US",] %>%
      group_by(state) %>%
      summarise(count = n()) %>%
      top_n(1, count) %>%
      pull(state)
    
    valueBox(state_most_sightings, "Estado con más avistamientos", 
             icon = icon("map-marker"), color = "green")
  })
  
  output$city_most_sightings <- renderValueBox({
    city_most_sightings <- UFO_sightings %>%
      group_by(city) %>%
      summarise(count = n()) %>%
      top_n(1, count) %>%
      pull(city)
    
    valueBox(city_most_sightings, "Ciudad con más avistamientos", 
             icon = icon("building"), color = "red")
  })
  
  # create plots of US data
  data_US <- reactive({
    UFO_sightings[UFO_sightings$shape == input$shape, ]
  })
  
  output$sightings_by_year <- renderPlot({
    data <- data_US()
    sightings_by_year <- data %>% 
      group_by(year, city) %>% 
      summarise(count = n())
    ggplot(sightings_by_year, aes(x = year, y = count)) +
      geom_bar(stat = "identity") +
      labs(x = "Año", y = "Avistamientos")
  })
  
  output$common_shapes_plot <- renderPlot({
    data <- data_US()
    common_cities <- data %>% 
      group_by(city) %>% 
      summarise(count = n()) %>% 
      arrange(desc(count)) %>% 
      head(10)
    ggplot(common_cities, aes(x = city, y = count)) +
      geom_bar(stat = "identity") +
      labs(x = "Ciudad de US", y = "Avistamientos")
  })
  
  output$duration_seconds_plot <- renderPlot({
    sightings_without_outliers <- UFO_sightings[UFO_sightings$duration_seconds < 100 & UFO_sightings$year == input$year, ]
    ggplot(sightings_without_outliers, aes(x = duration_seconds)) +
      geom_histogram(binwidth = 1, fill = "blue", color = "black") +
      labs(x = "Duración avistamiento (seg.)", y = "Q")
  })
  
  output$day_part_plot <- renderPlot({
    sightings <- UFO_sightings[UFO_sightings$year == input$year, ]
    ggplot(sightings, aes(x = day_part)) +
      geom_bar(fill = "blue", color = "black") +
      labs(x = "Momento del día", y = "Q")
  })
  
  # render time section --------------------------------------------------------
  
  observeEvent(input$change, {
    
    output$timeSeriesPlot <- renderPlot({
      # filter data based on input year
      data_filtered <- UFO_sightings %>%
        filter(year == input$year)
      
      # group by day_part and calculate the number of sightings per day_part
      sightings_by_day_part <- data_filtered %>%
        group_by(day_part) %>%
        summarise(count = n())
      
      # create a time series plot
      ggplot(sightings_by_day_part, aes(x = day_part, y = count)) +
        geom_bar(stat = "identity") +
        labs(x = "Momento del día", y = "Avistamiento")
    })
  })
  
  # download .csv logic
  output$download_csv <- downloadHandler(
    filename = function() {
      paste("sightings_day_parts-", input$year, ".csv", sep="")
    },
    content = function(file) {
      withProgress(message = 'Descarga en proceso', value = 0, {
        # filter data based on input year
        incProgress(1/3)
        Sys.sleep(1)
        data_filtered <- UFO_sightings %>%
          filter(year == input$year) %>%
          group_by(day_part) %>%
          summarise(count = n())
        
        # write the data to a CSV file
        incProgress(1/3)
        Sys.sleep(1)
        write.csv(data_filtered, file, row.names = FALSE)
        
        # complete the progress bar
        incProgress(1/3)
        Sys.sleep(1)
      })
    }
  )
}