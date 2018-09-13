# -----------------------------------------------------------------------------
# Purchase String Visualization (server.R)
# By : Edward Huh 
# Date: August 31, 2018
# -----------------------------------------------------------------------------

function(input, output, session) {
  
  # initial data manipulation
  getData <- reactive({
    inFile <- input$file
    if (is.null(inFile))
      return(NULL)
    
    info <- read.csv(inFile$datapath)
    df <- as.tibble(info)
    agg = aggregate(COUNT~TRIAL+SW.1+SW.2+SW.3, data = df, sum)
    return(agg)
  })
  
  prepV1 <- reactive({
    agg <- getData()
    agg$rep = ifelse(
      (agg$SW.1 == "REPEAT" | agg$SW.2 == "REPEAT" | 
         agg$SW.3 == "REPEAT"), 1, 0)
    agg$swi = ifelse((agg$SW.1 == "SWITCH" | agg$SW.2 == "SWITCH" | 
                        agg$SW.3 == "SWITCH"), 1, 0)
    
    switch = aggregate(COUNT~TRIAL, data = agg[which(agg$swi == 1), ], sum)
    rep    = aggregate(COUNT~TRIAL, data = agg[which(agg$rep == 1), ], sum)
    trials = aggregate(COUNT~TRIAL, data = agg, sum)
    
    names(switch)[names(switch) == "COUNT"] = "SWITCHERS"
    names(rep)[names(rep) == "COUNT"]       = "REPEATERS"
    names(trials)[names(trials) == "COUNT"] = "TRIERS"
    
    agg_matrix = merge(trials,rep, by = "TRIAL")
    agg_matrix = merge(agg_matrix,switch, by = "TRIAL")
    return(agg_matrix)
  })
  
  
  # plot for visualization 1
  output$vis1plot <- plotly::renderPlotly({
    req(input$file)
    
    agg_matrix <- prepV1()
    vis1_df    <- dplyr::select(agg_matrix, c(TRIAL, TRIERS))
    total      <- sum(vis1_df$TRIERS)
    vis1_df    <- mutate(vis1_df, PERCENT = round(TRIERS / total * 100 ,
                                                  digits = 2))
    
    vis1_df$TRIAL <- factor(
      vis1_df$TRIAL, levels = unique(vis1_df$TRIAL)[order(
        vis1_df$TRIERS, decreasing = FALSE)])
    plot_ly(
      x            = vis1_df$PERCENT,
      y            = vis1_df$TRIAL,
      text         = vis1_df$PERCENT,
      textposition = 'outside',
      input        = "bar",
      orientation  = 'h',
      color        = I(input$vis1col)
    ) 
    # Title the graph by uncommenting this element!
    # %>% layout(title = "Visualization 1")
  })
  
  output$vis2plot <- plotly::renderPlotly({
    req(input$file)
    
    agg_matrix       <- prepV1()
    agg_matrix$TRIAL <- factor(agg_matrix$TRIAL, levels = unique(
      agg_matrix$TRIAL)[order(agg_matrix$TRIERS, decreasing = FALSE)])
    plot_ly(agg_matrix, 
            y           = ~TRIAL,
            x           = ~REPEATERS,
            type        = 'bar',
            orientation = 'h',
            color       = I(input$vis2col_r),
            name        = "Repeaters") %>%
      add_trace(x = ~TRIERS,
                color = I(input$vis2col_t),
                name  = "Triers") %>%
      layout(title   = "Point of Entry Trial and Repeat", 
             xaxis   = list(title = "Count"),
             barmode = 'group')
  })
  output$vis4plot <- plotly::renderPlotly({
    req(input$file)
    
    agg_matrix       <- prepV1()
    agg_matrix$TRIAL <- factor(agg_matrix$TRIAL, levels = unique(
      agg_matrix$TRIAL)[order(agg_matrix$REPEATERS, decreasing = FALSE)])
    plotly::plot_ly(agg_matrix, 
                    y           = ~TRIAL,
                    x           = ~SWITCHERS,
                    type        = 'bar',
                    orientation = 'h',
                    color = I(input$vis4col_s),
                    name  = "Switchers") %>%
      add_trace(x     = ~REPEATERS,
                color = I(input$vis4col_r),
                name  = "Repeaters") %>%
      layout(
        xaxis   = list(title = "Count"),
        yaxis   = list(title = ""), 
        barmode = 'group')
  })
  # ---------------------------------------------------------------------------
  # Visualization version 3
  # ---------------------------------------------------------------------------
  # product list in the input data.
  getProd <- reactive({
    df <- getData()
    unique(df$TRIAL) %>%
      str_replace(" ", "_") -> productlist
    return(productlist)
  })
  
  output$sankey_plots <- renderUI({
    req(input$file)
    product_list <- getProd()
    plotcount    <- length(product_list)
    iter         <- length(input$products)
    
    # call renderPlotly for each plot. PLots are only generated when they 
    # are visible on the web page
    # citation : https://gist.github.com/wch/5436415/ for inspiration
    # https://stackoverflow.com/questions/44550521/r-shiny-how-to-loop-output-plotly-graphs for question.
    for (i in 1:plotcount) {
      
      # break if there is nothing selected.
      if (iter == 0) {
        break
      }
      local({
        my_i     <- i
        plotname <- paste0("plot", my_i)
        
        output[[plotname]] <- renderPlotly({
          agg       <- getData()
          agg$TRIAL <- agg$TRIAL %>% 
            str_replace(" ", "_")
          plist     <- getProd()
          
          subset <- agg %>%
            filter(agg$TRIAL == plist[my_i])
          base   <- sum(subset$COUNT)
          mutate(subset, 
                 PERCENT = round(subset$COUNT / base * 100)) -> subset
          
          final  = builder(subset)
          nodes  <- sort(unique(c(as.character(final$SOURCE),
                                  as.character(final$END))))
          
          # change types
          final$COUNT   <- as.numeric(final$COUNT)
          final$PERCENT <- as.numeric(final$PERCENT)
          final %>%
            mutate(SOURCE = factor(SOURCE, levels = nodes)) %>%
            mutate(END = factor(END, levels = nodes)) -> sdf
          
          link.colors <- colorScale(3)
          final$COLOR <- c(link.colors[1],link.colors[2], link.colors[3])
          
          # ---- modify the node label with percentages -------#
          newlabel <- nameupdate(final$END, final$PERCENT)
          newlabel <- c(newlabel, glue("{plist[my_i]}: 100%"))
          newlabel <- sort(newlabel)
          # ----- decode node labels to legible format --------#
          gsub("SW\\d", "SWITCH",
               gsub("REP\\d", "REPEAT",
                    gsub("NP\\d", "NO PURCHASE", newlabel))) -> nodelabel
          
          # generate the sankey diagrams
          trace1 <- list(
            type   = "sankey",
            domain = list(
              x    =  c(0,1),
              y    =  c(0,1)
            ),
            node = list(label = nodelabel, color = 'black'),
            link = list(
              source = as.numeric(sdf$SOURCE) - 1,
              target = as.numeric(sdf$END) - 1,
              value  = sdf$COUNT,
              label  = sprintf("COUNT : %d, PERCENTAGE: %d%%", 
                               sdf$COUNT, sdf$PERCENT),
              color  = final$COLOR
            )
          )
          p <- plot_ly(
            domain = trace1$domain,
            link   = trace1$link,
            node   = trace1$node,
            type   = trace1$type
          ) %>%
            layout(font = t)
          
          while (!is.null(dev.list())) {
            dev.off()
          }  
          p
        })
      })
    }
    # Outputting relevant graphs-----------------------------------------------
    total_index <- {}
    for (product in input$products) {
      index       <- grep(glue::glue("^{product}$"), product_list)
      total_index <- c(total_index, index)
      total_index <- unique(total_index)
    }
    plot_output_list <- lapply(total_index, function(i) {
      plotname <- paste0("plot", i)
      plotly::plotlyOutput(plotname)
    })
    # convert the list to a tagList - this is necessary for the list of
    # items to display properly
    
    do.call(tagList, plot_output_list)
  })
  
  output$choose_prod1 <- renderUI({
    product_list <- getProd()
    checkboxGroupInput("products", "Choose Products", 
                       product_list)
  })
  
  
  
  # ---------------------------------------------------------------------------
  # Visualization version 5
  # ---------------------------------------------------------------------------
  # product list in the input data (exact same code as above)
  # Shiny applications do not support multiple rendering of the same output.
  output$choose_prod2 <- renderUI({
    product_list <- getProd()
    checkboxGroupInput("products5", "Choose Products", 
                       product_list)
  })
  
  prepV5 <- reactive({
    req(input$file)
    inFile <- input$file
    if (is.null(inFile))
      return(NULL)
    
    data <- read.csv(inFile$datapath, stringsAsFactors = FALSE, header = TRUE,
                     sep = ",")
    
    products    = unique(data$TRIAL)
    grid        = data[,c(1:4,8)]
    
    switch_grid = as.data.frame(products, stringsAsFactors = FALSE)
    names(switch_grid)[names(switch_grid) == "products"] = "TRIAL"
    
    for (k in 1:length(products)) {
      grid[products[k]] = ifelse(
        (grid$X1ST.RPEAT == products[k] | grid$X2ND.REPEAT == products[k] | 
           grid$X3RD.REPEAT == products[k]), 1,0)
      switch_grid1 = aggregate(
        COUNT~TRIAL, data = grid[which(grid[products[k]]	== 1),],sum)
      names(switch_grid1)[names(switch_grid1) == "COUNT"] = products[k]
      switch_grid = merge(switch_grid,switch_grid1, by = "TRIAL")
    }
    return(switch_grid)
  })
  
  output$bar_plots <- renderUI({
    req(input$file)
    switch_grid       <- prepV5()
    switch_grid$TRIAL <- switch_grid$TRIAL %>%
      stringr::str_replace(" ", "_")
    product_list      <- getProd()
    plotcount         <- length(product_list)
    iter              <- length(input$products5)
    # call renderPlotly for each plot. Plots are only generated when they are visible on the web page
    # citation : https://gist.github.com/wch/5436415/ for inspiration
    # https://stackoverflow.com/questions/44550521/r-shiny-how-to-loop-output-plotly-graphs for question.
    for (j in 1:plotcount) {
      # break if there is nothing selected.
      if (iter == 0) {
        break
      }
      local({
        my_j     <- j
        plotname <- paste0("bar", my_j)
        
        output[[plotname]] <- renderPlotly({
          switch_grid %>%
            filter(switch_grid$TRIAL == product_list[my_j]) -> subset
          names <- colnames(subset)
          yvals <- names[-1]
          
          names(subset) <- NULL
          xvals         <- as.numeric(subset[-1])
          
          vis5_df       <- data.frame(yvals, xvals)
          vis5_df$yvals <- factor(vis5_df$yvals, 
                                  levels = unique(vis5_df$yvals)[order(
                                    vis5_df$xvals, decreasing = FALSE)])
          
          p <- plot_ly(
            x           = vis5_df$xvals,
            y           = vis5_df$yvals,
            type        = "bar",
            # uncomment this if you want to see the count information.
            # text = vis5_df$xvals,
            # textposition = 'outside',
            orientation = 'h'
          )  %>%
            layout(#font = t,
              title = product_list[my_j],
              # yaxis = list(title = 'Trial Product'),
              xaxis = list(title = 'COUNT')
            )
          
          while (!is.null(dev.list())) {
            dev.off()
          }  
          p
        })
      })
    }
    # -------------------------------------------------------------------------
    total_index <- {}
    for (product in input$products5) {
      index       <- grep(glue::glue("^{product}$"), product_list)
      total_index <- c(total_index, index)
      total_index <- unique(total_index)
    }
    plot_output_list <- lapply(total_index, function(i) {
      plotname       <- paste0("bar", i)
      plotly::plotlyOutput(plotname)
    })
    # convert the list to a tagList - this is necessary for the list of
    # items to display properly
    
    do.call(tagList, plot_output_list)
  })
  
} # end of server function  

