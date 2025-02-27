#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

# Define server logic required to draw a histogram
function(input, output, session) {

# Input Rendering
    
    max_date <- max(treasury_data_server$date)
    min_date <- min(treasury_data_server$date)
    maturities_included <- ncol(treasury_data_server) - 1
    print(head(treasury_data_server))
    print(str(treasury_data_server))
    
    print(min_date)
    print(max_date)
    
    # Model Training Window
    output$training_window_dates <- renderUI(sliderInput(inputId = "training_range", label = "Training Window (PCA and VAR):",
                                                         value = c(min_date, max_date),
                                                         min = min_date,
                                                         max = max_date,
                                                         width = "90%"))
    
    observeEvent(input$max_date, {
        updateDateInput(session, "selected_yield", value = as.Date(max_date))
    })
    
    maturities <- colnames(treasury_data_server %>% dplyr::select(-date)) %>% as.numeric()
    app_state <- shiny::reactiveValues()
    # Data and Model Updates
    
    pc_deltas_sd <- NULL
    app_state$init <- NULL
    
    observeEvent(input$training_range,{
        print("Model Training")
        
        pca_data <<- treasury_data_server %>% 
            filter(date >= min(input$training_range) & date <= max(input$training_range)) %>% 
            select(-date)
        yield_mean <- colMeans(pca_data)
        yield_matrix_centered <- sweep(pca_data, 2, yield_mean, "-")
        pca_result <- prcomp(yield_matrix_centered, center = FALSE, scale. = TRUE)
        PCs <<- pca_result$rotation
        
        pc_historical <<- load_to_pc(yield_matrix_centered, PCs)
        pc_deltas_historical <<- deltas(pc_historical)
        pc_deltas_sd <<- pc_deltas_historical %>% 
            dplyr::summarize(dplyr::across(dplyr::any_of(colnames(pc_deltas_historical)),sd))
        
        if(is.null(app_state$init)){
            app_state$init <- 0
        }else{
            app_state$init <- app_state$init + 1
        }
        
        print("Done modelling")
    })
    
    observeEvent(list(input$selected_yield, input$parallel_shift, input$steepening, input$curvature, app_state$init), {
        print("Running Yield Curve Plot")
        req(pc_deltas_sd)
        print(input$selected_yield)
        
        pcs <- c(input$parallel_shift*pc_deltas_sd$PC1, input$steepening*pc_deltas_sd$PC2, input$curvature*pc_deltas_sd$PC3)
        pcs <- c(pcs, rep(0, ncol(PCs) - length(pcs)))
        pcs <- t(as.matrix(pcs))
        
        stress <- unload_pc(pcs, PCs) %>% as.vector() %>% unlist()
        current_yield <- treasury_data_server %>% filter(date <= input$selected_yield) %>% filter(date == max(date)) %>% select(-date)
   
        stressed_curve <- as.numeric(current_yield) + stress
        # Where are PC levels currently, by current selected yield?
        current_pca <- load_to_pc(current_yield, PCs)
        
        df <- data.frame(
        Term = colnames(treasury_data_server %>% dplyr::select(-date)) %>% as.numeric(),
        Base = as.numeric(current_yield),
        Stressed = stressed_curve
        )
        
        # df$TermNum <- as.numeric(gsub("T", "", df$Term))
        
        term_factor_levels <- df %>% 
            dplyr::mutate(Term = as.numeric(df$Term)) %>% 
            dplyr::arrange(Term) %>% 
            dplyr::pull(Term) %>% 
            unique() %>% 
            as.character()
        
        print(term_factor_levels)
        
        df_viz <- df %>% dplyr::mutate(Term = factor(as.character(Term), levels = term_factor_levels))
        
        output$yield_curve_plot <- renderPlot({ggplot(df_viz, aes(x = Term)) +
               geom_line(aes(y = Base, color = "Base Curve"), size = 1, group = 1) +
               geom_line(aes(y = Stressed, color = "Stressed Curve"), size = 1, group = 1, linetype=3) +
               labs(title = "Yield Curve Stress Testing", x = "Term (Months)", y = "Yield (%)") +
               # scale_x_continuous(breaks = df$TermNum, labels = df$Term) +
               scale_color_manual(values = c("Base Curve" = "black", "Stressed Curve" = "red")) +
               theme_minimal()})
        
        print(df)
        h_spline <- fit_h_spline(x = as.numeric(df$Term), y = as.numeric(df$Stressed), missing = 0:360)
        saveRDS(h_spline, "yield_curve.rds")
        
        boot_df <- h_spline %>% 
            data.frame(term = as.numeric(names(.)), yield = .) %>% 
            ai_from_df(date_in = input$selected_yield) %>% 
            bootstrap_1()
        
        output$boot_dt <- renderDT({
            boot_df
        })
        
    })
    
    
    
    #                   
    #    # Prevents early reactivity
    #    print(loaded)
    #    if(loaded){
    #                   
    #    pcs <- c(input$parallel_shift*pc_deltas_sd$PC1, input$steepening*pc_deltas_sd$PC2, input$curvature*pc_deltas_sd$PC3)
    #    pcs <- c(pcs, rep(0, ncol(PCs) - length(pcs)))
    #    pcs <- t(as.matrix(pcs))
    #    
    #    stress <- unload_pc(pcs, PCs) %>% as.vector() %>% unlist()
    #    current_yield <- treasury_data %>% filter(date <= input$yield_date_selected) %>% filter(date == max(date)) %>% select(-date)
    #    
    #    stressed_curve <- as.numeric(current_yield) + stress
    #    # Where are PC levels currently, by current selected yield?
    #    current_pca <- load_to_pc(current_yield, PCs)
    #    
    #    }
    
    # Today versus stressed curve
    #output$yield_curve_plot <- renderPlot({
        #This is the interactivity
        
    #    
        
        # Get's the current yield based on the last observable date prior or equal to the user selection:
    #    print(str(input$yield_date_selected))
    #    
    #    
        
    #    
        
    #    df <- data.frame(
    #        Term = colnames(pca_data),
    #        Base = as.numeric(current_yield),
    #        Stressed = stressed_curve
    #    )
        
        #Names to numeric for purpose of ordering x-axis
    #    df$TermNum <- as.numeric(gsub("T", "", df$Term))
        
    #    ggplot(df, aes(x = TermNum)) +
    #        geom_line(aes(y = Base, color = "Base Curve"), size = 1, group = 1) +
    #        geom_line(aes(y = Stressed, color = "Stressed Curve"), size = 1, group = 1) +
    #        labs(title = "Yield Curve Stress Testing", x = "Term", y = "Yield (%)") +
    #        scale_x_continuous(breaks = df$TermNum, labels = df$Term) +
    #        scale_color_manual(values = c("Base Curve" = "blue", "Stressed Curve" = "red")) +
    #        theme_minimal()
    #})
    
    # Distribution of PCA factor deltas (Risk)
    
    #pc_delta_plot <- pc_deltas_historical %>% 
    #    mutate(temp = 1) %>% 
    #    pivot_longer(cols = -temp) %>% 
    #    mutate(name = factor(name, levels = paste0("PC", 1:maturities_included))) %>% 
    #    select(-temp) %>% 
    #    ggplot() + 
    #    geom_histogram(aes(x = value, y = ..count../(sum(..count..)/maturities_included)), bins = 100) + 
    #    facet_wrap(~name) +
    #    labs(x = "Value", y = "Density")
    
    #output$pc_risk_plot <- renderPlot(pc_delta_plot)
    
    # Current Yield Curve PCA Weightings
    
    #current_pca_bar <- bind_cols(
    #    t(current_pca),
    #    t(pcs)
    #) %>%
    #    as.data.frame() %>% 
    #    mutate(component = 1, component = cumsum(component)) %>% 
    #    set_names(c("Current", "Scenario Delta", "PC #"))
    
    #current_pca_bar %>% 
    #    pivot_longer(cols = -`PC #`) %>% 
    #    ggplot() +
    #    geom_col(aes(x = `PC #`, y = value, fill = name)) +
    #    labs(title = "Breakdown of Principle Components",
    #         subtitle = "Current Yield Curve and Scenario",
    #         x = "Principal Component",
    #         y = "Value")
    
    # Data prep
    #pca_data <- treasury_data %>% 
    #    select(-date)
    #colnames(pca_data) <- paste0("T", 1:maturities_included)
    #yield_mean <- colMeans(pca_data)
    #yield_matrix_centered <- sweep(pca_data, 2, yield_mean, "-")
    
    #observeEvent(c(input$yield_date_selected) ,{
    #    pca_data <- treasury_data %>% 
    #        filter(date >= min(input$training_range) & date >= max(input$training_range)) %>% 
    #        select(-date)
    #    colnames(pca_data) <- paste0("T", 1:maturities_included)
    #    yield_mean <- colMeans(pca_data)
    #    yield_matrix_centered <- sweep(pca_data, 2, yield_mean, "-")
    #})
    
    # PCA Fitting
    #pca_result <- prcomp(yield_matrix_centered, center = FALSE, scale. = TRUE)
    #PCs <- pca_result$rotation
    
    #pc_historical <- load_to_pc(yield_matrix_centered, PCs)
    #pc_deltas_historical <- deltas(pc_historical)
    #pc_deltas_sd <- pc_deltas_historical %>% 
    #    dplyr::summarize(dplyr::across(dplyr::any_of(colnames(pc_deltas_historical)),sd))
    
    # pct_deltas: Will provide some dampening with respect to high + low level, but won't gaurentee shifts to cause non_negative yields across the board.
    # pc_pct_deltas_historical <- deltas(pc_historical)
    
    
}
