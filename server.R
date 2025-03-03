#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(shinydashboard)


# Define server logic required to draw a histogram
server <- function(input, output, session) {

# Input Rendering
    
    max_date <- max(treasury_data_server$date)
    min_date <- min(treasury_data_server$date)
    maturities_included <- ncol(treasury_data_server) - 1
    # print(head(treasury_data_server))
    # print(str(treasury_data_server))
    
    # print(min_date)
    # print(max_date)
    
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
    
    output$stressed_curve_scalar_value <- renderUI({
      value_box(title = "Test Case:",
                value = 0)})
    
    observeEvent(list(input$training_range, input$delta_lag),{
        
        req(input$training_range)
        req(input$delta_lag)
        
        # print("Model Training")
        
        # print(input$training_range)
        
        pca_data <<- treasury_data_server %>% 
            filter(date >= min(input$training_range) & date <= max(input$training_range)) %>% 
            select(-date)
        yield_mean <- colMeans(pca_data)
        yield_matrix_centered <- sweep(pca_data, 2, yield_mean, "-")
        
        # print("Prior to PCA train")
        # print(str(pca_data))
        
        pca_result <- prcomp(yield_matrix_centered, center = FALSE, scale. = TRUE)
        PCs <<- pca_result$rotation
        saveRDS(PCs, "PCs.rds")
        
        pc_historical <<- load_to_pc(yield_matrix_centered, PCs)
        pc_deltas_historical <<- deltas(pc_historical, input$delta_lag)
        saveRDS(pc_deltas_historical, "deltas.rds")
        
        pc_deltas_sd <<- pc_deltas_historical %>% 
            dplyr::summarize(dplyr::across(dplyr::any_of(colnames(pc_deltas_historical)),sd))
        
        if(is.null(app_state$init)){
            app_state$init <- 0
        }else{
            app_state$init <- app_state$init + 1
        }
        
        # print("Done modelling")
    })
    
    observeEvent(list(input$selected_yield, input$parallel_shift, input$steepening, input$curvature, app_state$init), {
        # print("Running Yield Curve Plot")
        req(pc_deltas_sd)
        # print(input$selected_yield)
        
        pcs <- c(input$parallel_shift*pc_deltas_sd$PC1, input$steepening*pc_deltas_sd$PC2, input$curvature*pc_deltas_sd$PC3)
        pcs <- c(pcs, rep(0, ncol(PCs) - length(pcs)))
        pcs <- t(as.matrix(pcs))
        
        stress <- unload_pc(pcs, PCs) %>% as.vector() %>% unlist()
        current_yield <- treasury_data_server %>% filter(date <= input$selected_yield) %>% filter(date == max(date)) %>% select(-date)
        saveRDS(current_yield, "yield_selected.rds")
   
        stressed_curve <- as.numeric(current_yield) + stress
        
        for(i in 1:length(stressed_curve)){
            stressed_curve[i] <- max(stressed_curve[i],0)
        }
        
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
        
        # print(term_factor_levels)
        
        df_viz <- df %>% dplyr::mutate(Term = factor(as.character(Term), levels = term_factor_levels))
        
        output$yield_curve_plot <- renderPlot({ggplot(df_viz, aes(x = Term)) +
               geom_line(aes(y = Base, color = "Base Curve"), size = 1, group = 1) +
               geom_line(aes(y = Stressed, color = "Stressed Curve"), size = 1, group = 1, linetype=3) +
               labs(title = "Yield Curve Stress Testing", x = "Term (Months)", y = "Yield (%)") +
               # scale_x_continuous(breaks = df$TermNum, labels = df$Term) +
               scale_color_manual(values = c("Base Curve" = "black", "Stressed Curve" = "red")) +
               theme_minimal()})
        
        # print(df)
        h_spline <- fit_h_spline(x = as.numeric(df$Term), y = as.numeric(df$Stressed), missing = 0:360)
        saveRDS(h_spline, "yield_curve.rds")
        
        boot_df <- h_spline %>% 
            data.frame(term = as.numeric(names(.)), yield = .) %>% 
            #dplyr::rowwise() %>% 
            #dplyr::mutate(yield = max(yield, 0)) %>% 
            #dplyr::ungroup() %>% 
            # ai_from_df(date_in = input$selected_yield) %>% 
            dplyr::mutate(
              date_in = input$selected_yield) %>% 
            #  date_in = as_date(input_date)) %>%
            ai_df_cpp() %>% 
          dplyr::mutate(iter = 1) %>% 
            prep_ai_for_bs() %>% 
            bootstrap_cpp()
        
        app_state$boot_stressed <- boot_df
        
        output$boot_dt <- renderDT({
            boot_df
        })
        
        saveRDS(boot_df, "Final Pricing Startpoint/boot.rds")
        
        mono <- boot_df %>% dplyr::mutate(x = 1, x = cumsum(x)) %>% 
            dplyr::arrange(desc(dcf)) %>% 
            dplyr::mutate(x = x - dplyr::lag(x)) %>% 
            dplyr::filter(x != 1) %>% 
            dplyr::pull(x)
        
        output$mono <- renderText({ifelse(length(mono) > 0, "Zero Curve Is NOT Monotonic!", "Zero Curve Abides Monotonicity")})
        
    })
    
    #Reactive for bonds
    num_bonds <- reactiveVal(1) #start with one
    
    observeEvent(input$addBond, {
      num_bonds(num_bonds() + 1)
    })
    
    observeEvent(input$subBond, {
      num_bonds(max(num_bonds() - 1, 1))
    })
    
    
    # Change UI as we add bonds
    
    output$bond_inputs <- renderUI({
      lapply(1:num_bonds(), function(i) {
        card(
          card_header(paste0("Bond ", i)),
          fluidRow(
            column(4, numericInput(paste0("face_value_", i), "Face Value", value = NA)),
            column(4, numericInput(paste0("coupon_rate_", i), "Coupon Rate", value = NA)),
            column(4, dateInput(paste0("maturity_date_", i), "Maturity Date", value = Sys.Date() + 365))
          )
        )
      })
    })
    
    # Portfolio value
    
    bond_data <- reactive(
      
      {
      
      req(num_bonds())
      
      tibble::tibble(
        bond_id = paste0("Bond ", 1:num_bonds()),
        face_value = sapply(1:num_bonds(), function(i) input[[paste0("face_value_", i)]]),
        coupon_rate = sapply(1:num_bonds(), function(i) input[[paste0("coupon_rate_", i)]]),
        maturity_date = sapply(1:num_bonds(), function(i) as.character(input[[paste0("maturity_date_", i)]]))
       )
      
      }
      
    )
    
    observeEvent(list(bond_data(), app_state$boot_stressed), {

      print(bond_data())
      
      today <- Sys.Date()
      
      # print(str(bond_data()$face_value[1]))
      # print(str(bond_data()$coupon_rate[1]))
      
      # print(head(bond_data()))
      # print(today)
      # print(head(boot))
      
      # print("filtering")
      
      saveRDS(bond_data(), "bond_inputs_2.rds")
      
      bond_data_in <- bond_data() %>% 
        tidyr::unnest(face_value) %>% 
        tidyr::unnest(coupon_rate) %>% 
        tidyr::unnest(maturity_date) %>% 
        dplyr::filter(
          !is.na(face_value) & !is.na(coupon_rate) & !is.na(maturity_date)
          )
      
      print(bond_data_in)
      
      if(nrow(bond_data_in > 0)){
        
        req(app_state$boot_stressed)
        
        date_in <- Sys.Date()
        
        cf_bonds <- bond_data_in %>% cf_schedule(date_in) %>% 
          dplyr::group_by(date) %>% 
          dplyr::summarise(cf = sum(cf), .groups = "keep") %>% 
          dplyr::mutate(dtm = as.numeric(date - date_in)) %>% 
          dplyr::filter(date > date_in)
        
        stressed_pv <- interpolate_and_price(app_state$boot_stressed, cf_bonds)

        output$stressed_curve_scalar_value <- renderUI({
          value_box(title = "Test Case:",
                    value = round(sum(stressed_pv$pv), 2))})
          
        print(sum(stressed_pv$pv))
        
        # Interpolate zero curve (s)
        
        # Price
        
        # saveRDS(bond_data_in, "bond_inputs.rds")
        # saveRDS(cf_bonds, "bond_schedule.rds")
        
        #inter_bootstrap <- interpolation_function(cf_schedule, zero_curve)
        
        #pv1 <- cf_schedule %>% pv_function(bootstrap_df_normal)
        #pv2 <- cf_schedule %>% pv_function(bootstrap_df_stressed)
        
        #output$output_price <- renderText(sum_function(pv1))
        #sum_function(pv2)
        
        #result <- price_portfolio(bond_data(), today, boot)
        #portfolio_value <- result$portfolio_value
        #formatted_value <- scales::dollar(portfolio_value)
        #output$stressed_curve_scalar_value <- renderText(
        #  {formatted_value}
        
        #)
      }
      
    })
    
    # IMPORTANT CODE BELOW
    
    #output$portfolio_value_box <- shinydashboard::renderValueBox({
    #  req(num_bonds() > 0)
    #  # sapply dynamically creates multiple inputs for each of the face values, coupon rates, and maturity dates
    #  bond_data <<- tibble::tibble(
    #    bond_id = paste0("Bond ", 1:num_bonds()),
    #    face_value = sapply(1:num_bonds(), function(i) input[[paste0("face_value_", i)]]),
    #    coupon_rate = sapply(1:num_bonds(), function(i) input[[paste0("coupon_rate_", i)]]),
    #    maturity_date = sapply(1:num_bonds(), function(i) as.character(input[[paste0("maturity_date_", i)]]))
    #  )
    #  
    #  today <- Sys.Date()
    #  
    #  # boot <- read_rds("boot.rds")
    #  boot <-  app_state$boot_stressed
    #  
    #  result <- price_portfolio(bond_data, today, boot)
    #  
    #  # print(result$bond_data)
    #  
    #  portfolio_value <- result$portfolio_value
    #  
    #  formatted_value <- scales::dollar(portfolio_value)
    #  
    #  
    #  shinydashboard::valueBox(
    #    value = formatted_value,
    #    subtitle = "Portfolio Value",
    #    icon = icon("chart-line")
    #  )
    #})
    
    # IMPORTANT CODE ABOVE
    
    
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




# observeEvent(input$addBond, {
#   output$bondB_card <- renderUI({
#     card(
#       card_header("Bond B data"),
#       fluidRow(
#         column(
#           4,
#       numericInput(
#         "BondB_face_value",
#         "Bond B Face Value",
#         value = input$bondA_face_value
#       )
#       ),
#       column(4,
#       numericInput(
#         "bondB_coupon_rate",
#         "Bond B Coupon Rate (%)",
#         value = input$bondA_coupon_rate
#       )
#       ),
#       column(4,
#       dateInput(
#         "bondB_maturity_date",
#         "Bond B Maturity date",
#         value = input$bondA_maturity_date
#       )
#       )
#     )
#     )
#   })
# })