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
library(shinyalert)

# Define server logic required to draw a histograms
server <- function(input, output, session) {

# Input Rendering
    
    max_date <- max(treasury_data_server$date)
    min_date <- min(treasury_data_server$date)
    maturities_included <- ncol(treasury_data_server) - 1
    
    output$delta_lag_ui <- renderUI({
      shiny::numericInput("delta_lag", "Risk Time Frame (Trading Days)", value = 1, min = 1, max = 1000)
    })
    
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
    
    observeEvent(input$training_range,{
      
      min_window <- min(input$training_range)
      max_window <- min(input$training_range)
      max_delta <- as.numeric(max_window - min_window)-50
      
      output$delta_lag_ui <- renderUI({
        shiny::numericInput("delta_lag", "Risk Time Frame (Trading Days)", value = 1, min = 1, max = max(1,max_delta))
      })
    })
    
    maturities <- colnames(treasury_data_server %>% dplyr::select(-date)) %>% as.numeric()
    app_state <- shiny::reactiveValues()
    
    # Data and Model Updates
    
    pc_deltas_sd <- NULL
    app_state$bonds <- list()
    app_state$init <- NULL
    
    
    
    output$stressed_curve_scalar_value <- renderUI({
      value_box(title = "Test Case:",
                value = paste0("$ ",0))})
    
    output$current_curve_scalar_value <- renderUI({
      value_box(title = "Current Value:",
                value = paste0("$ ",0))})
    
    output$pct_chg <- renderUI({
      value_box(title = "Percent Change:",
                value = paste0(0, "%"))})
    
    observeEvent(list(input$training_range, input$delta_lag),{
        
        req(input$training_range)
        req(input$delta_lag)
        
        # print("Model Training")
        
        # print(input$training_range)
        
        if(input$delta_lag < as.numeric(max(input$training_range) - min(input$training_range))){
          
          output$warn_lag <- renderText("")
        
        pca_data <<- treasury_data_server %>% 
            filter(date >= min(input$training_range) & date <= max(input$training_range)) %>% 
            select(-date)
        yield_mean <- colMeans(pca_data)
        yield_matrix_centered <- sweep(pca_data, 2, yield_mean, "-")
        
        # print("Prior to PCA train")
        # print(str(pca_data))
        
        pca_result <- prcomp(yield_matrix_centered, center = FALSE, scale. = TRUE)
        PCs <<- pca_result$rotation
        
        app_state$rot_matrix <- PCs
        
        # saveRDS(PCs, "PCs.rds")
        
        pc_historical <<- load_to_pc(yield_matrix_centered, PCs)
        pc_deltas_historical <<- deltas(pc_historical, input$delta_lag)
        
        app_state$delta_historical <- pc_deltas_historical
        # print(app_state$delta_historical)
        
        # saveRDS(pc_deltas_historical, "deltas.rds")
        
        pc_deltas_sd <<- pc_deltas_historical %>% 
            dplyr::summarize(dplyr::across(dplyr::any_of(colnames(pc_deltas_historical)),sd))
        
        if(is.null(app_state$init)){
            app_state$init <- 0
        }else{
            app_state$init <- app_state$init + 1
        }
        
        }else{
          output$warn_lag <- renderText("Lag exceeds length of training window. Please shorten lag or lengthen window!")
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
        app_state$current_yield <- current_yield
        
        # saveRDS(current_yield, "yield_selected.rds")
   
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
               theme_minimal() +
            theme(legend.position = "none")})
        
        # print(df)
        h_spline_stressed <- fit_h_spline(x = as.numeric(df$Term), y = as.numeric(df$Stressed), missing = 0:360)
        
        boot_df_stressed <- h_spline_stressed %>% 
            data.frame(term = as.numeric(names(.)), yield = .) %>% 
            dplyr::mutate(
              date_in = input$selected_yield) %>% 
            ai_df_cpp() %>% 
          dplyr::mutate(iter = 1) %>% 
            prep_ai_for_bs() %>% 
            bootstrap_cpp()
        
        app_state$boot_stressed <- boot_df_stressed
        
        h_spline_current <- fit_h_spline(x = as.numeric(df$Term), y = as.numeric(df$Base), missing = 0:360)
        
        boot_df_current <- h_spline_current %>% 
          data.frame(term = as.numeric(names(.)), yield = .) %>% 
          dplyr::mutate(
            date_in = input$selected_yield) %>% 
          ai_df_cpp() %>% 
          dplyr::mutate(iter = 1) %>% 
          prep_ai_for_bs() %>% 
          bootstrap_cpp()
        
        app_state$boot_current <- boot_df_current
        
        
        
        #output$boot_dt <- renderDT({
        #    boot_df
        #})
        
        mono <- boot_df_stressed %>% dplyr::mutate(x = 1, x = cumsum(x)) %>% 
            dplyr::arrange(desc(dcf)) %>% 
            dplyr::mutate(x = x - dplyr::lag(x)) %>% 
            dplyr::filter(x != 1) %>% 
            dplyr::pull(x)
        
        output$mono <- renderText({ifelse(length(mono) > 0, "Zero Curve Is NOT Monotonic!", "Zero Curve Abides Monotonicity")})
        
    })
    
    #Reactive for bonds
    num_bonds <- reactiveVal(1) #start with one
    
    observeEvent(input$addBond, {
      for (i in num_bonds()) {
        app_state$bonds[[paste0("face_value_", i)]] <- input[[paste0("face_value_", i)]]
        app_state$bonds[[paste0("coupon_rate_", i)]] <- input[[paste0("coupon_rate_", i)]]
        app_state$bonds[[paste0("maturity_date_", i)]] <- input[[paste0("maturity_date_", i)]]
      }
      num_bonds(num_bonds() + 1)
    })
    
    observeEvent(input$subBond, {
      if (num_bonds() > 1) {
        for (i in 1:num_bonds()) {
          app_state$bonds[[paste0("face_value_", i)]] <- input[[paste0("face_value_", i)]]
          app_state$bonds[[paste0("coupon_rate_", i)]] <- input[[paste0("coupon_rate_", i)]]
          app_state$bonds[[paste0("maturity_date_", i)]] <- input[[paste0("maturity_date_", i)]]
        }
        num_bonds(max((num_bonds() - 1), 1))
      }
    })
    
    
    
    
    # Change UI as we add bonds
    
    output$bond_inputs <- renderUI({
      lapply(1:num_bonds(), function(i) {
        card(
          card_header(paste0("Bond ", i)),
            fluidRow(
              column(4, numericInput(
                inputId = paste0("face_value_", i),
                label = "Face Value",
                value = ifelse(!is.null(app_state$bonds[[paste0("face_value_", i)]]),
                               app_state$bonds[[paste0("face_value_", i)]],
                               NA)
              )),
            column(4, 
                   numericInput(paste0("coupon_rate_", i), "Coupon Rate", value = ifelse(!is.null(app_state$bonds[[paste0("coupon_rate_", i)]]),
                                                                                         app_state$bonds[[paste0("coupon_rate_", i)]],
                                                                                         NA)
                                )),
            column(4, dateInput(paste0("maturity_date_", i), "Maturity Date", value = ifelse(!is.null(app_state$bonds[[paste0("maturity_date_", i)]]),
                                                                                             app_state$bonds[[paste0("maturity_date_", i)]],
                                                                                             Sys.Date() + 365)
                                ))
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

      # print(bond_data())
      
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
      
      # print(bond_data_in)
      
      if(nrow(bond_data_in > 0)){
        
        req(app_state$boot_stressed)
        
        date_in <- Sys.Date()
        
        cf_bonds <- bond_data_in %>% cf_schedule(date_in) %>% 
          dplyr::group_by(date) %>% 
          dplyr::summarise(cf = sum(cf), .groups = "keep") %>% 
          dplyr::mutate(dtm = as.numeric(date - date_in)) %>% 
          dplyr::filter(date > date_in)
        
        stressed_pv <- interpolate_and_price(app_state$boot_stressed, cf_bonds)
        current_pv <- interpolate_and_price(app_state$boot_current, cf_bonds)

        output$stressed_curve_scalar_value <- renderUI({
          value_box(title = "Test Case:",
                    value = paste0("$ ",round(sum(stressed_pv$pv), 2)))})
        
        app_state$current_pv_var_ref <- sum(current_pv$pv)
        
        output$current_curve_scalar_value <- renderUI({
          value_box(title = "Current Value:",
                    value = paste0("$ ",round(sum(current_pv$pv), 2)))})
        
        output$pct_chg <- renderUI({
          value_box(title = "Percent Change:",
                    value = paste0(round(
                      100*(sum(stressed_pv$pv) - sum(current_pv$pv))/sum(current_pv$pv), 2), "%"))})
            
          df <- dplyr::left_join(
          current_pv %>% dplyr::transmute(date, dtm, cf, pv_1 = pv),
          stressed_pv %>% dplyr::transmute(dtm, pv_2 = pv),
          by = "dtm"
        ) %>% 
          dplyr::mutate(d_pv = (pv_2 - pv_1)/pv_1) %>% 
          dplyr::mutate(weight = (pv_2 - pv_1)/sum(pv_1)) %>% 
          dplyr::select(date, cf, pv_1, d_pv, weight)
          
          
          min_date_ggplot <- min(df$date)
          max_date_ggplot <- max(df$date)
          
          ggplot_x_range <- as.numeric(max_date_ggplot - min_date_ggplot)
          gg_steps <- 100
          width_val <-  ceiling((ggplot_x_range / gg_steps)/2)*2-1
          
          adj_term <- (width_val - as.numeric(max_date_ggplot - min_date_ggplot) %% width_val)
          
          # adj_term
          
          df <- seq(from = min_date_ggplot, 
              to = max_date_ggplot + adj_term, 
              by = "day") %>% 
            data.frame(date = .) %>% 
            dplyr::full_join(df, by = "date") %>% 
            dplyr::mutate(temp = as.numeric(date - min(date)) %% width_val,
                          change = ifelse(temp == 0, 1, 0),
                          group_var = cumsum(change)) %>% 
            dplyr::group_by(group_var) %>% 
            dplyr::mutate(date = median(date)) %>% 
            tidyr::drop_na() %>% 
            dplyr::summarise(cf = sum(cf),
                             pv_1 = sum(pv_1),
                             d_pv = sum(d_pv),
                             weight = sum(weight),
                             .groups = "keep",
                             date = first(date)) %>% 
            dplyr::ungroup() %>% 
            dplyr::select(-group_var)
          
          print(df)
          
      output$multi_plot <- 
            
        renderPlot({
          df %>% 
            tidyr::pivot_longer(-date) %>% 
            dplyr::mutate(name = dplyr::case_when(
              name == "cf" ~ "Cash Flow",
              name == "pv_1" ~ "Present Value",
              name == "d_pv" ~ "Percent Change in PV",
              name == "weight" ~ "Relative Weighting"
            ),
            name = factor(name, levels = c("Cash Flow", "Present Value", "Percent Change in PV", "Relative Weighting"))) %>% 
          ggplot() +
          geom_col(aes(x = date, y = value), width = width_val*0.90*(Sys.Date() - Sys.Date()+1)) +
          facet_grid(name~., scales = "free_y") +
          labs(x = "Date", y = "Value")
            
          })
        
        app_state$cf_schedule <- cf_bonds
        
        # print(sum(stressed_pv$pv))
        
      }
      
    })
    
    observeEvent(input$run_var, {
      
    shinyalert(title = "Loading Value at Risk", text = "VAR is running! Please avoid spamming additional inputs, as they will queue and execute once VAR is complete. Processing most sample sizes takes around 30 seconds.")
      
      # Checks to ensure valid inputs exist
    reference_df <-  bond_data() %>% tidyr::drop_na()
    
    if(nrow(!reference_df < nrow(bond_data()))){
      
    y_in <- app_state$current_yield
    r_in <- app_state$rot_matrix
    d_in <- app_state$delta_historical
    s_size <- input$s_size
    cfs_in <- app_state$cf_schedule
    
    var_set <- pca_sample_yields(y_in, d_in, r_in, s_size) %>% 
      bootstrap_cpp() %>% 
      interpolate_boot_var(cfs_in)
    
    output$var_plot <- 
      renderPlot({
      var_set %>% 
      ggplot() +
      geom_histogram(aes(x = pv, y = ..count../sum(..count..)), bins = 100) +
      labs(x = "PV Simulated", y = "Density")
      })
    
    var_price <- var_set$pv %>% quantile(probs = 0.05)
    
    output$var5 <- renderUI({
      value_box(title = "VaR at 5% | Price:",
                value = paste0("$ ",round(var_price, 2)))})
    
    var_pct_risk <- (var_price - app_state$current_pv_var_ref)/app_state$current_pv_var_ref
    
    output$var5pct <- renderUI({
      value_box(title = "VaR at 5% | % Change:",
                value = paste0(round(var_pct_risk * 100, 2), " %"))})
    
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