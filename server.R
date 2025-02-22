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

    # Training Period
    
    # Data prep
    pca_data <- treasury_data %>% select(-date)
    colnames(pca_data) <- paste0("T", 1:10)
    yield_mean <- colMeans(pca_data)
    yield_matrix_centered <- sweep(pca_data, 2, yield_mean, "-")
    
    # PCA Fitting
    pca_result <- prcomp(yield_matrix_centered, center = FALSE, scale. = TRUE)
    PCs <- pca_result$rotation
    
    pc_historical <- load_to_pc(yield_matrix_centered, PCs)
    pc_deltas_historical <- deltas(pc_historical)
    
    # Today versus stressed curve
    output$yield_curve_plot <- renderPlot({
        #This is the interactivity
        
        pcs <- c((input$parallel_shift / 100), input$steepening, input$curvature)
        pcs <- c(pcs, rep(0, ncol(PCs) - length(pcs)))
        pcs <- t(as.matrix(pcs))
        
        stress <- unload_pc(pcs, PCs) %>% as.vector() %>% unlist()
        
        current_yield <- as.numeric(tail(pca_data, 1))
        stressed_curve <- current_yield + stress
        
        df <- data.frame(
            Term = colnames(pca_data),
            Base = current_yield,
            Stressed = stressed_curve
        )
        
        #Names to numeric for purpose of ordering x-axis
        df$TermNum <- as.numeric(gsub("T", "", df$Term))
        
        ggplot(df, aes(x = TermNum)) +
            geom_line(aes(y = Base, color = "Base Curve"), size = 1, group = 1) +
            geom_line(aes(y = Stressed, color = "Stressed Curve"), size = 1, group = 1) +
            labs(title = "Yield Curve Stress Testing", x = "Term", y = "Yield (%)") +
            scale_x_continuous(breaks = df$TermNum, labels = df$Term) +
            scale_color_manual(values = c("Base Curve" = "blue", "Stressed Curve" = "red")) +
            theme_minimal()
    })
    
    # Distribution of PCA factor deltas
    
    
    
}
