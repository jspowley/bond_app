#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(bslib)
library(lubridate)
library(shinydashboard)

# Define UI for application that draws a histogram
ui <- bslib::page_navbar(
    title = "Yield Curve Scenario Analysis",
    theme = bs_theme(bootswatch = "flatly"),

    nav_panel(
        title = "Tab 1",
        fluidPage(
            h2("Welcome"),
            p("Insert content here for tab 1.")
        )
    ),

    nav_panel(title = "Tab 2",
    bslib::layout_sidebar(
        sidebar = sidebar(
            
            card(card_header("Model"),
                 uiOutput("training_window_dates"),
                 shiny::numericInput("delta_lag", "Risk Time Frame (Trading Days)", min = 1, max = 3650, value = 1)),
            
            card(card_header("Yield"),
            dateInput("selected_yield", "Select Yield Curve Date:", min = as.Date(ui_date_min), max = as.Date(ui_date_max), value = ui_date_max),
            actionButton("max_date", "Reset to Most Recent")),
            
            card(card_header("Scenario, (Z Score)"),
            sliderInput("parallel_shift", "Level Factor",
                        min = -4, max = 4, value = 0, step = 0.05),
            sliderInput("steepening", "Steepening Factor",
                        min = -4, max = 4, value = 0, step = 0.05),
            sliderInput("curvature", "Curvature Factor",
                        min = -4, max = 4, value = 0, step = 0.05)),
            textOutput("mono"),
            width = 800,
            
            actionButton("addBond", "Add Bond"),
            actionButton("subBond", "Remove Bond"),
            uiOutput("bond_inputs"),
            bslib::card(bslib::card_header("Value at Risk"),
                shiny::numericInput("s_size", "Sample Size", value = 10000),
                shiny::actionButton("run_var", "Run Value at Risk")
            ),
            bslib::card(bslib::card_header("VAR Output"),
            shiny::plotOutput("var_plot"),
            bslib::value_box(title = "5% VAR", value = textOutput("var5")),
            full_screen = TRUE
            )
            # Reactive Counter To Increase/Decrease Bonds
              #https://www.youtube.com/watch?v=ML54auObmL8
        ),
        
        bslib::layout_columns(
        
        bslib::card(plotOutput("yield_curve_plot")),
            
        uiOutput("current_curve_scalar_value"),
        uiOutput("stressed_curve_scalar_value"),
        uiOutput("pct_chg"),
        
        bslib::card(
            plotOutput("multi_plot"),
            full_screen = TRUE
        ),
        
        col_widths = c(12,4,4,4,12)
        )
            
    )
    )
)

    
    
    
      
    
                
              


