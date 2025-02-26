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

# Define UI for application that draws a histogram
bslib::page_navbar(
    title = "Yield Curve Scenario Analysis",
    bslib::layout_sidebar(
        sidebar = sidebar(
            
            card(card_header("Model"),
                 uiOutput("training_window_dates")),
            
            card(card_header("Yield"),
            dateInput("selected_yield", "Select Yield Curve Date:", min = as.Date(ui_date_min), max = as.Date(ui_date_max), value = Sys.Date()),
            actionButton("max_date", "Reset to Most Recent")),
            
            card(card_header("Scenario, (Z Score)"),
            sliderInput("parallel_shift", "Level Factor",
                        min = -4, max = 4, value = 0, step = 0.01),
            sliderInput("steepening", "Steepening Factor",
                        min = -4, max = 4, value = 0, step = 0.01),
            sliderInput("curvature", "Curvature Factor",
                        min = -4, max = 4, value = 0, step = 0.01)),
            width = 800
        ),
            plotOutput("yield_curve_plot"),
            plotOutput("pc_risk_plot"),
            plotOutput("pc_pct_risk_plot"),
            DT::DTOutput("boot_dt")
        )
    )
