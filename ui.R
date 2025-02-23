#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

# Define UI for application that draws a histogram
fluidPage(
    titlePanel("Yield Curve Scenario Analysis"),
    sidebarLayout(
        sidebarPanel(
            sliderInput("parallel_shift", "Level Factor",
                        min = -200, max = 200, value = 0, step = 10),
            sliderInput("steepening", "Steepening Factor",
                        min = -1, max = 1, value = 0, step = 0.1),
            sliderInput("curvature", "Curvature Factor",
                        min = -1, max = 1, value = 0, step = 0.1)
        ),
        mainPanel(
            plotOutput("yield_curve_plot"),
            plotOutput("pc_risk_plot")
        )
    )
)
