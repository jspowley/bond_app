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
  title = "Portfolio Management Dashboard",
  theme = bs_theme(bootswatch = "flatly"),
  
  nav_panel(
    title = "About",
    fluidPage(
      div(style = "position: relative;",
          h2(tags$u("Welcome to Fixed Income Portfolio Management")),
          p("This dashboard is designed to help investors and portfolio managers analyze portfolio performance, assess risk, and optimize investment strategies."),
          
          h3("Features"),
          div(style = "overflow: hidden;",
              tags$ul(
                tags$li("Historical Yield Curves"),
                tags$li("Scenario Analysis & Monotonicty Indicator"),
                tags$li("Bond inputs for your portfolio"),
                tags$li("VaR simulation based on current/hypothetical portfolio")
              ),
              
              tags$img(
                src = "bull2.jpg", 
                width = "80%", 
                style = "position: absolute; top: 100px; right: 0; max-width: 800px;"
              )
          )
      ),
      
      
      
      h4("Historical Yield Curves"),
      p("Within the 'yield' panel, select the date of choice and the visual will update to that day's yield curve"),
      
      h4("Scenario Analysis & Monotonicty Indicator"),
      p("Adjust the training window to a period of time that you expect to be the most similar to your trading narrative"),
      p(tags$span(style = "font-size: 13px; margin-left: 20px;", "i.e.) ~2008 if you expect a similar market")),
      p("Adjust risk time frame to the number of days forward that you expect your inputs to apply for"),
      p("Adjust the level, steepening, and curvature factors as needed. Changes are reflected in the plotted chart"),
      p(tags$span(style = "font-size: 13px; margin-left: 20px;", "Level: Adjusts yield curve up/down")),
      p(tags$span(style = "font-size: 13px; margin-left: 20px;", "Steepening: moves short-term rates up/down relative to long-term rates")),
      p(tags$span(style = "font-size: 13px; margin-left: 20px;", "Curvature: moves mid-term rates up/down relative  to short and long-term rates")),
      p("Monotonicity is stated for the stressed curve; trading strategies can be created based upon this observation"),
      
      h4("Portfolio Planning & Value at Risk (VaR)"),
      p("Insert your current or a hypothetical portfolio using the bonds section, then hit 'Run Value at Risk' and wait for the simulation to compute"),
      
      
      div(style = "position: absolute; bottom: 0; width: 100%; text-align: center; padding: 10px;",
          HTML(
            '<a href="https://www.linkedin.com/in/powleyjustin/" target="_blank" text-decoration: none;">Justin Powley</a>
                |
                <a href="https://www.linkedin.com/in/travis-nowak-072010147/" target="_blank" text-decoration: none;">Travis Nowak</a>')
      ),
    )
  ),
  
  nav_panel(
    title = "Methodology",
    fluidPage(
      div(style = "overflow: hidden;",
          h2(tags$u("Under the Hood")),
          p("For the purpose of deeper understanding, this section details the data sources, modeling techniques, and calculations driving the outputs"),
          
          tags$img(
            src = "hood.png", 
            width = "75%", 
            style = "position: absolute; top: 100px; right: 400px; max-width: 300px;"
          )
      ),
      
      h3("Data Preparation"),
      p(
        "Yield curve data is based upon the par yield curve and has been webscraped from ",
        a(
          "the U.S. Department of Treasury", 
          href = "https://home.treasury.gov/policy-issues/financing-the-government/interest-rate-statistics", 
          target = "_blank"
        ),
        p(" and is stored then continually updated for quick lookups and calculations."),
        
        
        h3("Scenario Analysis Using Principle Components"),
        p("Many yield curve variations can be captured by just a few 'principal components,' commonly interpreted as level, steepening, and curvature."),
        p("By applying PCA to historical yield-curve data, this dashboard identifies these factors and allows the PM to stress-test the curve by shifting it up/down, tilting it, or bending it."),
        p("These sliders correspond to Z-score adjustments of the three components, allowing the PM to create user-defined yield curves for hypothetical market conditions and examine their exposure to those conditions."),
        p("After shifting yields, the model uses spline interpolation to produce a smooth, continuous curve. It then checks if the resulting curve remains monotonic or not, giving the PM feedback"),
        p("on whether their assumptions are consistent with typical yield curve shapes."),
        
        h3("Bond Cash Flow Modeling and Portfolio Pricing"),
        p("The app creates a schedule of coupon and principal payments for each bond, stepping back in 6 month intervals from the maturity date."),
        p("These cashflows are then discounted by the app's internally calculated zero curve to value the portfolio."),
        
        h3("Value at Risk (VaR) Simulation"),
        p("In the sidebar, you choose how many trading days ahead you want to assess risk and the number of simulations to run. The app then randomly simulates changes in the yield curve over your defined period based upon"),
        p("the training data you have selected, creating a distribution of gains/losses and returns the loss threshold that has only a 5% chance of being exceeded."),
        
      ),
      
      div(style = "position: absolute; bottom: 0; width: 100%; text-align: center; padding: 10px;",
          HTML(
            '<a href="https://www.linkedin.com/in/powleyjustin/" target="_blank" text-decoration: none;">Justin Powley</a>
                |
                <a href="https://www.linkedin.com/in/travis-nowak-072010147/" target="_blank" text-decoration: none;">Travis Nowak</a>')
      ),
      
    ),
    
    
  ),
  
  nav_panel(title = "Dashboard",
            bslib::layout_sidebar(
              sidebar = sidebar(
                
                card(card_header("Model"),
                     uiOutput("training_window_dates"),
                     
                     # Max needs to be capped
                     shiny::uiOutput("delta_lag_ui"),
                     shiny::textOutput("warn_lag")),
                     # shiny::numericInput("delta_lag", "Risk Time Frame (Trading Days)", min = 1, max = 3650, value = 1)),
                
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
