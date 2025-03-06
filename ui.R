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
                tags$li("Robust PCA Risk Engine: Evaluate risk by time horizon and historical training period."),
                tags$li("Structural Scenario Analysis: View in realtime changes in level, slope and curvature. View Cashflows, Present Value, Percent Change by Cashflow and Relative Weighting on Total Percent Change."),
                tags$li("VaR Simulation: Using PCA to decorrelate risk drivers, to avoid overstating/understating volatility by statistical shortcomings.")
              ),
              
              tags$img(
                src = "bull2.jpg", 
                width = "90%", 
                style = "position: absolute; top: 100px; right: 0; max-width: 800px;  z-index: -1; opacity: 0.5;"
              )
          )
      ),
      
      
      
      h4("Historical Yield Curves"),
      p("Within the 'yield' panel, select the date of choice and the visual will update to that day's yield curve."),
      
      h4("Scenario Analysis"),
      p("Adjust the training window to a period of time that you expect to be the most similar to your trading narrative:"),
      p(tags$span(style = "font-size: 13px; margin-left: 20px;", "i.e.) ~2008 if you expect a similar market")),
      p("Adjust risk time frame to the number of days forward that you expect your inputs to apply for."),
      p("Adjust the level, steepening, and curvature factors as needed. Changes are reflected in the plotted chart."),
      p(tags$span(style = "font-size: 13px; margin-left: 20px;", "Level: Adjusts yield curve up/down.")),
      p(tags$span(style = "font-size: 13px; margin-left: 20px;", "Steepening: moves short-term rates up/down relative to long-term rates.")),
      p(tags$span(style = "font-size: 13px; margin-left: 20px;", "Curvature: moves mid-term rates up/down relative  to short and long-term rates.")),
      p("View Cash Flows, Present Values, and Percent Changes and Relative Weighting when under the scenario conditions relative to today."),
      
      h4("Portfolio Planning & Value at Risk (VaR)"),
      p("Insert your current or a hypothetical portfolio using the bonds section, time horizon, and training window; then hit 'Run Value at Risk' and wait for the simulation to compute."),
      
      div(style = "bottom: 0; width: 100%; text-align: center; padding: 10px;",
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
          p("For the purpose of deeper understanding;"),
          p("This section details the data sources, modeling techniques, and calculations used for driving the outputs."),
          
          tags$img(
            src = "hood.png", 
            width = "75%", 
            style = "position: absolute; top: 100px; right: 400px; max-width: 300px;"
          )
      ),
      
      bslib::accordion(
        
        open = FALSE,
        
        bslib::accordion_panel(title = "Data Preparation",
      p("Yield curve data is based upon the par yield curve and has been webscraped from ",
        a(
          "the U.S. Department of Treasury", 
          href = "https://home.treasury.gov/policy-issues/financing-the-government/interest-rate-statistics", 
          target = "_blank"
        ),
        " and is stored then continually updated for quick lookups and calculations."),
      
        p("SOFR and predecessor overnight rates are used to estimate 1 day lending rates for front month interpolation of the yield curve. These rates are regression adjusted to mitigate counterparty risks."),
        ),
      
      bslib::accordion_panel(title = "Bond Cash Flow Modeling and Portfolio Pricing",
      p("Starting from the yield curve data (sofr included), hermetic spline interpolation to montoncially fill older data between knots. PCA requires no NA values,so this interpolation is necessary to use backdata to 1990 without artificially limiting ourselves from using all avaialable yield data today."),
      p("From there, a PCA matrix is trained on the ttraining window specified by the user. Yield data is rotated into principle components, where descriptive stats on these components are determined. Changes to the yield curve are usually determined at the priciple component level, added to the rotated curve, before being 'unloaded' back to its original coordinate system."),
      p("Another spline fit estimates each months yield, before applying zero curve bootstrapping in c++."),
      p("A schedule of coupon and principal payments for each bond, stepping back in 6 month intervals from the maturity date. These cashflows are then discounted by the app's internally calculated zero curve to value the portfolio."),
      p("Zero arbitrage is checked for, and the user is notified when their scenario violates no arbitrage pricing.")
      ),
      
      bslib::accordion_panel(
        title = "Scenario Analysis and VAR Using Principle Components",
        p("Many yield curve variations can be captured by just a few 'principal components,' commonly interpreted as level, steepening, and curvature."),
        p("By applying PCA to historical yield-curve data, this dashboard identifies these factors and allows the PM to stress-test the curve by shifting it up/down, tilting it, or bending it."),
        p("These sliders correspond to Z-score adjustments of the three components, allowing the PM to create user-defined yield curves for hypothetical market conditions and examine their exposure to those conditions. Z scores are determined by calculating descriptive statistics on the data in principle component form."),
        p("Yields are monotonically spline fitted once to determine key yields for zero curve bootstrapping. Bootstrapping then occurs in compiled C++. This allows us to rapidly resample simulated yields and to price with zeros rapidly and repeatedly in simulation. Checks if the resulting zero curve is in bounds for no arbitrage, giving the PM feedback when scenarios violate the condition."),
        p("PCA provides great advantages in VAR simluation. First and foremast, it decorrelates all components. One can then sample from these components independently without requiring cholesky decomposition, copula fitting etc. For thos wishing to use such methods, it also avoids the downstream pitfalls of trying to adjust such methods to properly account for fatter tails and skew.")
      ),
       
        
       bslib::accordion_panel(title = "Value at Risk (VaR) Simulation",
        p("In VaR, we use Cornish Fisher expansion to quantile adjust a normal distribution based of the estimate of skew and kurtosis of the principle component. Due to lack of formal domain validity functions, a 'draw between the lines' approach is used to determine when the domain expansion is valid. This boundary condition doesn't quite maximize the valid use of the function, however should consistently avoid cases of quantile swapping."),
        p("When the domain condition of Cornish Fisher doesn't hold, emperically sampling of that principle component distribution is used. Note that random sampling occurs for each and all principle components independent of one another, which is valid due to the orthogonality of each component."),
        p("The 'trading days' selection by the user determines the lag between princinple component observations used to estimate the descriptive statistics on the deltas (for cornish fisher) or to determine the distribution of deltas to emperically sample from."),
        p("VaR get it all. Whereas the scenario analysis engine uses only the highest three principle components, VaR runs sampling over all components of the trained model."),
        p("The training data you have selected, creating a distribution of gains/losses and returns the loss threshold that has only a 5% chance of being exceeded.")
        
      )
      )
      ),
      
      div(style = "bottom: 0; width: 100%; text-align: center; padding: 8px;",
          HTML(
            '<a href="https://www.linkedin.com/in/powleyjustin/" target="_blank" text-decoration: none;">Justin Powley</a>
                |
                <a href="https://www.linkedin.com/in/travis-nowak-072010147/" target="_blank" text-decoration: none;">Travis Nowak</a>')
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
                            bslib::layout_columns(
                            shiny::uiOutput("var5"),
                            shiny::uiOutput("var5pct")
                            ),
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
