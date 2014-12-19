library(shiny); library(stargazer)

shinyUI(fluidPage(
    
    h3("Bivariate linear regression"),

    sidebarLayout(position = "left",
        sidebarPanel(width = 3,
            h5("Description:"),
            p("This app fits a linear model to simulated data using:"),
            withMathJax(helpText("$$y = 
                             \\beta_0 + \\beta_1X_1 + \\epsilon$$")),
            p("The x- and y-values are given a linear relationship using 
              the formula:"),
            p("$$\\beta_0 + x * \\beta_1 + N$$"),
            sliderInput("n", "N:", min = 0, 
                    max = 500, value = 250, step = 2),
            numericInput("beta0", "$$\\beta_0$$", 5, step = 0.5),
            numericInput("beta1", "$$\\beta_1$$", 1, step = 0.25),
            numericInput("sigma", "$$\\sigma$$", 0.075, step = 0.025),
            br(),
            strong("Created by:", 
                   tags$a("Joseph V. Casillas", 
                          href="http://www.jvcasillas.com")),
            strong("Source code:", 
                   tags$a("Github", 
                          href="https://github.com/jvcasill/shiny_crossOver/"))
        ),
        
        mainPanel(
            tabsetPanel(type = "pills", position = "left",
                tabPanel("Plot", 
                         plotOutput("modPlot"),
                         tableOutput("values")), 
                tabPanel("Residuals", 
                         plotOutput("resid"),
                         tableOutput("residValues"))
            )
        )
    )
))