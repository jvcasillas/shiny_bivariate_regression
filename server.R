library(shiny); library(stargazer)
set.seed(1)

shinyServer(function(input, output) {

    x <- reactive({
        x <- runif(input$n)
    })
    y <- reactive({
        input$beta0 + x() * input$beta1 + rnorm(input$n, sd = input$sigma)
    })

    mod <- reactive({
        mod <- lm(y() ~ x())
    })
    output$modPlot <- renderPlot({
        x <- x()
        y <- y()
        mod <- mod()
        
        plot(x, y)
        points(x, y, pch = 16, col = rgb(150, 0, 204, 102, 
                                         maxColorValue = 255))
        abline(c(mod$coeff[1], mod$coeff[2]), lty = 1, lwd = 2,
               col = rgb(0, 0, 204, 102, maxColorValue = 255))
        abline(a = mean(y), b = 0, col = 'grey', lty = 2)
        abline(v = mean(x), col = 'grey', lty = 3)
        legend("topleft", lty = c(2, 3), col = "grey",
                legend = c(expression(bar(y)), expression(bar(x))))
    })
    
    output$values <- renderPrint({
        fit <- mod()
        stargazer(fit, type = 'html', single.row=TRUE, 
                  ci=TRUE, ci.level=0.95, align=FALSE,
                  covariate.labels = c("x", "Intercept"), 
                  dep.var.labels = "y", keep.stat=c("n", "F", "rsq"))
    })
    
    output$resid <- renderPlot({
        fit <- mod()
        par(mfrow = c(2, 2))
        plot(fit)
    })
    
    output$residValues <- renderPrint({
        fit <- mod()
        stargazer(fit, type = 'html', single.row=TRUE, 
                  ci=TRUE, ci.level=0.95, align=FALSE,
                  covariate.labels = c("x", "Intercept"), 
                  dep.var.labels = "y")
    })
})