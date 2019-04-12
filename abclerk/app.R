library(shiny)
require(shinythemes)
require(pwr)
require(ggplot2)
require(scales)


#### UI ####
ui <- fluidPage(theme = shinytheme("yeti"),
        titlePanel("A/B Clerk: A/B Test Power Calculator"), sidebarLayout(
        sidebarPanel(
                numericInput("p1",
                             "Plan A Conversion Rate %",
                             value = 5,
                             min = 0,
                             max = 100
                ),
                numericInput("uplift",
                             "Expected Uplift % from Plan B",
                             value = 10,
                             min = 0
                ),
                selectInput("confidence",
                             "Confidence %",
                            choices = c(90, 95, 99, 99.9),
                            selected = 95,
                            selectize = FALSE
                ),
                sliderInput("desired_power",
                             "Desired Power",
                             value = 0.8,
                             min = 0.5,
                             max = 1,
                             step = 0.05
                )
        ),
        mainPanel(
                plotOutput("plot_01",
                           click = clickOpts("plot_01_click")),
                textOutput("conclusion"),
                verbatimTextOutput("xxx")
        )
))

#### Server ####
server <- function(input, output) {

        n <- seq(250, 100000, by = 100)
        p1 <- reactive(input$p1 / 100)
        uplift <- reactive(input$uplift / 100)
        p2 <- reactive(p1()*(1 + uplift()))
        sig <- reactive(1 - as.numeric(input$confidence) / 100)

        power <- reactive({sapply(
                        n,
                        function(.) pwr.2p.test(h = ES.h(p1 = p2(), p2 = p1()),
                                                n = .,
                                                sig.level = sig())$power
        )})

        d <- reactive(data.frame(n = n, power = power()))

        desired_power <- reactive(input$desired_power)
        necessary_n <- reactive({
                with(d(),n[which(power > desired_power())[1]])
                })

        plot_00 <- reactive({
                req(power)
                ggplot(d(), aes(x = n, y = power())) +
                        theme_minimal()+
                        geom_line(size = 1.5, color = "deepskyblue3") +
                        scale_y_continuous(name = NULL,
                                           labels = percent
                        )
        })

        output$plot_01 <- renderPlot({

                plot_00() +
                        geom_point(aes(x = necessary_n(), y = desired_power()),
                                   color = "red", size = 4, shape = 15)
                        #  geom_label(aes(x = necessary_n(), y = desired_power()),
                        #            label  = paste0("Power: ", percent(desired_power()),
                        #                            "\n",
                        #                            "Visits:", necessary_n()
                        #            ),
                        #            position = position_nudge(x = 0.10 * max(n),
                        #                                      y = -0.05 * max(power())
                        #            ))
        })


        output$conclusion <- renderText({
                req(power)
                paste("You need", necessary_n(), "visitors to make sure that your A/B test has a", percent(desired_power()), "probability to confirm if Plan B will produce a", percent(uplift()), "uplift from Plan A's", percent(p1()), "conversion rate, owing less than", percent(sig(), accuracy = 0.01), "to random chance.")})
        output$xxx <- renderPrint(input$plot_01_click$x)
}

# Run the application
shinyApp(ui = ui, server = server)

