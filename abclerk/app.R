library(shiny)
require(pwr)

#### UI ####
ui <- fluidPage(
        titlePanel("ABClerk: A/B Test Power Calculator"),
        sidebarLayout(
        sidebarPanel(
                numericInput("plan_a_trial",
                          "Plan A Trial",
                          value = 50000,
                          min = 0,
                          step = 1
                          ),
                numericInput("plan_a_success",
                             "Plan A Success",
                             value = 4000,
                             min = 0,
                             # max = input$plan_a_trial,
                             step = 1
                             ),
                numericInput("plan_b_trial",
                          "Plan B Trial",
                          value = 50000,
                          min = 0,
                          step = 1
                        ),
                numericInput("plan_b_success",
                             "Plan B Success",
                             value = 4000,
                             min = 0,
                             # max = input$plan_b_trial,
                             step = 1
                             )
                ),
        mainPanel(



                )
        )
)

#### Server ####
server <- function(input, output) {
        x <- 1
}

# Run the application
shinyApp(ui = ui, server = server)

