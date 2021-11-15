#load packages and data
library(shiny)
library(ggplot2)
neiss <- readr::read_csv("neiss2020.csv")

#define ui
ui <- fluidPage(
    sidebarLayout(
        #Select inputs
        sidebarPanel(
            selectInput(
                inputId = "x",
                label = "X-axis:",
                choices = c("Race", "Sex", "Body_Part", "Diagnosis"),
                selected = "Race"
            )
        ),
        #Select output
        mainPanel(
            plotOutput(outputId = "histogram")
        )
    )
)
#Define server
server <- function(input, output, session) {
    
    output$histogram <- renderPlot({
        ggplot(data = neiss, aes_string(x = input$x))+
            geom_bar()
    })
    
}

#create Shiny app object
shinyApp(ui, server)