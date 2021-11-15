#load packages and data
library(shiny)
library(ggplot2)
neiss <- readr::read_csv("neiss2020.csv")
bow <- neiss %>% 
    subset(Age > 200) %>% 
    mutate( Age_in_Months = Age - 200)
aow <- neiss %>% 
    subset(Age <200 & Age > 17)
kow <- neiss %>%
    subset(Age <18 & Age < 1)

#define ui
ui <- fluidPage(
    sidebarLayout(
        #Select inputs
        sidebarPanel(
            # Select data set
            selectInput(
                inputId = "data_set", 
                label = "Select Age",
                choices = c("Under 2 years old" = "bow",
                            "2-17 years old"    = "kow",
                            "18 years or older" = "aow"
                           ),
                selected = "bow"),
            
            #Select x-axis
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
        ggplot(data = input$data_set, aes_string(x = input$x))+
            geom_bar()
    })
    
}

#create Shiny app object
shinyApp(ui, server)