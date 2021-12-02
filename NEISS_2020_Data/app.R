#load packages and data
library(shiny)
library(tidyverse)
library(hrbrthemes)
library(shinythemes)

neiss <- readr::read_csv("neiss2020.csv")

neiss <- neiss %>% 
    mutate(Product_1 = case_when(
        Product_1 == 4076 ~ "beds or bedframes",
        Product_1 == 1807 ~ "floors or flooring materials",
        Product_1 == 1842 ~ "stairs or steps",
        Product_1 == 679 ~ "couches",
        Product_1 == 4057 ~ "tables",
        Product_1 == 4074 ~ "chairs",
        Product_1 == 1893 ~ "doors",
        Product_1 == 1395 ~ "toys",
        Product_1 == 611 ~ "bathtubs",
        Product_1 == 1931 ~ "tablet or capsule drugs",
        TRUE ~ as.character(Product_1)
    ))

Babies <- neiss %>% 
    subset(Age > 200) %>% 
    mutate( Age_in_Months = Age - 200)
Adults <- neiss %>% 
    subset(Age <200 & Age > 17)
Kids <- neiss %>%
    subset(Age <18 & Age > 1)
    

#define ui
ui <- fluidPage(theme = shinytheme("slate"),
    titlePanel(h1("Investigating Consumer Product Injuries", align = "center")),
    div(img( src = "wordcloud.png", align = "center", height = "250", width = "500"), style = "text-align: center;"),
    br(),
    h6("This app looks at data from the National Electronic Injury Surveillance System in the U.S..
       Each observation is a patient's summary of a hospital visit.", align = "center"),
    a("NEISS Coding Manual", href = "https://www.cpsc.gov/s3fs-public/2019_NEISS_Coding_Manual.pdf"),
    #creates separate tabs
    tabsetPanel(
        #creates panel with demographics
        tabPanel("Demographics",
    sidebarLayout(
        #Select inputs
        sidebarPanel(
            h5("Use this tab to explore the demographics associated with the data."),
            # Select data set
            radioButtons(
                inputId = "data_set", 
                label = "Select Age Range:",
                choices = c("Under 2 years old" = "Babies",
                            "2-17 years old"    = "Kids",
                            "18 years or older" = "Adults"
                           ),
                selected = "Babies"),
            
            #Select x-axis
            selectInput(
                inputId = "x",
                label = "Demographic of Interest:",
                choices = c("Race", "Sex", "Body Part" = "Body_Part", "Diagnosis"),
                selected = "Race"
            )
        ),
        #Select output
        mainPanel(
            plotOutput(outputId = "histogram"),
            
            dataTableOutput(outputId = "summary")
        )
    )
    ),
        #creates panel with Products
        tabPanel("Products",
                 sidebarLayout(
                     #Select inputs
                     sidebarPanel(
                         radioButtons(
                             inputId = "Product_set", 
                             label = "Select Age Range:",
                             choices = c("Under 2 years old" = "Babies",
                                         "2-17 years old"    = "Kids",
                                         "18 years or older" = "Adults"
                             ),
                             selected = "Babies")),
                    #Select output
                         mainPanel(
                             plotOutput(outputId = "Product_histogram")
                         )
                     )
                 )
                 ),
        tabPanel("Narrative")
    )

#Define server
server <- function(input, output, session) {
    
    data_selected <- reactive({
        get(input$data_set)
    })
    
    Product_selected <- reactive({
        get(input$Product_set)
    })
    #demographic tab
    output$histogram <- renderPlot({
        ggplot(data = data_selected() , aes_string(x = input$x))+
            geom_bar( fill = "blue")+
            labs( title = paste(input$x,"Distribution for",input$data_set))+
            theme_bw()
    })
    
    output$summary <- renderDataTable({
        data_selected() %>%
            req(input$x) %>%
            group_by(!! sym(input$x)) %>%
            summarise(n=n()) %>% 
            arrange(desc(n))
    })
    
    #product tab
    output$Product_histogram <- renderPlot({
        Product_selected() %>%
            group_by(Product_1) %>% 
            summarise(n=n()) %>% 
            arrange(desc(n)) %>%
            slice_head(n=10) %>% 
            ggplot(aes(x= Product_1, y=n))+
            geom_bar(stat = "identity", fill = "blue")+
            coord_flip()+
            labs( title = paste("Most Frequent Products Involved in Injuries for",input$Product_set))+
            theme_bw()
    })
    
}

#create Shiny app object
shinyApp(ui, server)