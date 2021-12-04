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
        Product_1 == 5040 ~ "bicycles or accessories",
        Product_1 == 1267 ~ "soccer",
        Product_1 == 1233 ~ "trampolines",
        Product_1 == 1211 ~ "football",
        Product_1 == 1205 ~ "basketball",
        Product_1 == 464 ~ "knives",
        Product_1 == 4078 ~ "ladders",
        Product_1 == 3299 ~ "exercise",
        Product_1 == 1884 ~ "ceilings and walls",
        TRUE ~ as.character(Product_1)),
        Race = case_when(
            Race == 0 ~ "Not Stated",
            Race == 1 ~ "White",
            Race == 2 ~ "Black",
            Race == 3 ~ "Other",
            Race == 4 ~ "Asian",
            Race == 5 ~ "American Indian",
            Race == 6 ~ "Native Hawaiian",
            TRUE ~ as.character(Race)),
        Sex = case_when(
            Sex == 1 ~ "Male",
            Sex == 2 ~ "Female",
            Sex == 0 ~ "Not recorded",
            TRUE ~ as.character(Sex)),
        #collapsed Body_Part for readability
        Body_Part = case_when(
            Body_Part == 75 | Body_Part == 94 | Body_Part == 77 | Body_Part == 76 | Body_Part == 88 | Body_Part == 89 ~ "Neck, Face, or Head",
            Body_Part == 30 | Body_Part == 31 ~ "Upper trunk",
            Body_Part == 80 | Body_Part == 32 | Body_Part == 33 | Body_Part == 34 | Body_Part == 82 | Body_Part == 92 ~ "Arm or Hand",
            Body_Part == 79 | Body_Part == 38 ~ "Lower trunk",
            Body_Part == 81 | Body_Part == 35 | Body_Part == 36 | Body_Part == 37 | Body_Part == 83 | Body_Part == 93 ~ "Leg or Foot",
            Body_Part == 84 | Body_Part == 85 ~ "Large percent of body",
            Body_Part == 87 ~ "Not Stated",
            Body_Part == 0 ~ "Internal",
            TRUE ~ as.character(Body_Part))
        )
        
        
        
    

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
    h6("For more information on NEISS or the data set, click here:",a("NEISS Coding Manual", href = "https://www.cpsc.gov/s3fs-public/2019_NEISS_Coding_Manual.pdf")
       ,align = "center"),
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
            ),
            
            #Add text
            textOutput("demo_tab"),
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
                       h5("Use this tab to view the top 10 products involved with hospital visits.
                          You can also look up a product using the NEISS Coding Manual to see its frequency.
                          Product codes begin on page 175."),
                         radioButtons(
                             inputId = "Product_set", 
                             label = "Select Age Range:",
                             choices = c("Under 2 years old" = "Babies",
                                         "2-17 years old"    = "Kids",
                                         "18 years or older" = "Adults"
                             ),
                             selected = "Babies"),
                     
                     #Add text
                     textOutput("product_tab")),
                     
                    #Select output
                         mainPanel(
                             plotOutput(outputId = "Product_histogram"),
                             
                             dataTableOutput(outputId = "prod_summary")
                         )
                     )
                 ),
          #Narrative tab 
        tabPanel("Narrative",
                 paste("Use this tab to explore the short reports on an injury.
                       You can use the search bar to look at a certain product or word in the narrative."),
                 paste("The first numbers in Narrative are the age of the patient. The next 3 symbols stand
                       for whether the age is in month or years and the patients sex."),
                 paste("Ex: 12YOF means the narrative is for a 12 year old female"),
                 br(),
                  dataTableOutput(outputId = "narrative"),
                  )
    )
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
    
    output$demo_tab <- renderText({
        if (input$x == "Race"){
            paste("There seems to be similar proportions of each race for each age group.
                  It is hard to tell if this is truly representative of the U.S. since so
                   many patients did not have their race stated.")
        } else if (input$x == "Sex"){
            paste("There are significantly more male than female hospital visits for the younger age groups.
                  This balances out in the adult age group.")
        } else if (input$x == "Body_Part"){
            paste("The most common injuries for all age groups are on the neck, face, or head.
                  These injuries make up a large proportion of the baby group.")
        } else {paste("Many different diagnoses were given the same code.
                      For this reason, I left the codes as stated in the NEISS Coding Manual
                       and encourage you to look one up in the link above if you are interested.
                      The codes for diagnoses begin on page 163.")}
    })
    
    output$histogram <- renderPlot({
        ggplot(data = data_selected() , aes_string(x = input$x))+
            geom_bar( fill = "blue")+
            coord_flip()+
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
    
    output$product_tab <- renderText({
      paste("Floor, bed, and stair products were frequently involved with injuries for each age group.
            Sports were a popular reason for injury in the 2-17 year old age group.")
    })
    
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
    
    output$prod_summary <- renderDataTable({
      data_selected() %>%
        group_by(Product_1) %>%
        summarise(n=n()) %>% 
        arrange(desc(n))
    })
    
    #narrative tab
    
    output$narrative <- renderDataTable({
      neiss %>% 
        mutate( Product = Product_1, Narrative = Narrative_1) %>% 
        select(c(Product,Narrative)) %>%
        arrange(Product)
      
    })
}

#create Shiny app object
shinyApp(ui, server)