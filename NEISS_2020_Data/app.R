#load packages and data
library(shiny)
library(tidyverse)
library(hrbrthemes)
library(shinythemes)
library(chisq.posthoc.test)

neiss <- readr::read_csv("neiss2020.csv")

neiss <- neiss %>%
  #labeled top products
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
        Product_1 == 1555 ~ "high chairs",
        Product_1 == 1545 ~ "cribs",
        Product_1 == 478 ~ "drinking glasses",
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
            TRUE ~ as.character(Body_Part)),
        #edit vars for Chi tab
        Fire_Involvement = case_when(
          Fire_Involvement == 0 ~ "No", TRUE ~ "Yes"
        ),
        Alcohol = case_when(
          Alcohol == 0 ~ "No", TRUE ~ "Yes"
        ),
        Drug = case_when(
          Drug == 0 ~ "No", TRUE ~ "Yes"
        ),
        Disposition = case_when(
          Disposition == 1 ~ "Treated & Released", Disposition == 2 ~ "Treated & Transferred", Disposition == 4 ~ "Treated & Hospitalized", Disposition == 5 ~ "Held for Observation",
          Disposition == 6 ~ "Left Without Being Seen", Disposition == 8 ~ "Fatality",
          TRUE ~ "Unknown"
        )) %>% 
          mutate(Fire = Fire_Involvement)

#edit treat_date variable      
neiss <- neiss %>% 
  separate(Treatment_Date, into = c("Month", "Day", "Year"),"/") %>% 
  select( -("Year")) %>% 
  mutate( Month = case_when(
          Month == 1 ~ "January", Month == 2 ~ "February", Month == 3 ~ "March", Month == 4 ~ "April", Month == 5 ~ "May", Month == 6 ~ "June", 
          Month == 7 ~ "July", Month == 8 ~ "August", Month == 9 ~ "September", Month == 10 ~ "October", Month == 11 ~ "November", TRUE ~ "December"
        ))
        
#separate ages
Babies <- neiss %>% 
    subset(Age > 200) %>% 
    mutate( Age_in_Months = Age - 200)
Adults <- neiss %>% 
    subset(Age <200 & Age > 17)
Kids <- neiss %>%
    subset(Age <18 & Age > 1)

#data used for chisq
chi <- neiss %>% 
  select(c(Fire, Alcohol, Sex, Drug, Disposition))
    


#define ui
ui <- fluidPage(theme = shinytheme("slate"),
    titlePanel(h1("Investigating Consumer Product-Related Injuries", align = "center")),
    div(img( src = "wordcloud.png", align = "center", height = "250", width = "500"), style = "text-align: center;"),
    br(),
    h6("This app looks at data from the 2020 National Electronic Injury Surveillance System in the U.S..
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
                  ),
      #Date tab
      tabPanel("Treatment Date",
               sidebarLayout(
                 #Select inputs
                 sidebarPanel(
                   h5("This tab shows you the amount of hospital visits associated with
                   products in 2020 per month.
                      It is also a place to play with some of the settings on the plot."),
                   radioButtons(
                     inputId = "date_set", 
                     label = "Select Age Range:",
                     choices = c("Under 2 years old" = "Babies",
                                 "2-17 years old"    = "Kids",
                                 "18 years or older" = "Adults"
                     ),
                     selected = "Babies"),
                   
                   textInput(
                     inputId = "line_color", 
                     label = "Type Line Color (red, grey, etc.):",
                     value = "blue",
                     placeholder = "-Enter a color-"),
                   
                   textInput(
                     inputId = "dot_color", 
                     label = "Type Dot Color (red, grey, etc.):",
                     value = "red",
                     placeholder = "-Enter a color-"),
                   
                   sliderInput(
                     inputId = "dot_size",
                     label = "Choose Dot Size:",
                     min = 1, max = 10,
                     value = 3,
                     step = 1
                   ),
                   
                   #Add text
                   textOutput("date_tab")),
                 
                 #Select output
                 mainPanel(
                   plotOutput(outputId = "date_lollipop")
                 )
               )
               ),
      #Chi-Square Tests tab
      tabPanel("Chi-Squared Tests",
               sidebarLayout(
                 #Select inputs
                 sidebarPanel(
                   h5("Use this tab to perform chi-squared tests on some of the variables."),
  
                   #Select var1
                   varSelectInput(
                     inputId = "var1",
                     label = "Select First Variable:",
                     data = chi,
                     selected = "Fire"
                   ),
                   
                   #Select var2
                   varSelectInput(
                     inputId = "var2",
                     label = "Select Second Variable:",
                     data = chi,
                     selected = "Alcohol"
                   ),
                   #Add text
                   textOutput("chi_tab"),
                 ),
                 #Select output
                 mainPanel(
                   h4("Frequency Table", h5("(the first variable is on the left)")),
                   verbatimTextOutput(outputId = "freq"),
                   h4("Chi-Squared Test"),
                   verbatimTextOutput(outputId = "chi_test"),
                   h4("Post hoc Test", h6("This test is only needed when the frequency table is larger than 2x2.
                                          Thus, this only really applies when Sex or Disposition are selected.")),
                   verbatimTextOutput(outputId = "post_hoc"),
                   paste("Ex: Choose Sex & Alcohol as your variables. The p-value for female is significant.
            Looking at the residuals, you can say there are significantly less than expected females
            where alcohol was involved with their injury since the No residual was positive."))
                   
                 )
               ),
        tabPanel("About",
                 h5("If you have questions or suggestions, please email jferkull@yahoo.com", align = "center"),
                 h6("To view the data dictionary, click here:",a("Data Dictionary", href = "https://docs.google.com/spreadsheets/d/1N4b0hZIfVm6J6CuZKTN2Q9bRcWmiWTQX/edit#gid=999577082")
                    ,align = "center"),
                 h3("Demographics:", align = "center"),
                 h5("Explore the Age, Race, Sex, Body Part, or Diagnosis variables", align = "center"),
                 h3("Products:", align = "center"),
                 h5("Explore the products most frequently involved with injury for different age groups", align = "center"),
                 h3("Narrative:", align = "center"),
                 h5("Search the data set for products of interest or treatment outcomes", align = "center"),
                 h3("Treatment Date:", align = "center"),
                 h5("See how the number of hospital visits differed per month", align = "center"),
                 h3("Chi-Squared Tests:", align = "center"),
                 h5("Perform some statisical testing of your own", align = "center")
                 )
      ),
               )

#Define server
server <- function(input, output, session) {
    
    data_selected <- reactive({
        get(input$data_set)
    })
    
    data_p_selected <- reactive({
      get(input$Product_set)
    })
    
    Product_selected <- reactive({
        get(input$Product_set)
    })
    
    date_selected <- reactive({
      get(input$date_set)
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
            mutate(Product_1 = fct_reorder(Product_1, n, .desc = TRUE)) %>%
            ggplot(aes(x= Product_1, y=n))+
            geom_bar(stat = "identity", fill = "blue")+
            coord_flip()+
            labs( title = paste("Most Frequent Products Involved in Injuries for",input$Product_set))+
            theme_bw()
    })
    
    output$prod_summary <- renderDataTable({
      data_p_selected() %>%
        group_by(Product_1) %>%
        summarise(n=n()) %>% 
        arrange(desc(n))
    })
    
    #narrative tab
    
    output$narrative <- renderDataTable({
      neiss %>% 
        mutate( Product = Product_1, Narrative = Narrative_1) %>% 
        select(c(Product,Narrative, Disposition)) %>%
        arrange(Product)
      
    })
    
    #date tab
    
    output$date_tab <- renderText({
      paste("April has far fewer hospital visits than the other months,
            but June is the declared National Safety Month in the U.S.
            January had the most hospital visits in 2020.")
    })
    
    output$date_lollipop <- renderPlot({
      date_selected() %>% 
        group_by(Month) %>%
        summarise(day_count = n()) %>% 
        arrange(day_count) %>% 
        mutate(Month = factor(Month, Month)) %>% 
        ggplot(aes(x=Month, y=day_count)) +
        geom_point(size = input$dot_size, color = input$dot_color) + 
        geom_segment( aes(x=Month, xend=Month, y=0, yend=day_count), color= input$line_color)+
        coord_flip()+
        labs( title = "Number of Consumer Product Related Hospital Visits by Month in 2020", y= "Number of Hospital Visits")
    })
    
    #Chi square tab
    output$chi_tab <- renderText({
      paste("The frequency table shows the counts of the variables you selected.
            The Chi-Square test determines if the categorical variables are independent of each other.
            If the p-value is less than .05, you can conclude the categorical variables are associated
            and look at the Post hoc test.")
    })
    
    output$freq <- renderPrint({
      var1_sym <- input$var1
      var2_sym <- input$var2
      M <- table(chi[[var1_sym]], chi[[var2_sym]])
      M
    })
    
    output$chi_test <- renderPrint({
      var3_sym <- input$var1
      var4_sym <- input$var2
      Q <- table(chi[[var3_sym]], chi[[var4_sym]])
      
      chisq.test(Q, correct = FALSE)
    })
    
    output$post_hoc <- renderPrint({
      var5_sym <- input$var1
      var6_sym <- input$var2
      P <- table(chi[[var5_sym]], chi[[var6_sym]])
      
      chisq.posthoc.test(P, method = "bonferroni")
      
    })
    
}

#create Shiny app object
shinyApp(ui, server)