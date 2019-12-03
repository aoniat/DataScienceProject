#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)


df<- read.csv("clean_dataset.csv")

ui <- fluidPage(
                 fluidRow(
                     column(width = 4, "price",
                                  sliderInput("pricerange", label= "Select a price range for your wine",
                                              min = 0, max = 100, value = c(15, 25))),
                     column(width = 4,textInput("description1", label = "Write a word that describes your wine",
                                            value = "fruity")),
                     column(width = 4,textInput("description2", label = " ",
                                            value = " "))
                     ),
                     fluidRow(tableOutput(outputId = "wineTable")
                     )
                 )

        
myvars <- c("score", "price","title")        

server <- function(input, output) {
    output$wineTable <- renderTable({df %>%
            filter(price > input$pricerange[1]) %>%
            filter(price < input$pricerange[2]) %>%
            filter(str_detect(description, input$description1)) %>%
            filter(str_detect(description, input$description2)) %>%
            arrange(desc(score)) %>%
            select(myvars) %>%
            top_n(10)
    })
    
}


shinyApp(ui = ui, server = server)


##### Testing field


