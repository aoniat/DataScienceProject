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
library(RColorBrewer)
library(dplyr)

df<- read.csv("clean_dataset.csv")

ui <- fluidPage(
  fluidRow(
    column(width=4, h2("         ")),
    column(width=8, h1("Wine Recommender"))
  ),
  fluidRow(
    column(width=12, h5("       "))
  ),
  fluidRow(
    column(width=12, h5("       "))
  ),
  fluidRow(
    column(width = 4, sliderInput("pricerange", label= "Select a price range for your wine",
                                  min = 0, max = 100, value = c(15, 25))),
    column(width = 4, textInput("description1", label = "Write a word that describes your wine",
                                value = " "),
           fluidRow(
             column(width = 12, h5("Try terms like 'dry', 'aromatic', 'full-bodied', 'fruity', 'honey' or 'fresh'")))),
    column(width = 4,textInput("description2", label = "Add another word to describe your wine ",
                               value = " "))
  ),
  fluidRow(
    column(width=12, h5("       "))
  ),
  fluidRow(
    column(width=12, h5("       "))
  ),
  fluidRow(
    column(width=12, h5("       "))
  ),
  fluidRow(
    column(width=12, h5("       "))
  ),
  fluidRow(
    column(width=6, tableOutput(outputId = "wineTable")),
    column(width=6, plotOutput(outputId = "wineGraph"))
  )

)

myvars <- c("Score", "Price", "Title", "Country") 

countries = c("Argentina", "Australia", "Canada", "France", "Italy", "Spain", "US")
countsbycountry = c(1056, 1199, 165, 8801, 7843, 2177, 26611)

server <- function(input, output) {
  output$wineTable <- renderTable({df %>%
      filter(Price >= input$pricerange[1]) %>% #used filter function to only keep wines whose prices are greater than lower limit of the price range
      filter(Price <= input$pricerange[2]) %>% # used filter function to only keep wines whose prices less than the higher limit of the price range
      filter(str_detect(Description, input$description1)) %>% # used filter function to only keep wines whose description included the keyword inputted
      filter(str_detect(Description, input$description2)) %>% # used filter function to only keep wines whose description included the keyword inputted
      arrange(desc(Score)) %>% # arranged the wines in the descending order by their score
      head(8) %>% # looked at the top 8 wines from the entire list
      select(myvars) # selected those variables that we wanted to display in the table
  })
  
  output$wineGraph <- renderPlot ({dfgraph <-df %>%
    filter(Price >= input$pricerange[1]) %>% # used the filter function to only keep wines with prices greater than the lower limit of the price range 
    filter(Price <= input$pricerange[2]) %>% # used the filter function to only keep wines with prices less than the upper limit of the prince range
    filter(str_detect(Description, input$description1)) %>% # used the filter function to only keep wines whose description included the keyword inputted
    filter(str_detect(Description, input$description2)) # used the filter function to only keep wines whose description included the keyword inputted 
  for(i in 1:7){
    if(length(which(dfgraph$Country == countries[i])) == 0){
      countsbycountry <- countsbycountry[-i]
    }
  } # used a for loop to eliminate those countries from the countsbycountry vector whose count for the particular case was 0
  
  dfgraph %>% ggplot(aes(x=Country))+
    geom_bar(aes(y=(..count..)/(countsbycountry)),fill="darkred", color="black")+
    ggtitle("Percentage of wines by country that meet your criteria") +
    ylab(" ") + xlab(" ") + theme(axis.text.y = element_text(size = 12),
                                  axis.text.x = element_text(size = 11),
                                  plot.title = element_text(size = 15))+
    scale_y_continuous(labels = scales::percent_format(accuracy = 0.1))
  })
}


shinyApp(ui = ui, server = server)