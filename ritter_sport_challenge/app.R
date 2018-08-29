#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(readr)
library(tidyr)
library(dplyr)
library(ggplot2)

# read data
challenge_data <- read_csv('./data/ritter.csv')
challenge_melt <- gather(challenge_data[,2:13], person, ranking, -Cokolada)
challenge_melt <- challenge_melt[!is.na(challenge_melt$ranking),]
challenge_melt$rank <- as.factor(challenge_melt$ranking)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Ritter Sport Challenge"),
   
   # Sidebar with a dropdown menu to choose a chocolate
   fluidRow(
     column(2, 
            selectInput("choco", "Select Chocolate:", 
                        choices = challenge_data$Cokolada)
      ),
      
      # Show a plot 
      column(10, 
         plotOutput("choco_barPlot")
      )
   ),
   
   titlePanel("Due to popular demand..."),
   
   fluidRow(
     column(2,
            selectInput("who", "Select person:",
                        choices = unique(challenge_melt$person))
     ),
     column(10,
            tabsetPanel(
              tabPanel("Plot", plotOutput("person_barPlot")),
              tabPanel("Table", div(tableOutput("personTable")), 
                       style = "font-size:80%")
            )
     )
   )
)

# Define server logic required to draw a plot
server <- function(input, output) {
   
   output$choco_barPlot <- renderPlot({

      # draw plot for the chosen chocolate
      challenge_melt %>% 
        filter(Cokolada ==  input$choco) %>% 
        ggplot(aes(x = rank, y = (..count..)/sum(..count..), fill= "choice")) +
        geom_bar(data = challenge_melt, 
                 aes(x = rank, y = (..count..)/sum(..count..), fill = "all"),
                 alpha = 0.5) +
        geom_bar(alpha = 0.6) +
        scale_fill_manual(values =c('all'='grey','choice'='firebrick'), labels = c('All', input$choco)) +
        ylab("Percent") +
        xlab("Rank") +
        scale_y_continuous(labels = scales::percent, 
                           limits = c(0, 0.75),
                           breaks = seq(0, 0.75, by = 0.1) ) +
        theme_bw() +
        theme(legend.position = "bottom", 
              legend.title = element_blank(),
              legend.text = element_text(size = 12),
              axis.text = element_text(size = 12)
              ) 
   })
   
   output$person_barPlot <- renderPlot({
     
     # draw plot for the chosen person
     challenge_melt %>% 
       filter(person ==  input$who) %>% 
       ggplot(aes(x = rank, y = (..count..)/sum(..count..), fill= "who")) +
       geom_bar(data = challenge_melt, 
                aes(x = rank, y = (..count..)/sum(..count..), fill = "all"),
                alpha = 0.5) +
       geom_bar(alpha = 0.6) +
       scale_fill_manual(values =c('all'='grey','who'='firebrick'), labels = c('All', input$who)) +
       ylab("Percent") +
       xlab("Rank") +
       scale_y_continuous(labels = scales::percent, 
                          limits = c(0, 0.75),
                          breaks = seq(0, 0.75, by = 0.1) ) +
       theme_bw() +
       theme(legend.position = "bottom", 
             legend.title = element_blank(),
             legend.text = element_text(size = 12),
             axis.text = element_text(size = 12)
       ) 
   })
   
   output$personTable <- renderTable({ 
     challenge_melt %>% 
       group_by(Cokolada) %>% 
       summarise(mean = round(mean(ranking), digits = 2), 
                 median = as.integer(median(ranking))) %>% 
       full_join(., filter(challenge_melt, person ==  input$who)) %>% 
       select(c("Cokolada", "ranking", "median", "mean")) %>% 
       rename(!!quo_name(input$who) := ranking) %>% 
       rename(Chocolate = Cokolada) 
   }) 

}

# Run the application 
shinyApp(ui = ui, server = server)

