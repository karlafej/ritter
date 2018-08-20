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
challenge_melt$ranking <- as.factor(challenge_melt$ranking)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Ritter Sport Challenge"),
   
   # Sidebar with a dropdown menu to choose a chocolate
   sidebarPanel(
     selectInput("choco", "Select Chocolate:", 
                 choices=challenge_data$Cokolada)
   ),
      
      # Show a plot 
      mainPanel(
         plotOutput("barPlot")
      )
   )

# Define server logic required to draw a plot
server <- function(input, output) {
   
   output$barPlot <- renderPlot({

      # draw plot for the chosen chocolate
      challenge_melt %>% 
        filter(Cokolada ==  input$choco) %>% 
        ggplot(aes(x = ranking, y = (..count..)/sum(..count..), fill= "choice")) +
        geom_bar(data = challenge_melt, 
                 aes(x = ranking, y = (..count..)/sum(..count..), fill = "all"),
                 alpha = 0.5) +
        geom_bar(alpha = 0.6) +
        scale_fill_manual(values =c('all'='grey','choice'='firebrick'), labels = c('All', input$choco)) +
        ylab("Percent") +
        xlab("Rank") +
        theme_bw() +
        theme(legend.position = "bottom", 
              legend.title = element_blank(),
              legend.text = element_text(size = 12),
              axis.text = element_text(size = 12)
              ) 
            
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

