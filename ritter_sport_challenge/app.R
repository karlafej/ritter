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
library(plotly)

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
     column(5,
            plotOutput("person_barPlot")
     ),
     column(5,
            plotlyOutput("person_chocoPlot")) #table will be better...
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
   
   output$person_chocoPlot <- renderPlotly({ 
     p <-
     challenge_melt %>% 
       group_by(Cokolada) %>% 
       summarise(mean = round(mean(ranking), digits = 2), 
                 median = median(ranking)) %>% 
       full_join(., filter(challenge_melt, person ==  input$who)) %>% 
       select(c("Cokolada", "median", "mean", "ranking")) %>% 
       rename(!!quo_name(input$who) := ranking) %>% 
       rename(Chocolate = Cokolada) %>% 
       gather(varname, ranking, -Chocolate) %>% 
       ggplot(aes(y = Chocolate, x = varname)) +
       geom_tile(aes(fill = ranking), color = "white") +
       scale_fill_gradientn(colours = colorRampPalette(c("firebrick", "grey"))(8),
                            na.value = "white", limits = c(1,8),
                            breaks = seq(1,8)) +
       scale_y_discrete(position = "right") +
       theme_bw() +
       theme(legend.position = "bottom", 
             legend.title = element_text(size = 1, colour = "white" ),
             legend.text = element_text(size = 6),
             axis.text = element_text(size = 4),
             axis.text.x = element_text(size = 8),
             axis.title = element_blank()
       ) 
       ggplotly(p, tooltip = c("Chocolate", "ranking"))
   }) 

}

# Run the application 
shinyApp(ui = ui, server = server)

