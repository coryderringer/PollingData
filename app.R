library(shiny)
library(tidyverse)


# Define UI for application that draws a histogram
ui <- fluidPage(

   # Application title
   titlePanel("Margin of Error by Sample Size"),

   # Sidebar with a slider input for number of bins
   sidebarLayout(
      sidebarPanel(
         
         sliderInput("N",
                     "Sample size:",
                     min = 0,
                     max = 5000,
                     value = 2500),
         
         sliderInput("Percentage",
                     "Vote Percentage (Candidate 1):",
                     min=0,
                     max=100,
                     value=50),
         
        
         textOutput("s_output"),
         textOutput("p_output1"),
         textOutput("p_output2"),
         textOutput("moe_output")
         
      ),
    
      mainPanel(
         plotOutput("distPlot")
      )
   )
)



# Define server logic required to draw a histogram
server <- function(input, output) {
    output$s_output <- renderText({
        paste('Sample Size: ', input$N, sep='')
    })
    
    output$p_output1 <- renderText({
        paste('Candidate 1 Percentage: ', input$Percentage/100, sep='')
    })
    output$p_output2 <- renderText({
        q <- 1-(input$Percentage/100)
        paste('Candidate 2 Percentage: ', q, sep='')
    })
    
    output$moe_output <- renderText({
        p <- input$Percentage/100
        q <- 1-p
        
        n <- as.numeric(input$N)
        
        diff_squared <- (p-q)^2
        
        # look into this more, I'm not super familiar (this is literally taken from Wikipedia at this point)
        squared <- (p + q - diff_squared)/n
        moe <- sqrt(squared)*100
        
        paste('Margin of Error: ', round(moe, 2), '%', sep='')
    })
    
    output$distPlot <- renderPlot({
        p <- input$Percentage/100
        q <- 1-p
        
        candidates <- factor(c('Candidate 1', 'Candidate 2'))
        candidates <- fct_relevel(candidates, 'Candidate 2')
        
        percent <- c(p,q)
        poll <- c('one', 'one')
        
        d <- data.frame(percent, candidates, poll)
        
        
        d %>%
            
            ggplot(aes(y = percent, x = poll)) +
            geom_bar(aes(fill=candidates), stat="identity", width=.3) +
            scale_fill_manual(values = c('light blue', 'red')) +
            xlab('')
        
    })
    
   
}

# Run the application 
shinyApp(ui = ui, server = server)

