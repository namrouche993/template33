#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(readxl)
livraison_wilayas <- read_excel(paste0(getwd(),"/livraison_wilayas.xlsx"))

plic <- installed.packages(.Library, fields = "License")
plica1=rownames(plic)

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("Old Faithful USMALGsqaazeazERiiaae Geyjjjjjjzazejjjjjjjjser Data"),
    
    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            textOutput("texta1"),
            plotOutput("distPlot"),
            tableOutput("table0")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$table0<-renderTable({
        data.frame(head(livraison_wilayas))
    })
    
    output$texta1<-renderText({
        paste(c(plica1,getwd()))
    })
    
    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)
        
        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
}

# Run the application
shinyApp(ui = ui, server = server)

