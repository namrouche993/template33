#setwd("C:/Users/nadji/amrouche2/electron-quick-start/ElectronShinyAppWindows/electron-quick-start-win32-ia32/resources/app")
#getwd()

#.libPaths(paste0(getwd(),"/R-Portable-Win/library"))
#.Library=paste0(getwd(),"/R-Portable-Win/library")

library(shiny)



# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful USMAAA Geyser sffffffffffffsData"),

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
           textOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderText({
     
paste(.packages())
     
    })
 
}

# If you want to automatically reload the app when your codebase changes - should be turned off in production
options(shiny.autoreload = TRUE)

options(shiny.host = '0.0.0.0')
options(shiny.port = 8080)

# Create Shiny app ---- 
shinyApp(ui = ui, server = server)
