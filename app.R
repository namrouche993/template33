#setwd("C:/Users/nadji/amrouche2/electron-quick-start/ElectronShinyAppWindows/electron-quick-start-win32-ia32/resources/app")
#getwd()

#.libPaths(paste0(getwd(),"/R-Portable-Win/library"))
#.Library=paste0(getwd(),"/R-Portable-Win/library")

library(shiny)
#library(fresh)
#library(shinydashboard)
#library(shinydashboardPlus)
#library(shinythemes)
#library(shinyWidgets)
#library(shinycssloaders)
#library(ECharts2Shiny)
#library(rAmCharts)
#library(shinyBS)

 #library(leaflet)
#library(remotes)

#library(htmltools)

 # library(leaflet.extras)

# library(rgdal)
 #library(sp)
 #install.packages('highcharter')
#library(devtools)
#library(htmlwidgets)
library(tidyverse)
 library(highcharter)


 #library(excelR)


  #library(farver)
  #library(readxl)


 #library(reactable)
#library(grDevices)
#library(janitor)

 #library(shinyjs)



# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser abaca sffffffffffffsData"),

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
           highcharter::highchartOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- highcharter::renderHighchart({
     
    highcharter::hchart(mtcars,'scatter',hcaes(x=wt,y=mpg))    
     
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
