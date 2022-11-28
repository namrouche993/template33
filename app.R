#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyWidgets)
library(shinyalert)
library(shinycookie)
library(googledrive)

ww51=c("01-ADRAR","02-CHLEF","03-LAGHOUAT","04-Oum el Bouaghi","05-BATNA","06-Béjaïa","07-BISKRA","08-Béchar","09-BLIDA","10-BOUIRA","11-Tamanghasset","12-Tébessa","13-TLEMCEN","14-TIARET","15-TIZI OUZOU","16.1-ALGER Bir Mourad Rais","16.2-ALGER Dar el Beida","16.3-ALGER Hussein dey","17-DJELFA","18-JIJEL","19-Sétif","20-Saïda","21-SKIKDA","22-Sidi Bel Abbès","23-ANNABA","24-GUELMA","25-CONSTANTINE","26-Médéa","27-MOSTAGANEM","28-Msila","29-MASCARA","30-OUARGLA","31-ORAN","32-EL BAYADH","33-ILLIZI","34-Bordj Bou Arréridj","35-Boumerdès","36-EL TARF","37-TINDOUF","38-TISSEMSILT","39-EL OUED","40-KHENCHELA","41-Souk Ahras","42-TIPAZA","43-MILA","44-Aïn Defla","45-NAAMA","46-Aïn Témouchent","47-Ghardaïa","48-RELIZANE")
ww48=c("01-ADRAR","02-CHLEF","03-LAGHOUAT","04-Oum el Bouaghi","05-BATNA","06-Béjaïa","07-BISKRA","08-Béchar","09-BLIDA","10-BOUIRA","11-Tamanghasset","12-Tébessa","13-TLEMCEN","14-TIARET","15-TIZI OUZOU","16 ALGER","17-DJELFA","18-JIJEL","19-Sétif","20-Saïda","21-SKIKDA","22-Sidi Bel Abbès","23-ANNABA","24-GUELMA","25-CONSTANTINE","26-Médéa","27-MOSTAGANEM","28-Msila","29-MASCARA","30-OUARGLA","31-ORAN","32-EL BAYADH","33-ILLIZI","34-Bordj Bou Arréridj","35-Boumerdès","36-EL TARF","37-TINDOUF","38-TISSEMSILT","39-EL OUED","40-KHENCHELA","41-Souk Ahras","42-TIPAZA","43-MILA","44-Aïn Defla","45-NAAMA","46-Aïn Témouchent","47-Ghardaïa","48-RELIZANE")


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

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
            textOutput("text1"),
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

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
