library(shiny)
library(httr)


authUI <- function() {
  fluidPage(
    # Application title
    tags$head(
      
      tags$style(HTML("
    #login {
    font-size: 24px;
    text-align: left;
    position: absolute;
    top: 30%;
    left: 95%;
}

#home {
    font-size: 24px;
    text-align: left;
    position: absolute;
    top: 80%;
    left: 45%;
}

    ")),
      
      
      tags$title("Login")
    ),
    div(
      id = "login",
      wellPanel(
        textInput("email", "Email"),
        passwordInput("password", "Password"),
        tags$br(),
        actionButton("loginButton", "Log in"),
        actionButton("signupButton", "Sign up")
      )
    ),
    div(id = "authStatus",
        h3(textOutput("status")),
        br(),
        h3(textOutput("body")))
  )
}
mainUI <- function() {
  fluidPage(
    
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
        plotOutput("distPlot")
      )
    )
  )
  
  }

ui <- (htmlOutput("screen"))


server <- function(input, output) {
  isAuth <- reactiveVal(value = FALSE)
  observe({
    if (isAuth() == FALSE) {
      output$screen <- renderUI({
        div(do.call(bootstrapPage, c("", authUI())))
      })
    } else if (isAuth() == TRUE) {
      output$screen <- renderUI({
        div(do.call(bootstrapPage, c("", mainUI())))
      })
    }
  })
  
  
  
  # LOGIN
  observeEvent(input$loginButton, {
    authEmail <- input$email
    authPassword <- input$password
    response <-
      POST(
        "https://identitytoolkit.googleapis.com/v1/accounts:signInWithPassword?key=AIzaSyC66rZ0wR0enwFsThvIA6BtQOdR2mVOD48",
        body = list(
          email = toString(authEmail),
          password = toString(authPassword),
          returnSecureToken = TRUE
        ),
        encode = "json"
      )
    status <- http_status(response)$category
    body <- content(response, "text")
    output$status <- renderText(status)
    output$body <- renderText(body)
    if (status == 'Success' &&
        gregexpr(pattern = '"idToken"', text = body) > 0) {
      isAuth(TRUE)
      
    } else {
      print(status)
      print(body)
      isAuth(FALSE)
    }
    
  })
  
  # SIGNUP
  observeEvent(input$signupButton, {
    authEmail <- input$email
    authPassword <- input$password
    response <-
      POST(
        "https://identitytoolkit.googleapis.com/v1/accounts:signUp?key=AIzaSyC66rZ0wR0enwFsThvIA6BtQOdR2mVOD48",
        body = list(
          email = toString(authEmail),
          password = toString(authPassword),
          returnSecureToken = TRUE
        ),
        encode = "json"
      )
    status <- http_status(response)$category
    body <- content(response, "text")
    output$status <- renderText(status)
    output$body <- renderText(body)
    if (status == 'Success' &&
        gregexpr(pattern = '"idToken"', text = body) > 0) {
      print(status)
      print(body)
      isAuth(TRUE)
      
    } else {
      print(status)
      print(body)
      start <-
        gregexpr(pattern = '"message"', text = body)[[1]][1] + 11
      end <- start + 30
      errorMessage <- substr(body, start, end)
      message <-
        paste('Not successfull Authentication with error',
              errorMessage,
              sep = " -> ")
      isAuth(FALSE)
      output$message <- renderText(message)
    }
    
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
