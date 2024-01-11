#CHeck if dependencies are installed
if (!requireNamespace("shiny", quietly = TRUE)) {
  install.packages("shiny")
}
if (!requireNamespace("shinydashboard", quietly = TRUE)) {
  install.packages("shinydashboard")
}


#load libraries
library(shiny)
library(shinydashboard)



#UI-definition
ui <- dashboardPage(
  dashboardHeader(title = "R Tools Collection"),
  dashboardSidebar(),
  dashboardBody(
    column(4,
           numericInput("number1", "c(CBD) [mg/ml]: ", value = 0),
           numericInput("number2", "V(CBD) [µl]: ", value = 0), 
           
           actionButton("submitBtn", "Submit")
           
    ),
    column(8,
           radioButtons(
             "select1", "Select Dye:", choices = c("Atto550", "FITC"), selected = "Atto550"
           ),
           verbatimTextOutput("result")
    )
  )
)

#server def
server <- function(input, output, session) {
  observeEvent(input$submitBtn, {
    
    num1 <- as.numeric(input$number1)
    num2 <- as.numeric(input$number2)
    
    selDye <- input$select1
    
    #update dye parameters
    if (selDye == "Atto550") {
      mw <- 716 #dalton
      c <- 5000 #µg/ml
      excess <- 1.3 #molar ecxess of dye
    } else if (selDye == "FITC") {
      mw <- 427.4 #dalton
      c <- 4266 #µg/ml
      excess <- 25 #molar ecxess of dye
    }
    
    if (!is.na(num1) && !is.na(num2)) {
      
      CBD <- num1/28000
      CBD <- CBD * 1000000
      CBD <- CBD * num2
      Dye <- CBD * excess
      Dye <- (Dye * mw) / 1000000
      Dye <- (Dye/c) * 1000
      if (Dye > 1) {
        output$result <- renderPrint(
          cat("Add ", Dye, "µl dye.")
      } else if (Dye < 1) {
        Dye <- Dye * 10
        output$result <- renderPrint(
          cat("Concenctration of Protein is too high. Dilute Protein 10 fold and add ", Dye, "µl dye.")
        )
        
      }
      
    }
    else {
      output$result <- renderText("Please enter valid numbers.")
    }
  })
}

#run the app
shinyApp(ui, server)