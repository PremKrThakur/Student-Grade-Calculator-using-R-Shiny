# Shiny = a way to turn your R code into a website

library(shiny)

# ui = what the USER SEES  (buttons, dropdowns, charts)
# server = what the APP DOES   (calculations, making charts)
# shinyApp(ui, server) = STARTS the app

ui <- fluidPage(      # fluidPage = a blank webpage. Everything goes inside here.

  titlePanel("My First Shiny App — Student Grade Calculator"), # titlePanel = big title at the top of your page

  sidebarLayout(   # sidebarLayout = splits page into LEFT sidebar + RIGHT main area


    sidebarPanel(  # LEFT SIDE — user puts their inputs here
      textInput(
        inputId = "student_name",
        label   = "Enter Student Name:",
        value   = "Prem"
      ),

      numericInput(
        inputId = "score_math",
        label   = "Math Score (out of 100):",
        value   = 75,    # default value
        min     = 0,     # minimum allowed
        max     = 100    # maximum allowed
      ),

      numericInput(
        inputId = "score_english",
        label   = "English Score (out of 100):",
        value   = 80,
        min     = 0,
        max     = 100
      ),

      numericInput(
        inputId = "score_science",
        label   = "Science Score (out of 100):",
        value   = 70,
        min     = 0,
        max     = 100
      ),

      actionButton(
        inputId = "btn_calculate",
        label   = "Calculate Grade",
        style   = "background-color:#4CAF50; color:white; width:100%; margin-top:10px; written here for future use"
      )

    ),

    mainPanel(

      h3("Results"),
      textOutput("result_grade"),

      br(),

      plotOutput("result_chart", height = "300px")

    ) # mainPanel ends here

  ) # sidebarLayout ends here

) # fluidPage ends here

# server is always a function with 3 arguments: input, output, session
# input  = everything the USER typed or selected
# output = everything WE want to SHOW the user
# session = technical stuff (ignore for now)

server <- function(input, output, session) {

# observeEvent() means: "Watch the button. When user CLICKS it, run the code inside { }"
  
  observeEvent(input$btn_calculate, {
    name    <- input$student_name
    math    <- input$score_math
    english <- input$score_english
    science <- input$score_science

    average <- (math + english + science) / 3
    average <- round(average, 1)   # round to 1 decimal place

    grade <- if (average >= 90) {
      "A — Excellent!"
    } else if (average >= 80) {
      "B — Good!"
    } else if (average >= 70) {
      "C — Average"
    } else if (average >= 60) {
      "D — Needs Improvement"
    } else {
      "F — Please Study More"
    }

    output$result_grade <- renderText({
      paste0(
        "Student: ", name, "  |  ",
        "Average: ", average, "%  |  ",
        "Grade: ", grade
      )
    })

    output$result_chart <- renderPlot({

      scores_df <- data.frame(
        subject = c("Math", "English", "Science"),
        score   = c(math, english, science)
      )
      barplot(
        height = scores_df$score,         # heights of the bars
        names.arg = scores_df$subject,    # labels under each bar
        col    = c("#4e79a7", "#f28e2b", "#59a14f"),  # colors
        ylim   = c(0, 100),               # y axis from 0 to 100
        main   = paste0(name, "'s Scores"),  # chart title
        ylab   = "Score",                 # y axis label
        border = NA                       # no border on bars
      )

      abline(h = 75, col = "red", lty = 2, lwd = 2)

      text(x = 0.7, y = 63, "Pass Mark", col = "red", cex = 0.9)
    })

  }) 
}

shinyApp(ui = ui, server = server)


#  1. library(shiny)              load shiny
#  2. ui <- fluidPage(...)        design the page
#  3. textInput / numericInput    get info FROM user
#  4. textOutput / plotOutput     show info TO user
#  5. server <- function(input, output, session)  logic
#  6. input$id                    read user's value
#  7. output$id <- renderText()   send text to screen
#  8. output$id <- renderPlot()   send chart to screen
#  9. observeEvent(input$btn, {}) react to button click
#  10. shinyApp(ui, server)       start the app


#  Change 1: Change the title to your own name
#            Find: titlePanel("My First Shiny App...")
#            Try:  titlePanel("Prem's Grade Calculator")
#
#  Change 2: Add a 4th subject — add numericInput for "History"
#            Then add it to the average calculation
#
#  Change 3: Change the pass mark line from 60 to 50
#            Find: abline(h = 60, .....)
#            Try:  abline(h = 50, ....)
