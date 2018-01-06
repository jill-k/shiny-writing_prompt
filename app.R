library(shiny)
library(shinyjs)
library(shinythemes)
require(dplyr)
require(readr)

prompts <- c("What is your favorite animal and why?", "What was the best thing you did last weekend?",
            "What is your favorite food and when did you first try it?", "Describe the best day of your life (real or imagined).",
            "Describe the most memorable dream you've ever had.", "What is your favorite kind of weather?  Why?")

ui <- fluidPage(theme = shinytheme("yeti"),
  useShinyjs(),
  tags$head(
    tags$style(HTML(
      "label { font-size:120%; }"
    ))
    ),
  titlePanel(windowTitle="Write Now", fluidRow(align="center", strong("Write Now"))),
  fluidRow(align="center", actionButton(inputId = 'click', label = "Click for a topic!",  
         style="color: #fff; background-color: #337ab7")),
  fluidRow(align="center", h4(textOutput('prompt'))),
  fluidRow(align="center", hidden(actionButton(inputId = 'confirm', label = "Choose this one"))),
  fluidRow(hr()),
  fluidRow(column(width=12, h4(textOutput('confirmedprompt')))),
  fluidRow(column(width=12, hidden(textAreaInput(inputId = 'response', label = "", value = '', placeholder = "What do you think?",
                width = '1050px',
                height = '350px')))),
  fluidRow(column(width=12, hidden(downloadButton("report", "Download my response", 
                              style="color: #fff; background-color: #337ab7"))))
)

server <- function(input, output, session) {
  
  observe({
    req(input$click)

    updateActionButton(session, "click",
                       label = "Click for a different topic!")})
  
  p <- reactiveValues()
  
  observeEvent(input$click,{
    p$choice <- sample(prompts, 1)
    show("confirm")
  })
  
  observeEvent(input$confirm,{
    p$confirmed <- p$choice
    show("response")
    show("report")
    hide("click")
    hide("prompt")
    hide("confirm")
  })
  
  output$prompt <- renderText(p$choice)
  output$confirmedprompt <- renderText(p$confirmed)
  
  output$report <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = "report.html",
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "report.Rmd")
      file.copy("report.Rmd", tempReport, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      params <- list(response = input$response,
                     prompt = p$choice)
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )
}

shinyApp(ui = ui, server = server)