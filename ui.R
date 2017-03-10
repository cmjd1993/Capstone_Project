

library(shiny)
library(shinythemes)

# Define UI for application with theme as "superhero"
shinyUI(fluidPage( theme = shinytheme("superhero"),
  
  # Application title
  titlePanel("Next Word Predcition"),
  
  # Sidebar with a text input section
  sidebarLayout(
    sidebarPanel(
            textInput("String", "Enter a partial sentence here",value = ""),
      submitButton("Next Word")
    ),
    
    # create text outputs of next word
    mainPanel(
      h4("Next Word Predicted is..."),
      h4(textOutput("text1")),
      textOutput("text2"),
      textOutput("text3")
    )
  )
))
