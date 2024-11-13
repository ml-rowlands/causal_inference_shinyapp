library(shiny)
library(shinyWidgets)  
library(dagitty)
library(ggdag)


# Source module UI functions
source("modules/sandbox_module.R")
source("modules/economics.R")
source("modules/epi.R")
source("modules/social.R")

# Define the UI for the application
shinyUI(
  navbarPage(
    title = "Learn about DAGs",
    id = "navbar",
    
    # Introduction to DAGs
    tabPanel("Introduction to DAGs",
             fluidPage(
               h2("Introduction to Directed Acyclic Graphs"),
               uiOutput("intro_content"),
               uiOutput("example_content")
             )
    ),
    
    # Common DAG Structures
    tabPanel("Common DAG Structures",
             fluidPage(
               h2("Exploring Common DAG Structures"),
               uiOutput("dag_structures_content")
             )
    ),
    
    tabPanel("What to Adjust for?",
             fluidPage(
               h2("Closing Backdoor Paths"),
               uiOutput("")
               )
    ),
    tabPanel("Sandbox", fluidPage(
             sandboxUI("sandbox")
             ))
)
)