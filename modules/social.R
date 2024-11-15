socialSciencesUI <- function(id) {
  ns <- NS(id)
  tagList(
    h5("In progress... take a look at some of the other examples!"),
  )
}

socialSciencesServer <- function(id, analysis_type) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    values <- reactiveValues()
    
  })
    
}