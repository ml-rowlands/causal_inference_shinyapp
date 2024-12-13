# server.R

library(shiny)
library(ggplot2)
library(ggdag)
library(igraph)
library(simDAG)
library(brms)
library(bayesplot)
library(dagitty)

# Source module server functions
source("modules/sandbox_module.R")
source("modules/economics.R")
source("modules/epi.R")
source("modules/social.R")


# Define the server logic
shinyServer(function(input, output, session) {
  
  # Reactive values to store user's information
  user_name <- reactiveVal()
  user_field <- reactiveVal()
  analysis_type <- reactiveVal("Frequentist")  # Default to Frequentist analysis
  
  # Initialize 'values' at the top level
  values <- reactiveValues()
  
  # Show the initial modal to collect the user's name and analysis preference
  observeEvent(TRUE, {
    showModal(modalDialog(
      title = "Welcome to the DAG Learning Tool",
      textInput("user_name_input", "Please enter your name:", value = ""),
      selectInput("analysis_type_input", "Choose your preferred analysis type:",
                  choices = c("Frequentist", "Bayesian")),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("submit_name", "Next")
      ),
      easyClose = FALSE,
      fade = TRUE
    ))
  }, once = TRUE)
  
  # When the user submits their name and analysis type
  observeEvent(input$submit_name, {
    if (nzchar(input$user_name_input)) {
      user_name(input$user_name_input)
      analysis_type(input$analysis_type_input)
      removeModal()
      
      # Now show the second modal to ask for the field of interest
      showModal(modalDialog(
        title = "Tell us about your interests",
        selectInput("user_field_input", "What field are you interested in?",
                    choices = c("Economics", "Biostatistics/Epidemiology",
                                "Social Sciences")),
        footer = tagList(
          modalButton("Cancel"),
          actionButton("submit_field", "Start Learning")
        ),
        easyClose = FALSE,
        fade = TRUE
      ))
    } else {
      showNotification("Please enter your name.", type = "error")
    }
  })
  
  # When the user submits their field of interest
  observeEvent(input$submit_field, {
    if (!is.null(input$user_field_input)) {
      user_field(input$user_field_input)
      removeModal()
      # Navigate to the Introduction to DAGs page
      updateTabsetPanel(session, inputId = "navbar", selected = "Introduction to DAGs")
    } else {
      showNotification("Please select a field of interest.", type = "error")
    }
  })
  
  # Introduction to DAGs content with Simpson's Paradox examples
  # Introduction to DAGs content with Simpson's Paradox examples
  output$intro_content <- renderUI({
    req(user_name(), user_field())
    tagList(
      h3(paste("Welcome,", user_name(), "! Let's Learn About Directed Acyclic Graphs (DAGs)")),
      p("In the world of data analysis and statistics, understanding the relationships between variables is crucial. Sometimes, these relationships are not straightforward due to the presence of confounding variables."),
      h4("What is a DAG?"),
      p("A Directed Acyclic Graph (DAG) is a visual representation that helps us understand and analyze the relationships and causal structures between different variables. It consists of nodes (which represent variables) and directed edges (which represent causal effects from one variable to another). The term 'acyclic' means that the graph doesn't contain any loops; you cannot start at one node and follow a path that leads back to the same node."),
      img(src = "https://evalf20.classes.andrewheiss.com/example/dags_files/figure-html/simple-status-1.png", height = "300px", alt = "Simple DAG Example"),
      p("DAGs are powerful tools in", strong(user_field()), "because they allow us to:"),
      tags$ul(
        tags$li("Visualize complex relationships between variables."),
        tags$li("Identify potential confounding variables."),
        tags$li("Determine the appropriate variables to adjust for in statistical models to estimate causal effects.")
      ),
      h4("Simpson's Paradox: An Illustrative Example"),
      p("Simpson's Paradox occurs when a trend that appears in different groups of data disappears or reverses when the groups are combined. This paradox highlights the importance of considering underlying variables that may affect the observed relationships."),
      p("Let's explore how DAGs can help us understand and resolve Simpson's Paradox in the context of", strong(user_field()), "."),
      actionButton("start_example", "Explore the Example")
    )
  })
  
  # Observe the action button to start the example
  observeEvent(input$start_example, {
    # Depending on the user's field, render the appropriate module UI
    if (user_field() == "Economics") {
      output$example_content <- renderUI({
        economicsUI("economics")
      })
    } else if (user_field() == "Biostatistics/Epidemiology") {
      output$example_content <- renderUI({
        epiUI("epi")
      })
    } else if (user_field() == "Social Sciences") {
      output$example_content <- renderUI({
        socialSciencesUI("social")
      })
    } else {
      # For other fields or default case
      output$example_content <- renderUI({
        p("Examples for your selected field are under development.")
      })
    }
  })
  
  # Call the module server functions based on the user's field
  observe({
    req(user_field())
    if (user_field() == "Economics") {
      economicsServer("economics", analysis_type = analysis_type)
    } else if (user_field() == "Biostatistics/Epidemiology") {
      epiServer("epi", analysis_type = analysis_type)
    } else if (user_field() == "Social Sciences") {
      socialSciencesServer("social", analysis_type = analysis_type)
    }
  })
  
  output$dag_structures_content <- renderUI({
    tagList(
      h3("Understanding Common DAG Structures"),
      p("In this section, we'll explore common DAG structures and see how accounting for different variables affects our statistical analysis."),
      br(),
      selectInput("dag_type", "Choose a DAG structure to explore:",
                  choices = c("Confounder", "Collider", "Mediator/Chain")),
      actionButton("dag_explore", "Explore DAG"),
      br(), br(),
      uiOutput("dag_description"),
      plotOutput("dag_plot"),
      br(),
      actionButton("simulate_data", "Simulate Data"),
      br(), br(),
      uiOutput("modeling_content")
    )
  })
  
  # Reactive values for DAG exploration
  dag_values <- reactiveValues()
  
  # Observe when the user clicks "Explore DAG"
  observeEvent(input$dag_explore, {
    dag_type <- input$dag_type
    dag_values$dag_type <- dag_type
    
    # Generate the appropriate DAG and description
    if (dag_type == "Confounder") {
      dag_values$dag <- graph.formula(X -+ Y, Z -+ X, Z -+ Y)
      dag_values$description <- tagList(
        h4("Confounder DAG"),
        p("In this DAG, Z is a confounder that affects both X and Y. If you want to understand the \"direct effect\" of X on Y, you need to account for Z in your model. The example in the Introduction to DAGs tab shows an example of confounding.")
      )
    } else if (dag_type == "Collider") {
      dag_values$dag <- graph.formula(X -+ Z, Y -+ Z)
      dag_values$description <- tagList(
        h4("Collider DAG"),
        p("In this DAG, Z is a collider that is influenced by both X and Y. If you want to understand the effect of X on Y and accidentally include a collider in your model, you will induce a spurious negative effect of X on Y! An example of a collider is:"),
        tags$ul(
          tags$li("X: Genetic predisposition to a disease"),
          tags$li("Y: Environmental exposure (e.g., pollution)"),
          tags$li("Collider (Z): Diagnosis of the disease")
        ),
        p("There should be no relationship between genetic predispostition and environmental exposure, but if you conditon on disease status you will incorrectly infer that there is a negative relationship between genetic predistposition and environmental exposure. This is a good reason to not just include every variable in your regression!")
      )
    } else if (dag_type == "Mediator/Chain") {
      dag_values$dag <- graph.formula(X -+ Z, Z -+ Y)
      dag_values$description <- tagList(
        h4("Mediator DAG"),
        p("In this DAG, Z is a mediator between X and Y. This DAG says that X affects Y only through Z. If we control for Z in our regression, then we will not see the direct effect of X on Y (this may or may not be what you want to see)! An example of a mediator is:"),
        tags$ul(
          tags$li("Initial Cause (X): Level of education"),
          tags$li("Mediator (Z): Income level"),
          tags$li("Outcome (Y): Health status")
        ),
      )
    }
    
    # Update the UI outputs
    output$dag_description <- renderUI({
      dag_values$description
    })
    
    output$dag_plot <- renderPlot({
      plot(dag_values$dag, vertex.size = 30, vertex.label.cex = 1.5,
           edge.arrow.size = 0.5, main = paste(dag_type, "DAG"))
    })
    
    # Clear previous modeling content
    output$modeling_content <- renderUI({})
  })
  
  # Observe when the user clicks "Simulate Data"
  observeEvent(input$simulate_data, {
    req(dag_values$dag_type)
    dag_type <- dag_values$dag_type
    
    # Simulate data based on the selected DAG
    if (dag_type == "Confounder") {
      set.seed(123)
      n <- 1000
      Z <- rnorm(n)
      X <- 0.5 * Z + rnorm(n)
      Y <- 0.5 * X + 0.5 * Z + rnorm(n)
      data <- data.frame(X, Y, Z)
      dag_values$data <- data
    } else if (dag_type == "Collider") {
      set.seed(123)
      n <- 1000
      X <- rnorm(n)
      Y <- rnorm(n)
      Z <- 0.5 * X + 0.5 * Y + rnorm(n)
      data <- data.frame(X, Y, Z)
      dag_values$data <- data
    } else if (dag_type == "Mediator/Chain") {
      set.seed(123)
      n <- 1000
      X <- rnorm(n)
      Z <- 0.5 * X + rnorm(n)
      Y <- 0.5 * Z + rnorm(n)
      data <- data.frame(X, Y, Z)
      dag_values$data <- data
    } else if (dag_type == "Fork") {
      # Fork is similar to confounder in terms of data generation
      set.seed(123)
      n <- 1000
      Z <- rnorm(n)
      X <- 0.5 * Z + rnorm(n)
      Y <- 0.5 * Z + rnorm(n)
      data <- data.frame(X, Y, Z)
      dag_values$data <- data
    } else if (dag_type == "Chain") {
      # Chain is similar to mediator in terms of data generation
      set.seed(123)
      n <- 1000
      X <- rnorm(n)
      Z <- 0.5 * X + rnorm(n)
      Y <- 0.5 * Z + rnorm(n)
      data <- data.frame(X, Y, Z)
      dag_values$data <- data
    }
    
    # Update the modeling content
    output$modeling_content <- renderUI({
      tagList(
        h4("Modeling and Analysis"),
        p("We can now analyze how including or excluding variable Z affects the estimated relationship between X and Y."),
        radioButtons("adjustment", "Include Z in the model?",
                     choices = c("No (Unadjusted Model)", "Yes (Adjusted Model)")),
        actionButton("run_model", "Run Model"),
        br(), br(),
        tableOutput("model_results"),
        plotOutput("scatter_plot")
      )
    })
  })
  
  # Observe when the user clicks "Run Model"
  observeEvent(input$run_model, {
    req(dag_values$data, input$adjustment)
    data <- dag_values$data
    adjustment <- input$adjustment
    
    # Fit the model based on adjustment choice
    if (adjustment == "No (Unadjusted Model)") {
      model <- lm(Y ~ X, data = data)
    } else {
      model <- lm(Y ~ X + Z, data = data)
    }
    
    # Store the model
    dag_values$model <- model
    
    # Display the model results
    output$model_results <- renderTable({
      summary(model)$coefficients
    }, rownames = TRUE)
    
    # Plot X vs. Y with regression line
    output$scatter_plot <- renderPlot({
      ggplot(data, aes(x = X, y = Y)) +
        geom_point(alpha = 0.5) +
        geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "blue") +
        labs(title = "Scatter Plot of X vs. Y",
             x = "X", y = "Y")
    })
  })
  sandboxServer("sandbox", analysis_type = analysis_type)
})
