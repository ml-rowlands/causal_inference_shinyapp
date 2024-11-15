library(shiny)
library(ggplot2)
library(ggdag)
library(igraph)
library(simDAG)
library(brms)
library(bayesplot)
library(dagitty)

epiUI <- function(id) {
  ns <- NS(id)
  tagList(
    h5("Epidemiology Example: Alzheimer's Education Program"),
    p("Suppose we are evaluating the effect of an Alzheimer's education program (Program) on awareness of Alzheimer's symptoms (Awareness)."),
    actionButton(ns("epi_step1"), "Step 1: Understand the Scenario")
  )
}

epiServer <- function(id, analysis_type) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    values <- reactiveValues()
    
    # Step 1: Understand the Scenario
    observeEvent(input$epi_step1, {
      showModal(modalDialog(
        title = "Step 1: Understanding the Scenario",
        p("In this example, we want to evaluate whether an Alzheimer's education program (Program) increases awareness of Alzheimer's symptoms (Awareness)."),
        p("However, participants' age (Age) may influence both their likelihood of joining the program and their awareness of symptoms."),
        p("This creates a confounding effect that we need to account for."),
        footer = tagList(
          modalButton("Close"),
          actionButton(ns("epi_step2"), "Proceed to Step 2")
        ),
        easyClose = FALSE
      ))
    })
    
    # Step 2: Simulate the Data
    observeEvent(input$epi_step2, {
      removeModal()
      showModal(modalDialog(
        title = "Step 2: Simulate the Data",
        p("We will now simulate data representing this situation, where the true direct effect of the program on awareness is a 5 point increase in awarness but is confounded by a positive effect of age on program participation."),
        actionButton(ns("epi_simulate_data"), "Simulate Data"),
        footer = modalButton("Close"),
        easyClose = FALSE
      ))
    })
    
    # Simulate the data
    observeEvent(input$epi_simulate_data, {
      removeModal()
      # Create an empty DAG
      dag_obj <- empty_dag()
      
      # Define nodes
      dag_obj <- dag_obj +
        node(name = "Age", type = "rnorm", mean = 45, sd = 15) +
        node(name = "Program", type = "binomial", parents = "Age", betas = 0.05, intercept = 1) +
        node(name = "Awareness", type = "gaussian", parents = c("Program", "Age"), betas = c(5, 0.3), intercept = 20, error = 3)
      
      # Simulate data
      set.seed(123)
      data <- sim_from_dag(dag_obj, n_sim = 1000)
      
      # Convert binary Program variable to numeric if necessary
      data$Program <- as.numeric(data$Program)
      
      # Store the data in reactive values for use in other steps
      values$epi_data <- data
      values$epi_dag_obj <- dag_obj
      
      # Show message and proceed to next step
      showModal(modalDialog(
        title = "Data Simulation Complete",
        p("The data has been simulated based on the specified DAG."),
        p("Notice that there may appear to be a stronger than 5 point relationship between Program and Awareness before accounting for confounders."),
        footer = tagList(
          modalButton("Close"),
          actionButton(ns("epi_step3"), "Proceed to Step 3")
        ),
        easyClose = FALSE
      ))
    })
    
    # Step 3: Visualize the DAG and Data
    observeEvent(input$epi_step3, {
      removeModal()
      
      # Render DAG plot
      output$epi_dag_plot <- renderPlot({
        req(values$epi_dag_obj)
        adj_matrix <- dag2matrix(values$epi_dag_obj)
        g <- graph_from_adjacency_matrix(adj_matrix, mode = "directed")
        plot(g, vertex.label = V(g)$name, main = "DAG Representing Situation",
             edge.arrow.size = 0.5)
      })
      
      # Render data plot
      output$epi_data_plot <- renderPlot({
        req(values$epi_data)
        ggplot(values$epi_data, aes(x = factor(Program), y = Awareness)) +
          geom_jitter(width = 0.2, height = 0.2, alpha = 0.5) +
          labs(title = "Program Participation vs Awareness",
               x = "Program Participation", y = "Awareness Score")
      })
      
      showModal(modalDialog(
        title = "Step 3: Visualize the DAG and Data",
        p("Here is the DAG representing the causal relationships in our scenario:"),
        plotOutput(ns("epi_dag_plot")),
        p("Let's also look at the relationship between Program participation and Awareness:"),
        plotOutput(ns("epi_data_plot")),
        footer = tagList(
          modalButton("Close"),
          actionButton(ns("epi_step4"), "Proceed to Step 4")
        ),
        easyClose = FALSE,
        size = "l"
      ))
    })
    
    # Step 4: Run the Unadjusted Model
    observeEvent(input$epi_step4, {
      removeModal()
      showModal(modalDialog(
        title = "Step 4: Run the Unadjusted Model",
        p("We will now run a regression model to see the effect of the Program on Awareness without adjusting for confounders."),
        actionButton(ns("epi_run_unadjusted"), "Run Unadjusted Model"),
        footer = modalButton("Close"),
        easyClose = FALSE
      ))
    })
    
    # Run unadjusted model
    observeEvent(input$epi_run_unadjusted, {
      removeModal()
      data <- values$epi_data
      
      # Run Frequentist or Bayesian regression
      if (analysis_type() == "Bayesian") {
        fit_unadjusted <- brm(Awareness ~ Program, data = data, family = gaussian(), silent = TRUE)
      } else {
        fit_unadjusted <- lm(Awareness ~ Program, data = data)
      }
      
      values$fit_unadjusted <- fit_unadjusted
      
      # Display regression results
      output$epi_unadjusted_results <- renderTable({
        req(values$fit_unadjusted)
        if (analysis_type() == "Frequentist") {
          summary(values$fit_unadjusted)$coefficients
        } else {
          as.data.frame(fixef(values$fit_unadjusted))
        }
      }, rownames = TRUE)
      
      # Plot posterior distributions for unadjusted model (Bayesian only)
      output$epi_unadjusted_posterior <- renderPlot({
        req(values$fit_unadjusted)
        posterior <- as_draws_df(values$fit_unadjusted)
        mcmc_areas(posterior, pars = c("b_Intercept", "b_Program"))
      })
      
      # Set suspendWhenHidden = FALSE
      outputOptions(output, "epi_unadjusted_results", suspendWhenHidden = FALSE)
      outputOptions(output, "epi_unadjusted_posterior", suspendWhenHidden = FALSE)
      
      showModal(modalDialog(
        title = "Unadjusted Model Results",
        p("Here are the results of the unadjusted model. Notice how the coefficient on Program is biased due to confounding."),
        tableOutput(ns("epi_unadjusted_results")),
        if (analysis_type() == "Bayesian") {
          tagList(
            p("Posterior distributions of the coefficients. Notice how almost all of the posterior mass of the coefficent on Program is larger than 1. This shows us what can happen if we ignore the effect of Education on both Program participation and Employment:"),
            plotOutput(ns("epi_unadjusted_posterior"))
          )
        },
        footer = tagList(
          modalButton("Close"),
          actionButton(ns("epi_step5"), "Proceed to Step 5")
        ),
        easyClose = FALSE,
        size = "l"
      ))
    })
    
    # Step 5: Run the Adjusted Model
    observeEvent(input$epi_step5, {
      removeModal()
      showModal(modalDialog(
        title = "Step 5: Run the Adjusted Model",
        p("Now, let's adjust for Age to see the unbiased effect of Program on Awareness."),
        actionButton(ns("epi_run_adjusted"), "Run Adjusted Model"),
        footer = modalButton("Close"),
        easyClose = FALSE
      ))
    })
    
    # Run adjusted model
    observeEvent(input$epi_run_adjusted, {
      removeModal()
      data <- values$epi_data
      
      # Run Frequentist or Bayesian regression
      if (analysis_type() == "Bayesian") {
        fit_adjusted <- brm(Awareness ~ Program + Age, data = data, family = gaussian(), silent = TRUE)
      } else {
        fit_adjusted <- lm(Awareness ~ Program + Age, data = data)
      }
      
      values$fit_adjusted <- fit_adjusted
      
      # Display regression results
      output$epi_adjusted_results <- renderTable({
        req(values$fit_adjusted)
        if (analysis_type() == "Frequentist") {
          summary(values$fit_adjusted)$coefficients
        } else {
          as.data.frame(fixef(values$fit_adjusted))
        }
      }, rownames = TRUE)
      
      # Plot posterior distributions for adjusted model (Bayesian only)
      output$epi_adjusted_posterior <- renderPlot({
        req(values$fit_adjusted)
        posterior <- as_draws_df(values$fit_adjusted)
        mcmc_areas(posterior, pars = c("b_Intercept", "b_Program", "b_Age"))
      })
      
      # Set suspendWhenHidden = FALSE
      outputOptions(output, "epi_unadjusted_results", suspendWhenHidden = FALSE)
      outputOptions(output, "epi_unadjusted_posterior", suspendWhenHidden = FALSE)
      
      showModal(modalDialog(
        title = "Adjusted Model Results",
        p("Here are the results of the adjusted model. Notice how the coefficient on Program is more accurate after accounting for Age."),
        tableOutput(ns("epi_adjusted_results")),
        if (analysis_type() == "Bayesian") {
          tagList(
            p("Posterior distributions of the coefficients. We see that we do recover the correct results of a coefficent of around 1 on the effect of Program on Employment:"),
            plotOutput(ns("epi_adjusted_posterior"))
          )
        },
        footer = tagList(
          modalButton("Close"),
          actionButton(ns("epi_step6"), "Compare Models")
        ),
        easyClose = FALSE,
        size = "l"
      ))
    })
    
    # Step 6: Compare Models
    observeEvent(input$epi_step6, {
      removeModal()
      
      # Compare unadjusted and adjusted models
      output$epi_compare_results <- renderTable({
        req(values$fit_unadjusted, values$fit_adjusted)
        if (analysis_type() == "Frequentist") {
          unadjusted <- summary(values$fit_unadjusted)$coefficients
          adjusted <- summary(values$fit_adjusted)$coefficients
          
          compare_table <- data.frame(
            Model = c("Unadjusted", "Adjusted"),
            Program.Estimate = c(unadjusted["Program", "Estimate"], adjusted["Program", "Estimate"]),
            Std_Error = c(unadjusted["Program", "Std. Error"], adjusted["Program", "Std. Error"])
          )
        } else {
          unadjusted <- as.data.frame(fixef(values$fit_unadjusted))
          adjusted <- as.data.frame(fixef(values$fit_adjusted))
          
          compare_table <- data.frame(
            Model = c("Unadjusted", "Adjusted"),
            Program.Estimate = c(unadjusted["Program", "Estimate"], adjusted["Program", "Estimate"]),
            Est_Error = c(unadjusted["Program", "Est.Error"], unadjusted["Program", "Est.Error"])
          )
        }
        
        compare_table
      }, rownames = FALSE)
      
      showModal(modalDialog(
        title = "Compare Models",
        p("Let's compare the results of the unadjusted and adjusted models to see the impact of accounting for Age."),
        tableOutput(ns("epi_compare_results")),
        div(style = "overflow-x: auto; max-height: 400px;",
            tableOutput(ns("epi_compare_results"))
        ),
        p("We see that the model that doesn't account for the confounder of age returns the incorrect result that participation in the program leads to a", round(unadjusted["Program", "Estimate"],2), "point increase in Awareness score on average, while the model that 
          correctly adjusts for the confounder of age returns a", round(adjusted["Program", "Estimate"],2), "point increase."),
        p(strong('BUT'), "this does", strong("NOT"), "mean that all you need to do is adjust for every avaliable variable! Keep learning to find out what can go wrong if you do."),
        footer = modalButton("Close"),
        easyClose = FALSE,
        size = "l"
      ))
    })
    
  })
}


    