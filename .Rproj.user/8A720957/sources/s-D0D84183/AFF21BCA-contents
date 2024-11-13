
library(shiny)
library(simDAG)
library(dagitty)
library(ggplot2)
library(brms)
library(survival)
library(nnet)

# UI function
sandboxUI <- function(id) {
  ns <- NS(id)
  tagList(
    h2("Sandbox: Create Your Own DAG"),
    sidebarLayout(
      sidebarPanel(
        h4("Variables"),
        actionButton(ns("add_variable"), "Add Variable"),
        uiOutput(ns("variables_ui")),
        h4("DAG Structure"),
        uiOutput(ns("dag_structure_ui")),
        h4("Exposure and Outcome"),
        uiOutput(ns("exposure_ui")),
        uiOutput(ns("outcome_ui")),
        actionButton(ns("submit_dag"), "Submit DAG")
      ),
      mainPanel(
        tabsetPanel(
          tabPanel("DAG Plot", plotOutput(ns("dag_plot"))),
          tabPanel("Adjustment Sets",
                   actionButton(ns("compute_adjustment"), "Compute Adjustment Sets"),
                   verbatimTextOutput(ns("adjustment_text"))
          ),
          tabPanel("Simulate Data",
                   actionButton(ns("simulate_data"), "Simulate Data"),
                   p('Head of 1000 row dataset:'),
                   tableOutput(ns("data_table"))
          ),
          tabPanel("Regression",
                   uiOutput(ns("regression_ui")),
                   actionButton(ns("run_regression"), "Run Regression"),
                   tableOutput(ns("regression_table")),
                   plotOutput(ns("regression_plot"))
          
          )
        )
      )
    )
  )
}

# Server function
sandboxServer <- function(id, analysis_type = reactiveVal("Frequentist")) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    rv <- reactiveValues(
      variables = data.frame(Name = character(), Type = character(), stringsAsFactors = FALSE),
      dag = NULL,
      dagitty_dag = NULL,
      data = NULL,
      user_model = NULL,
      correct_model = NULL
    )
    
    # Add Variable Modal
    observeEvent(input$add_variable, {
      showModal(modalDialog(
        title = "Add Variable",
        textInput(ns("modal_var_name"), "Variable Name"),
        selectInput(ns("modal_var_type"), "Variable Type",
                    choices = c("Continuous", "Binary", "Count")),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("confirm_add_var"), "Add Variable")
        ),
        easyClose = TRUE
      ))
    })
    
    # Confirm Add Variable
    observeEvent(input$confirm_add_var, {
      req(input$modal_var_name, input$modal_var_type)
      rv$variables <- rbind(rv$variables, data.frame(Name = input$modal_var_name,
                                                     Type = input$modal_var_type,
                                                     stringsAsFactors = FALSE))
      removeModal()
    })
    
    # Variables UI
    output$variables_ui <- renderUI({
      req(nrow(rv$variables) > 0)
      tagList(
        lapply(rv$variables$Name, function(var) {
          checkboxInput(ns(paste0("include_var_", var)), label = var, value = TRUE)
        })
      )
    })
    
    # DAG Structure UI
    output$dag_structure_ui <- renderUI({
      req(nrow(rv$variables) > 0)
      tagList(
        lapply(rv$variables$Name, function(var) {
          selectizeInput(ns(paste0("parents_of_", var)), label = paste("Parents of", var),
                         choices = rv$variables$Name[rv$variables$Name != var],
                         multiple = TRUE)
        })
      )
    })
    
    # Exposure UI
    output$exposure_ui <- renderUI({
      req(nrow(rv$variables) > 0)
      selectInput(ns("exposure_var"), "Exposure Variable", choices = rv$variables$Name)
    })
    
    # Outcome UI
    output$outcome_ui <- renderUI({
      req(nrow(rv$variables) > 0)
      selectInput(ns("outcome_var"), "Outcome Variable", choices = rv$variables$Name)
    })
    
    # Observe 'Submit DAG' button click
    observeEvent(input$submit_dag, {
      req(nrow(rv$variables) >= 2)
      
      # Print rv$variables to verify its contents
      print("rv$variables content:")
      print(rv$variables)
      
      # Initialize an empty simDAG object
      dag <- simDAG::empty_dag()
      
      # Build simDAG object
      for (var_name in rv$variables$Name) {
        if (is.null(var_name) || var_name == "") {
          warning("Variable name is missing or empty. Skipping this variable.")
          next
        }
        
        print(paste("Processing variable:", var_name))
        
        var_info <- rv$variables[rv$variables$Name == var_name, ]
        parents <- input[[paste0("parents_of_", var_name)]]
        
        # Debugging output to verify parents
        print(paste("Variable:", var_name, "Parents:",
                    ifelse(is.null(parents) || length(parents) == 0, "None", paste(parents, collapse = ", "))))
        
        if (is.null(parents) || length(parents) == 0) {
          # Root node without parents
          if (var_info$Type == "Continuous") {
            dag <- dag + simDAG::node(name = var_name, type = "rnorm", mean = 0, sd = 1)
          } else if (var_info$Type == "Binary") {
            dag <- dag + simDAG::node(name = var_name, type = "rbernoulli", p = 0.5)
          } else if (var_info$Type == "Count") {
            dag <- dag + simDAG::node(name = var_name, type = "rpois", lambda = 1)
          } else {
            stop("Unknown variable type for root node")
          }
        } else {
          # Node with parents
          if (var_info$Type == "Continuous") {
            dag <- dag + simDAG::node(
              name = var_name,
              type = "gaussian",
              parents = as.character(parents),
              betas = rep(1, length(parents)),
              intercept = 0,
              error = 1
            )
          } else if (var_info$Type == "Binary") {
            dag <- dag + simDAG::node(
              name = var_name,
              type = "binomial",
              parents = as.character(parents),
              betas = rep(0.5, length(parents)),
              intercept = 0
            )
          } else if (var_info$Type == "Count") {
            dag <- dag + simDAG::node(
              name = var_name,
              type = "poisson",
              parents = as.character(parents),
              betas = rep(0.1, length(parents)),
              intercept = 0
            )
          } else {
            stop("Unknown variable type for node with parents")
          }
        }
      }
      
      # Save the simDAG object
      rv$dag <- dag
      print("simDAG object created.")
      
      # Build dagitty DAG directly from variable names and parent relationships
      dag_string <- "dag { "
      
      # Collect all variables to ensure they are included
      all_vars <- rv$variables$Name
      
      # Build the DAG string
      for (var_name in all_vars) {
        parents <- input[[paste0("parents_of_", var_name)]]
        
        # Debugging output
        print(paste("Variable:", var_name, "Parents:",
                    ifelse(is.null(parents) || length(parents) == 0, "None", paste(parents, collapse = ", "))))
        
        # Add edges to dag_string
        if (!is.null(parents) && length(parents) > 0) {
          for (parent in parents) {
            dag_string <- paste0(dag_string, parent, " -> ", var_name, " ; ")
          }
        } else {
          # No parents; ensure node is included
          dag_string <- paste0(dag_string, var_name, " ; ")
        }
      }
      
      # Close the DAG string
      dag_string <- paste0(dag_string, "}")
      
      # Debugging output
      print(paste("Generated dagitty string:", dag_string))
      
      # Create dagitty DAG
      dagitty_dag <- dagitty::dagitty(dag_string)
      
      # Set exposure and outcome variables
      exposure <- input$exposure_var
      outcome <- input$outcome_var
      
      if (exposure %in% names(dagitty_dag) && outcome %in% names(dagitty_dag)) {
        exposures(dagitty_dag) <- exposure
        outcomes(dagitty_dag) <- outcome
      } else {
        showNotification("Error: Exposure or outcome not found in DAG.", type = "error")
        return()
      }
      
      # Save the dagitty DAG
      rv$dagitty_dag <- dagitty_dag
      print("dagitty DAG created and exposure/outcome set.")
    })
    
    # # Plot the DAG
    output$dag_plot <- renderPlot({
      ggdag(rv$dagitty_dag) + theme_dag()
    })
    
    # Compute Adjustment Sets
    observeEvent(input$compute_adjustment, {
      req(rv$dagitty_dag)
      adj_sets <- dagitty::adjustmentSets(rv$dagitty_dag,
                                          exposure = input$exposure_var,
                                          outcome = input$outcome_var)
      output$adjustment_text <- renderPrint({
        if (length(adj_sets) > 0) {
          adj_sets
        } else {
          "No adjustment needed."
        }
      })
    })
    
    
    # Simulate Data when 'Simulate Data' button is clicked
    observeEvent(input$simulate_data, {
      req(rv$dag)
      
      # Simulate data from the simDAG object
      print("Simulating data from the DAG...")
      set.seed(123)  # For reproducibility
      rv$data <- simDAG::sim_from_dag(rv$dag, n_sim = 1000)  # Simulate 1000 observations
      
      rv$data[] <- lapply(rv$data, function(x) if (is.logical(x)) as.numeric(x) else x)
      
      # Display a notification
      showNotification("Data simulation complete.", type = "message")
      
      # Render the data table (head of the data)
      output$data_table <- renderTable({
        req(rv$data)
        head(rv$data)
      })
    })
    
    # Display selected analysis type
    output$analysis_type_display <- renderText({
      analysis_type()
    })
    
    # Generate UI for variable selection
    output$adjustment_vars_ui <- renderUI({
      req(rv$data)  # Ensure data is available
      adjust_vars <- setdiff(names(rv$data), c(input$exposure_var, input$outcome_var))
      checkboxGroupInput(ns("adjustment_vars"), "Adjustment Variables:",
                         choices = adjust_vars)
    })
    
    # Observe 'Run Regression' button click
    observeEvent(input$run_regression, {
      req(rv$data)
      req(input$exposure_var)
      req(input$outcome_var)
      
      # Determine the correct regression model based on outcome type and analysis type
      outcome_type <- rv$variables$Type[rv$variables$Name == input$outcome_var]
      if (outcome_type == "Continuous") {
        model_family <- gaussian(link = 'identity')
      } else if (outcome_type == "Binary") {
        if (analysis_type() == "Frequentist") {
          model_family <- binomial(link = 'logit')
        } else if (analysis_type() == "Bayesian") {
          model_family <- bernoulli(link = 'logit')
        } else {
          showNotification("Unknown analysis type.", type = "error")
          return()
        }
      } else if (outcome_type == "Count") {
        model_family <- poisson(link = 'log')
      } else {
        showNotification("Unknown outcome variable type.", type = "error")
        return()
      }
      
      
      # Build formula for user's model
      user_adjust_vars <- input$adjustment_vars
      user_formula <- as.formula(paste(input$outcome_var, "~", paste(c(input$exposure_var, user_adjust_vars), collapse = " + ")))
      
      # Build formula for correct model using first adjustment set
      adj_sets <- dagitty::adjustmentSets(rv$dagitty_dag, exposure = input$exposure_var, outcome = input$outcome_var)
      if (length(adj_sets) > 0) {
        correct_adjust_vars <- unlist(adj_sets[[1]])
      } else {
        correct_adjust_vars <- character(0)
      }
      correct_formula <- as.formula(paste(input$outcome_var, "~", paste(c(input$exposure_var, correct_adjust_vars), collapse = " + ")))
      
      # Run user's regression model
      if (analysis_type() == "Frequentist") {
        user_model <- glm(user_formula, data = rv$data, family = model_family)
        correct_model <- glm(correct_formula, data = rv$data, family = model_family)
      } else if (analysis_type() == "Bayesian") {
        # For Bayesian regression, use 'brms' package
        if (!requireNamespace("brms", quietly = TRUE)) {
          showModal(modalDialog(
            title = "Package Required",
            "The 'brms' package is required for Bayesian analysis. Please install it before proceeding.",
            easyClose = TRUE
          ))
          return()
        }
        user_model <- brms::brm(
          formula = user_formula,
          data = rv$data,
          family = model_family,
          chains = 2,       # Adjust number of chains as needed
          iter = 2000,      # Adjust number of iterations as needed
          refresh = 0,      # Suppress sampling progress output
          silent = TRUE,    # Suppress messages
          seed = 123        # For reproducibility
        )
        correct_model <- brms::brm(
          formula = correct_formula,
          data = rv$data,
          family = model_family,
          chains = 2,
          iter = 2000,
          refresh = 0,
          silent = TRUE,
          seed = 123
        )
      } else {
        showNotification("Unknown analysis type.", type = "error")
        return()
      }
      
      # Save models to reactive values
      rv$user_model <- user_model
      rv$correct_model <- correct_model
      
      # Display regression results
      output$regression_table <- renderTable({
        if (analysis_type() == "Frequentist") {
          user_coef <- coef(summary(rv$user_model))
          correct_coef <- coef(summary(rv$correct_model))
          
          # Get the list of all terms from both models
          all_terms <- union(rownames(user_coef), rownames(correct_coef))
          
          # Initialize vectors to store estimates
          User_Estimate <- User_Std_Error <- Correct_Estimate <- Correct_Std_Error <- numeric(length(all_terms))
          
          # For each term, extract estimates from user_coef and correct_coef
          for (i in seq_along(all_terms)) {
            term <- all_terms[i]
            User_Estimate[i] <- if (term %in% rownames(user_coef)) user_coef[term, "Estimate"] else NA
            User_Std_Error[i] <- if (term %in% rownames(user_coef)) user_coef[term, "Std. Error"] else NA
            Correct_Estimate[i] <- if (term %in% rownames(correct_coef)) correct_coef[term, "Estimate"] else NA
            Correct_Std_Error[i] <- if (term %in% rownames(correct_coef)) correct_coef[term, "Std. Error"] else NA
          }
          
          # Create a data frame to compare coefficients
          compare_table <- data.frame(
            Term = all_terms,
            User_Estimate = User_Estimate,
            User_Std_Error = User_Std_Error,
            Correct_Estimate = Correct_Estimate,
            Correct_Std_Error = Correct_Std_Error
          )
          
        } else {
          # For Bayesian models, use posterior summaries
          user_summary <- summary(rv$user_model)$fixed
          correct_summary <- summary(rv$correct_model)$fixed
          
          # Get the list of all terms from both models
          all_terms <- union(rownames(user_summary), rownames(correct_summary))
          
          # Initialize vectors to store estimates
          User_Estimate <- User_Std_Error <- Correct_Estimate <- Correct_Std_Error <- numeric(length(all_terms))
          
          # For each term, extract estimates from user_summary and correct_summary
          for (i in seq_along(all_terms)) {
            term <- all_terms[i]
            User_Estimate[i] <- if (term %in% rownames(user_summary)) user_summary[term, "Estimate"] else NA
            User_Std_Error[i] <- if (term %in% rownames(user_summary)) user_summary[term, "Est.Error"] else NA
            Correct_Estimate[i] <- if (term %in% rownames(correct_summary)) correct_summary[term, "Estimate"] else NA
            Correct_Std_Error[i] <- if (term %in% rownames(correct_summary)) correct_summary[term, "Est.Error"] else NA
          }
          
          # Create a data frame to compare coefficients
          compare_table <- data.frame(
            Term = all_terms,
            User_Estimate = User_Estimate,
            User_Std_Error = User_Std_Error,
            Correct_Estimate = Correct_Estimate,
            Correct_Std_Error = Correct_Std_Error
          )
        }
        
        return(compare_table)
      })
      
      
      # Display regression plot
      output$regression_plot <- renderPlot({
        # Example: Plotting the exposure effect estimates
        if (analysis_type() == "Frequentist") {
          estimates <- data.frame(
            Model = c("User Model", "Correct Model"),
            Estimate = c(coef(rv$user_model)[input$exposure_var], coef(rv$correct_model)[input$exposure_var]),
            Std_Error = c(summary(rv$user_model)$coefficients[input$exposure_var, "Std. Error"],
                          summary(rv$correct_model)$coefficients[input$exposure_var, "Std. Error"])
          )
        } else {
          estimates <- data.frame(
            Model = c("User Model", "Correct Model"),
            Estimate = c(fixef(rv$user_model)[input$exposure_var, "Estimate"],
                         fixef(rv$correct_model)[input$exposure_var, "Estimate"]),
            Std_Error = c(fixef(rv$user_model)[input$exposure_var, "Est.Error"],
                          fixef(rv$correct_model)[input$exposure_var, "Est.Error"])
          )
        }
        ggplot2::ggplot(estimates, ggplot2::aes(x = Model, y = Estimate)) +
          ggplot2::geom_point(size = 3) +
          ggplot2::geom_errorbar(ggplot2::aes(ymin = Estimate - 1.96 * Std_Error, ymax = Estimate + 1.96 * Std_Error), width = 0.2) +
          ggplot2::ggtitle("Exposure Effect Estimates with 95% CI") +
          ggplot2::ylab("Estimate") +
          ggplot2::theme_minimal()
      })
    })
  })
}



