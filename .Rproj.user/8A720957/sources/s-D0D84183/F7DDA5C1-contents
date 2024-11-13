











library(shiny)
library(dagitty)
library(ggdag)
library(ggplot2)
library(shinyWidgets)

# Define UI
ui <- fluidPage(
  titlePanel("Causal Inference and Regression Adjustment Tool"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Define Your Causal Model"),
      fileInput("dag_file", "Upload DAG (dagitty format):", accept = c(".txt", ".dagitty")),
      textAreaInput("dag_text", "Or specify DAG manually:",
                    "dag { y <- x; z -> y; x -> z }",
                    rows = 5),
      actionButton("update_dag", "Update DAG"),
      h5("Note: Variable names are converted to lowercase."),
      
      h4("Specify Outcome and Exposure Variables"),
      uiOutput("variable_selection_ui"),
      
      h4("View Sufficient Adjustment Sets"),
      actionButton("compute_adjustments", "Compute Adjustment Sets"),
      tableOutput("adjustment_sets_output"),
      
      h4("Specify Regression Model"),
      checkboxGroupInput("model_terms", "Select Variables for Regression:", choices = NULL),
      textInput("custom_formula", "Or specify regression formula:", value = ""),
      
      actionButton("run_analysis", "Run Analysis")
    ),
    
    mainPanel(
      h3("DAG Visualization"),
      plotOutput("dag_plot"),  # Using ggdag for plotting
      
      h3("Regression Results"),
      tableOutput("regression_output"),
      
      # Error message display
      div(style = "color: red;", textOutput("dag_error", inline = TRUE))
    )
  )
)

# Define Server
server <- function(input, output, session) {
  
  # Reactive value to store the DAG
  dag <- reactiveVal()
  
  # Reactive value to store error messages
  dag_error_message <- reactiveVal("")
  
  
  # Helper function to get all variable names from the DAG
  get_dag_variables <- function(dag_obj) {
    edges <- dagitty::edges(dag_obj)
    unique(c(edges$from, edges$to))
  }
  
  # Load DAG from file or manual input
  observeEvent(input$update_dag, {
    # Reset error message
    dag_error_message("")
    
    if (!is.null(input$dag_file)) {
      dag_content <- paste(readLines(input$dag_file$datapath), collapse = "\n")
    } else {
      dag_content <- input$dag_text
    }
    
    # Try to parse the DAG
    dag_parsed <- tryCatch({
      dagitty::dagitty(dag_content)
    }, error = function(e) {
      dag_error_message("Error: Not a valid DAG. Please check your syntax.")
      return(NULL)
    })
    
    if (!is.null(dag_parsed)) {
      dag(dag_parsed)
      
      # Update variable options using the helper function
      dag_vars <- get_dag_variables(dag())
      updateSelectInput(session, "outcome_var", choices = dag_vars, selected = NULL)
      updateSelectInput(session, "exposure_var", choices = dag_vars, selected = NULL)
      updateCheckboxGroupInput(session, "model_terms", choices = dag_vars, selected = NULL)
      
      # Reset adjustment sets and regression outputs
      adjustment_sets(NULL)
      output$adjustment_sets_output <- renderTable(NULL)
      output$regression_output <- renderTable(NULL)
    } else {
      dag(NULL)
      # Reset variable options
      updateSelectInput(session, "outcome_var", choices = NULL)
      updateSelectInput(session, "exposure_var", choices = NULL)
      updateCheckboxGroupInput(session, "model_terms", choices = NULL)
    }
  })
  
  
  # Display error messages
  output$dag_error <- renderText({
    dag_error_message()
  })
  output$variable_selection_ui <- renderUI({
    dag_obj <- dag()
    if (!is.null(dag_obj)) {
      dag_vars <- get_dag_variables(dag_obj)
      tagList(
        selectInput("outcome_var", "Select Outcome Variable:", choices = dag_vars),
        selectInput("exposure_var", "Select Exposure Variable:", choices = dag_vars)
      )
    }
  })
  
  
  
  # Compute and display sufficient adjustment sets
  adjustment_sets <- reactiveVal()
  
  observeEvent(input$compute_adjustments, {
    dag_obj <- dag()
    if (!is.null(dag_obj) && !is.null(input$outcome_var) && !is.null(input$exposure_var)) {
      adj_sets <- adjustmentSets(dag_obj, exposure = input$exposure_var, outcome = input$outcome_var)
      if (length(adj_sets) == 0) {
        showNotification("No sufficient adjustment sets found.", type = "warning")
        adjustment_sets(NULL)
        output$adjustment_sets_output <- renderTable(NULL)
      } else {
        adjustment_sets(adj_sets)
        adj_sets_list <- lapply(adj_sets, function(x) paste(x, collapse = ", "))
        output$adjustment_sets_output <- renderTable({
          data.frame("Adjustment Set" = adj_sets_list)
        }, rownames = FALSE, colnames = TRUE)
      }
    } else {
      showNotification("Please ensure DAG, outcome, and exposure variables are specified.", type = "error")
    }
  })
  
  # Plot DAG using ggdag
  output$dag_plot <- renderPlot({
    dag_obj <- dag()
    if (is.null(dag_obj)) return()  # Skip rendering if DAG is not valid
    
    ggdag::ggdag(dag_obj, text = FALSE, use_labels = "name") +
      ggplot2::theme_minimal() +
      ggplot2::ggtitle("DAG Visualization")
  })
  
  # Run analysis when button is clicked
  observeEvent(input$run_analysis, {
    dag_obj <- dag()
    if (!is.null(dag_obj) && !is.null(input$outcome_var) && !is.null(input$exposure_var)) {
      dag_vars <- dagitty::namesDAG(dag_obj)
      
      # Simulate data based on DAG variables
      set.seed(123)
      simulated_data <- as.data.frame(matrix(rnorm(1000 * length(dag_vars)), ncol = length(dag_vars)))
      colnames(simulated_data) <- dag_vars
      
      # If custom formula is provided, use it
      if (nzchar(input$custom_formula)) {
        formula <- as.formula(input$custom_formula)
      } else {
        # Otherwise, create formula based on selected model terms
        if (length(input$model_terms) == 0) {
          showNotification("Please select at least one variable for the regression model.", type = "error")
          return()
        }
        formula <- as.formula(paste(input$outcome_var, "~", paste(input$model_terms, collapse = " + ")))
      }
      
      # Run regression
      fit <- lm(formula, data = simulated_data)
      
      # Run regressions for sufficient adjustment sets
      adj_sets <- adjustment_sets()
      if (is.null(adj_sets)) {
        showNotification("Please compute adjustment sets first.", type = "error")
        return()
      }
      adj_results <- lapply(adj_sets, function(adj_set) {
        adj_vars <- unlist(adj_set)
        formula_adj <- as.formula(paste(input$outcome_var, "~", input$exposure_var, "+", paste(adj_vars, collapse = "+")))
        fit_adj <- lm(formula_adj, data = simulated_data)
        coef_summary <- summary(fit_adj)$coefficients
        data.frame(Adjustment_Set = paste(adj_vars, collapse = ", "),
                   Estimate = coef_summary[input$exposure_var, "Estimate"],
                   Std.Error = coef_summary[input$exposure_var, "Std. Error"],
                   t.value = coef_summary[input$exposure_var, "t value"],
                   Pr...t.. = coef_summary[input$exposure_var, "Pr(>|t|)"])
      })
      adj_results_df <- do.call(rbind, adj_results)
      
      output$regression_output <- renderTable({
        adj_results_df
      }, rownames = FALSE)
    } else {
      showNotification("Please ensure DAG, outcome, and exposure variables are specified.", type = "error")
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)


library(shiny)
library(simDAG)
library(igraph)
library(shinyWidgets)

# Define UI
ui <- fluidPage(
  titlePanel("Causal Inference and Regression Adjustment Tool"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Define Your Causal Model"),
      textAreaInput("dag_text", "Specify DAG edges (e.g., 'X->Y; Z->Y; X->Z'):",
                    "X->Y; Z->Y; X->Z", rows = 5),
      actionButton("update_dag", "Update DAG"),
      h5("Note: Variable names are case-sensitive."),
      
      h4("Specify Outcome and Exposure Variables"),
      uiOutput("variable_selection_ui"),
      
      h4("Specify Regression Model"),
      checkboxGroupInput("model_terms", "Select Variables for Regression:", choices = NULL),
      textInput("custom_formula", "Or specify regression formula:", value = ""),
      
      actionButton("run_analysis", "Run Analysis")
    ),
    
    mainPanel(
      h3("DAG Visualization"),
      plotOutput("dag_plot"),
      
      h3("Regression Results"),
      tableOutput("regression_output"),
      
      # Error message display
      div(style = "color: red;", textOutput("dag_error", inline = TRUE))
    )
  )
)

# Define Server
server <- function(input, output, session) {
  
  # Reactive value to store the DAG
  dag <- reactiveVal()
  
  # Reactive value to store error messages
  dag_error_message <- reactiveVal("")
  
  # Load DAG from manual input
  observeEvent(input$update_dag, {
    # Reset error message
    dag_error_message("")
    
    dag_edges <- strsplit(input$dag_text, ";")[[1]]
    dag_edges <- trimws(dag_edges)  # Remove any leading/trailing whitespace
    
    # Try to parse the DAG
    dag_parsed <- tryCatch({
      # Extract unique variables
      variables <- unique(unlist(strsplit(gsub("->", ",", dag_edges), ",")))
      variables <- trimws(variables)
      
      # Create an adjacency matrix
      adj_matrix <- matrix(0, nrow = length(variables), ncol = length(variables))
      colnames(adj_matrix) <- variables
      rownames(adj_matrix) <- variables
      
      # Fill the adjacency matrix based on edges
      for (edge in dag_edges) {
        nodes <- strsplit(edge, "->")[[1]]
        if (length(nodes) != 2) {
          stop("Invalid edge format. Use 'A->B'.")
        }
        from_node <- trimws(nodes[1])
        to_node <- trimws(nodes[2])
        adj_matrix[from_node, to_node] <- 1
      }
      
      # Define node types (assuming Gaussian for all nodes for simplicity)
      node_types <- as.list(rep("rnorm", length(variables)))
      names(node_types) <- variables
      
      # Create DAG object
      dag_obj <- matrix2dag(mat = adj_matrix, type = node_types)
      
      list(dag_obj = dag_obj, variables = variables, adj_matrix = adj_matrix)
    }, error = function(e) {
      dag_error_message("Error: Not a valid DAG. Please check your syntax.")
      return(NULL)
    })
    
    if (!is.null(dag_parsed)) {
      dag(dag_parsed)
      
      # Update variable options
      dag_vars <- dag_parsed$variables
      updateSelectInput(session, "outcome_var", choices = dag_vars, selected = NULL)
      updateSelectInput(session, "exposure_var", choices = dag_vars, selected = NULL)
      updateCheckboxGroupInput(session, "model_terms", choices = dag_vars, selected = NULL)
      
      # Reset regression outputs
      output$regression_output <- renderTable(NULL)
    } else {
      dag(NULL)
      # Reset variable options
      updateSelectInput(session, "outcome_var", choices = NULL)
      updateSelectInput(session, "exposure_var", choices = NULL)
      updateCheckboxGroupInput(session, "model_terms", choices = NULL)
    }
  })
  
  # Display error messages
  output$dag_error <- renderText({
    dag_error_message()
  })
  
  # Output for outcome and exposure variable selection
  output$variable_selection_ui <- renderUI({
    if (!is.null(dag())) {
      dag_vars <- dag()$variables
      tagList(
        selectInput("outcome_var", "Select Outcome Variable:", choices = dag_vars),
        selectInput("exposure_var", "Select Exposure Variable:", choices = dag_vars)
      )
    }
  })
  
  # Plot DAG
  output$dag_plot <- renderPlot({
    dag_data <- dag()
    if (is.null(dag_data)) return()  # Skip rendering if DAG is not valid
    
    adj_matrix <- dag_data$adj_matrix
    
    # Create igraph object from adjacency matrix
    g <- graph_from_adjacency_matrix(t(adj_matrix), mode = "directed")
    
    plot(g, vertex.label = V(g)$name, main = "DAG Visualization",
         edge.arrow.size = 0.5)
  })
  
  # Run analysis when button is clicked
  observeEvent(input$run_analysis, {
    dag_data <- dag()
    if (!is.null(dag_data) && !is.null(input$outcome_var) && !is.null(input$exposure_var)) {
      dag_vars <- dag_data$variables
      
      # Simulate data based on DAG using sim_from_dag
      set.seed(123)
      num_samples <- 1000
      data <- sim_from_dag(dag_data$dag_obj, n_sim = num_samples)
      
      # If custom formula is provided, use it
      if (nzchar(input$custom_formula)) {
        formula <- as.formula(input$custom_formula)
      } else {
        # Otherwise, create formula based on selected model terms
        if (length(input$model_terms) == 0) {
          showNotification("Please select at least one variable for the regression model.", type = "error")
          return()
        }
        formula <- as.formula(paste(input$outcome_var, "~", paste(input$model_terms, collapse = " + ")))
      }
      
      # Run regression
      fit <- lm(formula, data = data)
      
      output$regression_output <- renderTable({
        summary(fit)$coefficients
      }, rownames = TRUE)
    } else {
      showNotification("Please ensure DAG, outcome, and exposure variables are specified.", type = "error")
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)





# Load necessary libraries
library(shiny)
library(ggplot2)
library(igraph)
library(simDAG)
library(shinyWidgets)

# Define UI
ui <- navbarPage(
  title = "Learn about DAGs",
  id = "navbar",
  
  # Introduction to DAGs
  tabPanel("Introduction to DAGs",
           fluidPage(
             h2("Introduction to Directed Acyclic Graphs"),
             uiOutput("intro_content")
           )
  ),
  
  # Understanding Paradoxes
  tabPanel("Understanding Paradoxes",
           fluidPage(
             h2("Exploring Paradoxes with DAGs"),
             uiOutput("paradox_content")
           )
  ),
  
  # Regression Estimates and Causality
  tabPanel("Regression Estimates",
           fluidPage(
             h2("Regression Estimates and Causal Inference"),
             uiOutput("regression_content")
           )
  ),
  
  # Interactive DAG Builder
  tabPanel("Interactive DAG Builder",
           fluidPage(
             h2("Build Your Own DAG"),
             uiOutput("builder_content")
           )
  )
)

# Define Server
server <- function(input, output, session) {
  # Reactive values to store user's information
  user_name <- reactiveVal()
  user_field <- reactiveVal()
  
  # Show the first modal to collect the user's name
  observeEvent(TRUE, {
    showModal(modalDialog(
      title = "Welcome to the DAG Learning Tool",
      textInput("user_name_input", "Please enter your name:", value = ""),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("submit_name", "Next")
      ),
      easyClose = FALSE,
      fade = TRUE
    ))
  }, once = TRUE)
  
  # When the user submits their name
  observeEvent(input$submit_name, {
    if (nzchar(input$user_name_input)) {
      user_name(input$user_name_input)
      removeModal()
      
      # Now show the second modal to ask for the field of interest
      showModal(modalDialog(
        title = "Tell us about your interests",
        selectInput("user_field_input", "What field are you interested in?", 
                    choices = c("Economics", "Biostatistics/Epidemiology", 
                                "Social Sciences", "Engineering", "Other")),
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
  output$intro_content <- renderUI({
    req(user_name(), user_field())
    tagList(
      h3(paste("Welcome,", user_name(), "! Let's learn about DAGs.")),
      p(paste("As someone interested in", user_field(), 
              ", understanding DAGs can be very beneficial.")),
      p("A Directed Acyclic Graph (DAG) is a graphical representation of variables and their causal relationships. Nodes represent variables, and edges represent causal effects."),
      h4("Simpson's Paradox Example"),
      uiOutput("simpson_example"),
      actionButton("next_intro", "Next")
    )
  })
  
  # Generate Simpson's Paradox example based on the user's field
  output$simpson_example <- renderUI({
    req(user_field())
    # Simulate data and create plots based on the selected field
    if (user_field() == "Economics") {
      # Economics example
      tagList(
        h5("Economics Example: The Employment Training Program"),
        p("Suppose we are evaluating the effectiveness of a job training program. Initial analysis suggests the program decreases employment rates, but after accounting for participants' prior education levels, we find it actually improves employment."),
        plotOutput("econ_dag_plot"),
        plotOutput("econ_data_plot"),
        tableOutput("econ_regression_results"),
        actionButton("econ_run_analysis", "Run Analysis")
      )
    } else if (user_field() == "Biostatistics/Epidemiology") {
      # Biostatistics/Epidemiology example
      tagList(
        h5("Biostatistics/Epidemiology Example: Smoking and Lung Cancer"),
        p("An initial analysis suggests that smoking is not associated with lung cancer. However, after accounting for age, we find that smoking increases the risk."),
        plotOutput("bio_dag_plot"),
        plotOutput("bio_data_plot"),
        tableOutput("bio_regression_results"),
        actionButton("bio_run_analysis", "Run Analysis")
      )
    } else if (user_field() == "Social Sciences") {
      # Social Sciences example
      tagList(
        h5("Social Sciences Example: Education Level and Income"),
        p("At first glance, higher education seems to be associated with lower income, but when considering work experience, the relationship reverses."),
        plotOutput("soc_dag_plot"),
        plotOutput("soc_data_plot"),
        tableOutput("soc_regression_results"),
        actionButton("soc_run_analysis", "Run Analysis")
      )
    } else {
      # General example
      tagList(
        h5("General Example of Simpson's Paradox"),
        p("A simple example demonstrating how aggregated data can mislead analyses."),
        plotOutput("gen_dag_plot"),
        plotOutput("gen_data_plot"),
        tableOutput("gen_regression_results"),
        actionButton("gen_run_analysis", "Run Analysis")
      )
    }
  })
  
  ### Economics Example
  observeEvent(input$econ_run_analysis, {
    # Create an empty DAG
    dag_obj <- empty_dag()
    
    # Define nodes
    # Education node (root node)
    dag_obj <- dag_obj + 
      node(name = "Education", 
           type = "rbernoulli", 
           p = 0.6)
    
    # Program node depends on Education
    dag_obj <- dag_obj + 
      node(name = "Program", 
           type = "binomial", 
           parents = "Education",
           betas = 1.5,
           intercept = -1)
    
    # Employment node depends on Program and Education
    dag_obj <- dag_obj + 
      node(name = "Employment", 
           type = "binomial", 
           parents = c("Program", "Education"),
           betas = c(1.0, 1.2),
           intercept = -2)
    
    # Simulate data
    set.seed(123)
    data <- sim_from_dag(dag_obj, n_sim = 1000)
    
    
    
    # Plot DAG
    output$econ_dag_plot <- renderPlot({
      adj_matrix <- dag2matrix(dag_obj)  # Corrected line using dag2matrix()
      g <- graph_from_adjacency_matrix(adj_matrix, mode = "directed")
      plot(g, vertex.label = V(g)$name, main = "Economics DAG",
           edge.arrow.size = 0.5)
    })
    
    # Plot data
    output$econ_data_plot <- renderPlot({
      ggplot(data, aes(x = factor(Program), y = factor(Employment))) +
        geom_jitter(width = 0.2, height = 0.2, alpha = 0.5) +
        labs(title = "Program Participation vs Employment",
             x = "Program Participation", y = "Employment Status")
    })
    
    # Convert variables to numeric if necessary
    data$Program <- as.numeric(data$Program)
    data$Employment <- as.numeric(data$Employment)
    data$Education <- as.numeric(data$Education)
    
    # Run regressions
    fit_unadjusted <- glm(Employment ~ Program, data = data, family = binomial)
    fit_adjusted <- glm(Employment ~ Program + Education, data = data, family = binomial)
    
    # Display regression results
    output$econ_regression_results <- renderTable({
      # Get coefficients
      unadjusted_coef <- summary(fit_unadjusted)$coefficients
      adjusted_coef <- summary(fit_adjusted)$coefficients
      
      
      
      
      
      
      
      
      
      
      