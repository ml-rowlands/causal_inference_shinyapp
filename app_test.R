library(shiny)
library(ggplot2)
library(igraph)
library(simDAG)
library(shinyWidgets)
library(plotly)
library(brms)       
library(bayesplot)  


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
  # Common DAG Structures
  tabPanel("Common DAG Structures",
           fluidPage(
             h2("Exploring Common DAG Structures"),
             uiOutput("dag_structures_content")
           )
  ),
)

# Define Server
server <- function(input, output, session) {
  # Reactive values to store user's information
  user_name <- reactiveVal()
  user_field <- reactiveVal()
  analysis_type <- reactiveVal("Frequentist")  # Default to Frequentist analysis
  
  # Initialize 'values' at the top level
  values <- reactiveValues()
  
  # Show the first modal to collect the user's name and analysis preference
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
      h3(paste("Welcome,", user_name(), "! Let's Learn About Directed Acyclic Graphs (DAGs)")),
      p("In the world of data analysis and statistics, understanding the relationships between variables is crucial. Sometimes, these relationships are not straightforward due to the presence of hidden factors or confounding variables."),
      h4("What is a DAG?"),
      p("A Directed Acyclic Graph (DAG) is a visual representation that helps us understand and analyze the relationships and causal structures between different variables. It consists of nodes (which represent variables) and directed edges (which represent causal effects from one variable to another). The term 'acyclic' means that the graph doesn't contain any loops; you cannot start at one node and follow a path that leads back to the same node."),
      img(src = "https://i.imgur.com/O7RZp5F.png", height = "300px", alt = "Simple DAG Example"),
      p("DAGs are powerful tools because they allow us to:"),
      tags$ul(
        tags$li("Visualize complex relationships between variables."),
        tags$li("Identify potential confounding variables."),
        tags$li("Determine the appropriate variables to adjust for in statistical models to estimate causal effects.")
      ),
      h4("Why are DAGs Useful?"),
      p("In many fields, including", strong(user_field()), ", we often want to understand whether one variable causes another. However, simply observing a relationship between two variables doesn't necessarily mean that one causes the other. There could be other variables influencing both."),
      p("DAGs help us:"),
      tags$ul(
        tags$li("Clarify assumptions about causal structures."),
        tags$li("Avoid common pitfalls like spurious correlations and Simpson's Paradox."),
        tags$li("Improve the validity of causal inferences from data.")
      ),
      h4("Simpson's Paradox: An Illustrative Example"),
      p("Simpson's Paradox occurs when a trend that appears in different groups of data disappears or reverses when the groups are combined. This paradox highlights the importance of considering underlying variables that may affect the observed relationships."),
      p("Let's explore how DAGs can help us understand and resolve Simpson's Paradox in the context of", strong(user_field()), "."),
      actionButton("start_example", "Explore the Example")
    )
  })
  
  # Generate Simpson's Paradox example based on the user's field
  # Observe the action button to start the example
  observeEvent(input$start_example, {
    # Depending on the user's field, trigger the appropriate example
    if (user_field() == "Economics") {
      # Trigger the first step of the Economics example
      showModal(modalDialog(
        title = "Economics Example: The Employment Training Program",
        p("Suppose we are evaluating the effectiveness of a job training program..."),
        # ... [Rest of the content as in the econ_step1 modal]
        footer = tagList(
          modalButton("Close"),
          actionButton("econ_step1", "Proceed to Step 1")
        ),
        easyClose = FALSE
      ))
    } else if (user_field() == "Biostatistics/Epidemiology") {
      # Trigger the first step of the Biostatistics/Epidemiology example
      showModal(modalDialog(
        title = "Biostatistics/Epidemiology Example: Smoking and Lung Cancer",
        p("An initial analysis suggests that smoking is not associated with lung cancer risk..."),
        # ... [Rest of the content as in the bio_step1 modal]
        footer = tagList(
          modalButton("Close"),
          actionButton("bio_step1", "Proceed to Step 1")
        ),
        easyClose = FALSE
      ))
    } else if (user_field() == "Social Sciences") {
      # Trigger the first step of the Social Sciences example
      showModal(modalDialog(
        title = "Social Sciences Example: Education Level and Income",
        p("At first glance, higher education seems to be associated with lower income..."),
        # ... [Rest of the content as in the soc_step1 modal]
        footer = tagList(
          modalButton("Close"),
          actionButton("soc_step1", "Proceed to Step 1")
        ),
        easyClose = FALSE
      ))
    } else {
      # For other fields, you can provide a generic message or default example
      showModal(modalDialog(
        title = "Example Under Development",
        p("Examples for your selected field are under development."),
        footer = modalButton("Close"),
        easyClose = FALSE
      ))
    }
  })
  
  ### Economics Example Steps
  # Step 1: Understand the Scenario
  observeEvent(input$econ_step1, {
    showModal(modalDialog(
      title = "Step 1: Understanding the Scenario",
      p("In this example, we want to evaluate whether a job training program (Program) is effective in increasing employment rates (Employment)."),
      p("However, participants' education level (Education) may influence both their likelihood of joining the program and their employment outcomes."),
      p("This creates a confounding effect that we need to account for."),
      footer = tagList(
        modalButton("Close"),
        actionButton("econ_step2", "Proceed to Step 2")
      ),
      easyClose = FALSE
    ))
  })
  
  # Step 2: Simulate the Data
  observeEvent(input$econ_step2, {
    removeModal()
    showModal(modalDialog(
      title = "Step 2: Simulate the Data",
      p("We will now simulate data that reflects this scenario using a DAG."),
      actionButton("econ_simulate_data", "Simulate Data"),
      footer = modalButton("Close"),
      easyClose = FALSE
    ))
  })
  
  # Simulate the data when the user clicks "Simulate Data"
  observeEvent(input$econ_simulate_data, {
    removeModal()
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
    
    # Convert variables to numeric if necessary
    data$Program <- as.numeric(data$Program)
    data$Employment <- as.numeric(data$Employment)
    data$Education <- as.numeric(data$Education)
    
    # Store the data in reactive values for use in other steps
    values$econ_data <- data
    values$econ_dag_obj <- dag_obj
    
    # Show message and proceed to next step
    showModal(modalDialog(
      title = "Data Simulation Complete",
      p("The data has been simulated based on the specified DAG."),
      p("Notice that there may appear to be a strong relationship between Program participation and Employment before accounting for Education."),
      footer = tagList(
        modalButton("Close"),
        actionButton("econ_step3", "Proceed to Step 3")
      ),
      easyClose = FALSE
    ))
  })
  
  # Step 3: Visualize the DAG and Data
  observeEvent(input$econ_step3, {
    removeModal()
    showModal(modalDialog(
      title = "Step 3: Visualize the DAG and Data",
      p("Here is the DAG representing the causal relationships in our scenario:"),
      plotOutput("econ_dag_plot"),
      p("Let's also look at the relationship between Program participation and Employment:"),
      plotOutput("econ_data_plot"),
      footer = tagList(
        modalButton("Close"),
        actionButton("econ_step4", "Proceed to Step 4")
      ),
      easyClose = FALSE,
      size = "l"
    ))
  })
  
  # Render DAG plot
  output$econ_dag_plot <- renderPlot({
    req(values$econ_dag_obj)
    adj_matrix <- dag2matrix(values$econ_dag_obj)
    g <- graph_from_adjacency_matrix(adj_matrix, mode = "directed")
    plot(g, vertex.label = V(g)$name, main = "Economics DAG",
         edge.arrow.size = 0.5)
  })
  
  # Render data plot
  output$econ_data_plot <- renderPlot({
    req(values$econ_data)
    ggplot(values$econ_data, aes(x = factor(Program), y = Employment)) +
      geom_jitter(width = 0.2, height = 0.2, alpha = 0.5) +
      labs(title = "Program Participation vs Employment",
           x = "Program Participation", y = "Employment Status")
  })
  
  # Step 4: Run the Unadjusted Model
  observeEvent(input$econ_step4, {
    removeModal()
    showModal(modalDialog(
      title = "Step 4: Run the Unadjusted Model",
      p("We will now run a regression model to see the effect of the Program on Employment without adjusting for Education."),
      actionButton("econ_run_unadjusted", "Run Unadjusted Model"),
      footer = modalButton("Close"),
      easyClose = FALSE
    ))
  })
  
  # Run unadjusted model
  observeEvent(input$econ_run_unadjusted, {
    removeModal()
    data <- values$econ_data
    # Run regression based on analysis type
    if (analysis_type() == "Frequentist") {
      fit_unadjusted <- glm(Employment ~ Program, data = data, family = binomial)
    } else {
      fit_unadjusted <- brm(Employment ~ Program, data = data, family = bernoulli(), silent = TRUE)
    }
    values$fit_unadjusted <- fit_unadjusted
    
    # Show results
    showModal(modalDialog(
      title = "Unadjusted Model Results",
      p("Here are the results of the unadjusted model:"),
      tableOutput("econ_unadjusted_results"),
      if (analysis_type() == "Bayesian") {
        tagList(
          p("Posterior distributions of the coefficients:"),
          plotOutput("econ_unadjusted_posterior")
        )
      },
      p("Interpretation: The coefficient for Program indicates the effect of the training program on employment without considering education."),
      footer = tagList(
        modalButton("Close"),
        actionButton("econ_step5", "Proceed to Step 5")
      ),
      easyClose = FALSE,
      size = "l"
    ))
  })
  
  # Display unadjusted regression results
  output$econ_unadjusted_results <- renderTable({
    req(values$fit_unadjusted)
    if (analysis_type() == "Frequentist") {
      summary(values$fit_unadjusted)$coefficients
    } else {
      as.data.frame(fixef(values$fit_unadjusted))
    }
  }, rownames = TRUE)
  
  # Plot posterior distributions for unadjusted model
  output$econ_unadjusted_posterior <- renderPlot({
    req(values$fit_unadjusted)
    posterior <- as_draws_df(values$fit_unadjusted)
    mcmc_areas(posterior, pars = c("b_Intercept", "b_Program"))
  })
  
  # Step 5: Run the Adjusted Model
  observeEvent(input$econ_step5, {
    removeModal()
    showModal(modalDialog(
      title = "Step 5: Run the Adjusted Model",
      p("Now, let's adjust for Education to see its impact on the relationship between Program and Employment."),
      actionButton("econ_run_adjusted", "Run Adjusted Model"),
      footer = modalButton("Close"),
      easyClose = FALSE
    ))
  })
  
  # Run adjusted model
  observeEvent(input$econ_run_adjusted, {
    removeModal()
    data <- values$econ_data
    # Run regression based on analysis type
    if (analysis_type() == "Frequentist") {
      fit_adjusted <- glm(Employment ~ Program + Education, data = data, family = binomial)
    } else {
      fit_adjusted <- brm(Employment ~ Program + Education, data = data, family = bernoulli(), silent = TRUE)
    }
    values$fit_adjusted <- fit_adjusted
    
    # Show results
    showModal(modalDialog(
      title = "Adjusted Model Results",
      p("Here are the results of the adjusted model:"),
      tableOutput("econ_adjusted_results"),
      if (analysis_type() == "Bayesian") {
        tagList(
          p("Posterior distributions of the coefficients:"),
          plotOutput("econ_adjusted_posterior")
        )
      },
      p("Interpretation: After adjusting for Education, the effect of the Program on Employment changes, highlighting the importance of accounting for confounding variables."),
      footer = tagList(
        modalButton("Close"),
        actionButton("econ_step6", "Compare Models")
      ),
      easyClose = FALSE,
      size = "l"
    ))
  })
  
  # Display adjusted regression results
  output$econ_adjusted_results <- renderTable({
    req(values$fit_adjusted)
    if (analysis_type() == "Frequentist") {
      summary(values$fit_adjusted)$coefficients
    } else {
      as.data.frame(fixef(values$fit_adjusted))
    }
  }, rownames = TRUE)
  
  # Plot posterior distributions for adjusted model
  output$econ_adjusted_posterior <- renderPlot({
    req(values$fit_adjusted)
    posterior <- as_draws_df(values$fit_adjusted)
    mcmc_areas(posterior, pars = c("b_Intercept", "b_Program", "b_Education"))
  })
  
  # Step 6: Compare Models
  observeEvent(input$econ_step6, {
    removeModal()
    # Extract odds ratios for interpretation
    if (analysis_type() == "Frequentist") {
      unadj_or <- round(exp(coef(values$fit_unadjusted)["Program"]), 2)
      adj_or <- round(exp(coef(values$fit_adjusted)["Program"]), 2)
    } else {
      unadj_or <- round(exp(fixef(values$fit_unadjusted)["Program", "Estimate"]), 2)
      adj_or <- round(exp(fixef(values$fit_adjusted)["Program", "Estimate"]), 2)
    }
    showModal(modalDialog(
      title = "Step 6: Compare Models and Interpret Results",
      p("Let's compare the unadjusted and adjusted models:"),
      tableOutput("econ_compare_results"),
      p("Interpretation:"),
      p("In the unadjusted model, the odds ratio for Program is ", unadj_or, ", indicating that participating in the program increases the odds of employment by a factor of ", unadj_or, "."),
      p("After adjusting for Education, the odds ratio changes to ", adj_or, ", suggesting that the effect of the program is different when accounting for education."),
      p("This demonstrates how failing to adjust for confounding variables can lead to misleading conclusions."),
      p("An odds ratio of 1 indicates no effect; values greater than 1 indicate increased odds, and values less than 1 indicate decreased odds."),
      footer = modalButton("Close"),
      easyClose = FALSE
    ))
  })
  
  # Display comparison of models
  output$econ_compare_results <- renderTable({
    req(values$fit_unadjusted, values$fit_adjusted)
    if (analysis_type() == "Frequentist") {
      unadjusted_coef <- summary(values$fit_unadjusted)$coefficients
      adjusted_coef <- summary(values$fit_adjusted)$coefficients
      # Extract coefficients for Program
      unadj_program <- unadjusted_coef["Program", ]
      adj_program <- adjusted_coef["Program", ]
      # Calculate odds ratios
      unadj_or <- exp(unadj_program["Estimate"])
      adj_or <- exp(adj_program["Estimate"])
      # Combine into a table
      results <- data.frame(
        Model = c("Unadjusted", "Adjusted"),
        Estimate = c(unadj_program["Estimate"], adj_program["Estimate"]),
        `Std. Error` = c(unadj_program["Std. Error"], adj_program["Std. Error"]),
        `P-value` = c(unadj_program["Pr(>|z|)"], adj_program["Pr(>|z|)"]),
        `Odds Ratio` = c(unadj_or, adj_or)
      )
    } else {
      # For Bayesian approach, include odds ratios
      unadjusted_coef <- as.data.frame(fixef(values$fit_unadjusted))
      adjusted_coef <- as.data.frame(fixef(values$fit_adjusted))
      # Extract coefficients for Program
      unadj_program <- unadjusted_coef["Program", ]
      adj_program <- adjusted_coef["Program", ]
      # Ensure the values are numeric
      unadj_estimate <- as.numeric(unadj_program["Estimate"])
      adj_estimate <- as.numeric(adj_program["Estimate"])
      # Calculate odds ratios
      unadj_or <- exp(unadj_estimate)
      adj_or <- exp(adj_estimate)
      # Combine into a table
      results <- data.frame(
        Model = c("Unadjusted", "Adjusted"),
        Estimate = c(unadj_estimate, adj_estimate),
        `Est. Error` = c(unadj_program["Est.Error"], adj_program["Est.Error"]),
        `Q2.5` = c(unadj_program["Q2.5"], adj_program["Q2.5"]),
        `Q97.5` = c(unadj_program["Q97.5"], adj_program["Q97.5"]),
        `Odds Ratio` = c(unadj_or, adj_or)
      )
    }
    results
  }, rownames = FALSE)
  
  ### Biostatistics/Epidemiology Example Steps
  # Initialize reactive values for this example
  values$bio_data <- NULL
  values$bio_dag_obj <- NULL
  
  # Step 1: Understand the Scenario
  observeEvent(input$bio_step1, {
    showModal(modalDialog(
      title = "Step 1: Understanding the Scenario",
      p("In this example, we examine whether smoking (Smoking) is associated with lung cancer (LungCancer)."),
      p("Age may influence both the likelihood of smoking and the risk of lung cancer."),
      p("We need to account for Age as a confounding variable."),
      footer = tagList(
        modalButton("Close"),
        actionButton("bio_step2", "Proceed to Step 2")
      ),
      easyClose = FALSE
    ))
  })
  
  # Step 2: Simulate the Data
  observeEvent(input$bio_step2, {
    removeModal()
    showModal(modalDialog(
      title = "Step 2: Simulate the Data",
      p("We will now simulate data that reflects this scenario using a DAG."),
      actionButton("bio_simulate_data", "Simulate Data"),
      footer = modalButton("Close"),
      easyClose = FALSE
    ))
  })
  
  # Simulate the data when the user clicks "Simulate Data"
  observeEvent(input$bio_simulate_data, {
    removeModal()
    # Create an empty DAG
    dag_obj <- empty_dag()
    
    # Define nodes
    # Age node (root node)
    dag_obj <- dag_obj +
      node(name = "Age",
           type = "rnorm",
           mean = 50,
           sd = 10)
    
    # Smoking node depends on Age
    dag_obj <- dag_obj +
      node(name = "Smoking",
           type = "binomial",
           parents = "Age",
           betas = 0.1,
           intercept = -5)
    
    # LungCancer node depends on Smoking and Age
    dag_obj <- dag_obj +
      node(name = "LungCancer",
           type = "binomial",
           parents = c("Smoking", "Age"),
           betas = c(1.2, 0.08),
           intercept = -6)
    
    # Simulate data
    set.seed(123)
    data <- sim_from_dag(dag_obj, n_sim = 1000)
    
    # Convert variables to numeric if necessary
    data$Smoking <- as.numeric(data$Smoking)
    data$LungCancer <- as.numeric(data$LungCancer)
    
    # Store the data in a reactive value for use in other steps
    values$bio_data <- data
    values$bio_dag_obj <- dag_obj
    
    # Show message and proceed to next step
    showModal(modalDialog(
      title = "Data Simulation Complete",
      p("The data has been simulated based on the specified DAG."),
      p("Notice that there may appear to be a weak or no relationship between Smoking and LungCancer before accounting for Age."),
      footer = tagList(
        modalButton("Close"),
        actionButton("bio_step3", "Proceed to Step 3")
      ),
      easyClose = FALSE
    ))
  })
  
  # Step 3: Visualize the DAG and Data
  observeEvent(input$bio_step3, {
    removeModal()
    showModal(modalDialog(
      title = "Step 3: Visualize the DAG and Data",
      p("Here is the DAG representing the causal relationships in our scenario:"),
      plotOutput("bio_dag_plot"),
      p("Let's also look at the relationship between Smoking and LungCancer:"),
      plotOutput("bio_data_plot"),
      footer = tagList(
        modalButton("Close"),
        actionButton("bio_step4", "Proceed to Step 4")
      ),
      easyClose = FALSE,
      size = "l"
    ))
  })
  
  # Render DAG plot
  output$bio_dag_plot <- renderPlot({
    req(values$bio_dag_obj)
    adj_matrix <- dag2matrix(values$bio_dag_obj)
    g <- graph_from_adjacency_matrix(adj_matrix, mode = "directed")
    plot(g, vertex.label = V(g)$name, main = "Biostatistics/Epidemiology DAG",
         edge.arrow.size = 0.5)
  })
  
  # Render data plot
  output$bio_data_plot <- renderPlot({
    req(values$bio_data)
    ggplot(values$bio_data, aes(x = factor(Smoking), y = LungCancer)) +
      geom_jitter(width = 0.2, height = 0.2, alpha = 0.5) +
      labs(title = "Smoking vs Lung Cancer",
           x = "Smoking Status", y = "Lung Cancer")
  })
  
  # Step 4: Run the Unadjusted Model
  observeEvent(input$bio_step4, {
    removeModal()
    showModal(modalDialog(
      title = "Step 4: Run the Unadjusted Model",
      p("We will now run a regression model to see the effect of Smoking on LungCancer without adjusting for Age."),
      actionButton("bio_run_unadjusted", "Run Unadjusted Model"),
      footer = modalButton("Close"),
      easyClose = FALSE
    ))
  })
  
  # Run unadjusted model
  observeEvent(input$bio_run_unadjusted, {
    removeModal()
    data <- values$bio_data
    # Run regression based on analysis type
    if (analysis_type() == "Frequentist") {
      fit_unadjusted <- glm(LungCancer ~ Smoking, data = data, family = binomial)
    } else {
      fit_unadjusted <- brm(LungCancer ~ Smoking, data = data, family = bernoulli(), silent = TRUE)
    }
    values$bio_fit_unadjusted <- fit_unadjusted
    
    # Show results
    showModal(modalDialog(
      title = "Unadjusted Model Results",
      p("Here are the results of the unadjusted model:"),
      tableOutput("bio_unadjusted_results"),
      if (analysis_type() == "Bayesian") {
        tagList(
          p("Posterior distributions of the coefficients:"),
          plotOutput("bio_unadjusted_posterior")
        )
      },
      p("Interpretation: The coefficient for Smoking indicates the effect of smoking on lung cancer without considering age."),
      footer = tagList(
        modalButton("Close"),
        actionButton("bio_step5", "Proceed to Step 5")
      ),
      easyClose = FALSE,
      size = "l"
    ))
  })
  
  # Display unadjusted regression results
  output$bio_unadjusted_results <- renderTable({
    req(values$bio_fit_unadjusted)
    if (analysis_type() == "Frequentist") {
      summary(values$bio_fit_unadjusted)$coefficients
    } else {
      as.data.frame(fixef(values$bio_fit_unadjusted))
    }
  }, rownames = TRUE)
  
  # Plot posterior distributions for unadjusted model
  output$bio_unadjusted_posterior <- renderPlot({
    req(values$bio_fit_unadjusted)
    posterior <- as_draws_df(values$bio_fit_unadjusted)
    mcmc_areas(posterior, pars = c("b_Intercept", "b_Smoking"))
  })
  
  # Step 5: Run the Adjusted Model
  observeEvent(input$bio_step5, {
    removeModal()
    showModal(modalDialog(
      title = "Step 5: Run the Adjusted Model",
      p("Now, let's adjust for Age to see its impact on the relationship between Smoking and LungCancer."),
      actionButton("bio_run_adjusted", "Run Adjusted Model"),
      footer = modalButton("Close"),
      easyClose = FALSE
    ))
  })
  
  # Run adjusted model
  observeEvent(input$bio_run_adjusted, {
    removeModal()
    data <- values$bio_data
    # Run regression based on analysis type
    if (analysis_type() == "Frequentist") {
      fit_adjusted <- glm(LungCancer ~ Smoking + Age, data = data, family = binomial)
    } else {
      fit_adjusted <- brm(LungCancer ~ Smoking + Age, data = data, family = bernoulli(), silent = TRUE)
    }
    values$bio_fit_adjusted <- fit_adjusted
    
    # Show results
    showModal(modalDialog(
      title = "Adjusted Model Results",
      p("Here are the results of the adjusted model:"),
      tableOutput("bio_adjusted_results"),
      if (analysis_type() == "Bayesian") {
        tagList(
          p("Posterior distributions of the coefficients:"),
          plotOutput("bio_adjusted_posterior")
        )
      },
      p("Interpretation: After adjusting for Age, the effect of Smoking on LungCancer becomes more apparent."),
      footer = tagList(
        modalButton("Close"),
        actionButton("bio_step6", "Compare Models")
      ),
      easyClose = FALSE,
      size = "l"
    ))
  })
  
  # Display adjusted regression results
  output$bio_adjusted_results <- renderTable({
    req(values$bio_fit_adjusted)
    if (analysis_type() == "Frequentist") {
      summary(values$bio_fit_adjusted)$coefficients
    } else {
      as.data.frame(fixef(values$bio_fit_adjusted))
    }
  }, rownames = TRUE)
  
  # Plot posterior distributions for adjusted model
  output$bio_adjusted_posterior <- renderPlot({
    req(values$bio_fit_adjusted)
    posterior <- as_draws_df(values$bio_fit_adjusted)
    mcmc_areas(posterior, pars = c("b_Intercept", "b_Smoking", "b_Age"))
  })
  
  # Step 6: Compare Models
  observeEvent(input$bio_step6, {
    removeModal()
    # Extract odds ratios for interpretation
    if (analysis_type() == "Frequentist") {
      unadj_or <- round(exp(coef(values$bio_fit_unadjusted)["Smoking"]), 2)
      adj_or <- round(exp(coef(values$bio_fit_adjusted)["Smoking"]), 2)
    } else {
      unadj_or <- round(exp(fixef(values$bio_fit_unadjusted)["Smoking", "Estimate"]), 2)
      adj_or <- round(exp(fixef(values$bio_fit_adjusted)["Smoking", "Estimate"]), 2)
    }
    showModal(modalDialog(
      title = "Step 6: Compare Models and Interpret Results",
      p("Let's compare the unadjusted and adjusted models:"),
      tableOutput("bio_compare_results"),
      p("Interpretation:"),
      p("In the unadjusted model, the odds ratio for Smoking is ", unadj_or, ", indicating a weak association between smoking and lung cancer."),
      p("After adjusting for Age, the odds ratio increases to ", adj_or, ", suggesting that smoking significantly increases the risk of lung cancer when accounting for age."),
      p("This demonstrates how adjusting for confounding variables can reveal the true effect of an exposure."),
      p("An odds ratio of 1 indicates no effect; values greater than 1 indicate increased odds."),
      footer = modalButton("Close"),
      easyClose = FALSE
    ))
  })
  
  # Display comparison of models
  output$bio_compare_results <- renderTable({
    req(values$bio_fit_unadjusted, values$bio_fit_adjusted)
    if (analysis_type() == "Frequentist") {
      unadjusted_coef <- summary(values$bio_fit_unadjusted)$coefficients
      adjusted_coef <- summary(values$bio_fit_adjusted)$coefficients
      # Extract coefficients for Smoking
      unadj_smoking <- unadjusted_coef["Smoking", ]
      adj_smoking <- adjusted_coef["Smoking", ]
      # Calculate odds ratios
      unadj_or <- exp(unadj_smoking["Estimate"])
      adj_or <- exp(adj_smoking["Estimate"])
      # Combine into a table
      results <- data.frame(
        Model = c("Unadjusted", "Adjusted"),
        Estimate = c(unadj_smoking["Estimate"], adj_smoking["Estimate"]),
        `Std. Error` = c(unadj_smoking["Std. Error"], adj_smoking["Std. Error"]),
        `P-value` = c(unadj_smoking["Pr(>|z|)"], adj_smoking["Pr(>|z|)"]),
        `Odds Ratio` = c(unadj_or, adj_or)
      )
    } else {
      # For Bayesian approach, include odds ratios
      unadjusted_coef <- as.data.frame(fixef(values$bio_fit_unadjusted))
      adjusted_coef <- as.data.frame(fixef(values$bio_fit_adjusted))
      # Extract coefficients for Smoking
      unadj_smoking <- unadjusted_coef["Smoking", ]
      adj_smoking <- adjusted_coef["Smoking", ]
      # Ensure the values are numeric
      unadj_estimate <- as.numeric(unadj_smoking["Estimate"])
      adj_estimate <- as.numeric(adj_smoking["Estimate"])
      # Calculate odds ratios
      unadj_or <- exp(unadj_estimate)
      adj_or <- exp(adj_estimate)
      # Combine into a table
      results <- data.frame(
        Model = c("Unadjusted", "Adjusted"),
        Estimate = c(unadj_estimate, adj_estimate),
        `Est. Error` = c(unadj_smoking["Est.Error"], adj_smoking["Est.Error"]),
        `Q2.5` = c(unadj_smoking["Q2.5"], adj_smoking["Q2.5"]),
        `Q97.5` = c(unadj_smoking["Q97.5"], adj_smoking["Q97.5"]),
        `Odds Ratio` = c(unadj_or, adj_or)
      )
    }
    results
  }, rownames = FALSE)
  
  ### Social Sciences Example Steps
  # Initialize reactive values for this example
  values$soc_data <- NULL
  values$soc_dag_obj <- NULL
  
  # Step 1: Understand the Scenario
  observeEvent(input$soc_step1, {
    showModal(modalDialog(
      title = "Step 1: Understanding the Scenario",
      p("In this example, we explore the relationship between education level (Education) and income (Income)."),
      p("Work experience (Experience) may influence both education level and income."),
      p("We need to account for Experience as a confounding variable."),
      footer = tagList(
        modalButton("Close"),
        actionButton("soc_step2", "Proceed to Step 2")
      ),
      easyClose = FALSE
    ))
  })
  
  # Step 2: Simulate the Data
  observeEvent(input$soc_step2, {
    removeModal()
    showModal(modalDialog(
      title = "Step 2: Simulate the Data",
      p("We will now simulate data that reflects this scenario using a DAG."),
      actionButton("soc_simulate_data", "Simulate Data"),
      footer = modalButton("Close"),
      easyClose = FALSE
    ))
  })
  
  # Simulate the data when the user clicks "Simulate Data"
  observeEvent(input$soc_simulate_data, {
    removeModal()
    # Create an empty DAG
    dag_obj <- empty_dag()
    
    # Define nodes
    # Experience node (root node)
    dag_obj <- dag_obj +
      node(name = "Experience",
           type = "rnorm",
           mean = 10,
           sd = 2)
    
    # Education node depends on Experience
    dag_obj <- dag_obj +
      node(name = "Education",
           type = "gaussian",
           parents = "Experience",
           betas = -0.5,  # Negative relationship
           intercept = 15,
           error = 1)  # Use 'error' instead of 'sd'
    
    # Income node depends on Education and Experience
    dag_obj <- dag_obj +
      node(name = "Income",
           type = "gaussian",
           parents = c("Education", "Experience"),
           betas = c(1000, 2000),
           intercept = 30000,
           error = 5000)  # Use 'error' instead of 'sd'
    
    # Simulate data
    set.seed(123)
    data <- sim_from_dag(dag_obj, n_sim = 1000)
    
    # Store the data in a reactive value for use in other steps
    values$soc_data <- data
    values$soc_dag_obj <- dag_obj
    
    # Show message and proceed to next step
    showModal(modalDialog(
      title = "Data Simulation Complete",
      p("The data has been simulated based on the specified DAG."),
      p("Notice that there may appear to be a negative relationship between Education and Income before accounting for Experience."),
      footer = tagList(
        modalButton("Close"),
        actionButton("soc_step3", "Proceed to Step 3")
      ),
      easyClose = FALSE
    ))
  })
  
  # Step 3: Visualize the DAG and Data
  observeEvent(input$soc_step3, {
    removeModal()
    showModal(modalDialog(
      title = "Step 3: Visualize the DAG and Data",
      p("Here is the DAG representing the causal relationships in our scenario:"),
      plotOutput("soc_dag_plot"),
      p("Let's also look at the relationship between Education and Income:"),
      plotOutput("soc_data_plot"),
      p("Additionally, let's examine the relationship between Experience and Education:"),
      plotOutput("soc_conf_treat_plot"),  # Plot the confounder vs. treatment
      footer = tagList(
        modalButton("Close"),
        actionButton("soc_step4", "Proceed to Step 4")
      ),
      easyClose = FALSE,
      size = "l"
    ))
  })
  
  # Render DAG plot
  output$soc_dag_plot <- renderPlot({
    req(values$soc_dag_obj)
    adj_matrix <- dag2matrix(values$soc_dag_obj)
    g <- graph_from_adjacency_matrix(adj_matrix, mode = "directed")
    plot(g, vertex.label = V(g)$name, main = "Social Sciences DAG",
         edge.arrow.size = 0.5)
  })
  
  # Render data plot
  output$soc_data_plot <- renderPlot({
    req(values$soc_data)
    ggplot(values$soc_data, aes(x = Education, y = Income)) +
      geom_point(alpha = 0.5) +
      labs(title = "Education Level vs Income",
           x = "Education Level", y = "Income")
  })
  
  # Plot the confounder vs. treatment variable
  output$soc_conf_treat_plot <- renderPlot({
    req(values$soc_data)
    ggplot(values$soc_data, aes(x = Experience, y = Education)) +
      geom_point(alpha = 0.5) +
      labs(title = "Experience vs Education Level",
           x = "Experience", y = "Education Level")
  })
  
  # Step 4: Run the Unadjusted Model
  observeEvent(input$soc_step4, {
    removeModal()
    showModal(modalDialog(
      title = "Step 4: Run the Unadjusted Model",
      p("We will now run a regression model to see the effect of Education on Income without adjusting for Experience."),
      actionButton("soc_run_unadjusted", "Run Unadjusted Model"),
      footer = modalButton("Close"),
      easyClose = FALSE
    ))
  })
  
  # Run unadjusted model
  observeEvent(input$soc_run_unadjusted, {
    removeModal()
    data <- values$soc_data
    # Run regression based on analysis type
    if (analysis_type() == "Frequentist") {
      fit_unadjusted <- lm(Income ~ Education, data = data)
    } else {
      fit_unadjusted <- brm(Income ~ Education, data = data, family = gaussian(), silent = TRUE)
    }
    values$soc_fit_unadjusted <- fit_unadjusted
    
    # Show results
    showModal(modalDialog(
      title = "Unadjusted Model Results",
      p("Here are the results of the unadjusted model:"),
      tableOutput("soc_unadjusted_results"),
      if (analysis_type() == "Bayesian") {
        tagList(
          p("Posterior estimates of the coefficients:"),
          tableOutput("soc_unadjusted_posterior_table")
        )
      },
      p("Interpretation: The coefficient for Education indicates the effect of education level on income without considering work experience."),
      footer = tagList(
        modalButton("Close"),
        actionButton("soc_step5", "Proceed to Step 5")
      ),
      easyClose = FALSE,
      size = "l"
    ))
  })
  
  # Display unadjusted regression results
  output$soc_unadjusted_results <- renderTable({
    req(values$soc_fit_unadjusted)
    if (analysis_type() == "Frequentist") {
      summary(values$soc_fit_unadjusted)$coefficients
    } else {
      as.data.frame(fixef(values$soc_fit_unadjusted))
    }
  }, rownames = TRUE)
  
  # Display posterior estimates table for Bayesian model
  output$soc_unadjusted_posterior_table <- renderTable({
    req(values$soc_fit_unadjusted)
    if (analysis_type() == "Bayesian") {
      as.data.frame(fixef(values$soc_fit_unadjusted))
    }
  }, rownames = TRUE)
  
  # Step 5: Run the Adjusted Model
  observeEvent(input$soc_step5, {
    removeModal()
    showModal(modalDialog(
      title = "Step 5: Run the Adjusted Model",
      p("Now, let's adjust for Experience to see its impact on the relationship between Education and Income."),
      actionButton("soc_run_adjusted", "Run Adjusted Model"),
      footer = modalButton("Close"),
      easyClose = FALSE
    ))
  })
  
  # Run adjusted model
  observeEvent(input$soc_run_adjusted, {
    removeModal()
    data <- values$soc_data
    # Run regression based on analysis type
    if (analysis_type() == "Frequentist") {
      fit_adjusted <- lm(Income ~ Education + Experience, data = data)
    } else {
      fit_adjusted <- brm(Income ~ Education + Experience, data = data, family = gaussian(), silent = TRUE)
    }
    values$soc_fit_adjusted <- fit_adjusted
    
    # Show results
    showModal(modalDialog(
      title = "Adjusted Model Results",
      p("Here are the results of the adjusted model:"),
      tableOutput("soc_adjusted_results"),
      if (analysis_type() == "Bayesian") {
        tagList(
          p("Posterior estimates of the coefficients:"),
          tableOutput("soc_adjusted_posterior_table")
        )
      },
      p("Interpretation: After adjusting for Experience, the effect of Education on Income changes."),
      footer = tagList(
        modalButton("Close"),
        actionButton("soc_step6", "Compare Models")
      ),
      easyClose = FALSE,
      size = "l"
    ))
  })
  
  # Display adjusted regression results
  output$soc_adjusted_results <- renderTable({
    req(values$soc_fit_adjusted)
    if (analysis_type() == "Frequentist") {
      summary(values$soc_fit_adjusted)$coefficients
    } else {
      as.data.frame(fixef(values$soc_fit_adjusted))
    }
  }, rownames = TRUE)
  
  # Display posterior estimates table for Bayesian model
  output$soc_adjusted_posterior_table <- renderTable({
    req(values$soc_fit_adjusted)
    if (analysis_type() == "Bayesian") {
      as.data.frame(fixef(values$soc_fit_adjusted))
    }
  }, rownames = TRUE)
  
  # Step 6: Compare Models
  observeEvent(input$soc_step6, {
    removeModal()
    showModal(modalDialog(
      title = "Step 6: Compare Models and Interpret Results",
      p("Let's compare the unadjusted and adjusted models:"),
      tableOutput("soc_compare_results"),
      p("Interpretation:"),
      p("In the unadjusted model, the coefficient for Education suggests a negative relationship with Income."),
      p("After adjusting for Experience, the coefficient for Education becomes positive, indicating that higher education actually leads to higher income when accounting for experience."),
      p("This demonstrates how confounding variables can mask the true relationship between variables."),
      footer = modalButton("Close"),
      easyClose = FALSE
    ))
  })
  
  # Display comparison of models
  output$soc_compare_results <- renderTable({
    req(values$soc_fit_unadjusted, values$soc_fit_adjusted)
    if (analysis_type() == "Frequentist") {
      unadjusted_coef <- summary(values$soc_fit_unadjusted)$coefficients
      adjusted_coef <- summary(values$soc_fit_adjusted)$coefficients
      # Extract coefficients for Education
      unadj_education <- unadjusted_coef["Education", ]
      adj_education <- adjusted_coef["Education", ]
      # Combine into a table
      results <- data.frame(
        Model = c("Unadjusted", "Adjusted"),
        Estimate = c(unadj_education["Estimate"], adj_education["Estimate"]),
        `Std. Error` = c(unadj_education["Std. Error"], adj_education["Std. Error"]),
        `P-value` = c(unadj_education["Pr(>|t|)"], adj_education["Pr(>|t|)"])
      )
    } else {
      # For Bayesian approach
      unadjusted_coef <- as.data.frame(fixef(values$soc_fit_unadjusted))
      adjusted_coef <- as.data.frame(fixef(values$soc_fit_adjusted))
      # Extract coefficients for Education
      unadj_education <- unadjusted_coef["Education", ]
      adj_education <- adjusted_coef["Education", ]
      # Combine into a table
      results <- data.frame(
        Model = c("Unadjusted", "Adjusted"),
        Estimate = c(unadj_education["Estimate"], adj_education["Estimate"]),
        `Est. Error` = c(unadj_education["Est.Error"], adj_education["Est.Error"]),
        `Q2.5` = c(unadj_education["Q2.5"], adj_education["Q2.5"]),
        `Q97.5` = c(unadj_education["Q97.5"], adj_education["Q97.5"])
      )
    }
    results
  }, rownames = FALSE)
  
  # Proceed to next tab when "Next" is clicked
  observeEvent(input$next_intro, {
    updateTabsetPanel(session, inputId = "navbar", selected = "Common DAG Structures")
  })
  
  output$dag_structures_content <- renderUI({
    tagList(
      h3("Understanding Common DAG Structures"),
      p("In this section, we'll explore common DAG structures and see how accounting for different variables affects our statistical analysis."),
      br(),
      selectInput("dag_type", "Choose a DAG structure to explore:",
                  choices = c("Confounder", "Collider", "Mediator/Chain", "Fork")),
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
        p("In this DAG, Z is a confounder that affects both X and Y. If you want to understand the \"direct effect\" of X on Y, you need to account for Z in your model.")
      )
    } else if (dag_type == "Collider") {
      dag_values$dag <- graph.formula(X -+ Z, Y -+ Z)
      dag_values$description <- tagList(
        h4("Collider DAG"),
        p("In this DAG, Z is a collider that is influenced by both X and Y. If you want to understand the effect of X on Y and accidentally include a collider in your model, you will induce a spurious negative effect of X on Y!")
      )
    } else if (dag_type == "Mediator/Chain") {
      dag_values$dag <- graph.formula(X -+ Z, Z -+ Y)
      dag_values$description <- tagList(
        h4("Mediator DAG"),
        p("In this DAG, Z is a mediator between X and Y. This DAG says that X affects Y only through Z. If we control for Z in our regression, then we will not see the effect of X on Y! An example of this that helps me, if X represents a fungal treatment for soil, Z represents amount of fungus in soil, and Y represents plant growth then if we account for ")
      )
    } else if (dag_type == "Fork") {
      dag_values$dag <- graph.formula(Z -+ X, Z -+ Y)
      dag_values$description <- tagList(
        h4("Fork DAG"),
        p("In this DAG, Z is a common cause of both X and Y.")
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
  
  
  
  
  
  
  
  
  
  
  # Understanding Paradoxes content
  output$paradox_content <- renderUI({
    req(user_field())
    tagList(
      h3("Understanding Paradoxes with DAGs"),
      p(paste("In", user_field(), ", it's important to be aware of statistical paradoxes that can arise in data analysis.")),
      if (user_field() == "Economics") {
        p("For example, Simpson's Paradox can occur when aggregated data hides underlying trends present in subgroups.")
      } else if (user_field() == "Biostatistics/Epidemiology") {
        p("Understanding selection bias through DAGs can help in designing better studies and avoiding incorrect conclusions.")
      } else if (user_field() == "Social Sciences") {
        p("DAGs can help untangle complexities when analyzing survey data.")
      } else if (user_field() == "Engineering") {
        p("DAGs can identify potential paradoxes in system design by highlighting feedback loops and dependencies.")
      } else {
        p("Paradoxes like Simpson's and Berkson's Paradox can affect data interpretation in any field.")
      },
      actionButton("next_paradox", "Next")
    )
  })
  
  # Proceed to next tab when "Next" is clicked
  observeEvent(input$next_paradox, {
    updateTabsetPanel(session, inputId = "navbar", selected = "Regression Estimates")
  })
  
  
  
  # Regression Estimates content
  output$regression_content <- renderUI({
    req(user_field())
    tagList(
      h3("Regression Estimates and Causal Inference"),
      p("Understanding how to properly adjust for variables in regression models is crucial for causal inference."),
      if (user_field() == "Economics") {
        p("In economics, failing to account for confounding variables can lead to incorrect policy recommendations.")
      } else if (user_field() == "Biostatistics/Epidemiology") {
        p("Proper adjustment affects conclusions about treatment effectiveness in medical studies.")
      } else if (user_field() == "Social Sciences") {
        p("Considering mediating variables can provide deeper insights into social phenomena.")
      } else if (user_field() == "Engineering") {
        p("Accounting for relevant variables is key when predicting system performance.")
      } else {
        p("Regardless of the field, understanding regression estimates is important for accurate data analysis.")
      },
      actionButton("next_regression", "Next")
    )
  })
  
  # Proceed to next tab when "Next" is clicked
  observeEvent(input$next_regression, {
    updateTabsetPanel(session, inputId = "navbar", selected = "Interactive DAG Builder")
  })
  
  # Interactive DAG Builder content
  output$builder_content <- renderUI({
    req(user_name())
    tagList(
      h3("Build Your Own DAG"),
      p(paste(user_name(), ", now it's time to apply what you've learned by building your own DAG.")),
      p("Interactive DAG builder coming soon!")
    )
  })
}

# Run the application
shinyApp(ui = ui, server = server)






##########################################################################################################################################
##########################################################################################################################################
##########################################################################################################################################
##########################################################################################################################################
##########################################################################################################################################
##########################################################################################################################################
##########################################################################################################################################
##########################################################################################################################################
##########################################################################################################################################




# Load necessary libraries
library(shiny)
library(simDAG)
library(dagitty)
library(ggplot2)
library(brms)

# UI Function
sandboxUI <- function(id) {
  ns <- NS(id)
  tagList(
    h3("Sandbox: Create Your Own DAG"),
    sidebarLayout(
      sidebarPanel(
        h4("Variables"),
        actionButton(ns("add_variable"), "Add Variable"),
        uiOutput(ns("variables_list")),
        hr(),
        
        # Exposure and Outcome Selection
        h4("Select Exposure and Outcome"),
        selectInput(ns("exposure_var"), "Select Exposure Variable:", choices = NULL, selected = NULL),
        selectInput(ns("outcome_var"), "Select Outcome Variable:", choices = NULL, selected = NULL),
        
        hr(),
        h4("DAG Structure"),
        p("Define parent-child relationships:"),
        uiOutput(ns("dag_structure_inputs")),
        actionButton(ns("submit_dag"), "Submit DAG")
        # Removed "Compute Adjustment Sets" button from here
      ),
      mainPanel(
        tabsetPanel(
          tabPanel("DAG", plotOutput(ns("dag_plot"))),
          tabPanel("Adjustment Sets",
                   actionButton(ns("compute_adjustments"), "Compute Adjustment Sets"),  # Moved button here
                   verbatimTextOutput(ns("adjustment_sets"))
          ),
          tabPanel("Simulate Data",
                   actionButton(ns("simulate_data"), "Simulate Data"),
                   p('Head of 1000 row simulated dataset:'),
                   tableOutput(ns("data_table"))  # Display simulated data
          ),
          tabPanel("Regression",
                   h4("Regression Analysis"),
                   p("Selected Analysis Type: ", textOutput(ns("analysis_type_display"))),
                   p("Select variables to adjust for in your model:"),
                   uiOutput(ns("adjustment_vars_ui")),
                   actionButton(ns("run_regression"), "Run Regression"),
                   hr(),
                   h4("Regression Results"),
                   tableOutput(ns("regression_table")),
                   plotOutput(ns("regression_plot"))
          )
        )
      )
    )
  )
}

# Server Function
sandboxServer <- function(id, analysis_type = reactiveVal("Frequentist")) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Reactive values to store variables and data
    rv <- reactiveValues(
      variables = data.frame(
        Name = character(),
        Type = character(),
        stringsAsFactors = FALSE
      ),
      dag = NULL,           # simDAG object
      dagitty_dag = NULL,   # dagitty object
      data = NULL,          # Simulated data
      user_model = NULL,    # User's regression model
      correct_model = NULL  # Correct regression model
    )
    
    # Observe 'Add Variable' button click to show modal dialog
    observeEvent(input$add_variable, {
      showModal(modalDialog(
        title = "Add Variable",
        textInput(ns("modal_var_name"), "Variable Name"),
        selectInput(ns("modal_var_type"), "Variable Type", choices = c("Continuous", "Binary", "Count")),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("confirm_add_variable"), "Add Variable")
        ),
        easyClose = FALSE
      ))
    })
    
    # Add the variable when confirmed
    observeEvent(input$confirm_add_variable, {
      var_name <- input$modal_var_name
      if (is.null(var_name) || var_name == "" || var_name %in% rv$variables$Name) {
        showNotification("Variable name cannot be empty or duplicated.", type = "error")
        return()
      }
      
      # Append to variables data frame
      rv$variables <- rbind(rv$variables, data.frame(
        Name = var_name,
        Type = input$modal_var_type,
        stringsAsFactors = FALSE
      ))
      
      # Update choices for exposure and outcome variables
      updateSelectInput(session, "exposure_var", choices = rv$variables$Name)
      updateSelectInput(session, "outcome_var", choices = rv$variables$Name)
      
      removeModal()
    })
    
    # Display list of variables
    output$variables_list <- renderUI({
      if (nrow(rv$variables) == 0) {
        p("No variables added yet.")
      } else {
        tagList(lapply(seq_len(nrow(rv$variables)), function(i) {
          var <- rv$variables[i, ]
          div(
            strong(var$Name),
            p("Type:", var$Type),
            hr()
          )
        }))
      }
    })
    
    # DAG Structure Inputs
    output$dag_structure_inputs <- renderUI({
      if (nrow(rv$variables) < 2) {
        p("Add at least two variables to define DAG structure.")
      } else {
        tagList(
          lapply(rv$variables$Name, function(child_var) {
            selectizeInput(ns(paste0("parents_of_", child_var)),
                           label = paste("Parents of", child_var),
                           choices = rv$variables$Name[rv$variables$Name != child_var],
                           multiple = TRUE)
          })
        )
      }
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
    
    # Render the DAG plot (using simDAG)
    output$dag_plot <- renderPlot({
      req(rv$dag)
      plot(rv$dag)
    })
    
    # Calculate adjustment sets when 'Compute Adjustment Sets' button is clicked
    observeEvent(input$compute_adjustments, {
      req(rv$dagitty_dag)
      req(input$exposure_var)
      req(input$outcome_var)
      
      print("Computing adjustment sets...")
      
      exposure <- input$exposure_var
      outcome <- input$outcome_var
      
      # Calculate adjustment sets using dagitty DAG
      adj_sets <- dagitty::adjustmentSets(rv$dagitty_dag, exposure = exposure, outcome = outcome)
      
      output$adjustment_sets <- renderPrint({
        if (is.null(adj_sets) || length(adj_sets) == 0) {
          print("No adjustment sets available for the selected exposure and outcome.")
        } else {
          print("Adjustment sets:")
          print(adj_sets)
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









#########################################################################################################################################################################################
##########################################################################################################################################
##########################################################################################################################################
##########################################################################################################################################
##########################################################################################################################################
##########################################################################################################################################
##########################################################################################################################################
##########################################################################################################################################
####################################################





# modules/sandbox_module.R

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
                   tableOutput(ns("data_table"))
          ),
          tabPanel("Regression",
                   uiOutput(ns("regression_ui")),
                   actionButton(ns("run_regression"), "Run Regression"),
                   tableOutput(ns("regression_table"))
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
                    choices = c("Continuous", "Binary", "Count", "Time-to-Event", "Categorical")),
        conditionalPanel(
          condition = paste0("input['", ns("modal_var_type"), "'] == 'Categorical'"),
          numericInput(ns("modal_num_categories"), "Number of Categories", value = 3, min = 2)
        ),
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
    
    # Submit DAG
    observeEvent(input$submit_dag, {
      req(input$exposure_var, input$outcome_var)
      dag <- simDAG::empty_dag()
      
      for (var_name in rv$variables$Name) {
        var_info <- rv$variables[rv$variables$Name == var_name, ]
        parents <- input[[paste0("parents_of_", var_name)]]
        if (length(parents) == 0) {
          parents <- NULL
        }
        
        if (is.null(parents)) {
          # Handle root nodes
          if (var_info$Type == "Continuous") {
            dag <- dag + simDAG::node(
              name = var_name,
              type = "rnorm",
              mean = 0,
              sd = 1
            )
          } else if (var_info$Type == "Binary") {
            dag <- dag + simDAG::node(
              name = var_name,
              type = "rbernoulli",
              p = 0.5
            )
          } else if (var_info$Type == "Count") {
            dag <- dag + simDAG::node(
              name = var_name,
              type = "rpois",
              lambda = 1
            )
          } else if (var_info$Type == "Categorical") {
            num_categories <- input$modal_num_categories
            dag <- dag + simDAG::node(
              name = var_name,
              type = "rcategorical",
              probs = rep(1 / num_categories, num_categories),
              labels = paste0("Category_", 1:num_categories)
            )
          } else {
            stop("Unknown variable type for root node")
          }
        } else {
          # Handle child nodes
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
              betas = rep(1, length(parents)),
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
          } else if (var_info$Type == "Time-to-Event") {
            dag <- dag + simDAG::node_td(
              name = var_name,
              type = "time_to_event",
              parents = as.character(parents),
              prob_fun = function(data) rep(0.01, nrow(data)),
              event_duration = 1
            )
          } else if (var_info$Type == "Categorical") {
            num_categories <- input$modal_num_categories
            dag <- dag + simDAG::node(
              name = var_name,
              type = "multinomial",
              parents = as.character(parents),
              betas = matrix(1, ncol = length(parents), nrow = num_categories - 1),
              intercepts = rep(0, num_categories - 1),
              labels = paste0("Category_", 1:num_categories)
            )
          } else {
            stop("Unknown variable type for child node")
          }
        }
      }
      
      # Save the constructed DAG to reactive values
      rv$dag <- dag
      
      
      # Create dagitty DAG for adjustment sets
      dagitty_string <- "dag {"
      for (var_name in rv$variables$Name) {
        parents <- input[[paste0("parents_of_", var_name)]]
        if (length(parents) > 0) {
          for (parent in parents) {
            dagitty_string <- paste0(dagitty_string, parent, " -> ", var_name, " ; ")
          }
        } else {
          dagitty_string <- paste0(dagitty_string, var_name, " ; ")
        }
      }
      dagitty_string <- paste0(dagitty_string, "}")
      
      rv$dagitty_dag <- dagitty::dagitty(dagitty_string)
      exposures(rv$dagitty_dag) <- input$exposure_var
      outcomes(rv$dagitty_dag) <- input$outcome_var
      
      # Plot the DAG
      output$dag_plot <- renderPlot({
        ggdag(rv$dagitty_dag) + theme_dag()
      })
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
    
    # Simulate Data
    observeEvent(input$simulate_data, {
      req(rv$dag)
      
      # Check for Time-to-Event variables
      if ("Time-to-Event" %in% rv$variables$Type) {
        rv$data <- simDAG::sim_discrete_time(rv$dag, n_sim = 1000, max_t = 100)
        rv$data <- rv$data$data_final_state
      } else {
        rv$data <- simDAG::sim_from_dag(rv$dag, n_sim = 1000)
      }
      
      # Convert logical variables to numeric
      rv$data[] <- lapply(rv$data, function(x) {
        if (is.logical(x)) as.numeric(x) else x
      })
      
      # Convert categorical variables to factors
      rv$data[] <- lapply(rv$data, function(x) {
        if (is.character(x)) as.factor(x) else x
      })
      
      # Render data table
      output$data_table <- renderTable({
        head(rv$data)
      })
    })
    
    # Regression UI
    output$regression_ui <- renderUI({
      req(rv$data)
      selectizeInput(ns("adjustment_vars"), "Adjustment Variables",
                     choices = setdiff(rv$variables$Name, c(input$exposure_var, input$outcome_var)),
                     multiple = TRUE)
    })
    
    # Run Regression
    observeEvent(input$run_regression, {
      req(rv$data)
      req(input$exposure_var)
      req(input$outcome_var)
      
      # Determine the correct regression model based on outcome type
      outcome_type <- rv$variables$Type[rv$variables$Name == input$outcome_var]
      if (outcome_type == "Continuous") {
        model_family <- gaussian(link = 'identity')
      } else if (outcome_type == "Binary") {
        if (analysis_type() == "Frequentist") {
          model_family <- binomial(link = 'logit')
        } else if (analysis_type() == "Bayesian") {
          model_family <- bernoulli(link = 'logit')
        }
      } else if (outcome_type == "Count") {
        model_family <- poisson(link = 'log')
      } else if (outcome_type == "Time-to-Event") {
        model_family <- "survival"
      } else if (outcome_type == "Categorical") {
        model_family <- "multinomial"
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
      
      # Convert variables to appropriate types
      rv$data[] <- lapply(rv$data, function(x) {
        if (is.character(x)) as.factor(x) else x
      })
      
      # Run user's regression model
      if (analysis_type() == "Frequentist") {
        if (model_family == "survival") {
          # Survival analysis
          user_formula <- as.formula(paste("Surv(", input$outcome_var, "_time, ", input$outcome_var, "_event) ~", paste(c(input$exposure_var, user_adjust_vars), collapse = " + ")))
          correct_formula <- as.formula(paste("Surv(", input$outcome_var, "_time, ", input$outcome_var, "_event) ~", paste(c(input$exposure_var, correct_adjust_vars), collapse = " + ")))
          
          user_model <- survival::coxph(user_formula, data = rv$data)
          correct_model <- survival::coxph(correct_formula, data = rv$data)
        } else if (model_family == "multinomial") {
          # Multinomial regression
          user_model <- nnet::multinom(user_formula, data = rv$data)
          correct_model <- nnet::multinom(correct_formula, data = rv$data)
        } else {
          user_model <- glm(user_formula, data = rv$data, family = model_family)
          correct_model <- glm(correct_formula, data = rv$data, family = model_family)
        }
      } else if (analysis_type() == "Bayesian") {
        if (model_family == "survival") {
          # Bayesian survival analysis
          user_formula <- as.formula(paste("bf(Surv(", input$outcome_var, "_time, ", input$outcome_var, "_event) ~", paste(c(input$exposure_var, user_adjust_vars), collapse = " + "), ")"))
          correct_formula <- as.formula(paste("bf(Surv(", input$outcome_var, "_time, ", input$outcome_var, "_event) ~", paste(c(input$exposure_var, correct_adjust_vars), collapse = " + "), ")"))
          
          user_model <- brms::brm(
            formula = user_formula,
            data = rv$data,
            family = brms::cox(),
            chains = 2,
            iter = 2000,
            refresh = 0,
            seed = 123
          )
          correct_model <- brms::brm(
            formula = correct_formula,
            data = rv$data,
            family = brms::cox(),
            chains = 2,
            iter = 2000,
            refresh = 0,
            seed = 123
          )
        } else if (model_family == "multinomial") {
          # Bayesian multinomial regression
          user_model <- brms::brm(
            formula = user_formula,
            data = rv$data,
            family = brms::categorical(),
            chains = 2,
            iter = 2000,
            refresh = 0,
            seed = 123
          )
          correct_model <- brms::brm(
            formula = correct_formula,
            data = rv$data,
            family = brms::categorical(),
            chains = 2,
            iter = 2000,
            refresh = 0,
            seed = 123
          )
        } else {
          user_model <- brms::brm(
            formula = user_formula,
            data = rv$data,
            family = model_family,
            chains = 2,
            iter = 2000,
            refresh = 0,
            seed = 123
          )
          correct_model <- brms::brm(
            formula = correct_formula,
            data = rv$data,
            family = model_family,
            chains = 2,
            iter = 2000,
            refresh = 0,
            seed = 123
          )
        }
      }
      
      rv$user_model <- user_model
      rv$correct_model <- correct_model
      
      # Display regression results
      output$regression_table <- renderTable({
        if (model_family == "survival") {
          if (analysis_type() == "Frequentist") {
            user_coef <- summary(rv$user_model)$coefficients
            correct_coef <- summary(rv$correct_model)$coefficients
            all_terms <- union(rownames(user_coef), rownames(correct_coef))
            
            compare_table <- data.frame(
              Term = all_terms,
              User_Hazard_Ratio = exp(user_coef[all_terms, "coef"]),
              User_p_value = user_coef[all_terms, "Pr(>|z|)"],
              Correct_Hazard_Ratio = exp(correct_coef[all_terms, "coef"]),
              Correct_p_value = correct_coef[all_terms, "Pr(>|z|)"]
            )
          } else {
            # Bayesian survival analysis
            user_summary <- summary(rv$user_model)$fixed
            correct_summary <- summary(rv$correct_model)$fixed
            all_terms <- union(rownames(user_summary), rownames(correct_summary))
            
            compare_table <- data.frame(
              Term = all_terms,
              User_Estimate = user_summary[all_terms, "Estimate"],
              User_Std_Error = user_summary[all_terms, "Est.Error"],
              Correct_Estimate = correct_summary[all_terms, "Estimate"],
              Correct_Std_Error = correct_summary[all_terms, "Est.Error"]
            )
          }
        } else if (model_family == "multinomial") {
          if (analysis_type() == "Frequentist") {
            user_coef <- summary(rv$user_model)$coefficients
            user_se <- summary(rv$user_model)$standard.errors
            correct_coef <- summary(rv$correct_model)$coefficients
            correct_se <- summary(rv$correct_model)$standard.errors
            
            compare_table <- data.frame(
              Term = rownames(user_coef),
              User_Estimate = as.vector(user_coef),
              User_Std_Error = as.vector(user_se),
              Correct_Estimate = as.vector(correct_coef),
              Correct_Std_Error = as.vector(correct_se)
            )
          } else {
            # Bayesian multinomial regression
            user_summary <- summary(rv$user_model)$fixed
            correct_summary <- summary(rv$correct_model)$fixed
            all_terms <- union(rownames(user_summary), rownames(correct_summary))
            
            compare_table <- data.frame(
              Term = all_terms,
              User_Estimate = user_summary[all_terms, "Estimate"],
              User_Std_Error = user_summary[all_terms, "Est.Error"],
              Correct_Estimate = correct_summary[all_terms, "Estimate"],
              Correct_Std_Error = correct_summary[all_terms, "Est.Error"]
            )
          }
        } else {
          if (analysis_type() == "Frequentist") {
            user_coef <- coef(summary(rv$user_model))
            correct_coef <- coef(summary(rv$correct_model))
            all_terms <- union(rownames(user_coef), rownames(correct_coef))
            
            compare_table <- data.frame(
              Term = all_terms,
              User_Estimate = user_coef[all_terms, "Estimate"],
              User_Std_Error = user_coef[all_terms, "Std. Error"],
              Correct_Estimate = correct_coef[all_terms, "Estimate"],
              Correct_Std_Error = correct_coef[all_terms, "Std. Error"]
            )
          } else {
            user_summary <- summary(rv$user_model)$fixed
            correct_summary <- summary(rv$correct_model)$fixed
            all_terms <- union(rownames(user_summary), rownames(correct_summary))
            
            compare_table <- data.frame(
              Term = all_terms,
              User_Estimate = user_summary[all_terms, "Estimate"],
              User_Std_Error = user_summary[all_terms, "Est.Error"],
              Correct_Estimate = correct_summary[all_terms, "Estimate"],
              Correct_Std_Error = correct_summary[all_terms, "Est.Error"]
            )
          }
        }
        return(compare_table)
      })
    })
  })
}


