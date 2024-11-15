# modules/economics_module.R
library(shiny)
library(ggplot2)
library(igraph)
library(simDAG)
library(brms)
library(bayesplot)


economicsUI <- function(id) {
  ns <- NS(id)
  tagList(
    h5("Economics Example: The Employment Training Program"),
    p("Suppose we are evaluating the effectiveness of a job training program..."),
    actionButton(ns("econ_step1"), "Step 1: Understand the Scenario")
  )
}

economicsServer <- function(id, analysis_type) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    values <- reactiveValues()
    
    # Step 1: Understand the Scenario
    observeEvent(input$econ_step1, {
      showModal(modalDialog(
        title = "Step 1: Understanding the Scenario",
        p("In this example, we want to evaluate whether a job training program (Program) is effective in increasing employment rates (Employment)."),
        p("However, participants' education level (Education) may influence both their likelihood of joining the program and their employment outcomes."),
        p("This creates a confounding effect that we need to account for."),
        footer = tagList(
          modalButton("Close"),
          actionButton(ns("econ_step2"), "Proceed to Step 2")
        ),
        easyClose = FALSE
      ))
    })
    
    # Step 2: Simulate the Data
    observeEvent(input$econ_step2, {
      removeModal()
      showModal(modalDialog(
        title = "Step 2: Simulate the Data",
        p("We will now simulate data representing this situation, where the \"direct\" effect of the program would have a coefficent of 1, but is confounded by employment status."),
        actionButton(ns("econ_simulate_data"), "Simulate Data"),
        footer = modalButton("Close"),
        easyClose = FALSE
      ))
    })
    
    # Simulate the data
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
          actionButton(ns("econ_step3"), "Proceed to Step 3")
        ),
        easyClose = FALSE
      ))
    })
    
    # Step 3: Visualize the DAG and Data
    observeEvent(input$econ_step3, {
      removeModal()
      
      # Define outputs here
      # Render DAG plot
      output$econ_dag_plot <- renderPlot({
        req(values$econ_dag_obj)
        adj_matrix <- dag2matrix(values$econ_dag_obj)
        g <- graph_from_adjacency_matrix(adj_matrix, mode = "directed")
        plot(g, vertex.label = V(g)$name, main = "DAG Representing Situation",
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
      
      # Plot the confounder vs. treatment variable
      output$econ_conf_treat_plot <- renderPlot({
        req(values$econ_data)
        ggplot(values$econ_data, aes(x = Education, y = Program)) +
          geom_jitter(width = 0.2, height = 0.2, alpha = 0.5) +
          labs(title = "Education vs Program Participation",
               x = "Education Level", y = "Program Participation")
      })
      
      showModal(modalDialog(
        title = "Step 3: Visualize the DAG and Data",
        p("Here is the DAG representing the causal relationships in our scenario:"),
        plotOutput(ns("econ_dag_plot")),
        p("Let's also look at the relationship between Program participation and Employment:"),
        plotOutput(ns("econ_data_plot")),
        p("Additionally, let's examine the relationship between Education and Program participation:"),
        plotOutput(ns("econ_conf_treat_plot")),  # Plot the confounder vs. treatment
        footer = tagList(
          modalButton("Close"),
          actionButton(ns("econ_step4"), "Proceed to Step 4")
        ),
        easyClose = FALSE,
        size = "l"
      ))
    })
    
    # Step 4: Run the Unadjusted Model
    observeEvent(input$econ_step4, {
      removeModal()
      showModal(modalDialog(
        title = "Step 4: Run the Unadjusted Model",
        p("We will now run a regression model to see the effect of the Program on Employment without adjusting for Education."),
        actionButton(ns("econ_run_unadjusted"), "Run Unadjusted Model"),
        footer = modalButton("Close"),
        easyClose = FALSE
      ))
    })
    
    # Run unadjusted model
    observeEvent(input$econ_run_unadjusted, {
      removeModal()
      data <- values$econ_data
      
      # Initialize progress indicator
      if (analysis_type() == "Bayesian") {
        withProgress(message = 'Running Bayesian Model...', value = 0, {
          # Simulate progress
          n_iter <- 100  # Number of progress increments
          for (i in 1:n_iter) {
            incProgress(1/n_iter)
            Sys.sleep(0.001)  # Simulate computation time
          }
          # Run Bayesian model
          fit_unadjusted <- brm(Employment ~ Program, data = data, family = bernoulli(), silent = TRUE)
        })
      } else {
        # Run Frequentist model
        fit_unadjusted <- glm(Employment ~ Program, data = data, family = binomial)
      }
      values$fit_unadjusted <- fit_unadjusted
      
      # Define outputs here
      output$econ_unadjusted_results <- renderTable({
        req(values$fit_unadjusted)
        if (analysis_type() == "Frequentist") {
          summary(values$fit_unadjusted)$coefficients
        } else {
          as.data.frame(fixef(values$fit_unadjusted))
        }
      }, rownames = TRUE)
      
      # Plot posterior distributions for unadjusted model (Bayesian only)
      output$econ_unadjusted_posterior <- renderPlot({
        req(values$fit_unadjusted)
        posterior <- as_draws_df(values$fit_unadjusted)
        mcmc_areas(posterior, pars = c("b_Intercept", "b_Program"))
      })
      
      # Set suspendWhenHidden = FALSE
      outputOptions(output, "econ_unadjusted_results", suspendWhenHidden = FALSE)
      outputOptions(output, "econ_unadjusted_posterior", suspendWhenHidden = FALSE)
      
      # Show results
      showModal(modalDialog(
        title = "Unadjusted Model Results",
        p("Here are the results of the unadjusted model. Notice how the coefficent on Program is much larger than 1, which is the true value. This shows us what can happen if we ignore the effect of Education on both Program participation and Employment:"),
        tableOutput(ns("econ_unadjusted_results")),
        if (analysis_type() == "Bayesian") {
          tagList(
            p("Posterior distributions of the coefficients. Notice how almost all of the posterior mass of the coefficent on Program is larger than 1. This shows us what can happen if we ignore the effect of Education on both Program participation and Employment:"),
            plotOutput(ns("econ_unadjusted_posterior"))
          )
        },
        footer = tagList(
          modalButton("Close"),
          actionButton(ns("econ_step5"), "Proceed to Step 5")
        ),
        easyClose = FALSE,
        size = "l"
      ))
    })
    
    
    # Step 5: Run the Adjusted Model
    observeEvent(input$econ_step5, {
      removeModal()
      showModal(modalDialog(
        title = "Step 5: Run the Adjusted Model",
        p("Now, let's adjust for Education to see its impact on the relationship between Program and Employment."),
        actionButton(ns("econ_run_adjusted"), "Run Adjusted Model"),
        footer = modalButton("Close"),
        easyClose = FALSE
      ))
    })
    
    # Run adjusted model
    observeEvent(input$econ_run_adjusted, {
      removeModal()
      data <- values$econ_data
      
      # Initialize progress indicator
      if (analysis_type() == "Bayesian") {
        withProgress(message = 'Running Bayesian Model...', value = 0, {
          # Simulate progress
          n_iter <- 100  # Number of progress increments
          for (i in 1:n_iter) {
            incProgress(1/n_iter)
            Sys.sleep(0.001)  # Simulate computation time
          }
          # Run Bayesian model
          fit_adjusted <- brm(Employment ~ Program + Education, data = data, family = bernoulli(), silent = TRUE)
        })
      } else {
        # Run Frequentist model
        fit_adjusted <- glm(Employment ~ Program + Education, data = data, family = binomial)
      }
      values$fit_adjusted <- fit_adjusted
      
      # Define outputs here
      output$econ_adjusted_results <- renderTable({
        req(values$fit_adjusted)
        if (analysis_type() == "Frequentist") {
          summary(values$fit_adjusted)$coefficients
        } else {
          as.data.frame(fixef(values$fit_adjusted))
        }
      }, rownames = TRUE)
      
      # Plot posterior distributions for adjusted model (Bayesian only)
      output$econ_adjusted_posterior <- renderPlot({
        req(values$fit_adjusted)
        posterior <- as_draws_df(values$fit_adjusted)
        mcmc_areas(posterior, pars = c("b_Intercept", "b_Program", "b_Education"))
      })
      
      # Set suspendWhenHidden = FALSE
      outputOptions(output, "econ_adjusted_results", suspendWhenHidden = FALSE)
      outputOptions(output, "econ_adjusted_posterior", suspendWhenHidden = FALSE)
      
      # Show results
      showModal(modalDialog(
        title = "Adjusted Model Results",
        p("Here are the results of the adjusted model. We see that we do recover the correct results of a coefficent of around 1 on the effect of Program on Employment:"),
        tableOutput(ns("econ_adjusted_results")),
        if (analysis_type() == "Bayesian") {
          tagList(
            p("Posterior distributions of the coefficients. We see that we do recover the correct results of a coefficent of around 1 on the effect of Program on Employment:"),
            plotOutput(ns("econ_adjusted_posterior"))
          )
        },
        footer = tagList(
          modalButton("Close"),
          actionButton(ns("econ_step6"), "Compare Models")
        ),
        easyClose = FALSE,
        size = "l"
      ))
    })
    
    
    
    # Step 6: Compare Models
    observeEvent(input$econ_step6, {
      removeModal()
      
      # Define outputs here
      output$econ_compare_results <- renderTable({
        req(values$fit_unadjusted, values$fit_adjusted)
        if (analysis_type() == "Frequentist") {
          unadjusted_coef <- summary(values$fit_unadjusted)$coefficients
          adjusted_coef <- summary(values$fit_adjusted)$coefficients
          # Extract coefficients for Program
          unadj_program <- unadjusted_coef["Program", ]
          adj_program <- adjusted_coef["Program", ]
          # Calculate increase in odds
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
        # Round numeric columns for better display
        numeric_cols <- sapply(results, is.numeric)
        results[numeric_cols] <- lapply(results[numeric_cols], function(x) round(x, 3))
        results
      }, rownames = FALSE)
      
      # Set suspendWhenHidden = FALSE
      outputOptions(output, "econ_compare_results", suspendWhenHidden = FALSE)
      
      # Extract odds ratios for interpretation
      if (analysis_type() == "Frequentist") {
        unadj_or <- round(exp(coef(values$fit_unadjusted)["Program"]), 2)
        adj_or <- round(exp(coef(values$fit_adjusted)["Program"]), 2)
      } else {
        unadj_or <- round(exp(fixef(values$fit_unadjusted)["Program", "Estimate"]), 2)
        adj_or <- round(exp(fixef(values$fit_adjusted)["Program", "Estimate"]), 2)
      }
      
      # Show comparison results
      showModal(modalDialog(
        title = "Step 6: Compare Models and Interpret Results",
        p("Let's compare the unadjusted and adjusted models:"),
        div(style = "overflow-x: auto; max-height: 400px;",
            tableOutput(ns("econ_compare_results"))
        ),
        p("Interpretation:"),
        p("In the unadjusted model, the odds ratio for Program is ", unadj_or, ", indicating that participating in the program increases the odds of employment by a factor of ", unadj_or, "."),
        p("After adjusting for Education, the odds ratio changes to ", adj_or, ", suggesting that the effect of the program is different when accounting for education."),
        p("This demonstrates how failing to adjust for confounding variables can lead to misleading conclusions. In this case, without adjusting we overestimate the program's effectiveness! This simple example shows how bad analysis could lead to bad policy. "),
        p(strong('BUT'), "this does", strong("NOT"), "mean that all you need to do is adjust for every avaliable variable! Keep learning to find out what can go wrong if you do."),
        p("Note: An odds ratio of 1 indicates no effect; values greater than 1 indicate increased odds, and values less than 1 indicate decreased odds."),
        footer = modalButton("Close"),
        easyClose = FALSE,
        size = "xl"  # Set modal size to extra-large
      ))
    })
  })
}
