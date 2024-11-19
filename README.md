# DAG Learning App

This Shiny application is designed to teach users about Directed Acyclic Graphs (DAGs) and their use in causal inference, using an examples from various fields where DAGs are primarily used. The app demonstrates how confounding variables, such as age, can bias results if not accounted for in statistical models.


## Features

- **Educational Example**: Includes a guided walkthrough using a concrete example to show how causal inference principles can be applied in practice.
- **Interactive DAG Builder Sandbox**: Users can create their own DAGs by specifying variables, their types, and causal relationships.
- **Simulated Data**: The app generates synthetic data based on user-defined DAGs or pre-built examples.
- **Regression Analysis**: Run unadjusted and adjusted regression models to observe the impact of confounding variables on causal estimates.


## Installation

1. Clone this repository:
   ```bash
   git clone https://github.com/ml-rowlands/causal_inference_shinyapp.git
   cd casual_inference_shinyapp
   ```

2. Install required packages in .renv file or from this list:
   ```r
   install.packages(c('shiny','ggdag','igraph','simDAG','brms','bayesplot','daggity'))
   ```

3. Run the app in your IDE
   ```r
   library(shiny)
   runApp()
   ```

## Contributions 

App is still a work in progress and some aspects need to be cleaned up, but the Sandbox should work well for a low code DAG Explorer! 
Contributions are welcome! Feel free to open issues or submit pull requests to improve this app.





   
