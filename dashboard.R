# Load required libraries
library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(stringr)
library(haven)

x_min_lim <- -10
x_max_lim <- 9
# Define filter variables
filter_variables <- c('cohort', 'controls', 'wboot', 'estimation', 
                      'outcome', 'treatment_group')

es_results <- read_dta('Output/Tables/CS_results_matching.dta') %>%
  filter(!str_detect(var, '_avg')) %>%
  mutate(
    wboot = as.logical(wboot), 
    controls = as.logical(controls),
    dist = str_replace(var, 'tm', '-') %>% str_replace('tp', '') %>% as.integer()
  ) %>%
  filter(between(dist, x_min_lim, x_max_lim)) %>% 
  arrange(across(all_of(filter_variables)))

# Define UI for application
ui <- fluidPage(
  
  # Application title
  titlePanel("Event Study Dashboard"),
  
  # Sidebar layout with a input panel and output panel
  sidebarLayout(
    
    # Sidebar panel for inputs
    sidebarPanel(
      
      # Select input for each filter variable
      lapply(filter_variables, function(var) {
        multiple_choice <- TRUE
        # Default values.
        if (var %in% c('controls', 'wboot')) {
          default_values <- 'TRUE'
        } else if (var == 'cohort') {
          default_values <- '1995'
        } else if (var == 'outcome') {
          default_values <- 'pila_salario_r'
          multiple_choice <- FALSE
        } 
        else {
          default_values <- NULL
        }
        pickerInput(
          inputId = var,
          label = var,
          choices = unique(es_results[[var]]),
          multiple = multiple_choice,
          options = list(`actions-box` = TRUE),
          selected = default_values
        )
      }),
      
      # Checkbox group for coloring options
      pickerInput(
        inputId = "color_vars",
        label = "Color Points by:",
        choices = filter_variables,
        multiple = TRUE,
        selected = c('estimation', 'treatment_group'),
        options = list(`actions-box` = TRUE)
      )
      
    ),
    
    
    # Main panel for displaying outputs
    mainPanel(
      
      # Plot output
      plotOutput(outputId = "plot", width = "95%", height = '600', click = TRUE)
      
    )
  )
)

# Define server logic required to plot data
server <- function(input, output) {

  # Reactive expression to filter data based on user inputs
  filtered_data <- reactive({
    filtered <- es_results
    
    # Apply filters based on user inputs
    for (var in filter_variables) {
      # print(input[[var]])
      # print(filtered %>% glimpse)
      if (!is.null(input[[var]]) && length(input[[var]]) > 0) {
        filter_values <- as.character(input[[var]])
        filtered <- filtered %>% filter(!!sym(var) %in% filter_values)
      }
    }
    
    return(filtered)
  })
  
  # Reactive expression to get selected color variables
  selected_color_vars <- reactive({
    input$color_vars
  })
  
  # Render the plot based on filtered data
  output$plot <- renderPlot({
    filtered <- filtered_data()
    # Plot using ggplot
    ggplot(filtered, 
           aes(
             x = as.factor(dist), y = coef,
             color = paste(!!!syms(selected_color_vars()))
               )) +
      geom_hline(yintercept = 0, color = 'black', linewidth = 0.25) +
      geom_point() +
      geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper)) +
      labs(x = "Semesters from enrollment", y = 'Estimates') + 
      theme_minimal() +
      scale_y_continuous(n.breaks = 10) +
      theme(legend.position="bottom") +
      theme(legend.title=element_blank())
  })
}

# Run the application
shinyApp(ui = ui, server = server)
