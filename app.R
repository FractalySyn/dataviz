rm(list = ls())
required_libs = c('dplyr', 'ggplot2', 'plotly', 'readr', 'stringr', 'corrplot',
                  'shiny', 'knitr', 'bslib', 'gapminder', 'colourpicker',
                  'kableExtra', 'shinythemes', 'ggthemes', 'shinyhelper')
load_pack = function(lib = required_libs)
{
   if(!require(lib, character.only = TRUE)) install.packages(lib)
   library(lib, character.only = TRUE)
}
lapply(required_libs, load_pack); rm(required_libs)

# library(shiny); library(corrplot); library(ggplot2); library(knitr)
# library(dplyr); library(gapminder); library(colourpicker)
# library(readr); library(bslib); library(plotly); library(kableExtra)
# library(stringr); library(shinythemes); library(ggthemes)



# Functions ---------------------------------------------------------------

guess = function(var, max_categories = 20) {
   # var
   type = "other"
   
   # numeric - discrete if integer or can be coerced to integer - continuous else
   if(is.numeric(var)) {
      # if(length(var) < length(unique(var))*factor) type = "continuous"
      if(is.integer(var) | all(as.integer(var) == var)) type = "discrete"
      else type = "continuous"
   }
   
   # boolean - if logical or binary
   if(is.logical(var) | all(var %in% c(1, 0))) type = "boolean"
   
   # character
   if(is.character(var)) type = "string"
   
   # categorical - if factor or has few unique values
   if((type %in% c('discrete', 'string', 'continuous') & length(unique(var)) <= max_categories) | 
      is.factor(var)) type = "categorical"
   
   return(type)
}

my_summary = function(data){
   summary = data.frame()
   for(i in 1:ncol(data)){
      summary = rbind(summary, data.frame(`Column Name` = colnames(data)[i],
                                          `Column Class` = class(pull(data, i)),
                                          `Guessed Type` = guess(pull(data, i)),
                                          `Unique Values` = length(unique(pull(data, i))),
                                          `NAs` = sum(is.na(pull(data, i)))))
   }
   return(summary)
}



# App ---------------------------------------------------------------------

## Options
options(shiny.maxRequestSize=30*1024^2)

## UI ----
ui <- navbarPage(
   
   ## App Title
   title = "DataViz",
   
   ## Theme 
   # theme = bs_theme(version = 4, bootswatch = "flatly"),
   theme = shinytheme('lumen'),
   
   ## Tab1 : Data
   tabPanel(
      
      ## Page Title
      title = "Explore Data",
      
      fluidPage(
         
         ## Page Title
         titlePanel("Explore"),
         
         ## Create a layout with a sidebar
         sidebarLayout(
            
            ## Define sidebar objects 
            sidebarPanel(
               
               ## Dataset input
               selectInput(inputId = "dataset", label = "Choose a Dataset", 
                           choices = list(`Custom` = list('Upload your own CSV'),
                                          `Available datasets` = list('gapminder')), 
                           selected = 'gapminder', size = 20, selectize = F) %>%
                  helper(type = 'inline', title = 'Upload a dataset',
                         content = c("To upload your own dataset, select this option at the top of the selection panel. 
                                     Once selected, you will see an upload button below the panel. This lets you import 
                                     data from your computer.", "<hr>", 
                                     "Some samples are provided in the list so that
                       you can get in touch with the app. Furthermore, if the app is not able to work with your dataset, we recommend that you
                       look at the samples' formats. Typically, we discourage you to import non-tidy data as it could lead to unknown issues
                       and the crash of the app.")),
               ## Upload file input
               conditionalPanel("input.dataset == 'Upload your own CSV'",
                                fileInput(inputId = "datafile", "Upload csv", accept = ".csv")),
               
               tags$hr(),
               ## Size of data table
               numericInput(inputId = "n_rows", label = "Number of rows to load in the table (0 for all)",
                            value = 100, min = 0, max = 1000000, step = 1)
            ),
            
            ## Define Main Panel objects
            mainPanel(
               
               ## Sub-Tabs of the first Tab
               tabsetPanel(
                  
                  ## Corplot output
                  tabPanel("Correlations", 
                           
                           fluidRow(
                              ## Customization Panel
                              column(3, tags$h2("Customize"),
                                     tags$h4("Pick Variables") %>%
                                        helper(type = 'inline', title = 'Pick Variables for the Correlation Matrix',
                                               content = c('Note that only numeric variables are
                                            used to calculate correlations. By default the first ten variables 
                                            of the dataset are used for computation. You can easily choose the
                                            variables you want to compare in the correlation matrix.')),
                                     actionButton(inputId = "select_all", label = "Select all"),
                                     actionButton(inputId = "unselect_all", label = "Unselect all"),
                                     checkboxGroupInput(inputId = "variables_checkbox", label = ""),
                                     checkboxInput(inputId = "one_value_var", "Include 1-value variables ?") %>%
                                        helper(type = 'inline', title = 'Constant Variables',
                                               content = c("Constant variables have no variance and thus they are not correlated with other
                                                        variables. This results in question marks displayed in the matrix. 
                                                        We naturally remove these variables for the correlation matrix. However, you can
                                                        toggle them as this can help detect constant variables")),
                              tags$hr(),
                              selectInput(inputId = "method", label = "Upper Matrix Method",
                                          choices = c("color", "circle", "square", "pie", "shade", "ellipse")),
                              tags$hr(),
                              sliderInput(inputId = "shorten", label = "Column names lengths",
                                          value = 5, min = 2, max = 10, ticks = F),
                              textInput(inputId = "chars_to_remove", "Remove characters",
                                        value = "-_") %>%
                                 helper(type = 'inline', title = 'Remove characters from column names for the Matrix',
                                        content = c("You can choose characters to hide from names. By default we remove 
                                            '-' and '_' special characters by typing '-_' in the text box.",
                                                    "<hr>", "If you'd like to remove commas and points, just type <b>.,</b>"))),
                              
                              ## Matrix Display
                              column(9, plotOutput(outputId = "corplot", height = 600),
                                     p(strong('To save the Matrix right-click on it'), align = 'center'))
                           )),
                           
                           
                  
                  ## Summary output
                  tabPanel("Summary", tags$h1('Summary'),
                           verbatimTextOutput(outputId = "summary")),
                  
                  ## View / Head output
                  tabPanel("Table", dataTableOutput(outputId = "head"))
                  
               )
            )
         )
      )
   ),
   
   ## Tab2 : Plots
   tabPanel(
      
      ## Suppress Warnings on this Tab
      tags$style(type="text/css",
                 ".shiny-output-error { visibility: hidden; }",
                 ".shiny-output-error:before { visibility: hidden; }"),
      
      ## Page Title
      title = "Data Visualization",
      
      fluidPage(
         
         ## Page Title
         titlePanel("Visualization"),
         
         ## Create a layout with a sidebar
         sidebarLayout(
            
            ## Define sidebar objects 
            sidebarPanel(
               
               ## Variables Selection Input
               # tags$p(strong("Choose variables to plot")),
               tabsetPanel(
                  id = "vars",
                  tabPanel(title = "Check", 
                     checkboxGroupInput(inputId = "variables_plot", label = "")),
                  tabPanel(title = "Type",
                           selectizeInput(inputId = "variables_plot2", "", choices = NULL, multiple = T,
                                             options = list(plugins = list("remove_button"))))
               ),
               
               ## Threshold to be considered as a categorical variable
               sliderInput(inputId = "max_categ", label = "Categorical Threshold", value = 20, min = 2, max = 100, ticks = F) %>%
                  helper(type = 'inline', title = 'Categorical Threshold',
                         content = c('Here you can control the way we define a variable as categorical. Passed this threshold of different values,
                                     the variable will be considered asa discrete if numeric or as a character vector. Distributions of categorical 
                                     variables are represented with Bar plots.')),
               
               tags$hr(),
               
               ## Plot Selection
               selectInput(inputId = "plot_selection", label = "Choose a plotting method",
                           choices = c("None")),
               
               # Histogram & KDE Parameters
               conditionalPanel("input.plot_selection != 'None'",
                    tags$p(strong("Custom Chart Parameters")),
                    ## Barplot Parameters
                    conditionalPanel("input.plot_selection == 'Bars'",
                                     checkboxInput(inputId = "text", label = "Show Labels", value = T),
                                     conditionalPanel("input.text",
                                                      colourInput(inputId = 'text_col', label = 'Font Color', value = 'black'),
                                                      column(6, numericInput(inputId = 'text_pos', label = 'Position',
                                                                             value = 1.1, min = 0, step = 0.01)),
                                                      column(6, numericInput(inputId = 'text_size', label = 'Size', value = 4,
                                                                   min = 0.1, step = 0.1)))),
                    ## Histogram Parameters
                    conditionalPanel("input.plot_selection == 'Histogram'",
                                     sliderInput(inputId = "bins", label = "# of Bins", value = 30,
                                                 min = 10, max = 200, ticks = F)),
                    ## KDE Parameters
                    conditionalPanel("input.plot_selection == 'Kernel Density'",
                                     sliderInput(inputId = "smooth_x", label = "Smoothing", value = 1,
                                                 min = 0.05, max = 10, step = 0.05, ticks = F),
                                     selectInput(inputId = "kernel", label = "Kernel",
                                                 choices = c("gaussian", "rectangular", "triangular", "epanechnikov",
                                                             "biweight", "cosine", "optcosine"))),
                    ## Boxplot Parameters
                    conditionalPanel("input.plot_selection == 'Boxplot'",
                                     checkboxInput(inputId = 'outliers', label = 'Hide outliers?', value = F),
                                     checkboxInput(inputId = 'jitter', label = 'Show jittered points?', value = F),
                                     conditionalPanel("input.jitter == T",
                                                      sliderInput(inputId = "jit_alpha", label = "Points Alpha",
                                                                  value = 0.2, min = 0, max = 1, step = 0.1, ticks = F))),
                    ## General Parameters
                    selectInput(inputId = "trans", label = "Scale transformation",
                                choices = c("identity", "log", "log10", "log2", "sqrt")),
                    tags$p(strong("Custom Appearance")),
                    colourInput(inputId = "geom_color", label = "Lines Color", value = 'black'),
                    colourInput(inputId = "geom_color2", label = "Fill Color", value = "grey"),
                    sliderInput(inputId = 'alpha', label = "Fill Alpha",
                                value = 1, min = 0, max = 1, step = 0.1, ticks = F),
                    selectInput(inputId = "geom_theme", label = "Theme",
                                choices = "Loading"),
                    tags$p(strong("Custom labs")),
                    checkboxInput(inputId = "auto_title", label = "Generate Automatically",
                                  value = F),
                    textInput(inputId = "title", label = "Plot Title", value = ""),
                    textInput(inputId = "xlab", label = "X-Axis", value = ""),
                    textInput(inputId = "ylab", label = "Y-Axis", value = ""))

            ),
            
            
            ## Define Main Panel objects
            mainPanel(
               
               ## Plot Output
               plotlyOutput(outputId = "plot", height = 600),
               
               ## Plotly Options
               tags$hr(), tags$hr(),
               tags$p(strong("If you want to save the plot, you can control the output below
                              then click on the camera icon of the plot toolbar")),
               column(6, sliderInput(inputId = "width", label = "Width", value = 1000, 
                                      min = 100, max = 3000, ticks = F, step = 10)),
               column(6, sliderInput(inputId = "height", label = "Height", value = 800, 
                                       min = 100, max = 3000, ticks = F, step = 10)),
               selectInput(inputId = "format", label = "Format", choices = c('svg', 'png', 'jpg')),
      
               
            )
         )
      )
   ),
   
   ## Tab 3 : Wrangling
   tabPanel(
      
      ## Page title
      title = "Data Wrangling",
      
      fluidPage(
         
         ## Page Title
         titlePanel("Wrangling"),
         
         ## Vertical Layout
         verticalLayout(
               
            tabsetPanel(
               
               ## Cleaning Panel
               tabPanel(
                  title = "Filter",
                  
                  tags$h2("Filter Data", align = "center"), 
                  
                  ## Remove NAs Rows
                  verticalLayout(
                     tags$hr(),
                     fluidRow(
                        column(1,),
                        column(6, actionButton(inputId = "all_nas", "Remove all rows containing NA values")),
                        column(4, actionButton(inputId = "all_nas2", "Remove all columns containing NA values")),
                        column(1,)
                     ),
                     tags$hr()
                  ),
                  
                  ## Select Var to Clean
                  column(3, verticalLayout(
                     tags$br(), tags$br(), tags$br(), 
                     tags$h4('Remove Variables'),
                     selectizeInput(inputId = "select_var", "", 
                                 choices = NULL, multiple = T,
                                 options = list(plugins = list("remove_button"))),
                     fluidRow(
                        column(1,),
                        column(4, actionButton(inputId = "empty", label = "EMPTY")),
                        column(4, actionButton(inputId = "apply_select", label = "APPLY"))
                     ))
                  ),
                  
                  ## Filtering
                  column(9, 
                         column(3,), tags$hr(),
                         fluidRow(
                            ## NAs
                            column(5, verticalLayout(
                               selectInput(inputId = 'filter_nas', label = 'Remove NAs', 
                                           choices = NULL, width = '65%'),
                               fluidRow(
                                column(3,),
                                column(6, actionButton(inputId = 'apply_nas', 'APPLY')))
                             )),
                            ## Rows
                            column(7, verticalLayout(
                               sliderInput(inputId = "rows", label = "Select Rows", min = 1, 
                                           max = 1, value = c(1,1), ticks = F, width = '90%'),
                               fluidRow(
                                  column(3,),
                                  column(6, actionButton(inputId = 'apply_rows', 'APPLY')))
                            ))
                         ),
                         tags$hr(),
                         fluidRow(
                            ## Above / Below / Equal
                            column(6, verticalLayout(
                               fluidRow(
                                  column(2,),
                                  column(6, selectInput(inputId = 'filter_ab_bel', label = 'Basic Filtering',
                                                                 choices = NULL) %>%
                                            helper(type = 'inline', title = 'Basic Filtering',
                                                   content = c("Be cautious about the filter you apply. For example, do not use inequalities on
                                                               characters. You can see below the filters a summary of variables type. We assume 
                                                               that you know the data you work with and do not provide a detailed tutorial for 
                                                               avoidance of filtering issues.<hr> To use the 'in' filter, just separate different
                                                               values by a comma e.g. to filter 2001 and 2003 years of a year column, you should write
                                                               <b>2001,2003</b> in the text box. Do not use whitespaces.")))
                               ),
                               fluidRow(
                                  column(4, selectInput(inputId = 'symbol', label = '', 
                                         choices = c('=', '!=', 'in', '>', '>=', '<', '<='))),
                                 column(6, textInput(inputId = 'filter_value', label = '', value = ''))
                               ),
                               fluidRow(
                                  column(3,),
                                  column(7, actionButton(inputId = 'apply_ab_bel', 'APPLY')))
                            )),
                            ## Numeric Range
                            column(6, verticalLayout(
                               fluidRow(
                                  column(3,),
                                  column(6, selectInput(inputId = 'filter_range', label = 'Range', choices = NULL))
                               ),
                               fluidRow(
                                  column(6, numericInput(inputId = 'from', label = 'From', value = NULL)),
                                  column(6, numericInput(inputId = 'to', label = 'To', value = NULL))
                               ),
                               fluidRow(
                                  column(5,),
                                  column(7, actionButton(inputId = 'apply_range', 'APPLY')))
                            ))
                         ))
                         
                  
               ),
               
               ## Preview Panel
               tabPanel(
                  title = "Create",
                  
                  tags$h2("Create Variables", align = 'center')
               )
            ),
         
         tags$hr(), tags$hr(),
         
         ## Apply Button
         fluidPage(
            column(2,),
            column(3, actionButton(inputId = "reset", "Reset Last Modif")),
            column(3, actionButton(inputId = "apply_all", "Apply to Data")),
            column(3, actionButton(inputId = "reset_all", "Reset Data")),),
         
         ## Preview
         tags$hr(), tags$hr(), 
         tags$h2("Preview Changes", align = 'center'),
         tags$br(),
         tableOutput(outputId = "summary_1"),
         tags$br(),
         verbatimTextOutput(outputId = "summary_2"),
         
         verbatimTextOutput("abc")
         )
      )
   ),
   
   ## Tab 4 : About
   tabPanel(
      title = "About the App",
      
      tags$h1("Dataviz"),
      tagList(
         "This interactive Shiny application has been conceived to automate basic data cleaning and visualization tasks.
                        It requires an R-server to be work. For more information about the shiny framework you can visit",
         a("the shiny website.", href="https://shiny.rstudio.com/")
      ),
      
      tags$h1("Planning"),
      
      tags$h3("Explore Data Tab"),
      p(strong('Implemented features :')),
      tags$div(
         tags$ul(
            tags$li("Support for CSV files"),
            tags$li("Example datasets"),
            tags$li("Auto-restrict checked variables for the Matrix"),
            tags$li("Customize the Matrix appearance"),
            tags$li('Actions on column names for the Matrix'),
            tags$li('Simple summary'),
            tags$li('Sortable Data Table')
         )
      ),
      p(strong('Planned features :')),
      tags$div(
         tags$ul(
            tags$li("Support for more data formats"),
            tags$li("Import data from url"),
            tags$li("Summary : more advanced, more intuitive, customizable"),
            tags$li("SVG Matrix to fix circle/ellipse issue")
         )
      ),
      
      tags$h3("Data Visualization Tab"),
      p(strong('Implemented features :')),
      tags$div(
         tags$ul(
            tags$li("Guessing of variables types and possible plots"),
            tags$li("1D distribution plots for numeric and categorical variables")
         )
      ),
      p(strong('Planned features :')),
      tags$div(
         tags$ul(
            tags$li("1D distribution of strings (Word Cloud)"),
            tags$li("2D distribution plots (facet and stacked bar/kde/hist/boxplot)"),
            tags$li("Scatterplots and Regressions")
         )
      ),
      
      tags$h3("Data Wrangling Tab"),
      p(strong('Implemented features :')),
      tags$div(
         tags$ul(
            tags$li("Remove columns manually, remove columns with NAs"),
            tags$li("Remove rows manually, remove rows containing NAs"),
            tags$li("Choose columns to clean from NAs"),
            tags$li('Basic Filtering and Range'),
            tags$li('Reset and apply features')
         )
      ),
      p(strong('Planned features :')),
      tags$div(
         tags$ul(
            tags$li('Filter for characters'),
            tags$li('Date range filter'),
            tags$li('Filtering with OR statements'),
            tags$li("Save Modified Data"),
            tags$li('Create / Modify variables, modify column names'),
            tags$li("Merging features"),
            tags$li('Manual Coercion (in case guessing is not fine for plots or other issues on classes)')
         )
      ),
      
      tags$h3("General features"),
      p(strong('Implemented features :')),
      tags$div(
         tags$ul(
            tags$li("Add this about section"),
            tags$li("Add helpers")
         )
      ),
      p(strong('Planned features :')),
      tags$div(
         tags$ul(
            tags$li('Backend speed : Code optimization, DataTable instead of dplyr'),
            tags$li('Theming'),
            tags$li('Prevent crashing when the R-server produces an error'),
            tags$li('Progression bars'),
            tags$li("Fix the conditionalPanel issue if shiny doesn't")
         )
      )
   )
)
   
   

## Server ----
server <- function(input, output, session) {
   
   ## Activate helpers
   observe_helpers()
   
   ## Update datasets
   updateSelectInput(session, inputId = "dataset",  
      choices = list(`Custom` = list('Upload your own CSV'),
                     `Gapminder` = list('gapminder'),
                     `ggplot` = as.list(data(package = 'ggplot2')[[3]][c(1:2, 4:7, 10),3]),
                     `dplyr` = as.list(data(package = 'dplyr')[[3]][4:5,3]),
                     `R datasets` = as.list(data(package = 'datasets')[[3]][c(8, 17:18, 
                           21, 24:25, 32:34, 36, 38, 39, 43, 44, 48, 64:65, 73, 81, 84, 100),3])), 
      selected = 'gapminder')
  
   ## Read csv - evaluate local or upload / return NULL until a choice is made
   copy_data = reactive({
      if (input$dataset == 'Upload your own CSV') {
         if(is.null(input$datafile)) NULL
         else input$datafile$datapath %>% read_csv
      }
      else eval(parse(text = input$dataset)) %>% as_tibble})
   
   data = reactiveVal()
   observe({data(copy_data())})
   
   ## Get numeric cols and remove 1-value variables
   include = reactive({if(input$one_value_var) 0 else 1})
   numeric_data = reactive({
      data()[, data() %>% sapply(function(x) {
         (unique(x) %>% length) > include() & is.numeric(x)})]
      })
                 
   ## Update Correlation Matrix Variables Available
   observe({
      if(!is.null(data())) {
         n = ncol(numeric_data())
         choices = colnames(numeric_data())
         updateCheckboxGroupInput(session, "variables_checkbox", choices = choices, 
                                  selected = choices[1:ifelse(n > 10, 10, n)], inline = n > 10)
      }
   })
   
   ## Select All buttons
   observeEvent(input$select_all, {
      updateCheckboxGroupInput(session, "variables_checkbox", selected = colnames(numeric_data()))
   })
   observeEvent(input$unselect_all, {
      updateCheckboxGroupInput(session, "variables_checkbox", selected = NA)
   })
   
   ## Corplot output - exclude non-numeric columns 
   output$corplot = renderPlot({
      
      ## return NULL if no dataset is loaded or if there are no variables checkbox
      if (is.null(data()) | is.null(input$variables_checkbox)) return(NULL)
      
      ## Use selected variables 
      data_cor = data()[, input$variables_checkbox] %>%
         na.omit() %>%
         suppressWarnings()
      
      ## Shorten column names
      chars = split(input$chars_to_remove, "")[[1]]
      ## Escape special characters
      for (i in 1:length(chars)) {
         if (fixed(chars[i])[[1]] != regex(chars[i])[[1]]) {
            chars[i] = paste0("\\", chars[i])
         }
      }
      ## Shorten
      pattern = ifelse(input$chars_to_remove == "", "non-existent-word", 
                       paste0(c("[", chars, "]"), collapse = "")) 
      colnames(data_cor) = colnames(data_cor) %>% 
         sapply(str_remove_all, pattern) %>%
         sapply(substr, 1, input$shorten)
      ## FIX duplicates
      for(i in 2:ncol(data_cor)) {
         j = 2
         if(colnames(data_cor)[i] %in% colnames(data_cor)[1:i-1]) {
            colnames(data_cor)[i] = colnames(data_cor)[i] %>%
               paste0(j)
            j = j+1
         }
      }
      ## Generate the corrplot
      corrplot(cor(data_cor), type = 'upper', order = 'original', 
               method = input$method, tl.pos = "d", tl.col = 9)
      corrplot(cor(data_cor), add = TRUE, type = "lower", method = "number", 
               order = "original", diag = F, tl.pos = "n", cl.pos = "n")})
      
   
   ## Summary output
   output$summary = renderPrint({summary(data())})
   
   ## View / Head output - first define 0 as the entire dataset
   rows_to_display = reactive({ifelse(input$n_rows == 0, nrow(data()), input$n_rows)})
   output$head = renderDataTable({data() %>% head(rows_to_display())},
                                 options = list(pageLength = 10))
   
   ## Update Plots Variables Selection
   observe({
      inline = ncol(data()) > 6
      choices = colnames(data())
      updateSelectizeInput(session, "variables_plot2", choices = choices)
      updateCheckboxGroupInput(session, "variables_plot", choices = choices, inline = inline)
   })
   observe({
      if(input$vars == "Type") updateSelectizeInput(session, "variables_plot2", selected = input$variables_plot)
   })
   observe({
      updateCheckboxGroupInput(session, "variables_plot", selected = input$variables_plot2)
   })
   
   
   ## Guess variables type
   var_types = reactive({
      sapply(data()[,input$variables_plot], guess, input$max_categ)
   })
   
   ## Deduce plots
   plots = reactive({
      
      # Continuous & discrete distributions
      if(length(var_types()) == 1 & var_types()[1] %in% c("continuous", "discrete")) {
         c('Histogram', 'Kernel Density', 'Boxplot')
      }
      
      # Categorical and boolean distributions
      else if(length(var_types()) == 1 & var_types()[1] %in% c("boolean", "categorical")) {
         c('Bars')
      }

      # String
      else if(length(var_types()) == 1 & var_types()[1] == "string") {
         c('Word Cloud')
      }

      # Two variables
      else if(length(var_types()) == 2 & all(var_types() %in% c("continuous", "discrete"))) {
         c("Scatterplot")
      }
      
      else c('None')
   })
   
   ## Update Plot selection list
   observe({
      updateSelectInput(session, "plot_selection", choices = plots())
   })
   
   ## Get themes
   updateSelectInput(session, "geom_theme",
                     choices = ls("package:ggthemes")[ls("package:ggthemes") %>% str_detect("^theme")],
                     selected = "theme_clean")
   
   # ## Create plot functions
   plot_function = reactive({
      
      ## Histogram function
      if(input$plot_selection == "Histogram"){
         if(input$auto_title){
            updateTextInput(session, "title", value = paste0(input$variables_plot, " Histogram"))
            updateTextInput(session, "xlab", value = input$variables_plot)
            updateTextInput(session, "ylab", value = "count")
         }
         paste0("(data() %>% ggplot(aes(", input$variables_plot[1], ")) + 
                  geom_histogram(color = '", input$geom_color, "', fill = '", input$geom_color2, "',
                                 bins = ", input$bins, ", alpha = ", input$alpha, ") +",
                  input$geom_theme, "() + 
                  ggtitle('", input$title, "') + xlab('", input$xlab, "') + ylab('", input$ylab, "') +
                  theme(plot.title = element_text(hjust = 0.5)) +
                  scale_x_continuous(trans = '", input$trans, "')) %>% 
                  ggplotly %>% 
                  config(toImageButtonOptions = list(format = '", input$format, "',
                         width = ", input$width, ", height = ", input$height, "))")
      }
      
         ## KDE function
         else if(input$plot_selection == "Kernel Density"){
            if(input$auto_title){
               updateTextInput(session, "title", value = paste0(input$variables_plot, " Density"))
               updateTextInput(session, "xlab", value = input$variables_plot)
               updateTextInput(session, "ylab", value = "frequency")
            }
            paste0("(data() %>% ggplot(aes(", input$variables_plot[1], ")) + 
                  stat_density(color = '", input$geom_color, "', fill = '", input$geom_color2, "',
                               alpha = ", input$alpha, ",
                               adjust = ", input$smooth_x, ", kernel = '", input$kernel, "') +",
                  input$geom_theme, "() + 
                  ggtitle('", input$title, "') + xlab('", input$xlab, "') + ylab('", input$ylab, "') +
                  theme(plot.title = element_text(hjust = 0.5)) +
                  scale_x_continuous(trans = '", input$trans, "')) %>% 
                  ggplotly %>% 
                  config(toImageButtonOptions = list(format = '", input$format, "',
                         width = ", input$width, ", height = ", input$height, "))")
         }
      
      ## Boxplot function
      else if(input$plot_selection == "Boxplot"){
         if(input$auto_title){
            updateTextInput(session, "title", value = paste0(input$variables_plot, " Boxplot"))
            updateTextInput(session, "ylab", value = input$variables_plot)
         }
         jitter = ifelse(input$jitter, paste0("+ geom_jitter(shape=16, position = position_jitter(0.05), alpha = ", 
                                              input$jit_alpha, ")"), "")
         quants = ifelse(!input$outliers, paste0("quantile(data()$", input$variables_plot[1], ", c(0, 1))"), 
                         paste0("quantile(data()$", input$variables_plot[1], ", c(0.1, 0.9))"))
         paste0("(data() %>% ggplot(aes(x = 1, y = ", input$variables_plot[1], ")) + 
                  geom_boxplot(color = '", input$geom_color, "', fill = '", input$geom_color2, "',
                               alpha = ", input$alpha, ") ", jitter, " +",
                  input$geom_theme, "() +  
                  ggtitle('", input$title, "') + xlab('", input$xlab, "') + ylab('", input$ylab, "') +
                  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_blank(),
                        axis.ticks = element_blank()) +
                  scale_y_continuous(trans = '", input$trans, "', limits = ", quants, ")) %>% 
                  ggplotly %>% 
                  config(toImageButtonOptions = list(format = '", input$format, "',
                         width = ", input$width, ", height = ", input$height, "))")
      }
      
      ## Barplot function
      else if(input$plot_selection == "Bars"){
         if(input$auto_title){
            updateTextInput(session, "title", value = paste0(input$variables_plot, " Barplot"))
            updateTextInput(session, "xlab", value = input$variables_plot)
            updateTextInput(session, "ylab", value = "count")
         }
         labels = ifelse(input$text, paste0(" + geom_text(aes(label = n, y = n*", input$text_pos, "), color = '", input$text_col, "',
                                            size = ", input$text_size, ", position = position_dodge2(0.9)) +"),
                         ' + ')
         paste0("(data() %>% group_by(", input$variables_plot[1], ") %>% summarize(n = n()) %>%
                  ggplot(aes(", input$variables_plot[1], ", n)) +
                  geom_bar(color = '", input$geom_color, "', fill = '", input$geom_color2, "',
                           alpha = ", input$alpha, ", stat = 'identity', position = position_dodge2()) + ",
                  input$geom_theme, "()", labels, "
                  ggtitle('", input$title, "') + xlab('", input$xlab, "') + ylab('", input$ylab, "') +
                  theme(plot.title = element_text(hjust = 0.5)) +
                  scale_y_continuous(trans = '", input$trans, "')) %>%
                  ggplotly %>%
                  config(toImageButtonOptions = list(format = '", input$format, "',
                         width = ", input$width, ", height = ", input$height, "))")
      }
      
      ## No plot style selected
      else 'ggplot()'
   })

   ## Plot output
   output$plot = renderPlotly({
      eval(parse(text = plot_function()))
   })
   
   output$abc = renderPrint({print(input$select_var) %>% class})
   
   ## create the temp clean data
   clean_data = reactiveVal()
   observe({clean_data(data())})
   
   ## Update Variables in Wrangling Tab (select + NAs)
   observe({
      choices = colnames(clean_data())
      updateSelectInput(session, "select_var", choices = choices, selected = choices)
      updateSelectInput(session, "filter_nas", choices = choices)
   })
   
   ## Empty Variables in Select
   observeEvent(input$empty, {
      updateSelectInput(session, "select_var", selected = '')
   })
   
   ## Apply Column Selection
   observeEvent(input$apply_select, {
      clean = clean_data()
      clean = clean[, input$select_var]
      clean_data(clean)
   })
   
   ## Updtae Rows Selection input
   observe({
      nrows = nrow(clean_data())
      updateSliderInput(session, "rows", max = nrows, value = c(1, nrows), step = 1)
   })
   
   ## Remove all rows with NAs
   observeEvent(input$all_nas, {
      no_nas = clean_data()
      no_nas = no_nas %>% na.omit()
      clean_data(no_nas)
   })
   ## Remove all columns with NAs
   observeEvent(input$all_nas2, {
      no_nas = clean_data()
      no_nas = no_nas[, !apply(no_nas, 2, function(x) sum(is.na(x)) > 0)]
      clean_data(no_nas)
   })
   
   ## Remove NAs rows from a specific column
   observeEvent(input$apply_nas, {
      no_nas = clean_data()
      no_nas = no_nas[!is.na(no_nas[,input$filter_nas]),]
      clean_data(no_nas)
   })
   
   ## Apply Rows slicing
   observeEvent(input$apply_rows, {
      slice = clean_data()
      slice = slice[input$rows[1]:input$rows[2], ]
      clean_data(slice)
   })
   
   ## Update var filter
   observe({
      choices = colnames(clean_data()[, clean_data() %>% sapply(function(x) { is.numeric(x) })])
      updateSelectInput(session, "filter_ab_bel", choices = colnames(clean_data()))
      updateSelectInput(session, "filter_range", choices = choices)
   })
   
   ## Apply Basic filter 
   observeEvent(input$apply_ab_bel, {
      clean = clean_data()
      if(input$symbol %in% c('>', '<', '<=', '>=', '!=')) {
         eval(parse(text = paste0("clean = clean %>% 
             filter(", input$filter_ab_bel, " ", input$symbol, "'", input$filter_value, "')")))
      }
      else if(input$symbol == '=') {
         eval(parse(text = paste0("clean = clean %>% 
             filter(", input$filter_ab_bel, " == '", input$filter_value, "')")))
      }
      else if(input$symbol == 'in') {
         items = paste0("'", stringr::str_split(input$filter_value, ',')[[1]] %>%  paste0(collapse = "','"), "'")
         eval(parse(text = paste0("clean = clean %>% 
             filter(", input$filter_ab_bel, " %in% c(", items, "))")))  

      }
      clean_data(clean)
   })
   
   ## Apply Range Filter
   observeEvent(input$apply_range, {
      clean = clean_data()
      eval(parse(text = paste0("clean = clean %>% 
             filter(", input$filter_range, " %>% between(", input$from, ",", input$to, "))")))
      clean_data(clean)
   })
      
   ## Reset / Cancel last modifs
   observeEvent(input$reset, {
      reset = data()
      clean_data(reset)
   })
   
   ## Apply to data
   observeEvent(input$apply_all, {
      apply = clean_data()
      data(apply)
   })
   
   ## Reset all cleaning
   observeEvent(input$reset_all, {
      reset_all = copy_data()
      data(reset_all)
      clean_data(reset_all)
   })
   
   ## Summaries in Wrangling Tab
   output$summary_1 = function() {
      my_summary(clean_data()) %>% knitr::kable('html', align = 'c') %>% 
         kable_styling("striped", full_width = F)
   }
   output$summary_2 = renderPrint({
      list(Dimensions = paste('Rows : ', dim(clean_data())[1], '   Columns : ', dim(clean_data())[2]),
           Summary = summary(clean_data()))
   })
   
}

shinyApp(ui, server)

