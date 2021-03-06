shinyServer(function(input, output, session) {
   
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
   
})