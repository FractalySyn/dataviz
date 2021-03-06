# DataViz App

This interactive Shiny application has been conceived to automate basic data cleaning and visualization tasks. It requires an R-server to be work. For more information about the shiny framework you can visit [the shiny website](https://shiny.rstudio.com/).

### Launch the app

To launch DataViz you can either use the app.R file that is at the root of the repository or use the three different files located in the dataviz folder. For the latter, make sure that you place all files in the same folder. 

##### With the standalone file app.R

As everything is included in a single file, you are only required to run the whole code in your R console or IDE.

##### With the separate files

Make sure you have the `shiny` package installed and loaded. Then run the following command : `runApp('dir')` and replace `dir` by the directory relative or full path containing the three files ui.R, server.R and global.R.

##### With R Studio

Once shiny is installed, R Studio automatically replace the `Run` button by a `Run App` one when your opened R code has shiny main components in it. This functionality exists wether you are using the standalone file or the separate files.

### Use DataViz

This app allows you to import your dataset (only csv at the moment), visualize it, generate plots and perform some cleaning tasks. We do not provide tutorials for simple features and more advanced ones are accompanied by helpers (you can access them by clicking on question marks).

### Project Roadmap

You can find a raw version of this features' planning inside the app ('About the app' tab).

- [ ] **General features**
  - [x] About section and integrated helpers
  - [ ] Aesthetic : theming, progression bars....
  - [ ] Prevent crashes of the app when R errors occur
  - [ ] If shiny does not, fix the conditional panels issue
  - [ ] Backend speed
    - [ ] Optimize Shiny reactivity
    - [ ] DataTable over dplyr

- [ ] **Explore Data tab**
  - [ ] Import dataset
    - [x] csv
    - [ ] other common formats
    - [ ] flexibility : headers,...  
    - [ ] url
  - [x] Include sample datasets
  - [ ] Correlation Matrix
    - [x] Auto-restrict variables for the Matrix (numeric, 10 threshold, constant)
    - [x] Matrix appearance customization
      - [x] control column names
      - [x] display method
    - [ ] generate the matrix as an svg (circles issue)
  - [ ] Summary
    - [x] simple summary
    - [ ] improve it and allow for personalization
  - [x] Sortable Data Table

- [ ] **Data Visualization tab**
  - [x] Select variables in two ways (convenience for large datasets)
  - [ ] Guess variable types
    - [x] differentiate discrete / continuous / character / boolean
    - [x] guess categorical + manual control
    - [ ] handle date formats
  - [x] Guess possible plots (not complete but it will be easy to implement other guesses)
  - [ ] Generate plots
    - [ ] 1D distributions
      - [x] histograms, KDEs and boxplots for numeric non-categorical variables
      - [x] barplots for categorical and boolean variables
      - [ ] word clouds for characters
    - [ ] 2D distributions
    - [ ] Scatterplots
    - [ ] Advanced plots 
  - [x] Save plots + control dimensions and format
  - [x] General customization
    - [x] Theme, colors
    - [x] Titles


- [ ] **Data Wrangling tab**
  - [x] Select / Remove variables / rows
  - [x] Remove NAs (all rows, all columns or specific columns)
  - [ ] Filtering
    - [x] basic (=, >, in...)
    - [x] range (numeric only at the moment)
    - [ ] characters filters
    - [ ] range sliders (numeric + dates)
    - [ ] OR statements
  - [ ] Create / Modify variables / modify names
  - [ ] Merge datasets
  - [ ] Coercion
  - [ ] Actions on Data
    - [x] apply a specific filter
    - [x] apply filters to the main dataset (used in other tabs)
    - [x] reset last temporarily applied filters
    - [x] reset all filters (reset main dataset)
    - [ ] download data

 - [ ] **Regression tab**
