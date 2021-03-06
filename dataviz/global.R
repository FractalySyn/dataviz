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


## Options
options(shiny.maxRequestSize=30*1024^2)



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


