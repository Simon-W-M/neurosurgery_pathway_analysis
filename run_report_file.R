# run script

# load librarys
source('libs.R')

# load data
source('data_load.R')

# charts tables and functions
source('charts_functions.R')

# render report
rmarkdown::render(paste0("neuro_report.Rmd"),
                  output_file = paste0("neuro_rpt_", 
                                       format(Sys.Date(),'%y%m'), 
                                       '.html'),
                  envir = new.env())
