render_trial <- function(dyad, trial, type = "summary") {
  if(type == "summary"){
    input_string <- "C:\\Users\\mcwee\\Documents\\LIVELab\\Social_Drumming\\social_drumming_git\\markdown\\drumming_acf_md.Rmd"
    output_string <- paste0('C:\\Users\\mcwee\\Documents\\LIVELab\\Social_Drumming\\html_output\\', dyad, '_', trial, '_output_', Sys.Date(), '.html') 
  } else if(type == "cleaning"){
    input_string <- "C:\\Users\\mcwee\\Documents\\LIVELab\\Social_Drumming\\social_drumming_git\\markdown\\drumming_cleaning.Rmd"
    output_string <- paste0('C:\\Users\\mcwee\\Documents\\LIVELab\\Social_Drumming\\cleaning_html\\', dyad, '_', trial, '_cleaning_', Sys.Date(), '.html')
  }
  rmarkdown::render(
    input = input_string,
    params = list(dyad = dyad, trial = trial),
    output_file = output_string
  )
}

dyads <- c(101:119,201:212)
trials <- 1:4

start <- Sys.time()
for(dyad in dyads){
  for(trial in trials){
    
    tryCatch({
      render_trial(dyad, trial)  
    },
    error = function(e){
      message("An error occurred: ", conditionMessage(e))
    }
    )
    
  }
}
end <- Sys.time()
elapsed <- end-start

dyad <- 202
trial <- 2
render_trial(204, 1, type = "cleaning")

render_trial(204, 1, type = "summary")
