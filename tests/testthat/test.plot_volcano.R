
if (F){
  ## manual testing
  setwd('~/Toolbox/packages/pRoteomics/')  
  .libPaths(c("~/Toolbox/rlib/",.libPaths()))
  devtools::load_all()
  library(testthat)
}



test_that('test that Volcano plots can be plotted',{
  
  id <- 'A1'
  seed(1)
  data <- data.frame(gene=LETTERS, rep1 = runif(1:26), rep2=rnorm(1:26))
  result = calculate_moderated_ttest(data)
  plt <- plot_volcano(result, bait = 'A', main = 'randomly generated data points')
  
  
})