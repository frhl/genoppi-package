
if (F){
  ## manual testing
  setwd('~/Toolbox/packages/genoppi/')  
  .libPaths(c("~/Toolbox/rlib/",.libPaths()))
  devtools::load_all()
  library(testthat)
}



test_that('label data can be handled by genoppi',{
  
})

test_that('label-free data can be handled by genoppi',{
  
})
