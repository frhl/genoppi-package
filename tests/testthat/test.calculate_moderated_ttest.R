if (F){
  ## manual testing
  .libPaths(c("~/Toolbox/rlib/",.libPaths()))
  library(testthat)
  devtools::load_all()
}

# Required for testing
source('tests/testthat/functions/testpaths.R')
func = 'calculate_moderated_ttest'



### testing ###
test_that('Replicates with no normalization',{
  
  id = 'A1'
  set.seed(1)
  data <- data.frame(gene=LETTERS, rep1 = runif(1:26), rep2=rnorm(1:26))
  res = calculate_moderated_ttest(data, normalize = NULL)
  ref = testpaths(id, func)$ref
  expect_equal_to_reference(res, ref)
  #saveRDS(res, paste0('tests/testthat/reference/', func, '/', func, '.', id, '.rds'))
  
  id = 'A2'
  set.seed(2)
  data <- data.frame(gene=LETTERS, rep1 = runif(1:26), rep2=rnorm(1:26))
  res = calculate_moderated_ttest(data, normalize = NULL)
  ref = testpaths(id, func)$ref
  expect_equal_to_reference(res, ref)
  #saveRDS(res, paste0('tests/testthat/reference/', func, '/', func, '.', id, '.rds'))
  
})

test_that('Triplicates with no normalization',{
  
  id = 'B1'
  set.seed(1)
  data <- data.frame(gene=LETTERS, rep1 = runif(1:26), rep2=rnorm(1:26), rep3=rnorm(1:26))
  res = calculate_moderated_ttest(data, normalize = NULL)
  ref = testpaths(id, func)$ref
  expect_equal_to_reference(res, ref)
  #saveRDS(res, paste0('tests/testthat/reference/', func, '/', func, '.', id, '.rds'))
  
  id = 'B2'
  set.seed(2)
  data <- data.frame(gene=LETTERS, rep1 = runif(1:26), rep2=rnorm(1:26), rep3=rnorm(1:26))
  res = calculate_moderated_ttest(data, normalize = NULL)
  ref = testpaths(id, func)$ref
  expect_equal_to_reference(res, ref)
  #saveRDS(res, paste0('tests/testthat/reference/', func, '/', func, '.', id, '.rds'))
  
})

test_that('Xplicates',{
  
  ## generate data
  set.seed(1)
  data <- data.frame(gene=LETTERS, rep1 = runif(1:26), rep2=rnorm(1:26), rep3=rnorm(1:26),
                     rep4=rnorm(1:26), rep5=rnorm(1:26), rep6=rnorm(1:26))
  
  ## without normalization
  id = 'C1'
  res = calculate_moderated_ttest(data, normalize = NULL)
  ref = testpaths(id, func)$ref
  expect_equal_to_reference(res, ref)
  #saveRDS(res, paste0('tests/testthat/reference/', func, '/', func, '.', id, '.rds'))

  
  ## With normalization
  id = 'C2'
  res = calculate_moderated_ttest(data, normalize = 'median')
  ref = testpaths(id, func)$ref
  expect_equal_to_reference(res, ref)
  #saveRDS(res, paste0('tests/testthat/reference/', func, '/', func, '.', id, '.rds'))
  
})


test_that('Errors are prompted when data format is invalid',{
  
  ## this is OK
  data <- data.frame(rep1 = runif(1:26), rep2=rnorm(1:26), gene=LETTERS)
  res = calculate_moderated_ttest(data)
  
  ## 'gene' not in column names
  data <- data.frame(rep1 = runif(1:26), rep2=rnorm(1:26), mygene=LETTERS)
  expect_error(calculate_moderated_ttest(data))
  
  # replicate not in column name
  ## this is NOT OK
  data <- data.frame(myrname1 = runif(1:26), myrname2=rnorm(1:26), gene=LETTERS)
  expect_error(calculate_moderated_ttest(data))
  
  
})

