# Install the released version from CRAN

if (F){
  .libPaths(c("~/Toolbox/rlib/",.libPaths()))
  devtools::load_all()
}

test_that('normalize with median',{

  df <- data.frame(A=rnorm(100,5,15), B=runif(100,0,43))
  result = normalize(df, type = 'median')
  reference = as.data.frame(cbind(df$A-median(df$A), df$B-median(df$B)))
  colnames(reference) = c('A','B')
  expect_equal(result, reference)

})

test_that('normalize with mean',{
  
  df <- data.frame(A=rnorm(100,5,15), B=runif(100,0,43))
  result = normalize(df, type = 'mean')
  reference = as.data.frame(cbind(df$A-mean(df$A), df$B-mean(df$B)))
  colnames(reference) = c('A','B')
  expect_equal(result, reference)
  
})

test_that('normalize with one character column',{
  
  df <- data.frame(gene=LETTERS, A=rnorm(26,5,15), B=runif(26,0,43))
  result = normalize(df, type = 'median')
  reference = data.frame(gene=LETTERS, A=df$A-median(df$A), B=df$B-median(df$B))
  expect_equal(result, reference)
  
})
