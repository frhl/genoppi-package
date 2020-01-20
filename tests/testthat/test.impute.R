

test_that('imputation simulating low abundant proteins',{

  ### n is big
  set.seed(1)
  df <- data.frame(A=rnorm(10000, 0, sd = 4), B = rnorm(10000, 1, sd = 3))
  sam <- sample(1:10000, 1000)
  df[sam, 1] <- NA
  df <- impute_gaussian(df, width = 0.3, shift = -1.8)
  
  # expect lower mean
  expect_equal(mean(df[df$imputed==1, ]$A),  -7.36754, tolerance = 0.00001)
  expect_equal(mean(df[df$imputed==0, ]$A), -0.02089915, tolerance = 0.00001)

  ## n is small
  set.seed(1)  
  df <- data.frame(A=rnorm(2000, 0, sd = 4), B = rnorm(2000, 1, sd = 3))
  sam <- sample(1:2000, 100)
  df[sam, 1] <- NA
  df <- impute_gaussian(df, width = 0.5, shift = -1.8)
  
  # expect lower mean
  expect_equal(mean(df[df$imputed==1, ]$A),  -7.67628, tolerance = 0.00001)
  expect_equal(mean(df[df$imputed==0, ]$A), 0.009893299, tolerance = 0.00001)
  
  
})


test_that('imputation without shifting',{
  
  ## n is small
  set.seed(2)  
  df <- data.frame(A=rnorm(2000, 0, sd = 4), B = rnorm(2000, 1, sd = 3))
  sam <- sample(1:2000, 100)
  df[sam, 1] <- NA
  df <- impute_gaussian(df, width = 0.5, shift = 0)
  
  # expect same mean
  expect_equal(mean(df[df$imputed==1, ]$A),  0.2834652, tolerance = 0.00001)
  expect_equal(mean(df[df$imputed==0, ]$A), 0.154915, tolerance = 0.00001)

})







