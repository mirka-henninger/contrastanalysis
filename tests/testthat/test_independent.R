### Testcode

### Generate data
set.seed(1)
nGroup <- 4
lambda <- matrix(c(-1,-1,1,1,
                   -1,1,-1,1,
                   1,-1,-1,1),
                 ncol = 4,
                 byrow=TRUE)
N <- 50
dat <- data.frame(
  x = rep(c(1:nGroup),each = N),
  y = c(rnorm(N,-1,1),rnorm(N),rnorm(N),rnorm(N,1,1))
)

# contrast_independent -----------------------------------------------------
### Run contrast analysis
contrast_result <- contrast_independent(nGroup, lambda, dat)
# compare to linear model
dat$c1 <- ifelse(dat$x==1 | dat$x==2, -1,1)
dat$c2 <- ifelse(dat$x==1 | dat$x==3, -1,1)
dat$c3 <- ifelse(dat$x==2 | dat$x==3, -1,1)
lm_mod <- lm(y~c1+c2+c3,data=dat)
lm_result <- data.frame(coef(summary(lm_mod)))[-1,]
ano_result <- data.frame(anova(lm_mod))[-nGroup,]

### Tests
# equal sums of squares
expect_equal(contrast_result$SumsofSquares, ano_result[,2], tolerance = 1e-4)
# equal F value / t value
expect_equal(contrast_result$F, ano_result[,4], tolerance = 1e-4)
expect_equal(contrast_result$t, lm_result[,3], tolerance = 1e-4)
# equal p values (here tolerance is higher, because p-values may differ due to differences in degrees of freedom)
expect_equal(contrast_result$p, ano_result[,5], tolerance = 1e-2)
expect_equal(contrast_result$p, lm_result[,4], tolerance = 1e-2)
# squared t value is F value
expect_equal(contrast_result$F, contrast_result$t^2, tolerance = 1e-4)


# compare_independent -----------------------------------------------------
lambda1 <- c(-1,-1,1,1)
lambda2 <- c(-3,-1,1,3)
dat <- data.frame(
  x = rep(c(1:4),each = 50),
  y = c(rnorm(50,-1,1),rnorm(50),rnorm(50),rnorm(50,1,1))
)

compare_result <- compare_independent(nGroup, lambda1, lambda2, dat)

### Tests
# squared t value is F value
expect_equal(compare_result$results$F, compare_result$results$t^2, tolerance = 1e-4)
# difference in contrast wteights
expect_equal(compare_result$contrastWeights$lambda1Std - compare_result$contrastWeights$lambda2Std, compare_result$contrastWeights$lambdaDiff, tolerance = 1e-4)



# test warnings -----------------------------------------------------------
### contrast_independent
# group number is not correct
expect_error(contrast_independent(nGroup-1, lambda, dat))
expect_error(contrast_independent(nGroup+1, lambda, dat))

# lambda does not match group
expect_error(contrast_independent(nGroup, lambda[,-1], dat))
expect_error(contrast_independent(nGroup, lambda[,-4], dat))

# dat has strange format
expect_error(contrast_independent(nGroup, lambda, cbind(dat[,2], dat[,1])))
expect_error(contrast_independent(nGroup, lambda, dat[,1]))
expect_error(contrast_independent(nGroup, lambda, dat[,2]))

# contrast weights do not sum to zero
expect_error(contrast_independent(nGroup, lambda+1, dat))
expect_error(contrast_independent(nGroup, lambda-1, dat))



### compare_independent
# group number is not correct
expect_error(compare_independent(nGroup-1, lambda1, lambda2, dat))
expect_error(compare_independent(nGroup+1, lambda1, lambda2, dat))

# lambda does not match group
expect_error(compare_independent(nGroup,  lambda1[-1], lambda2, dat))
expect_error(compare_independent(nGroup,  lambda1, lambda2[-1], dat))

# dat has strange format
expect_error(compare_independent(nGroup, lambda1, lambda2, cbind(dat, dat[,1])))
expect_error(compare_independent(nGroup, lambda1, lambda2, dat[,-1]))
expect_error(compare_independent(nGroup, lambda1, lambda2, dat[,-2]))

# contrast weights do not sum to zero
expect_error(compare_independent(nGroup, lambda1+1, lambda2, dat))
expect_error(compare_independent(nGroup, lambda1, lambda2+1, dat))
