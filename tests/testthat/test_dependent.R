### Testcode
library(contrastanalysis)

### Generate data
set.seed(1)
nGroup <- 4
N <- 50
lambda <- matrix(c(3,-1,-1,-1,
                   0,2,-1,-1,
                   0,0,-1,1),
                 ncol = nGroup,
                 byrow=TRUE)
dat <- data.frame(
  x1 = sample(1:10, size = N, replace = TRUE),
  x2 = sample(1:15, size = N, replace = TRUE),
  x3 = sample(1:15, size = N, replace = TRUE),
  x4 = sample(1:20, size = N, replace = TRUE)
)

# contrast_independent -----------------------------------------------------
### Run contrast analysis
contResult <- contrast_dependent(nGroup, lambda,dat)

### Tests
# squared t value is F value
expect_equal(contResult$F, contResult$t^2, tolerance = 1e-3)


# compare_independent -----------------------------------------------------
lambda1 <- c(3,-1,-1,-1)
lambda2 <- c(0,2,-1,-1)

compareResult <- compare_dependent(nGroup, lambda1, lambda2, dat)

### Tests
# squared t value is F value
expect_equal(compareResult$results$F, compareResult$results$t^2, tolerance = 1e-3)
# difference in contrast wteights
expect_equal(compareResult$contrastWeights$lambda1Std - compareResult$contrastWeights$lambda2Std, compareResult$contrastWeights$lambdaDiff, tolerance = 1e-3)
