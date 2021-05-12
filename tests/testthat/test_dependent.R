### Testcode

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

longDat <- data.frame(
  dv = c(dat$x1, dat$x2, dat$x3, dat$x4),
  iv = rep(1:4, each = nrow(dat)),
  id = rep(1:50, 4)
)

a1 <- afex::aov_ez("id", "dv", within = "iv", data = longDat)
# compare to R functions
contrANOVA <- emmeans::emmeans(a1, ~ iv)
#Testing the contrasts
anoResult <- data.frame(emmeans::contrast(contrANOVA, list(lambda[1,], lambda[2,], lambda[3,])))


### Tests
# equal contrast estimate
expect_equal(contResult$contrast.estimate, anoResult$estimate, tolerance = 1e-3)
# equal p values
expect_equal(contResult$p, anoResult$p.value, tolerance = 1e-2)
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
