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
  x2 = sample(1:12, size = N, replace = TRUE),
  x3 = sample(1:15, size = N, replace = TRUE),
  x4 = sample(1:18, size = N, replace = TRUE)
)

# contrast_dependent -----------------------------------------------------
### Run contrast analysis
contResult <- contrast_dependent(nGroup, lambda,dat)

# results from anova obtained from R version 4.0.2 with ‘afex’ version 0.28-1 and ‘emmeans’ version 1.6.0:
# longDat <- data.frame(
#   dv = c(dat$x1, dat$x2, dat$x3, dat$x4),
#   iv = rep(1:4, each = nrow(dat)),
#   id = rep(1:50, 4)
# )
# a1 <- afex::aov_ez("id", "dv", within = "iv", data = longDat)
# contrANOVA <- emmeans::emmeans(a1, ~ iv)
# anoResult <- data.frame(emmeans::contrast(contrANOVA, list(lambda[1,], lambda[2,], lambda[3,])))

anoResult <- data.frame(
  estimate = c(-4.16, -3.2, 1.28),
  SE = c(1.9926805, 1.4090379, 0.8135084),
  df = c(147,147,147),
  t.ratio = c(-2.087640, -2.271053, 1.573432),
  p.value = c(0.03855571, 0.02459612, 0.11776870)
)

### Tests
# equal contrast estimate
expect_equal(contResult$contrast.estimate, anoResult$estimate, tolerance = 1e-4)
# equal p values (here tolerance is higher, because p-values may differ due to differences in degrees of freedom)
expect_equal(contResult$p, anoResult$p.value, tolerance = 1e-1)
# squared t value is F value
expect_equal(contResult$F, contResult$t^2, tolerance = 1e-4)


# compare_dependent -----------------------------------------------------
lambda1 <- c(3,-1,-1,-1)
lambda2 <- c(0,2,-1,-1)

compareResult <- compare_dependent(nGroup, lambda1, lambda2, dat)

### Tests
# squared t value is F value
expect_equal(compareResult$results$F, compareResult$results$t^2, tolerance = 1e-3)
# difference in contrast wteights
expect_equal(compareResult$contrastWeights$lambda1Std - compareResult$contrastWeights$lambda2Std, compareResult$contrastWeights$lambdaDiff, tolerance = 1e-4)

# test warnings -----------------------------------------------------------
# group number is not correct
expect_error(contrast_dependent(nGroup-1, lambda, dat))
expect_error(contrast_dependent(nGroup+1, lambda, dat))

# lambda does not match group
expect_error(contrast_dependent(nGroup, lambda[,-1], dat))
expect_error(contrast_dependent(nGroup, lambda[,-4], dat))

# dat has strange format
expect_error(contrast_dependent(nGroup, lambda, cbind(dat, dat[,1])))
expect_error(contrast_dependent(nGroup, lambda, dat[,-1]))
expect_error(contrast_dependent(nGroup, lambda, dat[,-4]))

# contrast weights do not sum to zero
expect_error(contrast_dependent(nGroup, lambda+1, dat))
expect_error(contrast_dependent(nGroup, lambda-1, dat))

