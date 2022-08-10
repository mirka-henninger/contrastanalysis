### Testcode

### Generate data
set.seed(1)
n_group <- 4
N <- 50
lambda <- matrix(c(3,-1,-1,-1,
                   0,2,-1,-1,
                   0,0,-1,1),
                 ncol = n_group,
                 byrow=TRUE)
dat <- data.frame(
  x1 = sample(1:10, size = N, replace = TRUE),
  x2 = sample(1:12, size = N, replace = TRUE),
  x3 = sample(1:15, size = N, replace = TRUE),
  x4 = sample(1:18, size = N, replace = TRUE)
)

# contrast_dependent -----------------------------------------------------
### Run contrast analysis
cont_result <- contrast_dependent(n_group, lambda,dat)

# results from anova obtained from R version 4.2.1 with ‘afex’ version 1.1-1 and ‘emmeans’ version 1.7.5
longDat <- data.frame(
  dv = c(dat$x1, dat$x2, dat$x3, dat$x4),
  iv = rep(1:4, each = nrow(dat)),
  id = rep(1:50, 4)
)
a1 <- afex::aov_ez("id", "dv", within = "iv", data = longDat)
contr_anova <- emmeans::emmeans(a1, ~ iv)
anova_result <- data.frame(emmeans::contrast(contr_anova,
                                             list(lambda[1,], lambda[2,], lambda[3,])))

### Tests
# equal contrast estimate
expect_equal(cont_result$contrast_estimate, anova_result$estimate, tolerance = 1e-4)
# equal p values
expect_equal(cont_result$p_value, anova_result$p.value, tolerance = 1e-3)
# compare t-values
expect_equal(cont_result$t_value, anova_result$t.ratio, tolerance = 1e-3)
# squared t value is F value
expect_equal(cont_result$F_value, cont_result$t_value^2, tolerance = 1e-4)


# compare_dependent -----------------------------------------------------
lambda1 <- c(3,-1,-1,-1)
lambda2 <- c(0,2,-1,-1)

compare_result <- compare_dependent(n_group, lambda1, lambda2, dat)

### Tests
# squared t value is F value
expect_equal(compare_result$results$F_value,
             compare_result$results$t_value^2, tolerance = 1e-3)
# difference in contrast wteights
expect_equal(compare_result$contrast_weights$lambda1_std - compare_result$contrast_weights$lambda2_std,
             compare_result$contrast_weights$lambda_diff, tolerance = 1e-4)

# test warnings -----------------------------------------------------------
### contrast_dependent
# group number is not correct
expect_error(contrast_dependent(n_group-1, lambda, dat))
expect_error(contrast_dependent(n_group+1, lambda, dat))

# lambda does not match group
expect_error(contrast_dependent(n_group, lambda[,-1], dat))
expect_error(contrast_dependent(n_group, lambda[,-4], dat))

# dat has strange format
expect_error(contrast_dependent(n_group, lambda, cbind(dat, dat[,1])))
expect_error(contrast_dependent(n_group, lambda, dat[,-1]))
expect_error(contrast_dependent(n_group, lambda, dat[,-4]))

# contrast weights do not sum to zero
expect_error(contrast_dependent(n_group, lambda+1, dat))
expect_error(contrast_dependent(n_group, lambda-1, dat))


### compare_dependent
# group number is not correct
expect_error(compare_dependent(n_group-1, lambda1, lambda2, dat))
expect_error(compare_dependent(n_group+1, lambda1, lambda2, dat))

# lambda does not match group
expect_error(compare_dependent(n_group,  lambda1[-1], lambda2, dat))
expect_error(compare_dependent(n_group,  lambda1, lambda2[-1], dat))

# dat has strange format
expect_error(compare_dependent(n_group, lambda1, lambda2, cbind(dat, dat[,1])))
expect_error(compare_dependent(n_group, lambda1, lambda2, dat[,-1]))
expect_error(compare_dependent(n_group, lambda1, lambda2, dat[,-4]))

# contrast weights do not sum to zero
expect_error(compare_dependent(n_group, lambda1+1, lambda2, dat))
expect_error(compare_dependent(n_group, lambda1, lambda2+1, dat))
