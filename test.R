source("cross-validation svy.R")

test2 = k_fold_cross_validate(data0, covariates = c("BMI", "Triglycerides"), folds = 3)


test = LOO_cross_validate(data0, binomial = TRUE, covariates = c("BMI", "Triglycerides"))
