source("cross-validation svy.R")
test = cross_validate(data0, response_var = "CKD", binomial = TRUE, covariates = c("BMI", "Triglycerides"))
