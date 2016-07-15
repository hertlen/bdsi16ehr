# install.packages("survey")
library("survey")

source("cross-validation svy.R")
source("k-fold cross-validation.R")


data0 = read.csv("NHANES.csv")
smoking_former = as.numeric((data0$Smoking == "Former Smoker"))
smoking_current = as.numeric((data0$Smoking == "Current Smoker"))
data0 = cbind(data0, smoking_former, smoking_current)

htn_gp2 = as.numeric((data0$htn_gp == 2))
htn_gp3 = as.numeric((data0$htn_gp == 3))
htn_gp4 = as.numeric((data0$htn_gp == 4))
data0 = cbind(data0, htn_gp2, htn_gp3, htn_gp4)

education2 = as.numeric((data0$education == 2))
education3 = as.numeric((data0$education == 3))
education4 = as.numeric((data0$education == 4))
education5 = as.numeric((data0$education == 5))

data0 = cbind(data0, education2, education3, education4, education5)

covariates_test = c("hypertension", "diabetes", "BMI", "smoking_former", "smoking_current",
                    "LDL", "age_years")
## eGFR model:
# 21% valid observations
# 11.9 error, SE 0.11

# lower BIC, possibly more reliable because higher % of valid observations
#####################
covariates_test = c("hypertension", "male", "diabetes", "education2", "education3", "education4", "education5",
                    "Triglycerides", "age_months", "private_ins")
## eGFR model:
# 21% valid observations
# 12.23 error, SE 0.12 
#######################
covariates_test = c("hypertension", "male", "diabetes", "LDL", "HDL", "Total_chol",
                    "Triglycerides", "age_months", "private_ins")
## eGFR model:
# 13.1% valid observations
# 11.89 error, SE 0.134

#######################

covariates_test = c("hypertension", "male", "diabetes", "education2", "education3", "education4", "education5",
             "LDL", "age_months", "private_ins")
## eGFR model:
# 11% valid observations
# 11.84 error, SE 0.15

# higher BIC, possibly less reliable because lower % of observations were usable

#######################
covariates_test = c("hypertension", "male", "diabetes", "education2", "education3", "education4", "education5",
                    "Triglycerides", "age_months", "private_ins")
## eGFR


###################




start_time = Sys.time()
test = k_fold_cross_validate(data0, binomial = FALSE, covariates = covariates_test, num_folds = 5,
                             first.year = 2, last.year = 8)
end_time = Sys.time()
elapsed = end_time - start_time
print(elapsed)

complete_errors = as.data.frame(rbind(test[[1]], test[[2]], test[[3]], test[[4]], test[[5]]))

colnames(complete_errors) = c("SEQN", "error")
ordered_errors = complete_errors[order(complete_errors$SEQN, decreasing = FALSE), ]
first.seqn = ordered_errors[1,1]
last.seqn = ordered_errors[nrow(ordered_errors), 1]
ordered_errors[,2] = sqrt(ordered_errors[,2])
data.of.interest = data0[data0$SEQN >= first.seqn & data0$SEQN <= last.seqn, ]
data.of.interest = cbind(data.of.interest, ordered_errors[,2])
colnames(data.of.interest)[length(colnames(data.of.interest))] = "error"

(percent_validated = sum(!is.na(ordered_errors[,2]))/nrow(ordered_errors))

weight = data.of.interest$WTMEC2YR / (8 - 2 + 1)

svyobj.with.errors = svydesign(
  ids = ~SDMVPSU,
  strata = ~SDMVSTRA,
  nest = TRUE,
  weights = ~weight,
  data = data.of.interest
)

weighted_errors = svymean(
  x = ~error,
  design = svyobj.with.errors,
  na.rm = TRUE
)


obesity_0 = as.numeric((data0$obese == 0))
obesity_1 = as.numeric((data0$obese == 1))
data1 = cbind(data0, obesity_0, obesity_1)

test1 = LOO_cross_validate(data0, binomial = TRUE, covariates = c("BMI", "obese"))

data0 = read.csv("NHANES.csv")

data_sub = data0[data0$SDDSRVYR >= 2 & data0$SDDSRVYR <= 8, ]
weight = data_sub$WTMEC2YR / 7
temp.svd = svydesign(
  ids = ~SDMVPSU,
  strata = ~SDMVSTRA,
  nest = TRUE,
  weights = ~weight,
  data = data_sub
)


model_ckd1 = svyglm(CKD_epi_eGFR ~ BMI + obese1, design = temp.svd)
new.data = temp.svd$variables[1, ]
predict(model_ckd1, newdata = new.data, type = "response")
