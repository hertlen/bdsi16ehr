# install.packages("survey")
library("survey")

source("cross-validation svy.R")
source("k-fold cross-validation.R")


data0 = read.csv("NHANES.csv")
data0 = data0[data0$age_years >= 18, ]
agegrp2 = as.numeric((data0$age_years >=40 & data0$age_years<=59))
agegrp3 = as.numeric((data0$age_years >= 60))
data0 = cbind(data0, agegrp2, agegrp3)

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

covariates_test = c("hypertension", "male", "diabetes", "race2", "race3", "race4", "race5",
                    "agegrp2", "agegrp3")


race2 = as.numeric((data0$race_eth == 2))
race3 = as.numeric((data0$race_eth == 3))
race4 = as.numeric((data0$race_eth == 4))
race5 = as.numeric((data0$race_eth == 5))

data0 = cbind(data0, race2, race3, race4, race5)


weight = data0$WTMEC2YR / 7
survey_design = svydesign(
  ids = ~SDMVPSU,
  strata = ~SDMVSTRA,
  nest = TRUE,
  weights = ~weight,
  data = data0
)

chosen_glm = svyglm(
  CKD_epi_eGFR ~ hypertension + diabetes + agegrp2 + agegrp3 + male + race2 + race3 + race4 + 
    race5,
  design = survey_design,
  family = gaussian
)

###
covariates_test = c("hypertension", "male", "diabetes", "race2", "race3", "race4", "race5",
                    "agegrp2", "agegrp3", "Total_chol")

# 85% observations used (34320)
#     mean     SE
# error 12.936 0.0763


###

covariates_test = c("hypertension", "male", "diabetes", "race2", "race3", "race4", "race5",
                    "agegrp2", "agegrp3")
# 85% observations used (34332)
#         mean     SE
# error 12.916 0.0768



test = k_fold_cross_validate(data0, binomial = FALSE, covariates = covariates_test, num_folds = 5,
                             first.year = 2, last.year = 8)

complete_errors = as.data.frame(rbind(test[[1]], test[[2]], test[[3]], test[[4]], test[[5]]))

colnames(complete_errors) = c("SEQN", "error")
ordered_errors = complete_errors[order(complete_errors$SEQN, decreasing = FALSE), ]
first.seqn = ordered_errors[1,1]
last.seqn = ordered_errors[nrow(ordered_errors), 1]
ordered_errors[,2] = sqrt(ordered_errors[,2])
data.of.interest = data0[data0$SEQN >= first.seqn & data0$SEQN <= last.seqn, ]
data.of.interest = cbind(data.of.interest, ordered_errors[,2])
colnames(data.of.interest)[length(colnames(data.of.interest))] = "error"

(sum(!is.na(ordered_errors[,2])))
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

weighted_errors







##########################################################################################


covariates_test = c("hypertension", "diabetes", "BMI", "smoking_former", "smoking_current",
                    "LDL", "age_years")
## eGFR model:
# 33.8% valid observations (13,500)
# 11.89 error, SE 0.116
# BIC: 8657
#####################
covariates_test = c("hypertension", "male", "diabetes", "education2", "education3", "education4", "education5",
                    "Triglycerides", "age_months", "private_ins")
## eGFR model:
# 36% valid observations (14600)
# 12.23 error, SE 0.12
# BIC: 10755
#######################
covariates_test = c("hypertension", "male", "diabetes", "LDL", "HDL", "Total_chol",
                    "Triglycerides", "age_months", "private_ins")
## eGFR model:
# 20.7% valid observations (8300)
# 11.86 error, SE 0.134
# BIC: 5703
#######################

covariates_test = c("hypertension", "male", "diabetes", "education2", "education3", "education4", "education5",
                    "LDL", "age_months", "private_ins")
## eGFR model:
# 19% valid observations (7667)
# 11.8 error, 0.148 SE
# BIC: 5542

#######################
covariates_test = c("hypertension", "male", "diabetes", "LDL", "age_months", "private_ins")
## eGFR model:
# 20.7% valid observations(8306)
# 11.89 error, 0.1364 SE
# BIC: 5699

#####################
covariates_test = c("hypertension", "male", "diabetes", "agegrp2", "agegrp3", "race2",
                    "race3", "race4", "race5")
## eGFR model:
# 58% valid observations
# 11.936 error, 0.085 SE
# BIC: 5296132
