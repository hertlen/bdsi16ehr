install.packages("survey")
library("survey")
install.packages("pROC")
library("pROC")

source("cross-validation svy.R")
source("k-fold cross-validation.R")

smoking_former = as.numeric((data0$Smoking == "Former Smoker"))
smoking_current = as.numeric((data0$Smoking == "Current Smoker"))
data0 = cbind(data0, smoking_never, smoking_former, smoking_current)

htn_gp2 = as.numeric((data0$htn_gp == 2))
htn_gp3 = as.numeric((data0$htn_gp == 3))
htn_gp4 = as.numeric((data0$htn_gp == 4))
data0 = cbind(data0, htn_gp2, htn_gp3, htn_gp4)

angina_2 = as.numeric((data0$angina_self == 2))
angina_3 = as.numeric((data0$angina_self == 3))
angina_4 = as.numeric((data0$angina_self == 4))
angina_5 = as.numeric((data0$angina_self == 5))
angina_6 = as.numeric((data0$angina_self == 6))
angina_7 = as.numeric((data0$angina_self == 7))
angina_8 = as.numeric((data0$angina_self == 8))
angina_9 = as.numeric((data0$angina_self == 9))
data0 = cbind(data0, angina_2,angina_3,angina_4,angina_5,angina_6,angina_7,angina_8,angina_9)




covariates_test = c("hypertension", "smoking_former", "smoking_current",
                    "Triglycerides", "htn_gp2", "htn_gp3", "htn_gp4",
                    "HDL", "LDL", "age_months", "Medicare_ins",
                    "Chol_self")

test = k_fold_cross_validate(data0, binomial = FALSE, covariates = covariates_test, num_folds = 2,
                             first.year = 2, last.year = 8)



data_one = test[[1]]
colnames(data_one) = c("SEQN", "err")
data_one = as.data.frame(data_one)
data_one = data_one[order(data_one$SEQN, decreasing = FALSE), ]

data_two = test[[2]]
colnames(data_two) = c("SEQN", "err")
data_two = as.data.frame(data_two)
data_two = data_two[order(data_two$SEQN, decreasing = FALSE),]


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
