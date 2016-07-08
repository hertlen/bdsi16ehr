install.packages("survey")
install.packages("ggplot2")
library("ggplot2")
library("survey")
source("subset_svydata.R")

#####################
## Task 4 (NHANES) ##
#####################

# create survey design ####
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

# Create GLMs ####

# races : 1 = mexican american, 2 = other hispanic, 3 = non-hispanic white, 4 = non-hispanic black,
# 5 = other

model_ckd1 = svyglm(CKD_epi_eGFR ~ age_years + BMI + Triglycerides, design = temp.svd)

model_ckd2 = svyglm(factor(CKD) ~ age_years + hypertension + BMI + Triglycerides
                   + Total_chol, design = temp.svd, family = quasibinomial)

model_ckd3 = svyglm(factor(CKD) ~ age_years, design = temp.svd, family = quasibinomial)

model_ckd4 = svyglm(factor(CKD) ~ age_years + smoking + hypertension + BMI, design = temp.svd,
                    family = quasibinomial)

# Predictions and Graphing ####
# to create predictions for GLMs:
# prediction = predict(model_name, newdata = data.frame(covariates = data.frame()))
# to plot
# lines(prediction)

# CKD is a binary variable for CKD_stage > 0

(range(data0[!is.na(data0$BMI), ]$BMI)) # BMI ranges from 7.99-130.21
quantile(data0$BMI, probs = c(0.025, 0.975), na.rm = TRUE) # 95% of people have 14.57-42.52

(range(data0[!is.na(data0$Triglycerides), ]$Triglycerides)) # Triglycerides ranges from 14-5210
quantile(data0$Triglycerides, probs = c(0.025, 0.975), na.rm = TRUE) # 95% of people have 36-394


# box plot for factor data
svyboxplot(BMI ~ factor(hypertension),
           design = temp.svd,
           all.outliers = TRUE,
           axes = FALSE,
           xlab = "Hypertension",
           ylab = "BMI",
           col = "yellow",
           border = "lightblue")
axis(1, at = c(1,2), labels = c("No", "Yes"))
axis(2)
