data0 = read.csv("NHANES.csv")
data0 = data0[data0$age_years >= 18, ]
data1 = data0[data0$SDDSRVYR >= 2 & data0$SDDSRVYR <= 8, ]
weight1 = data1$WTMEC2YR / 7

library("survey")
svy_qq = svydesign(
  ids = ~SDMVPSU,
  strata = ~SDMVSTRA,
  nest = TRUE,
  weights = ~weight1,
  data = data1
)

par(mfrow = c(3,2))
# "hypertension", "male", "diabetes", "education2", "education3", "education4", "education5",
# "Triglycerides", "age_months", "private_ins"

glm1 = svyglm(
  CKD_epi_eGFR ~ hypertension + male + diabetes + factor(education) +
    LDL + age_years,
  design = svy_qq,
  family = gaussian
)
qqnorm(glm1$residuals)
qqline(glm1$residuals)

# "hypertension", "male", "diabetes", "LDL", "HDL", "Total_chol",
# "Triglycerides", "age_months", "private_ins"

glm2 = svyglm(
  CKD_epi_eGFR ~ hypertension + male + diabetes + HDL + Total_chol +
    LDL + age_months + private_ins + Triglycerides,
  design = svy_qq,
  family = gaussian
)  
qqnorm(glm2$residuals)
qqline(glm2$residuals)
  

# "hypertension", "diabetes", "BMI", "smoking_former", "smoking_current",
# "LDL", "age_years"

glm3 = svyglm(
  CKD_epi_eGFR ~ hypertension + diabetes +
    LDL + age_years + factor(Smoking),
  design = svy_qq,
  family = gaussian
)  
qqnorm(glm3$residuals)
qqline(glm3$residuals)
  
# "hypertension", "male", "diabetes", "education2", "education3", "education4", "education5",
# "LDL", "age_months", "private_ins"

glm4 = svyglm(
  CKD_epi_eGFR ~ hypertension + diabetes + male + 
    LDL + age_months + factor(education) + private_ins,
  design = svy_qq,
  family = gaussian
)  
qqnorm(glm4$residuals)
qqline(glm4$residuals)


# "hypertension", "male", "diabetes", "LDL", "age_months", "private_ins"

glm5 = svyglm(
  CKD_epi_eGFR ~ hypertension + diabetes + male + 
    LDL + age_months + private_ins,
  design = svy_qq,
  family = gaussian
)  
qqnorm(glm5$residuals)
qqline(glm5$residuals)









