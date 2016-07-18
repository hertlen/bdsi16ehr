library("survey")

data0 = read.csv("NHANES.csv")
data0 = data0[data0$SDDSRVYR >= 2 & data0$SDDSRVYR <= 8, ]
data0 = data0[data0$age_years >= 18, ]
agegrp2 = as.numeric((data0$age_years >=40 & data0$age_years<=59))
agegrp3 = as.numeric((data0$age_years >= 60))
data0 = cbind(data0, agegrp2, agegrp3)

# bootstrap model
# covariates_test = c("hypertension", "male", "diabetes", "race2", "race3", "race4", "race5",
#                     "agegrp2", "agegrp3")

boot.size = 1000
storage = matrix(nrow = boot.size, ncol = 10, NA)
for(i in 1:boot.size) {
  # randomly sample n observations from the data set
  boot_obs = sample(1:nrow(data0), nrow(data0), replace = TRUE)
  boot_data = data0[boot_obs, ]
  boot_weights = boot_data$WTMEC2YR / 7
  # create survey object based on those observations
  boot_design = svydesign(
    ids = ~SDMVPSU,
    strata = ~SDMVSTRA,
    nest = TRUE,
    weights = ~boot_weights,
    data = boot_data
  )
  options(survey.lonely.psu = "adjust")
  boot_glm = svyglm(
    CKD_epi_eGFR ~ hypertension + male + diabetes + factor(race_eth) + agegrp2 + agegrp3,
    design = boot_design,
    family = gaussian
  )
  storage[i, ] = boot_glm$coefficients
}

colnames(storage) = c("intercept", "hypertension coef", "male coef", "diabetes coef",
                      "race2 coef", "race3 coef", "race4 coef", "race5 coef", "agegrp2 coef", "agegrp3 coef")
storage = as.data.frame(storage)

# compute 95% quantile CIs
quantiles = vector("list", ncol(storage))
for(j in 1:ncol(storage)) {
  quantiles[[j]] = quantile(storage[, j], probs = c(0.025, 0.975))
}
names(quantiles) = c("intercept", "hypertension coef", "male coef", "diabetes coef",
                        "race2 coef", "race3 coef", "race4 coef", "race5 coef", "agegrp2 coef", "agegrp3 coef")


quantiles




