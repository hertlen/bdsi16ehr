library(survey)
data0 = read.csv("NHANES.csv")
data0 = data0[data0$age_years >= 18,]

smoking_former = as.numeric((data0$Smoking == "Former Smoker"))
smoking_current = as.numeric((data0$Smoking == "Current Smoker"))

data0 = cbind(data0, smoking_former, smoking_current)

race2 = as.numeric((data0$race_eth == 2))
race3 = as.numeric((data0$race_eth == 3))
race4 = as.numeric((data0$race_eth == 4))
race5 = as.numeric((data0$race_eth == 5))

data0 = cbind(data0, race2, race3, race4, race5)

data1 <- data0[data0$SDDSRVYR >= 2 & data0$SDDSRVYR <= 8,]
WTMEC_1 <- data1$WTMEC2YR/7



NHANES.MEC.design1 <- svydesign(
  ids = ~SDMVPSU ,         
  strata = ~SDMVSTRA ,   
  nest = TRUE ,
  weights = ~WTMEC_1,
  data = data1
)

covariates = c("hypertension","age_years","male","diabetes",
               "BMI","Total_chol","factor(Smoking)","factor(race_eth)","obese",
               "factor(education)","Triglycerides","factor(annual_house_income)",
               "HDL","LDL",
               "factor(stroke_self)","age_months","insured",
               "private_ins","Medicare_ins","meals_not_home",

               "factor(htn_gp)",
               "factor(stroke_self)","age_months","insured",
               "private_ins","Medicare_ins","meals_not_home","Chol_self")

covariates = c("hypertension","age_years","male","diabetes",
               "BMI","Total_chol","factor(Smoking)","factor(race_eth)","obese",
               "Triglycerides",
               "HDL","LDL",
               "age_months","insured",
               "private_ins")

covariates = c("hypertension","age_years","male","diabetes",
               "BMI","Total_chol","obese",
               "Triglycerides",
               "HDL","LDL",
               "age_months","insured",
               "private_ins",
               "race2",
               "race3", "race4", "race5",
               "smoking_former", "smoking_current")
               



covariates = c("hypertension","age_years","male","diabetes",
               "BMI","Total_chol","factor(Smoking)","factor(race_eth)","obese",
               "factor(education)","Triglycerides",
               "HDL","LDL",
               "age_months","insured",
               "private_ins")


covariates2 = c("hypertension","BMI","diabetes","factor(Smoking)",
                "LDL","age_years"
                )
#BIC 11874
#BIC 8657 wout

covariates2 = c("hypertension","male","diabetes","factor(education)",
                "Triglycerides","age_months","private_ins"
)
#BIC 13926
#BIC 10755 wout

covariates2 = c("hypertension","male","diabetes","LDL","HDL","Total_chol",
                "Triglycerides","age_months","private_ins"
)
#BIC 7887
#BIC 5703
covariates2 = c("hypertension","male","diabetes","factor(education)",
                "LDL","age_months","private_ins"
)
#BIC 7176
#BIC 5542 wout

covariates2 = c("hypertension","male","diabetes",
                "age_months","race2",
                "race3", "race4", "race5"
)

covariates = c("hypertension","male","diabetes","age_months")
#5699.97 w/out <18 !!!!!!!


fm = as.formula(paste("CKD_epi_eGFR~",paste(covariates2, collapse= "+")))

fit1a = svyglm(fm, design = NHANES.MEC.design1, family="gaussian")
AIC(fit1a,k = log(length(fit1a)))[2]
summary(fit1a)

#AIC ----
aic.old = 10^6
list.aic = rep(0,length(covariates))

for (i in 1:length(covariates)){
  fm = as.formula(paste("CKD ~",covariates[i]))
  fit1a = svyglm(fm, design = NHANES.MEC.design1, family="binomial")
  aic.new = fit1a$aic
  list.aic[i] = aic.new
  good.predictors = covariates[which(list.aic == min(list.aic))]
  if (aic.new < aic.old){
    aic.old = aic.new
    best.fm = fm
    best.i = i
  }
}

aic.history = aic.old
best.aic = aic.old
best.is = best.i
done = 0
residuals.list = NULL
k = 0
while(done==0){
  k = k+1
  print(k)
  previous.aic = best.aic
  previous.fm = best.fm
  list.aic = NULL
  list.cov.aic = NULL
  loop.count = 0
  for (i in 1:length(covariates)){
    if(i %in% best.is) {
      next # skip iteration and go to next iteration
      cat(n) }
    test.predictors = c(good.predictors,covariates[i])
    fm = as.formula(paste("CKD~",paste(covariates[covariates %in% test.predictors], collapse= "+")))
    fit1a = svyglm(fm, design = NHANES.MEC.design1, family = "binomial")
    aic.new = fit1a$aic
    list.aic = c(list.aic,aic.new)
    list.cov.aic = c(list.cov.aic,covariates[i])
  }
  
 best.i = which(list.aic == min(list.aic)) 
 best.cov = list.cov.aic[best.i] 
 good.predictors = c(good.predictors,best.cov)
 best.aic = min(list.aic)
 aic.history = c(aic.history,min(list.aic))
 best.is = c(best.is,which(covariates == best.cov))
 best.fm = as.formula(paste("CKD~",paste(covariates[covariates %in% good.predictors], collapse= "+")))
#  fit1a = svyglm(best.fm, design = NHANES.MEC.design1, family = "binomial")
#  residual.model = resid(fit1a)
#  residuals.list = cbind(residuals.list,residual.model)
 print(best.fm)
 print(best.aic)
  if(previous.aic < best.aic ){
    done = 1
    best.fm = previous.fm 
    best.aic = previous.aic
  }
}
best.fm

#colnames(residuals.list) = seq(1:ncol(residuals.list))
fit1a = svyglm(best.fm, design = NHANES.MEC.design1, family="binomial")
summary(fit1a)
best.fm #BEST MODEL
best.aic #BEST AIC
plot(aic.history[2:length(aic.history)])
# plot(residuals.list[,6])
# hist(residuals.list[,6])
# hist(log(residuals.list[,6]))

#BIC -----
bic.old = 10^6
list.bic = rep(0,length(covariates))

for (i in 1:length(covariates)){
  fm = as.formula(paste("CKD_epi_eGFR ~",covariates[i]))
  fit1a = svyglm(fm, design = NHANES.MEC.design1, family="binomial")
  bic.new = fit1a$bic
  list.bic[i] = bic.new
  good.predictors = covariates[which(list.bic == min(list.bic))]
  if (bic.new < bic.old){
    bic.old = bic.new
    best.fm = fm
    best.i = i
  }
}

bic.history = bic.old
best.bic = bic.old
best.is = best.i
done = 0
residuals.list = NULL
j = 0
while(done==0){
  j = j+1
  print(j)
  previous.bic = best.bic
  previous.fm = best.fm
  list.bic = NULL
  list.cov.bic = NULL
  loop.count = 0
  for (i in 1:length(covariates)){
    if(i %in% best.is) {
      next # skip iteration and go to next iteration
      cat(n) }
    test.predictors = c(good.predictors,covariates[i])
    fm = as.formula(paste("CKD~",paste(covariates[covariates %in% test.predictors], collapse= "+")))
    fit1a = svyglm(fm, design = NHANES.MEC.design1, family = "binomial")
    bic.new = AIC(fit1a,k = log(length(fit1a)))[2]      
    list.bic = c(list.bic,bic.new)
    list.cov.bic = c(list.cov.bic,covariates[i])
  }
  
  best.i = which(list.bic == min(list.bic)) 
  best.cov = list.cov.bic[best.i] 
  good.predictors = c(good.predictors,best.cov)
  best.bic = min(list.bic)
  bic.history = c(bic.history,min(list.bic))
  best.is = c(best.is,which(covariates == best.cov))
  best.fm = as.formula(paste("CKD~",paste(covariates[covariates %in% good.predictors], collapse= "+")))
  #  fit1a = svyglm(best.fm, design = NHANES.MEC.design1, family = "binomial")
  #  residual.model = resid(fit1a)
  #  residuals.list = cbind(residuals.list,residual.model)
  print(best.fm)
  print(best.bic)
  if(previous.bic < best.bic ){
    done = 1
    best.fm = previous.fm 
    best.bic = previous.bic
  }
}
best.fm

#colnames(residuals.list) = seq(1:ncol(residuals.list))
fit1a = svyglm(best.fm, design = NHANES.MEC.design1, family="binomial")
summary(fit1a)
AIC(fit1a,k = log(length(fit1a)))[2]       
best.fm #BEST MODEL
best.bic

plot(bic.history[2:length(bic.history)])
# plot(residuals.list[,6])
# hist(residuals.list[,6])
# hist(log(residuals.list[,6]))

#AIC BACKWARD ----
aic.old = 10^6
list.aic = rep(0,length(covariates))

fm = as.formula(paste("CKD~",paste(covariates, collapse= "+")))

aic.history = NULL
bad.aic = 10^6
bad.is = NULL
done = 0
bad.predictors = NULL
previous.fm = fm
j = 0
while(done==0){
  j = j+1
  print(j)
  previous.aic = bad.aic
  previous.fm = best.fm
  list.aic = NULL
  list.cov.aic = NULL
  loop.count = 0
  for (i in 1:length(covariates)){
    if(i %in% bad.is) {
      next # skip iteration and go to next iteration
      cat(n) }
    test.predictors = c(bad.predictors,covariates[i])
    fm = as.formula(paste("CKD~",paste(covariates[!covariates %in% test.predictors], collapse= "+")))
    fit1a = svyglm(fm, design = NHANES.MEC.design1, family = "binomial")
    aic.new = fit1a$aic
    list.aic = c(list.aic,aic.new)
    list.cov.aic = c(list.cov.aic,covariates[i])
  }
  
  bad.i = which(list.aic == min(list.aic)) 
  worst.cov = list.cov.aic[bad.i] 
  bad.predictors = c(bad.predictors,worst.cov)
  bad.aic = min(list.aic)
  aic.history = c(aic.history,bad.aic)
  bad.is = c(bad.is,which(covariates == worst.cov))
  best.fm = as.formula(paste("CKD~",paste(covariates[!covariates %in% bad.predictors], collapse= "+")))
  print(best.fm)
  print(bad.aic)
  
  if(previous.aic < bad.aic){
    done = 1
    best.fm = previous.fm
    best.aic = previous.aic
  }
}

fit1a = svyglm(best.fm, design = NHANES.MEC.design1, family="binomial")
summary(fit1a)
best.fm #BEST MODEL
best.aic

plot(aic.history)

#BIC BACKWARD ------
bic.old = 10^6
list.bic = rep(0,length(covariates))

fm = as.formula(paste("CKD_epi_eGFR~",paste(covariates, collapse= "+")))

bic.history = NULL
bad.bic = 10^10
bad.is = NULL
done = 0
bad.predictors = NULL
previous.fm = fm
best.fm = NULL
j = 0
while(done==0){
  j = j+1
  print(j)
  previous.bic = bad.bic
  previous.fm = best.fm
  list.bic = NULL
  list.cov.bic = NULL
  loop.count = 0
  for (i in 1:length(covariates)){
    if(i %in% bad.is) {
      next # skip iteration and go to next iteration
      cat(n) }
    test.predictors = c(bad.predictors,covariates[i])
    fm = as.formula(paste("CKD_epi_eGFR~",paste(covariates[!covariates %in% test.predictors], collapse= "+")))
    fit1a = svyglm(fm, design = NHANES.MEC.design1, family = "gaussian")
    bic.new = AIC(fit1a,k = log(length(fit1a)))[2] 
    list.bic = c(list.bic,bic.new)
    list.cov.bic = c(list.cov.bic,covariates[i])
  }
  
  bad.i = which(list.bic == min(list.bic)) 
  worst.cov = list.cov.bic[bad.i] 
  bad.predictors = c(bad.predictors,worst.cov)
  bad.bic = min(list.bic)
  bic.history = c(bic.history,bad.bic)
  bad.is = c(bad.is,which(covariates == worst.cov))
  best.fm = as.formula(paste("CKD_epi_eGFR~",paste(covariates[!covariates %in% bad.predictors], collapse= "+")))
  
  print(best.fm)
  print(bad.bic)
  
  if(previous.bic < bad.bic){
    done = 1
    best.fm = previous.fm
    best.bic = previous.bic
  }
}

fit1a = svyglm(best.fm, design = NHANES.MEC.design1, family="gaussian")
summary(fit1a)
best.fm #BEST MODEL
best.bic #BEST BIC
plot(bic.history)


