library(survey)
data0 = read.csv("NHANES.csv")


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
               "private_ins","Medicare_ins","meals_not_home")
               "factor(htn_gp)","HDL","LDL",
               "factor(stroke_self)","age_months","insured",
               "private_ins","Medicare_ins","meals_not_home","Chol_self")

covariates = c("hypertension","age_years","male","diabetes",
               "BMI","Total_chol","factor(Smoking)","factor(race_eth)","obese",
               "factor(education)","Triglycerides","factor(annual_house_income)","private_ins",
               "factor(angina_self)")

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

fm = as.formula(paste("CKD~",paste(covariates, collapse= "+")))

bic.history = NULL
bad.bic = 10^6
bad.is = NULL
done = 0
bad.predictors = NULL
previous.fm = fm
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
    fm = as.formula(paste("CKD~",paste(covariates[!covariates %in% test.predictors], collapse= "+")))
    fit1a = svyglm(fm, design = NHANES.MEC.design1, family = "binomial")
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
  best.fm = as.formula(paste("CKD~",paste(covariates[!covariates %in% bad.predictors], collapse= "+")))
  
  print(best.fm)
  print(bad.bic)
  
  if(previous.bic < bad.bic){
    done = 1
    best.fm = previous.fm
    best.bic = previous.bic
  }
}

fit1a = svyglm(best.fm, design = NHANES.MEC.design1, family="binomial")
summary(fit1a)
best.fm #BEST MODEL
best.bic #BEST BIC
plot(bic.history)


