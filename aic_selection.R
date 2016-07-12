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
               "factor(htn_gp)","HDL","LDL","factor(angina_self)",
               "factor(stroke_self)","age_months","factor(kidney_told)","insured",
               "private_ins","Medicare_ins","sleep_amount","vigorous_activity","vigorous_work",
               "vigorous_rec","meals_not_home","Chol_self")

covariates = c("hypertension","age_years","male","diabetes",
               "BMI","Total_chol","factor(Smoking)","factor(race_eth)","obese",
               "factor(education)","Triglycerides","factor(annual_house_income)","private_ins",
covariates = c("factor(hypertension)","age_years","factor(male)","factor(diabetes)",
               "BMI","Total_chol","factor(Smoking)","factor(race_eth)","factor(obese)",
               "factor(education)","Triglycerides","factor(annual_house_income)","factor(private_ins)",
               "factor(angina_self)","sleep_amount")             

#AIC ----
aic.old = 10^6
list.aic = rep(0,length(covariates))

for (i in 1:length(covariates)){
  fm = as.formula(paste("CKD ~",covariates[i]))
  fit1a = svyglm(fm, design = NHANES.MEC.design1, family="binomial")
  aic.new = fit1a$aic
  list.aic[i] = aic.new
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
while(done==0){
  previous.aic = best.aic
  list.aic = NULL
  list.cov.aic = NULL
  loop.count = 0
  for (i in 1:length(covariates)){
    if(i %in% best.is) {
      loop.count = loop.count+1
      next # skip iteration and go to next iteration
      cat(n) }
    fm = as.formula(paste(deparse(best.fm),"+",covariates[i]))
    fit1a = svyglm(fm, design = NHANES.MEC.design1, family = "binomial")
    aic.new = fit1a$aic
    list.aic = c(list.aic,aic.new)
    list.cov.aic = c(list.cov.aic,covariates[i])
  }
  if (loop.count != length(covariates)){
    best.i = which(list.aic == min(list.aic))
    best.aic = min(list.aic)
    best.fm = as.formula(paste(deparse(best.fm),"+",list.cov.aic[best.i]))
    best.is = c(best.is,which(covariates == list.cov.aic[best.i]))
    aic.history = c(aic.history,min(list.aic))
    fit1a = svyglm(best.fm, design = NHANES.MEC.design1, family = "binomial")
    residual.model = resid(fit1a)
    residuals.list = cbind(residuals.list,residual.model)
  }
  if(previous.aic < best.aic | loop.count == length(covariates) ){
    done = 1
  }
}

colnames(residuals.list) = seq(1:ncol(residuals.list))
fit1a = svyglm(best.fm, design = NHANES.MEC.design1, family="binomial")
summary(fit1a)
best.fm #BEST MODEL

plot(aic.history)
plot(residuals.list[,6])
hist(residuals.list[,6])
hist(log(residuals.list[,6]))

#BIC -----

bic.old = 10^6
list.bic = rep(0,length(covariates))

for (i in 1:length(covariates)){
  fm = as.formula(paste("CKD ~",covariates[i]))
  fit1a = svyglm(fm, design = NHANES.MEC.design1, family="binomial")
  bic.new = AIC(fit1a,k = log(length(fit1a)))[2]
  list.bic[i] = bic.new
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
while(done==0){
  previous.bic = best.bic
  list.bic = NULL
  list.cov.bic = NULL
  loop.count = 0
  for (i in 1:length(covariates)){
    if(i %in% best.is) {
      loop.count = loop.count+1
      next # skip iteration and go to next iteration
      cat(n) }
    fm = as.formula(paste(deparse(best.fm),"+",covariates[i]))
    fit1a = svyglm(fm, design = NHANES.MEC.design1, family = "binomial")
    bic.new = AIC(fit1a,k = log(length(fit1a)))[2]
    list.bic = c(list.bic,bic.new)
    list.cov.bic = c(list.cov.bic,covariates[i])
  }
  if (loop.count != length(covariates)){
    best.i = which(list.bic == min(list.bic))
    best.bic = min(list.bic)
    best.fm = as.formula(paste(deparse(best.fm),"+",list.cov.bic[best.i]))
    best.is = c(best.is,which(covariates == list.cov.bic[best.i]))
    bic.history = c(bic.history,min(list.bic))
    fit1a = svyglm(best.fm, design = NHANES.MEC.design1, family = "binomial")
    residual.model = resid(fit1a)
    residuals.list = cbind(residuals.list,residual.model)
  }
  if(previous.bic < best.bic | loop.count == length(covariates) ){
    done = 1
  }
}

colnames(residuals.list) = seq(1:ncol(residuals.list))
fit1a = svyglm(best.fm, design = NHANES.MEC.design1, family="binomial")
summary(fit1a)
best.fm #BEST MODEL

plot(bic.history)
plot(residuals.list[,5])
hist(residuals.list[,5])
hist(log(residuals.list[,5]))
plot(residuals.list[,6])
hist(residuals.list[,6])
hist(log(residuals.list[,6]))
