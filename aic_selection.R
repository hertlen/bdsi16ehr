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
               "private_ins","Medicare_ins","sleep_amount","vigorous_activity",
               "meals_not_home","Chol_self")
              

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

plot(residuals.list[,6])
hist(residuals.list[,6])
hist(log(residuals.list[,6]))

##CV -----
index = sample(1:nrow(NHANES.MEC.design1), nrow(NHANES.MEC.design1)/2,
               replace=FALSE)
training_set = NHANES.MEC.design1[index,]
test_set = NHANES.MEC.design1[-index,]

list.acc = rep(0,length(covariates))
acc.old = 0
for (i in 1:length(covariates)){
  print(i)
  fm = as.formula(paste("CKD ~",covariates[i]))
  fit1a = svyglm(fm, design = training_set, family="binomial")
  prediction = predict(fit1a, test_set$variables, type = "response")
  ind.prediction = as.numeric(rownames(as.data.frame(prediction)))
  prediction = as.numeric(prediction > min(prediction))
  tab = table(prediction, test_set$variables$CKD[ind.prediction])
  acc.new = sum(diag(tab))/sum(tab)
  list.acc[i] = acc.new
  if (acc.new > acc.old){
    acc.old = acc.new
    best.fm = fm
    best.i = i
  }
}

acc.history = acc.old
best.acc = acc.old
best.is = best.i
done = 0
residuals.list = NULL
while(done==0){
  previous.acc = best.acc
  list.acc = NULL
  list.cov.acc = NULL
  loop.count = 0
  for (i in 1:length(covariates)){
    if(i %in% best.is) {
      loop.count = loop.count+1
      next # skip iteration and go to next iteration
      cat(n) }
    fm = as.formula(paste(deparse(best.fm),"+",covariates[i]))
    fit1a = svyglm(fm, design = training_set, family = "binomial")
    
    prediction = predict(fit1a, test_set$variables, type = "response")
    ind.prediction = as.numeric(rownames(as.data.frame(prediction)))
    
    prediction = as.numeric(prediction >= 0.55)
    tab = table(prediction, test_set$variables$CKD[ind.prediction])
    acc.new = sum(diag(tab))/sum(tab)
    
   
    list.acc = c(list.acc,acc.new)
    list.cov.acc = c(list.cov.acc,covariates[i])
  }
  if (loop.count != length(covariates)){
    best.i = which(list.acc == min(list.acc))
    best.acc = min(list.acc)
    best.fm = as.formula(paste(deparse(best.fm),"+",list.cov.acc[best.i]))
    best.is = c(best.is,which(covariates == list.cov.acc[best.i]))
    acc.history = c(acc.history,min(list.acc))
    fit1a = svyglm(best.fm, design = training_set, family = "binomial")
    residual.model = resid(fit1a)
    residuals.list = cbind(residuals.list,residual.model)
  }
  if(previous.acc > best.acc | loop.count == length(covariates) ){
    done = 1
  }
}

colnames(residuals.list) = seq(1:ncol(residuals.list))
fit1a = svyglm(best.fm, design = training_set, family="binomial")
summary(fit1a)

plot(residuals.list[,6])
hist(residuals.list[,6])
hist(log(residuals.list[,6]))