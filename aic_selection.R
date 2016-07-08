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
               "BMI","Total_chol","factor(Smoking)","factor(race_eth)")

svyglm(CKD_epi_eGFR ~ +hypertension+age_years+diabetes+
         BMI+Total_chol+factor(Smoking), design = NHANES.MEC.design1)

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
  }
  if(previous.aic < best.aic | loop.count == length(covariates) ){
    done = 1
  }
}

fit1a = svyglm(best.fm, design = NHANES.MEC.design1, family="binomial")
summary(fit1a)






data[[whatever]]

