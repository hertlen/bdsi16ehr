library(survey)

load("NHANES.RData")


data1 <- data0[data0$SDDSRVYR >= 2 & data0$SDDSRVYR <= 8,]
WTMEC_1 <- data1$WTMEC2YR/7

NHANES.MEC.design1 <- svydesign(
  ids = ~SDMVPSU ,         
  strata = ~SDMVSTRA ,   
  nest = TRUE ,
  weights = ~WTMEC_1,
  data = data1
)

covariates = c("hypertension","age_years","diabetes",
               "BMI","Total_chol")

aic.old = 10^6
list.aic = rep(0,5)
for (i in 1:length(covariates)){
  fm = as.formula(paste("CKD_epi_eGFR ~",covariates[i]))
  fit1a = svyglm(fm, design = NHANES.MEC.design1)
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
  for (i in 1:length(covariates)){
    if(i %in% best.is) {next # skip 3rd iteration and go to next iteration
      cat(n) }
    fm = as.formula(paste(deparse(best.fm),"+",covariates[i]))
    fit1a = svyglm(fm, design = NHANES.MEC.design1)
    aic.new = fit1a$aic
    list.aic = c(list.aic,aic.new)
    list.cov.aic = c(list.cov.aic,covariates[i])
  }
  
  best.aic = which(list.aic == min(list.aic))
  best.fm = as.formula(paste(deparse(best.fm),"+",list.cov.aic[best.aic]))
  best.is = c(best.is,which(covariates == list.cov.aic[best.aic]))
  aic.history = c(aic.history,min(list.aic))
  if(previous.aic == best.aic){
    done = 1
  }
}










