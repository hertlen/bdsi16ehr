install.packages("ROCR")
install.packages("pROC")

library(ROCR)
library(pROC)

data0=read.csv("NHANES.csv")

#function for computing weighted ROC curve for svyglm models
#returns AUC and ROC curve plot

#x is vector of response/predictor variables WITHOUT label factor()
#f is a vector of response/predictor variables WITH label factor()
#always put the response variable first 
#(i.e. x<-c("CKD","age_years","BMI) ==> CKD~age_years+BMI)

roc.curve <- function(x,f,data) {
  x=as.vector(x)
  data=as.data.frame(data)
  for (i in x) {
    data = data[which(!is.na(data[[i]])),]
  }
  newdata=data[data$SDDSRVYR>=2 & data$SDDSRVYR<=8,]
  WTMEC14YR = newdata$WTMEC2YR/7
  NHANES.design = svydesign(ids=~SDMVPSU , strata=~SDMVSTRA , nest = TRUE , 
                            weights = ~WTMEC14YR, data = newdata)
  form = as.formula(paste("CKD ~",paste(f[2:length(f)],collapse = "+")))
  model <- svyglm(form, design=NHANES.design, family=quasibinomial(link="logit"))
  roc.results = roc(newdata$CKD, model$fitted.values, plot=TRUE)
  return(roc.results)
}

x <- c("CKD","hypertension","diabetes","BMI","Total_chol",
       "Smoking","Triglycerides","annual_house_income",
       "HDL","LDL","age_months","Medicare_ins")
f <- c("CKD","factor(hypertension)","factor(diabetes)","BMI","Total_chol",
       "factor(Smoking)","Triglycerides","factor(annual_house_income)",
       "HDL","LDL","age_months","factor(Medicare_ins)")

roc.curve(x,f,data0)
