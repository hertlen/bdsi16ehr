install.packages("ROCR")
install.packages("pROC")

library(ROCR)
library(pROC)

#practice from Cran website
data("aSAH")
roc(aSAH$outcome, aSAH$s100b)
roc(outcome~s100b,aSAH)
x<-c("CKD","age_years")


#initial code for just one logistic regression
data0=read.csv("NHANES.csv")
data1=data0[data0$SDDSRVYR>=2 & data0$SDDSRVYR<=8,]
newdata = data1[which(!is.na(data1$age_years)),]
newdata = data1[which(!is.na(data1$CKD)),]
WTMEC14YR = newdata$WTMEC2YR/7
NHANES.design = svydesign(ids=~SDMVPSU , strata=~SDMVSTRA , nest = TRUE , 
                          weights = ~WTMEC14YR, data = newdata)

model6 <- svyglm(CKD ~ age_years, design=NHANES.design, family=quasibinomial(link="logit"))
roc(newdata$CKD, model6$fitted.values, plot=TRUE)


#Checking measurements to make sure response and predictor vectors will be same length
nrow(newdata)
length(model6$fitted.values)
nrow(newdata)
nrow(newdata[which(!is.na(newdata$age_years)),])
nrow(newdata[which(!is.na(newdata$CKD)),])

#function for computing weighted ROC curve for svyglm models
#always put the response variable first
roc.curve <- function(x,data) {
  x=as.vector(x)
  data=as.data.frame(data)
  for (i in x) {
    data = data[which(!is.na(data[[i]])),]
  }
  newdata=data[data$SDDSRVYR>=2 & data$SDDSRVYR<=8,]
  WTMEC14YR = newdata$WTMEC2YR/7
  NHANES.design = svydesign(ids=~SDMVPSU , strata=~SDMVSTRA , nest = TRUE , 
                            weights = ~WTMEC14YR, data = newdata)
  form = as.formula(paste("CKD ~",paste(x[2:length(x)],collapse = "+")))
  model <- svyglm(form, design=NHANES.design, family=quasibinomial(link="logit"))
  roc.results = roc(newdata$CKD, model$fitted.values, plot=TRUE)
  return(roc.results)
}

x <- c("CKD","age_years","BMI")
roc.curve(x,data1)
form = as.formula(paste("CKD ~",paste(x[2:length(x)],collapse = "+")))
