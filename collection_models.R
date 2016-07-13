library(ggplot2)
library(ResourceSelection)
library(locfit)
library(survey)

#ask about mallows cp...

data0=read.csv("NHANES.csv")
data1=data0[data0$SDDSRVYR>=2 & data0$SDDSRVYR<=8,]
WTMEC14YR = data1$WTMEC2YR/7
NHANES.design = svydesign(ids=~SDMVPSU , strata=~SDMVSTRA , nest = TRUE , 
                          weights = ~WTMEC14YR, data = data1)

model1 <- svyglm(CKD ~ LDL + vigorous_rec + age_months + Chol_self + private_ins, design=NHANES.design, family=quasibinomial(link="logit"))

model2 <- svyglm(CKD ~ sleep_amount + Triglycerides + private_ins + hypertension + 
  +factor(education), design=NHANES.design, family=quasibinomial(link="logit"))

model3 <- svyglm(CKD_epi_eGFR ~ hypertension + age_years + I(age_years^2) + 
                   BMI + BMI*age_years, design=NHANES.design, family="gaussian")

model4 <- svyglm(CKD_epi_eGFR ~ hypertension + age_years + I(age_years^2)
                 , design=NHANES.design, family="gaussian")

#adj.r.squared of 60
model5 <- svyglm(CKD ~ Triglycerides + factor(diabetes) + age_years + I(age_years^2)
                 , design=NHANES.design, family=quasibinomial(link="logit"))

model6 <- svyglm(CKD ~ age_years + I(age_years^2)
                 , design=NHANES.design, family=quasibinomial(link="logit"))

model7 <- svyglm(CKD_epi_eGFR ~ hypertension + age_years + I(age_years^2), design=NHANES.design, family="gaussian")

model8 <- svyglm(CKD_epi_eGFR ~ factor(hypertension) + age_years + +factor(diabetes) + 
                   factor(diabetes)*age_years + BMI*factor(diabetes) + UACR + factor(private_ins)
                 , design=NHANES.design, family="gaussian")


summary(model7)
a.r.2(model7)

cp(model1)[5]

hoslem.test(NHANES.design$variables$CKD, model1$fitted.values)

ss <- svytotal(~factor(CKD_stage), design=NHANES.design, na.rm=TRUE)
ss2 <- svytotal(~factor(hypertension), design=NHANES.design, na.rm=TRUE)
195842378+12326115+6328620+12905825+939149+361013
215279166+20981426
144958139+68510054

a.r.2 <- function(model){
R.2=1-(model$deviance/model$null.deviance)
P=length(model$coefficients)-1
N=300000000
adj.r.sq=1-abs((((1-R.2)*(N-1))/(P-N-1)))
return(adj.r.sq)
}

a.r.2(model1)

#MDS
attach(Orange)
d <- dist(Orange)
fit <- cmdscale(d, eig=TRUE, k=2)
x <- fit$points[,1]
y <- fit$points[,2]
plot(x,y, type = "n")
text(x,y, labels=row.names(Orange), cex=.7)

#MDS
data2<-data0[1:100,1:10]
d <- dist(data2)
fit <- cmdscale(d, eig=TRUE, k=2)
x <- fit$points[,1]
y <- fit$points[,2]
plot(x,y,type="n")
text(x,y, labels=row.names(data2), cex=.7)

mean(model6$residuals^2)
hist(model6$residuals)

plot(model6)

install.packages("ROCR")
library(ROCR)
install.packages("pROC")
library(pROC)


x <- data1[complete.cases(data1),]
x <- na.omit(data1)
newdata <- na.omit(data1)
model6<-glm(CKD~age_years+BMI, data=newdata, family="binomial")
prob=predict(model6, type="response")
newdata$prob=prob
g<-roc(CKD ~ prob, data=data1)
plot(g)

auc()
