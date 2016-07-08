library(survey)
library(ggplot2)
library(ggthemes)

load("/Users/lydialucchesi/Downloads/NHANES.RData")
data1=data0[data0$SDDSRVYR>=2 & data0$SDDSRVYR<=8,]
WTMEC14YR = data1$WTMEC2YR/7
NHANES.design = svydesign(ids=~SDMVPSU , strata=~SDMVSTRA , nest = TRUE , 
                          weights = ~WTMEC14YR, data = data1)


#hypertension
hyp.ckdstg.by <- svyby(formula=~hypertension, by=~CKD_stage, 
                       design=NHANES.design, FUN=svymean, na.rm=TRUE)

hyp.ckdstg <- as.data.frame(hyp.ckdstg, header=TRUE)

p.hyp <- ggplot(hyp.ckdstg, aes(CKD_stage,hypertension)) + 
  geom_point() + theme_minimal() + 
  labs(x='CKD Stage', y='Proportion with Hypertension',
       title='Hypertension and CKD')

#Triglycerides
tri.ckdstg.box <- svyboxplot(Triglycerides~factor(CKD_stage), design = NHANES.design, 
                             ylim=c(0,400))

tri.ckdstg.by <- svyby(formula=~Triglycerides, by=~CKD_stage, 
                       design=NHANES.design, FUN=svymean, na.rm=TRUE)

tri.ckdstg <- as.data.frame(tri.ckdstg.by, header=TRUE)

p.tri <- ggplot(tri.ckdstg, aes(CKD_stage,Triglycerides)) + 
  geom_point() + theme_minimal() + 
  labs(x='CKD Stage', y='Triglyceride Count',
       title='Triglycerides and CKD')

#BMI
bmi.ckdstg <- svyboxplot(BMI~factor(CKD_stage), design = NHANES.design)

bmi.ckdstg.by <- svyby(formula=~BMI, by=~CKD_stage, 
                       design=NHANES.design, FUN=svymean, na.rm=TRUE)

bmi.ckdstg.glm <- svyglm(BMI~factor(CKD_stage), design=NHANES.design)
bmi.ckdstg.scat <- svyplot(BMI~factor(CKD_stage), design=NHANES.design)

bmi.ckdstg <- as.data.frame(bmi.ckdstg.by, header=TRUE)

#weird scale
p.bmi <- ggplot(bmi.ckdstg, aes(CKD_stage,BMI)) + 
  geom_point() + theme_minimal() + 
  labs(x='CKD Stage', y='BMI',
       title='BMI and CKD')

#age_years
age.ckdstg <- svyboxplot(age_years~factor(CKD_stage), design = NHANES.design, na.rm=TRUE)

age.ckdstg.by <- svyby(formula=~age_years, by=~CKD_stage, 
                       design=NHANES.design, FUN=svymean, na.rm=TRUE)

age.ckdstg <- as.data.frame(age.ckdstg.by, header=TRUE)

p.age <- ggplot(age.ckdstg, aes(CKD_stage,age_years)) + 
  geom_point() + theme_minimal() + 
  labs(x='CKD Stage', y='Age',
       title='Age and CKD')

#Total_chol
chol.ckdstg <- svyboxplot(Total_chol~factor(CKD_stage), design = NHANES.design, 
                          ylim=c(50,320))

chol.ckdstg.by <- svyby(formula=~Total_chol, by=~CKD_stage, 
                        design=NHANES.design, FUN=svymean, na.rm=TRUE)

chol.ckdstg <- as.data.frame(chol.ckdstg.by, header=TRUE)

#weird scale
p.chol <- ggplot(chol.ckdstg, aes(CKD_stage,Total_chol)) + 
  geom_point() + theme_minimal() + 
  labs(x='CKD Stage', y='Cholesterol Level',
       title='Total Cholesterol and CKD')

#Smoking