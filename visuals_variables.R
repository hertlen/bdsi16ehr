# install.packages(c("ggplot2", "ggthemes", "reshape2", "survey"))

library(survey)
library(ggplot2)
library(ggthemes)
library(reshape2)

data0=read.csv("NHANES.csv")
data1=data0[data0$SDDSRVYR>=2 & data0$SDDSRVYR<=8,]
WTMEC14YR = data1$WTMEC2YR/7
NHANES.design = svydesign(ids=~SDMVPSU , strata=~SDMVSTRA , nest = TRUE , 
                          weights = ~WTMEC14YR, data = data1)


#hypertension
hyp.ckdstg.by <- svyby(formula=~hypertension, by=~CKD_stage, 
                       design=NHANES.design, FUN=svymean, na.rm=TRUE)

hyp.ckdstg <- as.data.frame(hyp.ckdstg.by, header=TRUE)

p.hyp <- ggplot(hyp.ckdstg, aes(CKD_stage,hypertension)) + 
  geom_point() + theme_minimal() + 
  labs(x='CKD Stage', y='Proportion with Hypertension',
       title='Hypertension and CKD')

#Triglycerides
tri.ckdstg.box <- svyboxplot(Triglycerides~factor(CKD_stage), design = NHANES.design, 
                             na.rm=TRUE, ylim=c(0,500))

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
smok.ckdstg.by <- svyby(formula=~Smoking, by=~CKD_stage, 
                        design=NHANES.design, FUN=svymean, na.rm=TRUE)

smok.ckdstg <- as.data.frame(smok.ckdstg.by, header=TRUE)

colnames(smok.ckdstg) <- c("Stage","Current_Smoker","Former_Smoker"
                           ,"Never", "SE", "SE", "SE")
smok.ckdstg <- melt(smok.ckdstg, id=c("Stage","SE"))
colnames(smok.ckdstg) <- c("Stage","Standard_Error","Smoking_Habits","Proportion")

#weird trend with former smokers
p.smok <- ggplot(smok.ckdstg, aes(x=Stage, y=Proportion)) + 
  geom_bar(stat="identity", position="dodge",aes(fill=Smoking_Habits)) + theme_few() + 
  labs(x='CKD Stage', y='Proportion Who Smoke', title='Smoking and CKD')
p.smok + scale_fill_manual(values=alpha(c('Current_Smoker'="#6CA6D9",
  'Former_Smoker'="#ABBF15", 'Never'="#403F37"), .85))

#diabetes
diab.ckdstg.by <- svyby(formula=~factor(diabetes), by=~CKD_stage, 
                        design=NHANES.design, FUN=svymean, na.rm=TRUE)

diab.ckdstg <- as.data.frame(diab.ckdstg.by, header=TRUE)

colnames(diab.ckdstg) <- c("Stage","No_Diabetes","Diabetes", "SE", "SE")
diab.ckdstg <- melt(diab.ckdstg, id=c("Stage","SE"))
colnames(diab.ckdstg) <- c("Stage","Standard_Error","Current_State","Proportion")

p.diab <- ggplot(diab.ckdstg, aes(x=Stage, y=Proportion)) + 
  geom_bar(stat="identity", position="dodge",aes(fill=Current_State)) + theme_few() + 
  labs(x='CKD Stage', y='Proportion with Diabetes', title='Diabetes and CKD')
p.diab + scale_fill_manual(values=alpha(c('No_Diabetes'="#8A8985",
           
                                                                                                                 'Diabetes'="#100904"), .85))
#private insurance
pri.ckdstg.by <- svyby(formula=~factor(private_ins), by=~CKD_stage, 
                       design=NHANES.design, FUN=svymean, na.rm=TRUE)

pri.ckdstg <- as.data.frame(pri.ckdstg.by, header=TRUE)

colnames(pri.ckdstg) <- c("Stage","No_Pri","Pri", "SE", "SE")
pri.ckdstg <- melt(pri.ckdstg, id=c("Stage","SE"))
colnames(pri.ckdstg) <- c("Stage","Standard_Error","Insurance","Proportion")

p.pri <- ggplot(pri.ckdstg, aes(x=Stage, y=Proportion)) + 
  geom_bar(stat="identity", position="dodge",aes(fill=Insurance)) + theme_few() + 
  labs(x='CKD Stage', y='Proportion with Private Insurance', title='Insurance and CKD')
p.pri + scale_fill_manual(values=alpha(c('No_Pri'="#8A8985",
                                         'Pri'="#100904"), .80))

#medicare insurance
medin.ckdstg.by <- svyby(formula=~factor(Medicare_ins), by=~CKD_stage, 
                       design=NHANES.design, FUN=svymean, na.rm=TRUE)

medin.ckdstg <- as.data.frame(medin.ckdstg.by, header=TRUE)

colnames(medin.ckdstg) <- c("Stage","No_Medin","Medin", "SE", "SE")
medin.ckdstg <- melt(medin.ckdstg, id=c("Stage","SE"))
colnames(medin.ckdstg) <- c("Stage","Standard_Error","Medicare","Proportion")

p.medin <- ggplot(medin.ckdstg, aes(x=Stage, y=Proportion)) + 
  geom_bar(stat="identity", position="dodge",aes(fill=Medicare)) + theme_few() + 
  labs(x='CKD Stage', y='Proportion with Medicare', title='Medicare and CKD')
p.medin + scale_fill_manual(values=alpha(c('No_Medin'="#8A8985",
                                         'Medin'="#100904"), .80))

#LDL
ldl.ckdstg.by <- svyby(formula=~LDL, by=~CKD_stage, 
                         design=NHANES.design, FUN=svymean, na.rm=TRUE)

ldl.ckdstg <- as.data.frame(ldl.ckdstg.by, header=TRUE)

colnames(ldl.ckdstg) <- c("Stage","LDL", "SE")

p.ldl <- ggplot(ldl.ckdstg, aes(x=Stage, y=LDL)) + 
  geom_bar(stat="identity", width=.85) + theme_few() + 
  labs(x='CKD Stage', y='LDL Level', title='LDL and CKD')

#HDL
hdl.ckdstg.by <- svyby(formula=~HDL, by=~CKD_stage, 
                       design=NHANES.design, FUN=svymean, na.rm=TRUE)

hdl.ckdstg <- as.data.frame(hdl.ckdstg.by, header=TRUE)

colnames(hdl.ckdstg) <- c("Stage","HDL", "SE")

p.hdl <- ggplot(hdl.ckdstg, aes(x=Stage, y=HDL)) + 
  geom_bar(stat="identity",width=.85) + theme_few() + 
  labs(x='CKD Stage', y='HDL Level', title='HDL and CKD')

#graphing several variables at once
Stages <- c(0,1,2,3,4,5)
df <- cbind(Stages,hyp.ckdstg$hypertension, tri.ckdstg$Triglycerides, 
            bmi.ckdstg$BMI, age.ckdstg$age_years,
            chol.ckdstg$Total_chol, smok.ckdstg$Proportion[1:6],
            smok.ckdstg$Proportion[7:12], smok.ckdstg$Proportion[13:18],
            diab.ckdstg$Proportion[1:6], diab.ckdstg$Proportion[7:12])

colnames(df) <- c("Stages","Hypertension", "Triglycerides", "BMI", "Age",
                  "Cholesterol","Smoke_Current", "Smoke_Former",
                  "Smoke_Never", "No_Diabetes","Diabetes")
df<-as.data.frame(df)
p <- ggplot(df)
p+geom_line(aes(Stages,Diabetes),color="#4daf4a")+ 
  geom_line(aes(Stages,Smoke_Current),color="#ff7f00")+
  geom_line(aes(Stages,Smoke_Former),color="#984ea3")+
  labs(x="CKD Stage",y="Proportion of U.S Population", 
  title="CKD and Potential Risk Factors")+theme_few()

#Cholesterol Components
Stages <- c(0,1,2,3,4,5)
chol.df <- cbind(Stages, tri.ckdstg$Triglycerides, ldl.ckdstg$LDL,
            hdl.ckdstg$HDL)

colnames(chol.df) <- c("Stages","Triglycerides", "LDL", "HDL")

chol.df<-as.data.frame(chol.df)

chol2.df <- melt(chol.df, id=c("Stages"))

p <- ggplot(chol.df)
p+geom_line(aes(Stages,Triglycerides),color="#4daf4a")+ 
  geom_line(aes(Stages,LDL),color="#ff7f00")+
  geom_line(aes(Stages,HDL),color="#984ea3")+
  labs(x="CKD Stage",y="level", 
       title="CKD and Total Cholesterol Components")+theme_few()

chol2.df[,1] <- as.factor(chol2.df[,1])

p <- ggplot(chol2.df, aes(Stages,value,colour=variable)) + geom_line()


#Changes over time
total.time = NULL
for (i in 2:8) {
  data1=data0[data0$SDDSRVYR==i,]
  wght = data1$WTMEC2YR
  NHANES.design2 = svydesign(ids=~SDMVPSU , strata=~SDMVSTRA , 
                        nest = TRUE , 
                        weights = ~wght, 
                        data = data1)
  total.year = svytotal(x=~factor(CKD_stage), design=NHANES.design2, na.rm=TRUE)
  total.year = as.data.frame(total.year)
  total.year$stage = seq(from=0, to=5, by=1)
  total.year$year = rep(i,6)
  total.time = rbind(total.time,total.year)
}

p.time <- ggplot(total.time)
p.time + geom_bar(aes(stage, total, fill=cut), 
                  position="dodge", stat="identity")

total.time[,4] <- as.character(total.time[,4])

#Stage vs. Total by Year
p.time <- ggplot(total.time, aes(x=stage, y=total)) + 
  geom_bar(stat="identity", position="dodge",aes(fill=year)) + theme_few() + 
  labs(x='CKD Stage', y='Total Number of People', title='CKD from 2001-2014')
p.time + scale_fill_manual(values=alpha(c('2'="#780D00",
  '3'="#1F232B", '4'="#E8AA0C", '5'="#23530D"
  , '6'="#121A52", '7'="#8B215F", 
   '8'="#EB6313"), .85))

#Year vs. Total by Stage
p.time <- ggplot(total.time, aes(x=year, y=total)) + 
  geom_bar(stat="identity", position="dodge",aes(fill=stage)) + theme_few() + 
  labs(x='CKD Stage', y='Total Number of People', title='CKD from 2001-2014')
p.time + scale_fill_manual(values=alpha(c('2'="#780D00",
                                          '3'="#1F232B", '4'="#E8AA0C", '5'="#23530D"
                                          , '6'="#121A52", '7'="#8B215F", 
                                          '8'="#EB6313"), .85))

total.time[,3] <- as.factor(total.time[,3])
total.time.2 <- melt(total.time, id=c("year","total","SE"))

p.time <- ggplot(total.time, aes(x=year,y=total, colour=stage)) + geom_line(aes(group=stage))+
  labs(x='year', y='Total Number of People', title='CKD from 2001-2014')
p.time + scale_fill_manual(values=alpha(c('2'="#780D00",
                                          '3'="#1F232B", '4'="#E8AA0C", '5'="#23530D"
                                          , '6'="#121A52", '7'="#8B215F", 
                                          '8'="#EB6313"), .85))




