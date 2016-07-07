
install.packages("survey")
source("subset_svydata.R")
library("survey")

#####################
## Task 4 (NHANES) ##
#####################

# (SDDSRVYR = {1 = 1999-2000, 2 = 2001-2002,
# ... 8 = 2013-2014})

data0 = read.csv("NHANES.csv")

# standardize by age
# load("AgePop2000.RData")
# AgePop = c(sum(AgePop2000[1:10,2]),sum(AgePop2000[11:14,2]),
#            sum(AgePop2000[15:20,2]),sum(AgePop2000[21:24,2]))
# AgeWeight = AgePop/sum(AgePop)
# 
# data1 <- data0[data0$SDDSRVYR >= 2 & data0$SDDSRVYR <= 8,]
# data1$age_group = ifelse(data1$age_years<=19, "<=19",NA)
# data1$age_group = ifelse(data1$age_years>=20 & data1$age_years<=39, "20-39",data1$age_group)
# data1$age_group = ifelse(data1$age_years>=40 & data1$age_years<=59, "40-59",data1$age_group)
# data1$age_group = ifelse(data1$age_years>=60, ">=60",data1$age_group)
# data1$age_group = factor(data1$age_group,levels=c("<=19","20-39","40-59",">=60"))
# 
# WTMEC_1 <- data1$WTMEC2YR/7
# 
# NHANES.MEC.design1 <- svydesign(
#   ids = ~SDMVPSU ,         
#   strata = ~SDMVSTRA ,   
#   nest = TRUE ,
#   weights = ~WTMEC_1,
#   data = data1
# )
# 
# age_names = c("<=19","20-39","40-59",">=60")
# 
# CKD.diabetes.prev.list = NULL
# for(i in 1:length(age_names)){
#   CKD.diabetes.prev = svyby(
#     formula = ~diabetes,
#     by = ~(CKD_stage == 5),
#     design = subset(NHANES.MEC.design1,age_group==age_names[i]),
#     FUN = svymean,
#     na.rm = TRUE
#   )
#   CKD.diabetes.prev.list = cbind(CKD.diabetes.prev.list,
#                                  CKD.diabetes.prev[,2])
# }
# 
# colnames(CKD.diabetes.prev.list) = age_names
# rownames(CKD.diabetes.prev.list) = c("Non-CKD stg5","CKD stg5")
# 
# # Age-Adjusted prevalence of CKD stg5 among those without/with diabetes
# c(sum(CKD.diabetes.prev.list[1,] * AgeWeight), sum(CKD.diabetes.prev.list[2,] * AgeWeight))


# construct initial data frame - because we're adding 2
# columns, cannot initialize as null
# 6 rows because CKD_stage can take on 6 levels
stat.CKDstg.year = matrix(NA, nrow = 2, ncol = 1)
test = c("Total_chol", "asdf")

# for each 2-year data set:
for(i in 1:length(unique(data0$SDDSRVYR))){
  # create survey design for each year
  temp.svd = subset.data.MEC(data0, first.year = i, last.year = i)
  # compute survey statistic
  temp.stat <- svyby(
    formula = ~Total_chol,
    by = ~factor(CKD_stage),
    design = temp.svd,
    na.rm = TRUE,
    FUN = svymean
  )
  # bind to existing set:
  stat.CKDstg.year = cbind(stat.CKDstg.year, temp.stat[, 2:3])
}

# delete first column
stat.CKDstg.year = stat.CKDstg.year[, 2:ncol(stat.CKDstg.year)]

# add row- and col-names
rownames(stat.CKDstg.year) = c("CKD 0", "CKD 1","CKD 2", "CKD 3", 
                               "CKD 4", "CKD 5")

colnames(stat.CKDstg.year) = c("99-00", "s.e.",
                               "01-02", "s.e.",
                               "03-04", "s.e.",
                               "05-06", "s.e.",
                               "07-08", "s.e.",
                               "09-10", "s.e.",
                               "11-12", "s.e.",
                               "13-14", "s.e.")

# transpose matrix may lend itself to analyzing data more easily
stat.transpose = t(stat.CKDstg.year)










