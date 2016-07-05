packages = packageStatus()
if ("survey" %in% packages$inst == FALSE) {
  install.packages("survey")
}

library("survey")

# Subsets of data - full data set (SDDSRVYR = {1 = 1999-2000, 2 = 2001-2002, 3 = 2003-2004,
# 4 = 2005-2006, 5 = 2007-2008,... 8 = 2013-2014})

data0 = read.csv("NHANES.csv")

data1 <- data0[data0$SDDSRVYR == 2, ]
WTMEC_1 <- data1$WTMEC2YR

NHANES.0102 <- svydesign(
  ids = ~SDMVPSU,         
  strata = ~SDMVSTRA,   
  nest = TRUE,
  weights = ~WTMEC_1,
  data = data1
)

avg.eGFR.by.CKDstage <- svyby(
  formula = ~CKD_epi_eGFR,
  by = ~factor(CKD_stage),
  design = NHANES.0108,
  na.rm = TRUE,
  FUN = svymean
)


# construct initial data set
eGFR.CKDstg.year = matrix(NA, nrow = 6, ncol = 1)

# for each 2-year data set:
for(i in 1:length(unique(data0$SDDSRVYR))){
  data.temp <- data0[data0$SDDSRVYR == i, ]
  WTMEC.temp <- data.temp$WTMEC2YR
  # create temporary survey design for each year in loop
  temp.svd <- svydesign(
    ids = ~SDMVPSU,    
    strata = ~SDMVSTRA,
    nest = TRUE,
    weights = ~WTMEC.temp,
    data = data.temp
  )
  # compute temporary survey statistic
  temp.eGFR <- svyby(
    formula = ~CKD_epi_eGFR,
    by = ~factor(CKD_stage),
    design = temp.svd,
    na.rm = TRUE,
    FUN = svymean
  )
  # bind to existing set
  eGFR.CKDstg.year = cbind(eGFR.CKDstg.year, temp.eGFR[, 2:3])
}

# eGFR.CKDstg.year was coerced to type 'data.frame'

# remove first row (NAs)
eGFR.CKDstg.year = eGFR.CKDstg.year[, 2:ncol(eGFR.CKDstg.year)]

# add row- and col-names
rownames(eGFR.CKDstg.year) = c("CKD 0", "CKD 1","CKD 2", "CKD 3", 
                               "CKD 4", "CKD 5")
colnames(eGFR.CKDstg.year) = c("eGFR - 99-00", "s.e.", 
                               "eGFR - 01-02", "s.e.",
                               "eGFR - 03-04", "s.e.",
                               "eGFR - 05-06", "s.e.",
                               "eGFR - 07-08", "s.e.",
                               "eGFR - 09-10", "s.e.",
                               "eGFR - 11-12", "s.e.",
                               "eGFR - 13-14", "s.e.")

# transpose matrix may lend itself to analyzing data more easily
eGFR.transpose = t(eGFR.CKDstg.year)
plot(as.numeric(eGFR.transpose[1,]) ~ as.factor(colnames(eGFR.transpose)), type = 'p')


# 2001-2014 : MEC weights
# second condition in line 11 subset is unnecessary now but could be worthwhile
# to preserve subset length when additional year data is added
data0114 = data0[data0$SDDSRVYR >= 2 & data0$SDDSRVYR <= 8, ]
WTMEC14 = data0114$WTMEC2YR / 7

NHANES.0114 <- svydesign(
  ids = ~SDMVPSU,         
  strata = ~SDMVSTRA,   
  nest = TRUE,
  weights = ~WTMEC14,
  data = data0114
)

avg.eGFR.by.CKDstage2 <- svyby(
  formula = ~CKD_epi_eGFR,
  by = ~factor(CKD_stage),
  design = NHANES.0114,
  na.rm = TRUE,
  FUN = svymean
)


# Task 1 #

# Facility level data visualization and facility level association, 
# e.g., what is the relationship between mortality (SMR) and other quality
# measures and facility characteristics?



# Task 4 #

# Association study, e.g., what is the relationship between CKD and
# potential risk factors?


### Potential risk factors for GLM: ###

# Triglycerides (correlated with diabetes, obesity) ("Triglycerides")
# 4 factor levels: < 150 mg/dL (normal), 150-199 (borderline high), 200-499 (high),
# 500+ (very high)

# Total Cholesterol ("Total_chol")
# 4 factor levels: < 100 mg/dL (optimal), 100-129 (near/above optimal), 130-159
# (Borderline high), 160-189 mg/dL (high)

# Hypertension ("hypertension")
# 1=Unware, 2=Aware not treated, 3=Aware treated uncontrolled, 4=Aware treated controlled

# Known correlated covariates: 

# UACR/Urine Albumin-to-Creatinine Ratio, ("UACR")
# Higher = worse, correlated with higher stages of CKD

# eGFR - estimated glomerular filtration rate
# lower = worse, correlated with higher stages of CKD

### Methods: ###

# ANOVA?

# Try certain combinations of GLMs with measures that are not directly correlated
# compare AIC/BIC score between GLMs to avoid overfitting
# cross-validate between GLMs, try different link functions

# compare CKD rates between races/ethnicities and control for BMI, UACR, eGFR?

# Causal/counterfactual inference




