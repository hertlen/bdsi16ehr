packages = packageStatus()
if ("survey" %in% packages$inst == FALSE) {
  install.packages("survey")
}

source("subset_svydata.R")
library("survey")

#####################
## Task 4 (NHANES) ##
#####################

# (SDDSRVYR = {1 = 1999-2000, 2 = 2001-2002,
# ... 8 = 2013-2014})

data0 = read.csv("NHANES.csv")

temp.svd = subset.data.MEC(data0, first.year = 1, last.year = 8)
variables = c("hypertension", "BMI", "diabetes", "sex")
cnames = c("")
for(i in 1:length(variables)){
  cnames = append(cnames, variables[i])
  cnames = append(cnames, "se")
}
cnames = cnames[2:length(cnames)]
storage = matrix(NA, ncol = 2, nrow = 6)
for(i in 1:length(variables)){
  temp.stat <- svyby(
    formula = ~eval(as.name(variables[i])),
    by = ~factor(CKD_stage),
    design = temp.svd,
    na.rm = TRUE,
    FUN = svymean
  )
  storage = cbind(storage, temp.stat[, 2:ncol(temp.stat)])
}
storage
storage = storage[, 3:ncol(storage)]
colnames(storage) = cnames
storage






