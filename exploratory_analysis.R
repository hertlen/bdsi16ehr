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
  cnames = append(cnames, paste("se", variables[i]))
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

CKD_stages = matrix(c(0:5), nrow = 6, ncol = 1)
storage = cbind(CKD_stages, storage)

install.packages("ggplot2")
library("ggplot2")

par(mfrow = c(2,2))

ggplot(storage, aes(x = CKD_stages, y = hypertension)) +
  geom_errorbar(aes(ymin = hypertension - 1.98*`se hypertension`, 
                    ymax = hypertension + 1.98*`se hypertension`)) +
  geom_point()

ggplot(storage, aes(x = CKD_stages, y = BMI)) +
  geom_errorbar(aes(ymin = BMI - 1.98*`se BMI`, 
                    ymax = BMI + 1.98*`se BMI`)) +
  geom_point()

ggplot(storage, aes(x = CKD_stages, y = diabetes)) +
  geom_errorbar(aes(ymin = diabetes - 1.98*`se diabetes`, 
                    ymax = diabetes + 1.98*`se diabetes`)) +
  geom_point()

