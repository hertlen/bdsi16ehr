install.packages("survey")
install.packages("ggplot2")
library("ggplot2")
library("survey")
source("subset_svydata.R")


#####################
## Task 4 (NHANES) ##
#####################


data0 = read.csv("NHANES.csv")

temp.svd = subset.data.MEC(data0, first.year = 2, last.year = 8)
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

storage = storage[, 3:ncol(storage)]
colnames(storage) = cnames

CKD_stages = matrix(c(0:5), nrow = 6, ncol = 1)
storage = cbind(CKD_stages, storage)

model = svyglm(CKD_epi_eGFR ~ Total_chol, design = temp.svd)

# Total_chol and diabetes are highly correlated 
# (high coefficient) with years - atp_chol less so

model2 = svyglm(Total_chol ~ age_years, design = temp.svd)
model3 = svyglm(atp_chol ~ age_years, design = temp.svd)
model4 = svyglm(diabetes ~ age_years, design = temp.svd, family = binomial(link = "logit"))
diab.predict = predict(model4, newdata = data.frame(age_years = 2:100))
lines(diab.predict)


prediction = predict(model, newdata = data.frame(age_years = 2:100))

data_sub = data0[data0$SDDSRVYR >= 2 & data0$SDDSRVYR <= 8, ]
weight = data_sub$WTMEC2YR / 7
temp.svd = svydesign(
  ids = ~SDMVPSU,
  strata = ~SDMVSTRA,
  nest = TRUE,
  weights = ~weight,
  data = data_sub
)
svyplot(CKD_epi_eGFR~age_years, design = temp.svd, pch = 19)
lines(prediction)

# graphs

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

