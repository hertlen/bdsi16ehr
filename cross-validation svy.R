# Arguments: data frame, family (default gaussian),
# equation (Response ~ covariate1 + covariate2...)

cross_validate = function(data, type.fam = gaussian(), response_var, covariates) {
  data_sub = data[data$SDDSRVYR >= 2 & data$SDDSRVYR <= 8, ]
  weight = data_sub$WTMEC2YR / 7
  survey_design = svydesign(
    ids = ~SDMVPSU,
    strata = ~SDMVSTRA,
    nest = TRUE,
    weights = ~weight,
    data = data_sub
  )
  # build equation from response, covariates
  equation = paste(response_var, "~", covariates[1])
  if(length(covariates) > 1) {
    for(i in 2:length(covariates)) {
      equation = paste(equation, "+", covariates[i], sep = " ")
    }
  }
  iterator = seq(from = 1, to = nrow(data_sub), by = 1)
  survey_design$variables = cbind(iterator, survey_design$variables)
  errors = numeric(length(iterator))
  for(i in 1:length(iterator)){
    # leave out the ith row, indicated by iterator
    current.design = subset(survey_design, survey_design$variables$iterator != i)
    survey_glm = svyglm(
      # reshape arguments into GLM equation
      as.formula(equation),
      design = current.design, 
      family = type.fam
    )
    print(survey_design[survey_design$variables$iterator == i, ])
    # probably need to have covariates passed/parsed as individual arguments
    # prediction = predict(survey_glm, newdata = survey_design[survey_design$variables$iterator == i, ]$)
    # save squared errors
    # errors[i] = (prediction - survey_design[survey_design$variables$iterator == i, ]$response_var) ^ 2
  }
}


