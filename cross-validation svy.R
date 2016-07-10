# Arguments: data frame, family (default gaussian),
# equation (Response ~ covariate1 + covariate2...)

cross_validate = function(data, binomial = TRUE, response_var, covariates) {
  if(binomial == FALSE) {
    return(cross_validate_gaussian(data, response_var, covariates))
  }
  data_sub = data[data$SDDSRVYR >= 2 & data$SDDSRVYR <= 8, ]
  weight = data_sub$WTMEC2YR / 7
  survey_design = svydesign(
    ids = ~SDMVPSU,
    strata = ~SDMVSTRA,
    nest = TRUE,
    weights = ~weight,
    data = data_sub
  )
  # reconstruct equation from response, covariates
  equation = paste(response_var, "~", covariates[1])
  if(length(covariates) > 1) {
    for(i in 2:length(covariates)) {
      equation = paste(equation, "+", covariates[i], sep = " ")
    }
  }
  iterator = seq(from = 1, to = nrow(data_sub), by = 1)
  survey_design$variables = cbind(iterator, survey_design$variables)
  errors = numeric(length(iterator))
  for(j in 1:length(iterator)){
    # leave out the jth row, indicated by iterator
    training_set = subset(survey_design, survey_design$variables$iterator != j)
    survey_glm = svyglm(
      equation,
      design = training_set, 
      family = quasibinomial
    )
    # for each covariate, extract the value from the test set
    test_set = matrix(nrow = 1, ncol = length(covariates))
    for(k in 1:length(covariates)){
      current_var = covariates[k]
      col_num = which(colnames(survey_design$variables) == current_var)
      print(col_num)
      # bugged
      current_row = survey_design$variables[survey_design$variables$iterator == j, ]
      print(current_row)
      test_set[1, k] = current_row[col_num]
    }
    colnames(test_set) = covariates
    print(test_set)
    prediction = predict(survey_glm, newdata = test_set)
    print(prediction)
    # save squared errors
    errors[j] = (prediction - survey_design[survey_design$variables$iterator == j, ]$response_var) ^ 2
    print(errors[j])
  }
}

cross_validate_gaussian = function(data, response_var, covariates) {
  data_sub = data[data$SDDSRVYR >= 2 & data$SDDSRVYR <= 8, ]
  weight = data_sub$WTMEC2YR / 7
  survey_design = svydesign(
    ids = ~SDMVPSU,
    strata = ~SDMVSTRA,
    nest = TRUE,
    weights = ~weight,
    data = data_sub
  )
  # reconstruct equation from response, covariates
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
      equation,
      design = current.design, 
      family = gaussian
    )
    ####### ####
  }
}

