# Arguments: data frame, family (default binomial to predict CKD),
# covariates

# if binomial is TRUE, the response variable will be CKD, if FALSE, then eGFR

LOO_cross_validate = function(data, binomial = TRUE, covariates) {
  if(binomial == FALSE) {
    return(cross_validate_gaussian(data, covariates))
  }
  data_sub = data[data$SDDSRVYR >= 7 & data$SDDSRVYR <= 8, ]
  weight = data_sub$WTMEC2YR / 2
  survey_design = svydesign(
    ids = ~SDMVPSU,
    strata = ~SDMVSTRA,
    nest = TRUE,
    weights = ~weight,
    data = data_sub
  )
  # reconstruct equation from response, covariates
  equation = paste("CKD ~", covariates[1])
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
      current_row = survey_design$variables[survey_design$variables$iterator == j, ]
      test_set[1, k] = current_row[1, col_num]
    }
    colnames(test_set) = covariates
    # if we're missing necessary data to compare, leave NA
    if(any(is.na(test_set))) {
      errors[j] = NA
    } else {
      prediction = predict(survey_glm, newdata = as.data.frame(test_set), type = "response")
      response_true = isTRUE(survey_design$variables[survey_design$variables$iterator == j, ]$CKD == 1)
      if(prediction[1] > 0.75 & response_true | prediction[1] < 0.25 & !response_true) {
        errors[j] = 0
      } else {
        errors[j] = 1
      }
    }
    print(j)
  }
  # re-bind errors to original data frame, create new design matrix with weighted errors
  data_with_error = cbind(data_sub, errors)
  colnames(data_sub)[length(colnames(data_sub))] = "weighted errors"
  weighted_errors = svydesign(
    ids = ~SDMVPSU,
    strata = ~SDMVSTRA,
    nest = TRUE,
    weights = ~weight,
    data = data_with_error
  )
  return(weighted_errors$variables$`weighted errors`)
}

k_fold_cross_validate = function(data, binomial = TRUE, covariates, folds = 2) {
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
  equation = paste("CKD ~", covariates[1])
  if(length(covariates) > 1) {
    for(i in 2:length(covariates)) {
      equation = paste(equation, "+", covariates[i], sep = " ")
    }
  }
  iterator = seq(from = 1, to = nrow(data_sub), by = 1)
  survey_design$variables = cbind(iterator, survey_design$variables)
  # randomly partition data set into folds
  for(i in 1:nrow(data_sub)) {
    survey_design$variables$iterator[i] = sample(1:folds, 1, replace = T)
  }
  print
  group_markers[length(group_markers)] = num_obs
  for(i in 1:length(group_markers)){
    training_set = subset()
    survey_glm = svyglm(
      equation,
      design = training_set,
      
    )
    # create GLM all but group i
    # use predict to find error rate on group i
  }
}







