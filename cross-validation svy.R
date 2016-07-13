# Arguments: data frame, family (default binomial to predict CKD),
# covariates, and covariates that need to be treated as factors

# if binomial is TRUE, the response variable will be CKD, if FALSE, then eGFR


LOO_cross_validate = function(data, binomial = TRUE, covariates,
                              first.year = 2, last.year = 8) {
  if(first.year > last.year | first.year < 1 | last.year > 8) {
    stop("Invalid year parameters")
  }
  if(binomial == FALSE) {
    return(cross_validate_gaussian(data, covariates))
  }
  data_sub = data[data$SDDSRVYR >= first.year & data$SDDSRVYR <= last.year, ]
  weight = data_sub$WTMEC2YR / (last.year - first.year + 1)
  survey_design = svydesign(
    ids = ~SDMVPSU,
    strata = ~SDMVSTRA,
    nest = TRUE,
    weights = ~weight,
    data = data_sub
  )
  # reconstruct equation from response, covariates
  equation = paste("factor(CKD) ~", covariates[1])
  if(length(covariates) > 1) {
    for(a in 2:length(covariates)) {
      equation = paste(equation, "+", covariates[a], sep = " ")
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
    test_set = as.data.frame(matrix(nrow = 1, ncol = length(covariates)))
    for(k in 1:length(covariates)){
      col_num = which(colnames(survey_design$variables) == covariates[k])
      current_row = survey_design$variables[survey_design$variables$iterator == j, ]
      test_set[1, k] = current_row[1, col_num]
    }
    colnames(test_set) = covariates
    # if we're missing necessary data to compare, leave NA
    if(any(is.na(test_set))) {
      errors[j] = NA
    } else {
      prediction = predict(survey_glm, newdata = test_set, type = "response")
      response_true = isTRUE(survey_design$variables[survey_design$variables$iterator == j, ]$CKD == 1)
      if(prediction[1] > 0.75 & response_true | prediction[1] < 0.25 & !response_true) {
        errors[j] = 0
      } else {
        errors[j] = 1
      }
    }
    print(paste(j, " of ", length(iterator)))
  }
  # re-bind errors to original data frame, create new design matrix with weighted errors
  data_with_error = cbind(data_sub, errors)
  colnames(data_with_error)[101] = "errors"
  design_with_error = svydesign(
    ids = ~SDMVPSU,
    strata = ~SDMVSTRA,
    nest = TRUE,
    weights = ~weight,
    data = data_with_error
  )
  weighted_mean_errors = svymean(x = ~errors, design = design_with_error, na.rm = TRUE)
  return(weighted_mean_errors)
}





