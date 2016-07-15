

k_fold_cross_validate = function(data, binomial = TRUE, covariates,
                                 num_folds = 2, first.year = 2,
                                 last.year = 8) {
  if(!binomial) {
    return(k_fold_cross_validate_gaussian(data, covariates,
                                          num_folds = num_folds, 
                                          first.yr = first.year,
                                          last.yr = last.year))
  }
  data_sub = data[data$SDDSRVYR >= first.year & 
                    data$SDDSRVYR <= last.year, ]
  weight = data_sub$WTMEC2YR / (last.year - first.year + 1)
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
  # randomize rows
  survey_design$variables = survey_design$variables[sample(nrow(survey_design$variables)), ] 
  # cut into folds
  folds = cut(seq(1, nrow(survey_design$variables)), breaks = num_folds, labels = FALSE)
  survey_design$variables = cbind(survey_design$variables, folds)
  grouped_errors = vector("list", num_folds)
  errors = numeric(0)
  for(j in 1:num_folds){
    print(paste("working on fold", j, "of", num_folds, "total"))
    
    test_data = subset(survey_design, survey_design$variables$folds == j)
    training_data = subset(survey_design, survey_design$variables$folds != j)
    survey_glm = svyglm(
      equation,
      design = training_data,
      family = quasibinomial
    )
    errors = matrix(NA, nrow = nrow(test_data$variables), ncol = 2)
    for(k in 1:nrow(test_data$variables)) {
      predict_set = as.data.frame(matrix(nrow = 1, ncol = length(covariates)))
      # for each covariate, extract the value from the test set
      for(l in 1:length(covariates)){
        col_num = which(colnames(test_data$variables) == covariates[l])
        current_row = test_data$variables[k, ]
        predict_set[1, l] = current_row[1, col_num]
      }
      colnames(predict_set) = covariates
      # if we're missing necessary data to compare, leave NA
      errors[k, 1] = test_data$variables[k, ]$SEQN
      if(all(!is.na(predict_set)) & all(!is.na(test_data$variables[k, ]$CKD))) {
        
        prediction = predict(survey_glm, newdata = predict_set, type = "response")
        response_true = isTRUE(test_data$variables[k, ]$CKD == 1)
        
        if(prediction[1] > 0.75 & response_true | prediction[1] < 0.25 & !response_true) {
          errors[k, 2] = 0
        } else {
          errors[k, 2] = 1
        }
      }
    }
    grouped_errors[[j]] = errors
  }
  return(grouped_errors)
}


k_fold_cross_validate_gaussian = function(data, covariates,
                                 num_folds, first.yr,
                                 last.yr) {
  data_sub = data[data$SDDSRVYR >= first.yr & 
                    data$SDDSRVYR <= last.yr, ]
  weight = data_sub$WTMEC2YR / (last.yr - first.yr + 1)
  survey_design = svydesign(
    ids = ~SDMVPSU,
    strata = ~SDMVSTRA,
    nest = TRUE,
    weights = ~weight,
    data = data_sub
  )
  # reconstruct equation from response, covariates
  equation = paste("CKD_epi_eGFR ~", covariates[1])
  if(length(covariates) > 1) {
    for(i in 2:length(covariates)) {
      equation = paste(equation, "+", covariates[i], sep = " ")
    }
  }
  # randomize rows
  survey_design$variables = survey_design$variables[sample(nrow(survey_design$variables)), ] 
  # cut into folds
  folds = cut(seq(1, nrow(survey_design$variables)), breaks = num_folds, labels = FALSE)
  survey_design$variables = cbind(survey_design$variables, folds)
  grouped_errors = vector("list", num_folds)
  errors = numeric(0)
  for(j in 1:num_folds){
    print(paste("working on fold", j, "of", num_folds, "total"))
    
    test_data = subset(survey_design, survey_design$variables$folds == j)
    training_data = subset(survey_design, survey_design$variables$folds != j)
    survey_glm = svyglm(
      equation,
      design = training_data,
      family = gaussian
    )
    errors = matrix(NA, nrow = nrow(test_data$variables), ncol = 2)
    for(k in 1:nrow(test_data$variables)) {
      predict_set = as.data.frame(matrix(nrow = 1, ncol = length(covariates)))
      # for each covariate, extract the value from the test set
      for(l in 1:length(covariates)){
        col_num = which(colnames(test_data$variables) == covariates[l])
        current_row = test_data$variables[k, ]
        predict_set[1, l] = current_row[1, col_num]
      }
      colnames(predict_set) = covariates
      errors[k, 1] = test_data$variables[k, ]$SEQN
      # if we're missing necessary data to compare, record NA error
      if(all(!is.na(predict_set)) & all(!is.na(test_data$variables[k, ]$CKD))) {
        prediction = predict(survey_glm, newdata = predict_set, type = "response")
        response_true = test_data$variables[k, ]$CKD_epi_eGFR
        errors[k, 2] = (prediction - response_true)^2
      }
    }
    grouped_errors[[j]] = errors
  }
  return(grouped_errors)
}  