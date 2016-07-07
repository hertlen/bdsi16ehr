# Arguments: data frame, family (default gaussian),
# equation (Response ~ covariate1 + covariate2...)


cross-validation = function(data, family = gaussian(), equation) {
  data_sub = data[data$SDDSRVYR >= 2 & data$SDDSRVYR <= 8, ]
  weight = data_sub$WTMEC2YR / 7
  survey_design = svydesign(
    ids = ~SDMVPSU,
    strata = ~SDMVSTRA,
    nest = TRUE,
    weights = ~weight,
    data = data_sub
  )
  iterator = seq(from = 1, to = nrow(data_sub), by = 1)
  survey_design$variables = cbind(iterator, survey_design$variables)
  for(i in 1:length(iterator)){
    # Check and make sure this works correctly:
    # leave out the ith row, indicated by iterator
    current.design = subset(survey_design, survey_design$variables$iterator != i)
    survey_glm = svyglm(
      as.formula(equation),
      design = current.design, 
      family = type.fam
    )
    
  }
}


