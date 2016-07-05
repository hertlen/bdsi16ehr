# returns a surveydesign object based on given data, parameters, using MEC (exam)
# weights

subset.data.MEC = function(surveydata, first.year = 1, last.year = 8) {
  data_sub = surveydata[surveydata$SDDSRVYR >= first.year & data0$SDDSRVYR <= last.year, ]
  weights = data_sub$WTMEC2YR / (first.year - last.year + 1)
  surveydesign.subset <- svydesign(
    ids = ~SDMVPSU,
    strata = ~SDMVSTRA,
    nest = TRUE,
    weights = ~weights,
    data = data_sub
  )
  return(surveydesign.subset)
}