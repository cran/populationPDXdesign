context("populationPDXdesignApp shiny app")

test_that("shiny app runs", {
  expect_silent(populationPDXdesignApp())
})

test_that("InSingleExperiment returns correct results", {
  PDXn=8; PDXr=3; C_Acc=0.95; Biol_RR=30
  result <- callsInSingleExperiment(PDXn, PDXr, C_Acc, Biol_RR)
  expect_is(result, class = "data.frame")
  expect_equal(dim(result)[1], PDXn)
  expect_gte(min(result$PDXclassification), 0)
  expect_lte(max(result$PDXclassification), 1)
})

test_that("varyingPDXnPDXrBiolRR returns correct results", {
  PDXn_range=c(8,10,12); PDXr_range=c(1,3,5); Biol_RR_range=c(30,40,50); C_Acc=0.95; iterations=50
  result <- varyingPDXnPDXrBiolRR(PDXn_range, PDXr_range, Biol_RR_range, C_Acc, iterations)
  expect_is(result, class = "data.frame")
  expect_equal(dim(result)[1], iterations*length(PDXn_range)*length(PDXr_range)*length(Biol_RR_range))
})

test_that("outcomeMultipleExperiments returns correct results", {
  PDXn=8; PDXr=3; C_Acc=0.95; Biol_RR=30; iterations=50
  result <- outcomeMultipleExperiments(PDXn, PDXr, C_Acc, Biol_RR, iterations)
  expect_is(result, class = "data.frame")
  expect_equal(dim(result)[1], iterations)
  expect_gte(min(result$Percent_lines_R), 0)
  expect_lte(max(result$Results_Percentlines_R), 100)
})

test_that("outcomeInSingleExperiment returns correct results", {
  PDXn=8; PDXr=3; C_Acc=0.95; Biol_RR=30
  df <- callsInSingleExperiment(PDXn, PDXr, C_Acc, Biol_RR)
  result <- outcomeInSingleExperiment(df, PDXn=8, PDXr=3, C_Acc=0.95, Biol_RR=30)
  expect_is(result, class = "numeric")
  expect_gte(result["Percent_lines_R"], 0)
  expect_lte(result["Results_Percentlines_R"], 100)
})

test_that("noMissedCalls returns correct results", {
  PDXn_range=c(8,10); PDXr_range=c(1,3); Biol_RR_range=c(30,40); C_Acc=0.95; iterations=50
  df <- varyingPDXnPDXrBiolRR(PDXn_range, PDXr_range, Biol_RR_range, C_Acc, iterations)
  GoNoGoThreshold=30
  result <- noMissedCalls(df, GoNoGoThreshold)
  expect_is(result, class = "numeric")
  expect_lte(result["number_Hit"], result["numberExperiment"])
  expect_gte(result["MissedCalls"], 0)
  expect_lte(result["MissedCalls"], 100)
})

test_that("noFalseCalls returns correct results", {
  PDXn_range=c(8,10); PDXr_range=c(1,3); Biol_RR_range=c(30,40); C_Acc=0.95; iterations=50
  df <- varyingPDXnPDXrBiolRR(PDXn_range, PDXr_range, Biol_RR_range, C_Acc, iterations)
  GoNoGoThreshold=30
  result <- noFalseCalls(df, GoNoGoThreshold)
  expect_is(result, class = "numeric")
  expect_lte(result["number_Hit"], result["numberExperiment"])
  expect_gte(result["FalsePositiverate"], 0)
  expect_lte(result["FalsePositiverate"], 100)
})

test_that("getMode(c(0,1,1) returns correct results", {
  v=c(0,1,1)
  result <- getMode(v)
  expect_is(result, class = "numeric")
  expect_gte(result, min(v))
  expect_lte(result, max(v))
})

