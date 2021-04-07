context("Status processing")


testthat::test_that("biocrates statuses are discarded (p400_test_01_LC)", {
  # Load unprocessed dataset
  load(test_files("data_p400_test_01_LC_org.RData", "testdata/datasets"))

  # Setup parameters
  statuses_to_keep <- c("Valid", "Semi Quant.")
  data_types <- c(
    CONCENTRATION = "Concentration [ng/ml]",
    AREA = "Analyte Peak Area [area]",
    INTENSITY = "Analyte Intensity [cps]",
    ISTD_AREA = "Internal Std. Peak Area [area]",
    ISTD_INTENSITY = "Internal Std. Intensity [cps]"
  )

  # Run status processing
  discarded <- discard_unreliable_measurements(
    data = biocrates,
    keep = statuses_to_keep,
    targets = data_types
  )

  # Check that headers didn't change
  testthat::expect_equal(names(biocrates), names(discarded))
  # Check number of missing concentration values
  # Before processing
  na_expected <- 2565
  na_actually <- sum(is.na(biocrates$`Concentration [ng/ml]`))
  testthat::expect_equal(na_actually, na_expected)
  # After processing
  na_expected <- 3069
  na_actually <- sum(is.na(discarded$`Concentration [ng/ml]`))
  testthat::expect_equal(na_actually, na_expected)
})


testthat::test_that("generic statuses are discarded (generric_test_03", {
  # Load unprocessed dataset
  load(test_files("data_generic_test_03_org.RData", "testdata/datasets"))

  # Setup parameters
  statuses_to_keep <- c("Valid", "Semi Quant.")
  data_types <- c(
    CONCENTRATION = "Concentration [ng/ml]",
    AREA = "Area"
  )

  # Run status processing
  discarded <- discard_unreliable_measurements(
    data = biocrates,
    keep = statuses_to_keep,
    targets = data_types
  )

  # Check that headers didn't change
  testthat::expect_equal(names(biocrates), names(discarded))
  # Check number of missing concentration values
  # Before processing
  na_expected <- 0
  na_actually <- sum(is.na(biocrates$`Concentration [ng/ml]`))
  testthat::expect_equal(na_actually, na_expected)
  # After processing
  na_expected <- 66
  na_actually <- sum(is.na(discarded$`Concentration [ng/ml]`))
  testthat::expect_equal(na_actually, na_expected)
})
