context("Report creation")


# Get paths of files included in the package
test_files <- function(files){
  return(system.file("extdata", files, package = "metaquac"))
}


### Tests run with Biocrates AbsoluteIDQ p400 HR Kit ###########################
test_that("report works for Biocrates AbsoluteIDQ p400 HR Kit LC data", {
  # Create p400 LC report
  create_qc_report(
    report_output_name = "biocrates_qc_lc",
    report_output_dir = "biocrates_p400_test_01",
    data_files = list(
      Batch1 = test_files(c(
        "biocrates_p400_test_01/Batch1_LC1.txt",
        "biocrates_p400_test_01/Batch1_LC2.txt"
      )),
      Batch2 = test_files(c(
        "biocrates_p400_test_01/Batch2_LC1.txt",
        "biocrates_p400_test_01/Batch2_LC2.txt"
      )),
      Batch3 = test_files(c(
        "biocrates_p400_test_01/Batch3_LC1.txt",
        "biocrates_p400_test_01/Batch3_LC2.txt"
      ))
    ),
    title = "Biocrates QC - p400 - LC",
    # author = "Mathias Kuhring",
    measurement_type = "LC",
    profiling_variables = c("Group", "Condition"),
    study_variables = list("Group", "Condition", "Group" = list("Condition")),
    replicate_variables = c("Group", "Condition"),
    pool_indicator = "Sample.Identification"
  )
})


test_that("report works for Biocrates AbsoluteIDQ p400 HR Kit FIA data", {
  # Create p400 FIA report
  create_qc_report(
    report_output_name = "biocrates_qc_fia",
    report_output_dir = "biocrates_p400_test_01",
    data_files = list(
      Batch1 = test_files(c(
        "biocrates_p400_test_01/Batch1_FIA1.txt",
        "biocrates_p400_test_01/Batch1_FIA2.txt"
      )),
      Batch2 = test_files(c(
        "biocrates_p400_test_01/Batch2_FIA1.txt",
        "biocrates_p400_test_01/Batch2_FIA2.txt"
      )),
      Batch3 = test_files(c(
        "biocrates_p400_test_01/Batch3_FIA1.txt",
        "biocrates_p400_test_01/Batch3_FIA2.txt"
      ))
    ),
    title = "Biocrates QC - p400 - FIA",
    author = "Mathias Kuhring",
    measurement_type = "FIA",
    profiling_variables = c("Group", "Condition"),
    study_variables = list(
      "Group", "Condition", "Group" = list("Condition")
    ),
    replicate_variables = c("Group", "Condition"),
    pool_indicator = "Sample.Identification"
  )
})


### Tests data with Biocrates MxP Quant 500 Kit ################################
test_that("report works for Biocrates MxP Quant 500 Kit LC data", {
  # Create Q500 LC report
  create_qc_report(
    report_output_name = "biocrates_qc_lc",
    report_output_dir = "biocrates_q500_test_01",
    data_files = list(
      Batch1 = test_files(c(
        "biocrates_q500_test_01/Batch1_LC.txt"
      ))
    ),
    title = "Biocrates QC - Q500 - LC",
    # author = "Mathias Kuhring",
    measurement_type = "LC",
    profiling_variables = c('Sex'),
    study_variables = list('Sex'),
    replicate_variables = c('Sex'),
    pool_indicator = "Sex",
    kit = "Biocrates MxP Quant 500 Kit"
  )
})


test_that("report works for Biocrates MxP Quant 500 Kit FIA data", {
  # Create Q500 FIA report
  create_qc_report(
    report_output_name = "biocrates_qc_fia",
    report_output_dir = "biocrates_q500_test_01",
    data_files = list(
      Batch1 = test_files(c(
        "biocrates_q500_test_01/Batch1_FIA.txt"
      ))
    ),
    title = "Biocrates QC - Q500 - FIA",
    author = "Mathias Kuhring",
    measurement_type = "FIA",
    profiling_variables = c('Sex'),
    study_variables = list('Sex'),
    replicate_variables = c('Sex'),
    pool_indicator = "Sex",
    kit = "Biocrates MxP Quant 500 Kit"
  )
})
