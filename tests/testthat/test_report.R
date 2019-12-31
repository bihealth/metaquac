context("Report creation")

test_files <- function(files){
  return(system.file("extdata", files, package = "metaquac"))
}

### Test run with Biocrates AbsoluteIDQ p400 HR Kit ################################################
test_that("report works for Biocrates AbsoluteIDQ p400 HR Kit", {

  # Output folder with date and time stamp
  stamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  report_output_dir <- paste0("biocrates_p400_test_01") # _", stamp)

  # Data files
  batches_lc <- list(
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
  )

  batches_fia <- list(
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
  )

  # Study and replicate variables
  study_variables <- list("Group", "Sex",
                          "Group" = list("Sex"))
  replicate_variables <- c("Group", "Sex")
  profiling_variables <- c("Group", "Sex")

  # Create reports
  create_qc_report(report_output_name = "biocrates_qc_lc",
                   report_output_dir = report_output_dir,
                   data_files = batches_lc,
                   title = "Biocrates QC - p400 - LC",
                   # author = "Mathias Kuhring",
                   measurement_type = "LC",
                   profiling_variables = profiling_variables,
                   study_variables = study_variables,
                   replicate_variables = replicate_variables,
                   pool_indicator = "Sample.Identification")

  create_qc_report(report_output_name = "biocrates_qc_fia",
                   report_output_dir = report_output_dir,
                   data_files = batches_fia,
                   title = "Biocrates QC - p400 - FIA",
                   author = "Mathias Kuhring",
                   measurement_type = "FIA",
                   profiling_variables = profiling_variables,
                   study_variables = study_variables,
                   replicate_variables = replicate_variables,
                   pool_indicator = "Sample.Identification")
})


### Test data with Biocrates MxP Quant 500 Kit #####################################################
test_that("report works for Biocrates MxP Quant 500 Kit", {

  # Output folder with date and time stamp
  stamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  report_output_dir <- paste0("biocrates_q500_test_01") # _", stamp)

  # Data files
  batches_lc <- list(
    Batch1 = test_files(c(
      "biocrates_q500_test_01/Batch1_LC.txt"
    )))

  batches_fia <- list(
    Batch1 = test_files(c(
      "biocrates_q500_test_01/Batch1_FIA.txt"
    )))


  # Study and replicate variables
  profiling_variables <- c('Sex')
  study_variables <- list('Sex')
  replicate_variables <- c('Sex')


  # Render main notebook
  create_qc_report(report_output_name = "biocrates_qc_lc",
                   report_output_dir = report_output_dir,
                   data_files = batches_lc,
                   title = "Biocrates QC - Q500 - LC",
                   # author = "Mathias Kuhring",
                   measurement_type = "LC",
                   profiling_variables = profiling_variables,
                   study_variables = study_variables,
                   replicate_variables = replicate_variables,
                   pool_indicator = "Sex",
                   kit = "Biocrates MxP Quant 500 Kit")

  create_qc_report(report_output_name = "biocrates_qc_fia",
                   report_output_dir = report_output_dir,
                   data_files = batches_fia,
                   title = "Biocrates QC - Q500 - FIA",
                   author = "Mathias Kuhring",
                   measurement_type = "FIA",
                   profiling_variables = profiling_variables,
                   study_variables = study_variables,
                   replicate_variables = replicate_variables,
                   pool_indicator = "Sex",
                   kit = "Biocrates MxP Quant 500 Kit")
})
