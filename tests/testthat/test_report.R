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
    kit = "Biocrates MxP Quant 500 Kit",
    metadata_import = test_files("biocrates_q500_test_01/extra_annotation.txt")
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


### Tests data with Biocrates MxP Quant 500 Kit and lowcon section #############
test_that("lowcon report works for Biocrates MxP Quant 500 Kit LC data", {
  # Create Q500 LC report
  metaquac::create_qc_report(
    report_output_name = "biocrates_qc_lc",
    report_output_dir = "biocrates_q500_test_02",
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
    kit = "Biocrates MxP Quant 500 Kit",
    lowcon_conditions = c("Sex", "Sample.Volume"),
    lowcon_sd_outlier_removal = FALSE,
    lowcon_scatter_x = NULL,
    lowcon_scatter_color = NULL,
    lowcon_scatter_sub_groups = NULL
    # lowcon_export_path = "biocrates_q500_test_01/lowcon/"
  )
})


test_that("lowcon report works for Biocrates MxP Quant 500 Kit FIA data", {
  # Create Q500 FIA report
  metaquac::create_qc_report(
    report_output_name = "biocrates_qc_fia",
    report_output_dir = "biocrates_q500_test_02",
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
    kit = "Biocrates MxP Quant 500 Kit",
    lowcon_conditions = c("Sex", "Sample.Volume"),
    lowcon_sd_outlier_removal = TRUE,
    lowcon_scatter_x = NULL,
    lowcon_scatter_color = NULL,
    lowcon_scatter_sub_groups = NULL
    # lowcon_export_path = "biocrates_q500_test_01/lowcon/"
  )
})


### Tests data with random generic data ########################################
test_that("report works for generic data, batches included", {
  # Create generic report
  metaquac::create_qc_report(
    report_output_name = "generic_qc_lc",
    report_output_dir = "generic_test_01",
    data_files = list(
      c(
        "Concentration [ng/ml]" =
          test_files(c("generic_test_01/random_matrix_conc.tsv")),
        "Area" =
          test_files(c("generic_test_01/random_matrix_area.tsv"))
      )
    ),
    kit = "Generic Data",
    generic_data_types = c(
      CONCENTRATION = "Concentration [ng/ml]",
      AREA = "Area"
    ),
    generic_index_first_compound = 6,
    measurement_type = "LC",
    profiling_variables = c('Group'),
    study_variables = list('Group'),
    replicate_variables = c('Group')
  )
})


test_that("report works for generic data, batches included", {
  # Create generic report
  metaquac::create_qc_report(
    report_output_name = "generic_qc_lc",
    report_output_dir = "generic_test_03",
    data_files = list(
      c(
        "Concentration [ng/ml]" =
          test_files(c("generic_test_03/Batch1_conc.csv")),
        "Area" =
          test_files(c("generic_test_03/Batch1_area.csv")),
        "Status" =
          test_files(c("generic_test_03/Batch1_status.csv"))
      )
    ),
    kit = "Generic Data",
    generic_data_types = c(
      CONCENTRATION = "Concentration [ng/ml]",
      AREA = "Area",
      STATUS = "Status"
    ),
    generic_index_first_compound = 5,
    profiling_variables = c('Sex'),
    study_variables = list('Sex'),
    replicate_variables = c('Sex')
  )
})


# # Separated batches not yet supported for generic data
# test_that("report works for generic data, batches separately", {
#   # Create generic report
#   metaquac::create_qc_report(
#     report_output_name = "generic_qc_lc",
#     report_output_dir = "generic_test_02",
#     data_files = list(
#       Batch1 = c(
#         Concentration =
#           test_files(c("generic_test_02/random_matrix_conc_batch1.tsv")),
#         Area =
#           test_files(c("generic_test_02/random_matrix_area_batch1.tsv"))
#       ),
#       Batch2 = c(
#         Concentration =
#           test_files(c("generic_test_02/random_matrix_conc_batch2.tsv")),
#         Area =
#           test_files(c("generic_test_02/random_matrix_area_batch2.tsv"))
#       )
#     ),
#     kit = "Generic Data",
#     generic_data_types = c(
#       CONCENTRATION = "Concentration [ng/ml]",
#       AREA = "Area"
#     ),
#     generic_index_first_compound = 5,
#     measurement_type = "LC",
#     profiling_variables = c('Group'),
#     study_variables = list('Group'),
#     replicate_variables = c('Group')
#   )
# })
