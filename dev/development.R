# Setup for development, i.e. to execute code within notebook independently from package.
# Author: Mathias Kuhring


# Load packages and code
library(dplyr)
library(ggfortify)
library(ggplot2)
library(assertthat)
library(rlang)
library(tidyr)

sapply(list.files("R", full.names = TRUE), source)


### p400 test ######################################################################################

# Output folder with date and time stamp
stamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
report_output_dir <- paste0("biocrates_p400_test_01_", stamp)

# Data files
batches_lc_normqc2 <- list(
  Batch1 = c(
    "inst/extdata/biocrates_p400_test_01/Batch1_LC1.txt",
    "inst/extdata/biocrates_p400_test_01/Batch1_LC2.txt"
  ),
  Batch2 = c(
    "inst/extdata/biocrates_p400_test_01/Batch2_LC1.txt",
    "inst/extdata/biocrates_p400_test_01/Batch2_LC2.txt"
  ),
  Batch3 = c(
    "inst/extdata/biocrates_p400_test_01/Batch3_LC1.txt",
    "inst/extdata/biocrates_p400_test_01/Batch3_LC2.txt"
  )
)

batches_fia_normqc2 <- list(
  Batch1 = c(
    "inst/extdata/biocrates_p400_test_01/Batch1_FIA1.txt",
    "inst/extdata/biocrates_p400_test_01/Batch1_FIA2.txt"
  ),
  Batch2 = c(
    "inst/extdata/biocrates_p400_test_01/Batch2_FIA1.txt",
    "inst/extdata/biocrates_p400_test_01/Batch2_FIA2.txt"
  ),
  Batch3 = c(
    "inst/extdata/biocrates_p400_test_01/Batch3_FIA1.txt",
    "inst/extdata/biocrates_p400_test_01/Batch3_FIA2.txt"
  )
)

# Study and replicate variables
study_variables <- list("Group", "Sex",
                        "Group" = list("Sex"))
replicate_variables <- c("Group")
profiling_variables <- c("Group", "Sex")

# Parameter LC
params = list(
  data_files = lapply(batches_lc_normqc2, function(x){unname(R.utils::getAbsolutePath(x))}),
  export_name = "biocrates_qc_lc_normqc2",
  export_dir = R.utils::getAbsolutePath(report_output_dir),
  title = "Biocrates QC - p400 - LC",
  author = "Mathias Kuhring",
  kit = "Biocrates AbsoluteIDQ p400 HR Kit",
  measurement_type = "LC",
  pool_indicator = "Sample.Identification",
  sample_filter = NULL,
  profiling_variables = profiling_variables,
  study_variables = study_variables,
  replicate_variables = replicate_variables,
  zero2na = TRUE,
  preproc_keep_status = "Valid",
  filter_compound_qc_max_mv_ratio = 0.2,
  filter_compound_qc_max_rsd = 15,
  filter_compound_bs_max_mv_ratio = 0.25,
  filter_sample_max_mv_ratio = 0.2
)
# Now execute child notebooks: nbc_setup > nbc_import > ...

# Parameter FIA
params = list(
  data_files = lapply(batches_fia_normqc2, function(x){unname(R.utils::getAbsolutePath(x))}),
  export_name = "biocrates_qc_fia_normqc2",
  export_dir = R.utils::getAbsolutePath(report_output_dir),
  title = "Biocrates QC - p400 - FIA",
  author = "Mathias Kuhring",
  kit = "Biocrates AbsoluteIDQ p400 HR Kit",
  measurement_type = "FIA",
  pool_indicator = "Sample.Identification",
  sample_filter = NULL,
  profiling_variables = profiling_variables,
  study_variables = study_variables,
  replicate_variables = replicate_variables,
  zero2na = TRUE,
  preproc_keep_status = "Valid",
  filter_compound_qc_max_mv_ratio = 0.2,
  filter_compound_qc_max_rsd = 15,
  filter_compound_bs_max_mv_ratio = 0.25,
  filter_sample_max_mv_ratio = 0.2
)
# Now execute child notebooks: nbc_setup > nbc_import > ...


### Quant 500 test #################################################################################

# Output folder with date and time stamp
stamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
report_output_dir <- paste0("biocrates_q500_test_01_", stamp)

# Data files
batches_lc_normqc2 <- list(Batch1 = c("inst/extdata/biocrates_q500_test_01/Batch1_LC.txt"))
batches_fia_normqc2 <- list(Batch1 = c("inst/extdata/biocrates_q500_test_01/Batch1_FIA.txt"))

# Study and replicate variables
profiling_variables <- c('Sex')
study_variables <- list('Sex')
replicate_variables <- c('Sex')

# Parameter LC
params = list(
  data_files = lapply(batches_lc_normqc2, function(x){unname(R.utils::getAbsolutePath(x))}),
  export_name = "biocrates_qc_lc_normqc2",
  export_dir = R.utils::getAbsolutePath(report_output_dir),
  title = "Biocrates QC - Q500 - LC",
  author = "Mathias Kuhring",
  kit = "Biocrates MxP Quant 500 Kit",
  measurement_type = "LC",
  pool_indicator = "Sex",
  sample_filter = NULL,
  profiling_variables = profiling_variables,
  study_variables = study_variables,
  replicate_variables = replicate_variables,
  zero2na = TRUE,
  preproc_keep_status = "Valid",
  filter_compound_qc_max_mv_ratio = 0.2,
  filter_compound_qc_max_rsd = 15,
  filter_compound_bs_max_mv_ratio = 0.25,
  filter_sample_max_mv_ratio = 0.2
)
# Now execute child notebooks: nbc_setup > nbc_import > ...

# Parameter FIA
params = list(
  data_files = lapply(batches_fia_normqc2, function(x){unname(R.utils::getAbsolutePath(x))}),
  export_name = "biocrates_qc_fia_normqc2",
  export_dir = R.utils::getAbsolutePath(report_output_dir),
  title = "Biocrates QC - Q500 - FIA",
  author = "Mathias Kuhring",
  kit = "Biocrates MxP Quant 500 Kit",
  measurement_type = "FIA",
  pool_indicator = "Sex",
  sample_filter = NULL,
  profiling_variables = profiling_variables,
  study_variables = study_variables,
  replicate_variables = replicate_variables,
  zero2na = TRUE,
  preproc_keep_status = "Valid",
  filter_compound_qc_max_mv_ratio = 0.2,
  filter_compound_qc_max_rsd = 15,
  filter_compound_bs_max_mv_ratio = 0.25,
  filter_sample_max_mv_ratio = 0.2
)
# Now execute child notebooks: nbc_setup > nbc_import > ...
