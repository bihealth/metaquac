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


### p400 test ##################################################################

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
study_variables <- list("Group", "Condition",
                        "Group" = list("Condition"))
replicate_variables <- c("Group", "Condition")
profiling_variables <- c("Group", "Condition")

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
  filter_compound_qc_ref_max_mv_ratio = 0.3,
  filter_compound_qc_ref_max_rsd = 15,
  filter_compound_qc_pool_max_mv_ratio = 0.3,
  filter_compound_qc_pool_max_rsd = 15,
  filter_compound_bs_max_mv_ratio = 0.3,
  filter_compound_bs_min_rsd = 15,
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
  filter_compound_qc_ref_max_mv_ratio = 0.3,
  filter_compound_qc_ref_max_rsd = 15,
  filter_compound_qc_pool_max_mv_ratio = 0.3,
  filter_compound_qc_pool_max_rsd = 15,
  filter_compound_bs_max_mv_ratio = 0.3,
  filter_compound_bs_min_rsd = 15,
  filter_sample_max_mv_ratio = 0.2
)
# Now execute child notebooks: nbc_setup > nbc_import > ...


### Quant 500 test #############################################################

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
  filter_compound_qc_ref_max_mv_ratio = 0.3,
  filter_compound_qc_ref_max_rsd = 15,
  filter_compound_qc_pool_max_mv_ratio = 0.3,
  filter_compound_qc_pool_max_rsd = 15,
  filter_compound_bs_max_mv_ratio = 0.3,
  filter_compound_bs_min_rsd = 15,
  filter_sample_max_mv_ratio = 0.2,
  metadata_import = R.utils::getAbsolutePath(
    "inst/extdata/biocrates_q500_test_01/extra_annotation.txt"
  ),
  metadata_import_overlap = "rename"
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
  filter_compound_qc_ref_max_mv_ratio = 0.3,
  filter_compound_qc_ref_max_rsd = 15,
  filter_compound_qc_pool_max_mv_ratio = 0.3,
  filter_compound_qc_pool_max_rsd = 15,
  filter_compound_bs_max_mv_ratio = 0.3,
  filter_compound_bs_min_rsd = 15,
  filter_sample_max_mv_ratio = 0.2
)
# Now execute child notebooks: nbc_setup > nbc_import > ...


### Quant 500 test with lowcon section #########################################

# Parameter LC
params = list(
  data_files = lapply(
    list(Batch1 = c("inst/extdata/biocrates_q500_test_01/Batch1_LC.txt")),
    function(x){unname(R.utils::getAbsolutePath(x))}
  ),
  export_name = "biocrates_qc_lc_normqc2",
  export_dir = R.utils::getAbsolutePath(
    paste0("biocrates_q500_test_02_", format(Sys.time(), "%Y%m%d_%H%M%S"))
  ),
  title = "Biocrates QC - Q500 - LC",
  author = "Mathias Kuhring",
  kit = "Biocrates MxP Quant 500 Kit",
  measurement_type = "LC",
  pool_indicator = "Sex",
  sample_filter = NULL,
  profiling_variables =  c('Sex'),
  study_variables = list('Sex'),
  replicate_variables =  c('Sex'),
  zero2na = TRUE,
  preproc_keep_status = "Valid",
  filter_compound_qc_ref_max_mv_ratio = 0.3,
  filter_compound_qc_ref_max_rsd = 15,
  filter_compound_qc_pool_max_mv_ratio = 0.3,
  filter_compound_qc_pool_max_rsd = 15,
  filter_compound_bs_max_mv_ratio = 0.3,
  filter_compound_bs_min_rsd = 15,
  filter_sample_max_mv_ratio = 0.2,
  lowcon_conditions = c("Sex", "Sample.Volume"),
  lowcon_sd_outlier_removal = FALSE,
  lowcon_scatter_x = NULL,
  lowcon_scatter_color = NULL,
  lowcon_scatter_sub_groups = NULL
  # lowcon_export_path = "dev/lowcon/"
)
# Now execute child notebooks: nbc_setup > nbc_import > ...

# Parameter FIA
params = list(
  data_files = lapply(
    list(Batch1 = c("inst/extdata/biocrates_q500_test_01/Batch1_FIA.txt")),
    function(x){unname(R.utils::getAbsolutePath(x))}
  ),
  export_name = "biocrates_qc_fia_normqc2",
  export_dir = R.utils::getAbsolutePath(
    paste0("biocrates_q500_test_02_", format(Sys.time(), "%Y%m%d_%H%M%S"))
  ),
  title = "Biocrates QC - Q500 - FIA",
  author = "Mathias Kuhring",
  kit = "Biocrates MxP Quant 500 Kit",
  measurement_type = "FIA",
  pool_indicator = "Sex",
  sample_filter = NULL,
  profiling_variables =  c('Sex'),
  study_variables = list('Sex'),
  replicate_variables =  c('Sex'),
  zero2na = TRUE,
  preproc_keep_status = "Valid",
  filter_compound_qc_ref_max_mv_ratio = 0.3,
  filter_compound_qc_ref_max_rsd = 15,
  filter_compound_qc_pool_max_mv_ratio = 0.3,
  filter_compound_qc_pool_max_rsd = 15,
  filter_compound_bs_max_mv_ratio = 0.3,
  filter_compound_bs_min_rsd = 15,
  filter_sample_max_mv_ratio = 0.2,
  lowcon_conditions = c("Sex", "Sample.Volume"),
  lowcon_sd_outlier_removal = FALSE,
  lowcon_scatter_x = NULL,
  lowcon_scatter_color = NULL,
  lowcon_scatter_sub_groups = NULL
  # lowcon_export_path = "dev/lowcon/"
)
# Now execute child notebooks: nbc_setup > nbc_import > ...


### Generic data test ##########################################################

# Output folder with date and time stamp
stamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
report_output_dir <- paste0("generic_data_test_01_", stamp)

# Data files
batches_included <- list(
  AllBatches = c(
    "Concentration [ng/ml]" = "inst/extdata/generic_test_01/random_matrix_conc.tsv",
    "Area" = "inst/extdata/generic_test_01/random_matrix_area.tsv"
  )
)

batches_separately <- list(
  Batch1 = c(
    Concentration = "inst/extdata/generic_test_02/random_matrix_conc_batch1.tsv",
    Area = "inst/extdata/generic_test_02/random_matrix_area_batch1.tsv"
  ),
  Batch2 = c(
    Concentration = "inst/extdata/generic_test_02/random_matrix_conc_batch2.tsv",
    Area = "inst/extdata/generic_test_02/random_matrix_area_batch2.tsv"
  )
)

# Study and replicate variables
profiling_variables <- c('Group')
study_variables <- list('Group')
replicate_variables <- c('Group')

# Parameter Generic
params = list(
  data_files = lapply(batches_included, function(x){getAbsolutePathWithNames(x)}),
  kit = "Generic Data",
  generic_data_types = c(
    CONCENTRATION = "Concentration [ng/ml]",
    AREA = "Area"
  ),
  generic_index_first_compound = 6,
  measurement_type = "LC",
  sample_filter = NULL,
  profiling_variables = profiling_variables,
  study_variables = study_variables,
  replicate_variables = replicate_variables,
  zero2na = TRUE,
  preproc_keep_status = "Valid",
  filter_compound_qc_ref_max_mv_ratio = 0.3,
  filter_compound_qc_ref_max_rsd = 15,
  filter_compound_qc_pool_max_mv_ratio = 0.3,
  filter_compound_qc_pool_max_rsd = 15,
  filter_compound_bs_max_mv_ratio = 0.3,
  filter_compound_bs_min_rsd = 0,
  filter_sample_max_mv_ratio = 0.2
)
# Now execute child notebooks: nbc_setup > nbc_import > ...


### Generic data test 03 ##########################################################

# Data files
batches_included <- list(
  AllBatches = c(
    "Concentration [ng/ml]" = "inst/extdata/generic_test_03/Batch1_conc.csv",
    "Area" = "inst/extdata/generic_test_03/Batch1_area.csv",
    "Status" = "inst/extdata/generic_test_03/Batch1_status.csv"
  )
)

# Study and replicate variables
profiling_variables <- c('Sex')
study_variables <- list('Sex')
replicate_variables <- c('Sex')

# Parameter Generic
params = list(
  data_files = lapply(batches_included, function(x){getAbsolutePathWithNames(x)}),
  kit = "Generic Data",
  generic_data_types = c(
    CONCENTRATION = "Concentration [ng/ml]",
    AREA = "Area",
    STATUS = "Status"
  ),
  generic_index_first_compound = 5,
  measurement_type = "LC",
  sample_filter = NULL,
  profiling_variables = profiling_variables,
  study_variables = study_variables,
  replicate_variables = replicate_variables,
  zero2na = TRUE,
  preproc_keep_status = "Valid",
  filter_compound_qc_ref_max_mv_ratio = 0.3,
  filter_compound_qc_ref_max_rsd = 15,
  filter_compound_qc_pool_max_mv_ratio = 0.3,
  filter_compound_qc_pool_max_rsd = 15,
  filter_compound_bs_max_mv_ratio = 0.3,
  filter_compound_bs_min_rsd = 0,
  filter_sample_max_mv_ratio = 0.2
)
# Now execute child notebooks: nbc_setup > nbc_import > ...
