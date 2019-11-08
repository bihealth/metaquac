# Create QC reports
# Author: Mathias Kuhring


#' Create a MeTaQuaC QC report based on Biocrates data (as exported by MetIDQ).
#'
#' @param data_files Data files as exported by MetIDQ (txt), indicated as an R list providing the
#' files per batch via named vectors. E.g. list(Batch1 = c("Batch1_LC1.txt", "Batch1_LC2.txt"),
#' Batch2 = c("Batch2_LC1.txt","Batch2_LC2.txt")).
#' @param kit The Biocrates Kit used to create the data to import. Currently supported are
#' "Biocrates AbsoluteIDQ p400 HR Kit" and "Biocrates MxP Quant 500 Kit" (default =
#' "Biocrates AbsoluteIDQ p400 HR Kit").
#' @param measurement_type The measurement type (i.e. injection type) of the data to import, i.e.
#' either "LC" or "FIA" (default = "LC").
#' @param title Custom title for report (default = "Biocrates QC Report").
#' @param author Name of the person responsible for creating the report (default = system user).
#' @param report_output_name Custom name of report file (default =
#' "YYYYMMDD_HHMMSS_qc_report_\{LC|FIA\}").
#' @param report_output_dir Custom path of an output directory (default = "reports").
#' @param pool_indicator Indicate a column/variable name which should be scanned for pooled QC
#' samples. (default = "Sample Identification", set NULL to disable). All samples containing "pool"
#' (case insensitive) anywhere in this variable's values are transformed to Sample Type = "Pooled
#' QC".
#' @param profiling_variables Indicate a vector of study variables of interest which will be
#' used for profiling group sizes (i.e. number of samples) within a variable but also the size of
#' group intersections between these variables. It is recommended to keep the variables limited to
#' factors of primary interest (for instance disease status or treatment and sex), otherwise
#' intersections might end up rather small.
#' @param study_variables Indicate a list (actual R list!) of study variables of interest.
#' These will be used to create group-colored versions of some plots to illustrate group
#' differences. Nested variables are possible by including named sublists, whereby plots
#' will be created recursively based on data filtered by groups in the list-naming variable.
#' @param replicate_variables Indicate a vector of study variables which donate the unique
#' grouping of samples (could be as simple as a patient identifier or several conditions)
#' Samples with the same characteristic in these variables are considered as replicates
#' for compound and class \%RSD analysis plots. If the data contains "BR" and "TR" columns
#' these plots will be extended for technical replicates.
#' @param preproc_keep_status Indicate which values are acceptable for processing with
#' respect to Biocrates statuses. The default includes only "Valid" measurements, the rest
#' is discarded (i.e. transformed to missing values, set to NA). Possible statuses to select from
#' include "Valid", "Smaller Zero", "< LOD", "< LLOQ", "> ULOQ", "No Intercept",
#' "Missing Measurement", "ISTD Out of Range", "STD/QC < Limit", "STD/QC > Limit",
#' "Invalid", "Incomplete" and "Blank Out of Range".
#' @param filter_compound_qc_max_mv_ratio Set maximum ratio of missing values allowed for compounds
#' in reference QC samples (Biocrates' QC Level 2) (default = 0.3, exclusive, disable with NULL).
#' @param filter_compound_qc_max_rsd Set maximum \%RSD allowed for compounds in reference QC
#' samples (Biocrates' QC Level 2) (default = 15\%, exclusive, disable with NULL).
#' @param filter_compound_bs_max_mv_ratio Set maximum ratio of missing values allowed for compounds
#' in biological samples (Biocrates' Sample) (default = 0.3, exclusive, disable with NULL).
#' @param filter_sample_max_mv_ratio Set maximum ratio of missing values allowed per biological
#' sample (Biocrates' Sample) (default < 0.2, exclusive, disable with NULL).
#' @param ... Masked parameters for development and testing only.
#'
#' @export
#'
#' @examples
#' # Biocrates MxP Quant 500 Kit - LC injection
#' metaquac::create_qc_report(
#' data_files = list(
#'   Batch1 = c(
#'     system.file("extdata", "biocrates_q500_test_01/Batch1_LC.txt", package = "metaquac"))),
#' kit = "Biocrates MxP Quant 500 Kit",
#' measurement_type = "LC",
#' title = "Biocrates QC - Q500 - LC",
#' report_output_name = "biocrates_qc_q500_lc",
#' report_output_dir = "biocrates_q500_test",
#' pool_indicator = "Sex",
#' profiling_variables = c('Sex'),
#' study_variables = list('Sex'),
#' replicate_variables = c('Sex')
#' )
#'
#' # Biocrates MxP Quant 500 Kit - FIA injection
#' metaquac::create_qc_report(
#' data_files = list(
#'   Batch1 = c(
#'     system.file("extdata", "biocrates_q500_test_01/Batch1_FIA.txt", package = "metaquac"))),
#' kit = "Biocrates MxP Quant 500 Kit",
#' measurement_type = "FIA",
#' title = "Biocrates QC - Q500 - FIA",
#' report_output_name = "biocrates_qc_q500_fia",
#' report_output_dir = "biocrates_q500_test",
#' pool_indicator = "Sex",
#' profiling_variables = c('Sex'),
#' study_variables = list('Sex'),
#' replicate_variables = c('Sex')
#' )
#'
#' # Biocrates AbsoluteIDQ p400 HR Kit - LC injection
#' metaquac::create_qc_report(
#' data_files = list(
#'   Batch1 = c(
#'     system.file("extdata", "biocrates_p400_test_01/Batch1_LC1.txt", package = "metaquac"),
#'     system.file("extdata", "biocrates_p400_test_01/Batch1_LC2.txt", package = "metaquac")),
#'   Batch2 = c(
#'     system.file("extdata", "biocrates_p400_test_01/Batch2_LC1.txt", package = "metaquac"),
#'     system.file("extdata", "biocrates_p400_test_01/Batch2_LC2.txt", package = "metaquac")),
#'   Batch3 = c(
#'     system.file("extdata", "biocrates_p400_test_01/Batch3_LC1.txt", package = "metaquac"),
#'     system.file("extdata", "biocrates_p400_test_01/Batch3_LC2.txt", package = "metaquac")),
#'   Batch4 = c(
#'     system.file("extdata", "biocrates_p400_test_01/Batch4_LC1.txt", package = "metaquac"),
#'     system.file("extdata", "biocrates_p400_test_01/Batch4_LC2.txt", package = "metaquac"))),
#' kit = "Biocrates AbsoluteIDQ p400 HR Kit",
#' measurement_type = "LC",
#' title = "Biocrates QC - p400 - LC",
#' report_output_name = "biocrates_qc_p400_lc",
#' report_output_dir = "biocrates_p400_test",
#' pool_indicator = "Sample.Identification",
#' profiling_variables = c("Group", "Sex"),
#' study_variables = list("Group", "Sex", "Group" = list("Sex")),
#' replicate_variables = c("Sex")
#' )
#'
#' # Biocrates AbsoluteIDQ p400 HR Kit - FIA injection
#' metaquac::create_qc_report(
#' data_files = list(
#'   Batch1 = c(
#'     system.file("extdata", "biocrates_p400_test_01/Batch1_FIA1.txt", package = "metaquac"),
#'     system.file("extdata", "biocrates_p400_test_01/Batch1_FIA2.txt", package = "metaquac")),
#'   Batch2 = c(
#'     system.file("extdata", "biocrates_p400_test_01/Batch2_FIA1.txt", package = "metaquac"),
#'     system.file("extdata", "biocrates_p400_test_01/Batch2_FIA2.txt", package = "metaquac")),
#'   Batch3 = c(
#'     system.file("extdata", "biocrates_p400_test_01/Batch3_FIA1.txt", package = "metaquac"),
#'     system.file("extdata", "biocrates_p400_test_01/Batch3_FIA2.txt", package = "metaquac")),
#'   Batch4 = c(
#'     system.file("extdata", "biocrates_p400_test_01/Batch4_FIA1.txt", package = "metaquac"),
#'     system.file("extdata", "biocrates_p400_test_01/Batch4_FIA2.txt", package = "metaquac"))),
#' kit = "Biocrates AbsoluteIDQ p400 HR Kit",
#' measurement_type = "FIA",
#' title = "Biocrates QC - p400 - FIA",
#' report_output_name = "biocrates_qc_p400_fia",
#' report_output_dir = "biocrates_p400_test",
#' pool_indicator = "Sample.Identification",
#' profiling_variables = c("Group", "Sex"),
#' study_variables = list("Group", "Sex", "Group" = list("Sex")),
#' replicate_variables = c("Sex")
#' )
#'
#' @import dplyr
#' @import ggplot2
#' @importFrom assertthat assert_that
#' @importFrom rlang sym UQ
#' @importFrom tidyr drop_na gather spread
create_qc_report <- function(
  data_files,
  kit = c("Biocrates AbsoluteIDQ p400 HR Kit",
          "Biocrates MxP Quant 500 Kit")[1],
  measurement_type = c("LC", "FIA")[1],
  title = "Biocrates QC Report",
  author = unname(Sys.info()["user"]),
  report_output_name = paste0(format(Sys.time(), "%Y%m%d_%H%M%S"),
                              "_qc_report_",
                              measurement_type),
  report_output_dir = "reports",
  pool_indicator = "Sample.Identification",
  # data_output_name = report_output_name,
  # data_output_dir = report_output_dir,
  # sample_filter = NULL,
  profiling_variables = NULL,
  study_variables = NULL,
  replicate_variables = NULL,
  preproc_keep_status = c(
    "Valid",
    "Smaller Zero",
    "< LOD",
    "< LLOQ",
    "> ULOQ",
    "No Intercept",
    "Missing Measurement",
    "ISTD Out of Range",
    "STD/QC < Limit",
    "STD/QC > Limit",
    "Invalid",
    "Incomplete",
    "Blank Out of Range"
  )[1],
  filter_compound_qc_max_mv_ratio = 0.3,
  filter_compound_qc_max_rsd = 15,
  filter_compound_bs_max_mv_ratio = 0.3,
  filter_sample_max_mv_ratio = 0.2,
  ...
){

  # Creates full absolute paths
  data_files <- lapply(data_files, function(x){unname(R.utils::getAbsolutePath(x))})
  # data_output_dir <- unname(R.utils::getAbsolutePath(data_output_dir))

  # Markdown rendering
  rmarkdown::render(
    input = system.file("rmd", "main.Rmd", package = "metaquac"),
    output_file = paste0(report_output_name, ".html"),
    output_dir = report_output_dir,
    params = list(
      data_files = data_files,
      kit = kit,
      measurement_type = measurement_type,
      title = title,
      author = author,
      pool_indicator = pool_indicator,
      # export_name = data_output_name,
      # export_dir = data_output_dir,
      # sample_filter = sample_filter,
      profiling_variables = profiling_variables,
      study_variables = study_variables,
      replicate_variables = replicate_variables,
      preproc_keep_status = preproc_keep_status,
      filter_compound_qc_max_mv_ratio = filter_compound_qc_max_mv_ratio,
      filter_compound_qc_max_rsd = filter_compound_qc_max_rsd,
      filter_compound_bs_max_mv_ratio = filter_compound_bs_max_mv_ratio,
      filter_sample_max_mv_ratio = filter_sample_max_mv_ratio,
      ...
    ),
    clean = TRUE)
}
