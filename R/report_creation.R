# Create QC reports
# Author: Mathias Kuhring


#' Create a MeTaQuaC QC report based on Biocrates data (as exported by MetIDQ).
#'
#' @param data_files
#' Data files to import. List MetIDQ data (txt), as an R list providing the
#' files per batch via named vectors. E.g.
#' list(Batch1 = c("Batch1_LC1.txt", "Batch1_LC2.txt"),
#' Batch2 = c("Batch2_LC1.txt","Batch2_LC2.txt")).
#' For generic data, use only one list element, but name the data types within
#' the vector (to be mapped via generic_data_types). E.g.
#' list(c("Concentration [ng/ml]" = "data_conc.tsv", "Area" = "data_area.tsv",
#' "Status" = "data_status.tsv")).
#' Listing batches via different files is currently not support for generic
#' data, but can be indicated via a "Batch" column within the data.
#' @param kit The Biocrates Kit used to create the data to import.
#' Currently supported are
#' "Biocrates Bile Acids Kit"
#' "Biocrates AbsoluteIDQ p180 Kit",
#' "Biocrates AbsoluteIDQ p400 HR Kit",
#' "Biocrates MxP Quant 500 Kit",
#' "Biocrates AbsoluteIDQ Stero17 Kit" and
#' "Generic Data"
#' (default = "Biocrates AbsoluteIDQ p400 HR Kit").
#' @param measurement_type The measurement type (i.e. injection type) of the
#' data to import, i.e. either "LC" or "FIA" (default = "LC").
#' Only relevant for Biocrates data.
#' @param generic_data_types
#' For generic data, indicate which file contains which data using the names
#' defined in data_files. This way MeTaQuaC knows which type of data is
#' available and allows for custom naming in the report (e.g. to include units).
#' E.g.
#' c(CONCENTRATION = "Concentration [ng/ml]", AREA = "Area", STATUS = "Status").
#' Currently supported data type mappings are
#' CONCENTRATION, AREA, INTENSITY, ISTD_AREA, ISTD_INTENSITY, STATUS
#' @param generic_index_first_compound
#' For generic data, indicate which columns contains the first compound after
#' the sample annotation columns (i.e. the beginning of the data matrix).
#' Note: Please double check this parameter, as with a wrong selection data
#' transformation and merging will fail.
#' @param title Custom title for report (default = "Biocrates QC Report").
#' @param author Name of the person responsible for creating the report (default = system user).
#' @param report_output_name Custom name of report file (default =
#' "YYYYMMDD_HHMMSS_qc_report_\{LC|FIA\}").
#' @param report_output_dir Custom path of an output directory (default = "reports").
#' @param pool_indicator
#' Indicate a column/variable name which should be scanned for pooled QC samples
#' (Biocrates only). (default = "Sample Identification", set NULL to disable).
#' All samples containing "pool" (case insensitive) anywhere in this variable's
#' values are transformed to Sample Type = "Pooled QC". For generic data, please
#' indicate pooled samples within the data using value "Pooled QC" in column
#' "Sample Type".
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
#' @param preproc_keep_status
#' Indicate which values are acceptable for processing with respect to
#' (Biocrates) statuses. The default includes only "Valid" and "Semi Quant."
#' measurements, the rest is discarded (i.e. transformed to missing values, set
#' to NA). For Biocrates, possible statuses to select from include "Valid",
#' "Semi Quant.", "Smaller Zero", "< LOD", "< LLOQ", "> ULOQ", "No Intercept",
#' "Missing Measurement", "ISTD Out of Range", "STD/QC < Limit",
#' "STD/QC > Limit", "Invalid", "Incomplete" and "Blank Out of Range".
#' For generic data, status values may differ depending on the software used.
#' @param filter_compound_qc_max_mv_ratio
#' Set maximum ratio of missing values allowed for compounds in QC samples
#' (Biocrates' QC Level 2, Reference QC or Pooled QC)
#' (default = 0.3, exclusive, disable with NULL).
#' @param filter_compound_qc_max_rsd
#' Set maximum \%RSD allowed for compounds in QC samples
#' (Biocrates' QC Level 2, Reference QC or Pooled QC)
#' (default = 15\%, exclusive, disable with NULL).
#' @param filter_compound_bs_max_mv_ratio
#' Set maximum ratio of missing values allowed for compounds in biological
#' samples (Sample) (default = 0.3, exclusive, disable with NULL).
#' @param filter_compound_bs_min_rsd
#' Set minimum \%RSD allowed for compounds in biological samples (Sample)
#' (default = 15\%, exclusive, disable with NULL).
#' @param filter_sample_max_mv_ratio
#' Set maximum ratio of missing values allowed per biological sample (Sample)
#' (default < 0.2, exclusive, disable with NULL).
#' @param data_tables Control data tables availability in reports. "all" (default) will show all
#' implemented data tables (with csv export buttons). "stats" will only show tables of summarized
#' data (such as countings, %RSDs, etc.), but not the actual measurements (neither original nor
#' pre-processed). "none" will show no data tables at all, i.e. the report is mainly limited to
#' visualizations.
#' @param metadata_import Indicate a text file (csv or tsv) with additional
#' metadata/annotations to import and merge (by column "Sample Identification").
#' @param metadata_import_overlap Specify the handling of overlaping columns.
#' "rename" (default) will extend duplicate column names in the metadata import
#' with ".D" to make them unique, where D is an increasing number according to
#' the occurance of the same name. Keep this in mind when indicating variables.
#' "replace" will replace original columns with new metadata columns.
#' "omit" will keep original columns and ignore new metadata columns.
#' @param metadata_name_mods_org Rename columns in the original data.
#' This is applied **before** the import the of additional metadata, if any.
#' Consider that non-unique column names are modified in general. Names not in
#' the data are ignored. Use a named vector to indicate columns to rename, e.g.:
#' c(oldname1 = "newname1", oldname2 = "newname2", ...)
#' @param metadata_name_mods_add Rename columns in original and added metadata.
#' This is applied **after** the import of additional metadata, if any. Consider
#' that non-unique column names are modified in general. Names not in the data
#' are ignored. Use a named vector to indicate columns to rename, e.g.:
#' c(oldname1 = "newname1", oldname2 = "newname2", ...)
#' @param metadata_value_mods Batch change values in the data, e.g. to correct
#' sample identifiers, groups, etc. This is applied after metadata import and
#' renaming, if any. Indicated columns and values not in the data are ignored.
#' Use a named list, with names indicating the columns and named vectors
#' indicate the changes to apply, e.g.:
#' list("columnX" = c("oldvalueA" = "newvalueA", "oldvalueB" = "newvalueB"),
#       "columnY" = c("1" = 5, "3" = 6, ...), ...)
#' @param lowcon_conditions Indicate a vector of study variables of interest
#' which will be applied to additional reproducibility analysis designed for low
#' concentration data below the limit of quantification. Hence, this analysis
#' is performed on completely unfiltered data using area (LC) or intensity
#' (FIA), resp.
#' @param lowcon_sd_outlier_removal If set to TRUE, data of sample groups with
#' same conditions (i.e. combination of indicated study variables) is removed if
#' SD is higher than 1.5 or not available (e.g. when group consists of only one
#' sample). This is ment to reject unreliable technical replicates and not
#' recommended to apply on actual study samples and thus biological variance.
#' @param lowcon_scatter_x Indiciate one study variable to be used for the
#' x-axis in the scatter plot of the additional reproducibility analysis.
#' This variable must be available in the conditions. If none is given,
#' the first study variable in the conditions
#' @param lowcon_scatter_color Indiciate one study variable to be used for
#' coloring samples in the response scatter plot of the additional
#' reproducibility analysis. This variable must be available in the conditions.
#' If none is given, the first study variable in the conditions
#' @param lowcon_scatter_sub_groups Indicate pairs of study variables and
#' corresponding groups in a named vector to be used for separate response
#' scatter plots (e.g. if experiments havn't been separated before). By default,
#' the scatter plot is not separated.
#' @param lowcon_export_path Indicate a path for exporting normalized
#' areas (LC) or intensities (FIA) and RSDs of additional low concentration
#' analysis (per scatter sub group, if indicated, else completely).
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
#' profiling_variables = c("Group", "Condition"),
#' study_variables = list("Group", "Condition", "Group" = list("Condition")),
#' replicate_variables = c("Group", "Condition")
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
#' profiling_variables = c("Group", "Condition"),
#' study_variables = list("Group", "Condition", "Group" = list("Condition")),
#' replicate_variables = c("Group", "Condition")
#' )
#'
#' @import dplyr
#' @import ggplot2
#' @importFrom assertthat assert_that
#' @importFrom rlang sym UQ
#' @importFrom tidyr drop_na gather spread
create_qc_report <- function(
  data_files,
  kit = c(
    "Biocrates Bile Acids Kit",
    "Biocrates AbsoluteIDQ p180 Kit",
    "Biocrates AbsoluteIDQ p400 HR Kit",
    "Biocrates AbsoluteIDQ Stero17 Kit",
    "Biocrates MxP Quant 500 Kit",
    "Generic Data"
  )[1],
  measurement_type = c("LC", "FIA")[1],
  generic_data_types = c( # TODO: first one has priority
    CONCENTRATION = "Concentration",
    AREA = "Area",
    INTENSITY = NULL,
    ISTD_AREA = NULL,
    ISTD_INTENSITY = NULL,
    STATUS = NULL
  ),
  generic_index_first_compound = NULL,
  title = ifelse(
    kit %in% KITS_BIOCRATES, "Biocrates QC Report", "Generic Data QC Report"
  ),
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
    "Semi Quant.", # Is this an official status?
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
  )[1:2],
  filter_compound_qc_max_mv_ratio = 0.3,
  filter_compound_qc_max_rsd = 15,
  filter_compound_bs_max_mv_ratio = 0.3,
  filter_compound_bs_min_rsd = 15,
  filter_sample_max_mv_ratio = 0.2,
  data_tables = c("all", "stats", "none")[1],
  metadata_import = NULL,
  metadata_import_overlap = c("rename", "replace", "omit")[1],
  metadata_name_mods_org = NULL,
  metadata_name_mods_add = NULL,
  metadata_value_mods = NULL,
  lowcon_conditions = NULL,
  lowcon_sd_outlier_removal = FALSE,
  lowcon_scatter_x = NULL,
  lowcon_scatter_color = NULL,
  lowcon_scatter_sub_groups = NULL,
  lowcon_export_path = NULL,
  ...
){

  # Creates full absolute paths
  data_files <- lapply(data_files, function(x){getAbsolutePathWithNames(x)})
  # data_output_dir <- unname(R.utils::getAbsolutePath(data_output_dir))
  if (!is.null(metadata_import)){
    metadata_import <- unname(R.utils::getAbsolutePath(metadata_import))
  }
  if (!is.null(lowcon_export_path)){
    lowcon_export_path <- unname(R.utils::getAbsolutePath(lowcon_export_path))
  }

  # Markdown rendering
  rmarkdown::render(
    input = system.file("rmd", "main.Rmd", package = "metaquac"),
    output_file = paste0(report_output_name, ".html"),
    output_dir = report_output_dir,
    params = list(
      data_files = data_files,
      kit = kit,
      measurement_type = measurement_type,
      generic_data_types = generic_data_types,
      generic_index_first_compound = generic_index_first_compound,
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
      filter_compound_bs_min_rsd = filter_compound_bs_min_rsd,
      filter_sample_max_mv_ratio = filter_sample_max_mv_ratio,
      data_tables = data_tables,
      metadata_import = metadata_import,
      metadata_import_overlap = metadata_import_overlap,
      metadata_name_mods_org = metadata_name_mods_org,
      metadata_name_mods_add = metadata_name_mods_add,
      metadata_value_mods = metadata_value_mods,
      lowcon_conditions = lowcon_conditions,
      lowcon_sd_outlier_removal = lowcon_sd_outlier_removal,
      lowcon_scatter_x = lowcon_scatter_x,
      lowcon_scatter_color = lowcon_scatter_color,
      lowcon_scatter_sub_groups = lowcon_scatter_sub_groups,
      lowcon_export_path = lowcon_export_path,
      ...
    ),
    clean = TRUE)
}


# Get absolute path, but keep original name instead of original path as name
getAbsolutePathWithNames <- function(path) {
  new_path <- R.utils::getAbsolutePath(path)
  names(new_path) <- names(path)
  return(new_path)
}
