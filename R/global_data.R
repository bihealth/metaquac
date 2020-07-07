# Global variables/data
# Author: Mathias Kuhring


# Supported kits
KIT_BIOCRATES_BILE <- "Biocrates Bile Acids Kit"
KIT_BIOCRATES_P180 <- "Biocrates AbsoluteIDQ p180 Kit"
KIT_BIOCRATES_P400 <- "Biocrates AbsoluteIDQ p400 HR Kit"
KIT_BIOCRATES_Q500 <- "Biocrates MxP Quant 500 Kit"
KIT_BIOCRATES_STERO17 <- "Biocrates AbsoluteIDQ Stero17 Kit"
KIT_GENERIC_DATA <- "Generic Data"

KITS_BIOCRATES <- c(
  KIT_BIOCRATES_BILE,
  KIT_BIOCRATES_P180,
  KIT_BIOCRATES_P400,
  KIT_BIOCRATES_Q500,
  KIT_BIOCRATES_STERO17
)
KITS <- c(
  KITS_BIOCRATES,
  KIT_GENERIC_DATA
)


# Allowed MetIDQ status values
METIDQ_STATUSES <- c(
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
)


# Import list of known compounds (for Biocrates Kit p400 only)
import_known_compounds <- function(filename){
  compounds <- readr::read_csv(file = filename, col_types = "cccc")
  assert_that(all(names(compounds) == c("Compound", "Name", "Class", "Method")))
  assert_that(nrow(compounds) > 0)
  return(compounds)
}
KNOWN_COMPOUNDS <- import_known_compounds(filename = "inst/resources/P400_table.csv")


# Import list of Q500 urine calibration limits (for Biocrates Kit Q500 only)
import_q500_urine_limits <- function(){
  q500_urin_compounds <- readr::read_csv(
    file = "inst/resources/Q500_urine_limits.csv",
    col_types = "cnn",
    locale = readr::locale(encoding = "ISO-8859-1")
  )
  assert_that(all(names(q500_urin_compounds) == c("Compound", "LLOQ_µM", "ULOQ_µM")))
  assert_that(nrow(q500_urin_compounds) > 0)
  return(q500_urin_compounds)
}
Q500_URINE_LIMITS <- import_q500_urine_limits()


# Color setup for significance
SIGNIFICANT_ADJUSTED <- "ADJUSTED"
SIGNIFICANT_UNADJUSTED <- "UNADJUSTED"
NOT_SIGNIFICANT <- "NO"
SIGNIFICANT_LEVELS <- c(SIGNIFICANT_ADJUSTED, SIGNIFICANT_UNADJUSTED, NOT_SIGNIFICANT)
SIGNIFICANT_COLORS <- c("#2dc937", "#e7b416", "#cc3232")
names(SIGNIFICANT_COLORS) <- SIGNIFICANT_LEVELS
SCALE_COLOR_SIGNIFICANT <- scale_color_manual(
  breaks = SIGNIFICANT_LEVELS, values = SIGNIFICANT_COLORS, drop = FALSE)
SCALE_FILL_SIGNIFICANT <- scale_fill_manual(
  breaks = SIGNIFICANT_LEVELS, values = SIGNIFICANT_COLORS, drop = FALSE)

# Function to recreate default ggplot2 color palette
gg_color_hue <- function(n) {
  hues = seq(15, 375, length=n+1)
  hcl(h=hues, l=65, c=100)[1:n]
}

# Colors for limits of NA ratios to be considered acceptable
NA_RATIO_COLORS <- c("#2dc937", "#cc3232")

# Limits of %RSDs to be considered acceptable
RSD_LIMITS <- c(0, 15, 20)

# Traffic light colors
COLORS_TRAFFIC <- c("#2dc937", "#e7b416", "#cc3232")


# Column names and values simplifiers

# Column names
COLUMN_SAMPLE_NAME <- "Sample.Name"
COLUMN_SAMPLE_TYPE <- "Sample.Type"

# TODO: replace/use everywhere:
COLUMN_BATCH <- "Batch"
COLUMN_FEATURE <- "Compound"

COLUMN_WELL_COORDINATES <- "Well.Coordinates"
COLUMN_WELL_POSITION <- "Well.Position"
COLUMN_SEQUENCE_POSITION <- "Sequence.Position"

# Measurement data type variables
MANIFESTATION_VARIABLES <- c(
  "CONCENTRATION", "INTENSITY", "AREA", "ISTD_INTENSITY", "ISTD_AREA", "STATUS"
)

# Sample types
SAMPLE_TYPE_BIOLOGICAL <- "Sample"
SAMPLE_TYPE_POOLED_QC <- "Pooled QC"
SAMPLE_TYPE_BLANK <- "Blank"


# Create an environment for global variables which needs to be changed during report creation
ENV <- new.env(parent = emptyenv())

# Initialize modifiable global variables (do at begin of report creation!)
init_dynamic_global_variables <- function(){
  # Reset environment
  remove(list = ls(), envir = ENV)

  # Init measurement value types with Biocrates types as default
  assign("CONCENTRATION", "Concentration", ENV) # Unit added on import
  assign("INTENSITY", "Analyte Intensity [cps]", ENV)
  assign("AREA", "Analyte Peak Area [area]", ENV)
  assign("ISTD_INTENSITY", "Internal Std. Intensity [cps]", ENV)
  assign("ISTD_AREA", "Internal Std. Peak Area [area]", ENV)

  # Init samples types with Biocrates types as default
  assign("SAMPLE_TYPE_REFERENCE_QC", "QC Level 2", ENV)

  # Init position type to be used in plots
  assign("PLOT_SAMPLE_LABEL", NULL, ENV)

  assign("TABLE_DISPLAY", "all", ENV)
}
