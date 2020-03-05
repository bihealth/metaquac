# Global variables/data
# Author: Mathias Kuhring


# Supported kits
BIOCRATES_P400 <- "Biocrates AbsoluteIDQ p400 HR Kit"
BIOCRATES_Q500 <- "Biocrates MxP Quant 500 Kit"
BIOCRATES_STERO17 <- "Biocrates AbsoluteIDQ Stero17 Kit"
KITS <- c(BIOCRATES_P400, BIOCRATES_Q500, BIOCRATES_STERO17)

# Allowed MetIDQ status values
METIDQ_STATUSES <- c(
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
)

# Import list of known compounds (Biocrates Kit p400 only)
import_known_compounds <- function(filename){
  compounds <- readr::read_csv(file = filename, col_types = "cccc")
  assert_that(all(names(compounds) == c("Compound", "Name", "Class", "Method")))
  assert_that(nrow(compounds) > 0)
  return(compounds)
}
KNOWN_COMPOUNDS <- import_known_compounds(filename = "inst/resources/P400_table.csv")

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

# Measurement types
# CONCENTRATION <- "Concentration" # moved to modifiable variables
INTENSITY <- "Analyte Intensity [cps]"
AREA <- "Analyte Peak Area [area]"
ITSD_INTENSITY <- "Internal Std. Intensity [cps]"
ITSD_AREA <- "Internal Std. Peak Area [area]"

# Sample types
SAMPLE_TYPE_BIOLOGICAL <- "Sample"
SAMPLE_TYPE_POOLED_QC <- "Pooled QC"
SAMPLE_TYPE_REFERENCE_QC <- "QC Level 2" # currently fixed for Biocrates


# Create an environment for global variables which needs to be changed during report creation
PKG_ENV <- new.env(parent = emptyenv())

# Initialize modifiable global variables (do at begin of report creation!)
init_dynamic_global_variables <- function(){
  # Reset environment
  remove(list = ls(), envir = PKG_ENV)

  assign("CONCENTRATION", "Concentration", PKG_ENV)
  assign("TABLE_DISPLAY", "all", PKG_ENV)
}
