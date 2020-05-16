# Functions for missing value imputation
# Author: Mathias Kuhring


# Simple imputation of missing values via median of several samples.
#
# Only samples of indicated sample types will be imputed, with median calculated
# and applied for each type separately.
impute_median <- function(data,
                                 target = ENV$CONCENTRATION,
                                 sample_types = c(SAMPLE_TYPE_BIOLOGICAL,
                                                  ENV$SAMPLE_TYPE_REFERENCE_QC,
                                                  SAMPLE_TYPE_POOLED_QC)){

  # Calculate medians for each sample type
  data_median <- data %>%
    group_by(Compound, Sample.Type) %>%
    mutate(Median = median(UQ(sym(target)), na.rm = TRUE))

  # Flag values for imputation
  data_median <- data_median %>%
    mutate(Impute = is.na(UQ(sym(target))) & Sample.Type %in% sample_types)

  # Impute
  data_median[data_median$Impute, target] <-
    data_median$Median[data_median$Impute]

  # Clean up
  data_median <- data_median %>% ungroup()
  data_median$Median <- NULL
  data_median$Impute <- NULL

  return(data_median)
}


# Currently DEPRECATED, as LoD is currently not parsed.
# Imputation of missing values with the half of LOD.
# Only samples of indicated sample types will be imputed.
impute_half_lod <- function(data,
                            target = ENV$CONCENTRATION,
                            sample_types = c(SAMPLE_TYPE_BIOLOGICAL,
                                             ENV$SAMPLE_TYPE_REFERENCE_QC,
                                             SAMPLE_TYPE_POOLED_QC),
                            lod = ENV$LOD_HEADER){

  assert_that(lod %in% names(data))

  # Calculate half LOD
  data_imputed <- data %>%
    mutate(HalfLOD = UQ(sym(lod)) / 2)

  # Flag values for imputation
  data_imputed <- data_imputed %>%
    mutate(Impute = is.na(UQ(sym(target))) & Sample.Type %in% sample_types)

  # Impute
  data_imputed[data_imputed$Impute, target] <-
    data_imputed$HalfLOD[data_imputed$Impute]

  # Clean up
  data_imputed$HalfLOD <- NULL
  data_imputed$Impute <- NULL

  return(data_imputed)
}


# Imputation of missing values with the half of minimum of a compound.
#
# Only samples of indicated sample types will be imputed.
impute_half_min <- function(data,
                            target = ENV$CONCENTRATION,
                            sample_types = c(SAMPLE_TYPE_BIOLOGICAL,
                                             ENV$SAMPLE_TYPE_REFERENCE_QC,
                                             SAMPLE_TYPE_POOLED_QC)){
  # Calculate half minimum
  data_imputed <- data %>%
    mutate(HalfMin = min(UQ(sym(target))) / 2)

  # Flag values for imputation
  data_imputed <- data_imputed %>%
    mutate(Impute = is.na(UQ(sym(target))) & Sample.Type %in% sample_types)

  # Impute
  data_imputed[data_imputed$Impute, target] <-
    data_imputed$HalfMin[data_imputed$Impute]

  # Clean up
  data_imputed$HalfLOD <- NULL
  data_imputed$Impute <- NULL

  return(data_imputed)
}


# Imputation of missing values based on kNN imputation.
#
# Only samples of indicated sample types will be imputed, with kNN imputation
# applied for each type separately.
impute_knn <- function(data,
                       target = ENV$CONCENTRATION,
                       sample_types = c(SAMPLE_TYPE_BIOLOGICAL,
                                        ENV$SAMPLE_TYPE_REFERENCE_QC,
                                        SAMPLE_TYPE_POOLED_QC),
                       ...){

  assert_that(any(sample_types %in% data$Sample.Type))

  # Create compound wide table
  data_wide <- data %>%
    select(Sample.Name, Sample.Type, Compound, UQ(sym(target))) %>%
    spread(key = Compound, value = UQ(sym(target)))

  # Iterate sample types
  for(sample_type in sample_types){
    message(paste0("Impute missing values in samples of type ",
                   sample_type, "..."))
    if (sample_type  %in% data_wide$Sample.Type){
      # Index of samples of sample type
      type_idx <- sample_type == data_wide$Sample.Type

      # Data matrix with target values only
      data_matrix <- data_wide %>%
        filter(sample_type == Sample.Type) %>%
        select(-Sample.Name, -Sample.Type)

      # Impute with kNN
      data_imputed <- DMwR2::knnImputation(data_matrix, ...)

      # Overwrite data in wide table
      data_wide[type_idx, -c(1,2)] <- data_imputed
    } else {
      message(paste("Warning: Skipping missing sample type for imputation:",
                    sample_type))
    }
  }

  # Gather data again and sort to enable replacment
  data_long <- data_wide %>%
    gather(key = "Compound", value = UQ(sym(target)),
           -Sample.Name, -Sample.Type) %>%
    ungroup %>%
    arrange(as.character(Sample.Name), as.character(Compound))

  # Sort original data to enable replacment
  data <- data %>%
    ungroup %>%
    arrange(as.character(Sample.Name), as.character(Compound))

  # Replace original target values
  data[, target] <- data_long[, target]

  return(data)
}
