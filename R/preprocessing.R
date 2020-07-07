# Preprocessing
# Author: Mathias Kuhring


# Discarding unreliable measurements
# Based on Biocratis measurement status
#
# Normal MetIDQ data export seems to contain
# for LC:
# * 0 for measurements below LOD (MetIDQ_Status "< LOD")
# * NA for invalid calibrated measurements (MetIDQ_Status "No Intercept")
# * Invalid for measurements exluded by user in MetIDQ or Xcalibur (MetIDQ_Status "Invalid")
# for FIA additionally:
# * Empty fields for measurements below LOD (MetIDQ_Status "< LOD")
# (difference between 0 and empty fields not known, yet)
#
# 0, NA as well as empty fields are concidered missing values and set to NA!
#
# In addition, only "Valid" measurements will be kept, i.e. all other statuses
# including "< LLOQ" and "> ULOQ" (out of quantification range / extrapolated
# calibration), "STD/QC < Limit" and "STD/QC > Limit" (insufficient accuracy in
# STDs and QCs), "ISTD Out of Range" (internal standard too high or low) and
# other statuses are considered as unreliable and are set to NA!
#
# Currently, there is no differentiation between stati and thus missing values.
discard_unreliable_measurements <- function(data,
                                            keep = c("Valid"),
                                            sample_types = NULL){

  # Identify and discard (i.e. set NA) every measurement but the onces to keep
  if (is.null(sample_types)){
    data <- data %>%
      mutate(Discard = !(MetIDQ_Status %in% keep)) %>%
      mutate_at(.vars = vars(matches(TABLE_TYPES_REGEX_ALL)),
                .funs = funs(ifelse(Discard, NA, .)))
  } else {
    data <- data %>%
      mutate(Discard =
               Sample.Type %in% sample_types &
               !(MetIDQ_Status %in% keep)) %>%
      mutate_at(.vars = vars(matches(TABLE_TYPES_REGEX_ALL)),
                .funs = funs(ifelse(Discard, NA, .)))
  }
  data <- data %>%
    select(-Discard)

  return(data)
}

# Summary stats on missing values per compound
summarize_missing_values_per_compound <- function(
  data, target = ENV$CONCENTRATION, plot = FALSE){

  # Count missing values
  mv_compounds <- data %>%
    group_by(Compound, Class) %>%
    summarize(`# Missing Values` = sum(is.na(UQ(sym(target)))),
              `% Missing Values` = sum(is.na(UQ(sym(target)))) / n() * 100,
              `# Total` = n()) %>%
    arrange(desc(`% Missing Values`)) %>%
    ungroup()

  # Factorize compounds ordered by compound class and name
  compound_levels <- mv_compounds %>%
    arrange(Class, Compound) %>%
    select(Compound) %>%
    distinct() %>%
    pull(Compound)
  mv_compounds$Compound <- factor(mv_compounds$Compound,
                                  levels = rev(compound_levels))

  # Plot number of missing values
  if (plot){
    g <- ggplot(data = mv_compounds,
                mapping = aes(x = Compound,
                              y = `# Missing Values`,
                              color = Class,
                              fill = Class)) +
      geom_bar(stat = "identity", alpha = 0.75) +
      coord_flip() +
      theme(legend.position = "bottom")
    print(g)
  }

  # Return table of missing values
  return(mv_compounds)
}


# Summary stats on missing values per sample
summarize_missing_values_per_sample <- function(
  data, target = ENV$CONCENTRATION, plot = FALSE){

  # Check for available sample location variables
  pos_cols <- c(
    COLUMN_WELL_POSITION,
    COLUMN_SEQUENCE_POSITION,
    COLUMN_WELL_COORDINATES
  )
  best_pos_col <- which(pos_cols %in% names(data))[1]

  # Count missing values
  mv_samples <- data %>%
    group_by(Sample.Name, Sample.Type)
  if (!is.na(best_pos_col)) {
    mv_samples <- mv_samples %>%
      group_by_at(vars(one_of(pos_cols[best_pos_col])), .add = TRUE)
  }
  mv_samples <- mv_samples %>%
    summarize(`# Missing Values` = sum(is.na(UQ(sym(target)))),
              `% Missing Values` = sum(is.na(UQ(sym(target)))) / n() * 100,
              `# Total` = n()) %>%
    arrange(desc(`% Missing Values`)) %>%
    ungroup()

  # Factorize samples ordered by sample type and name
  sample_levels <- mv_samples %>%
    arrange(Sample.Type, Sample.Name) %>%
    select(Sample.Name) %>%
    distinct() %>%
    pull(Sample.Name)
  mv_samples$Sample.Name <- factor(mv_samples$Sample.Name,
                                   levels = rev(sample_levels))

  # Plot number of missing values
  if (plot){
    g <- ggplot(data = mv_samples,
                mapping = aes(x = Sample.Name,
                              y = `# Missing Values`,
                              color = Sample.Type,
                              fill = Sample.Type)) +
      geom_bar(stat = "identity", alpha = 0.75) +
      coord_flip() +
      theme(legend.position = "bottom")
    print(g)
  }

  # Return table of missing values
  return(mv_samples)
}


# Run all filter preprocessing steps
execute_preprocessing <- function(data, ppparams){
  datasets <- list()
  datasets$original <- data

  # Special case: correct cal limits in Q500 urine data
  datasets$original_urine <- datasets$original
  if (ppparams$statuses_q500_urine) {
    if (grepl(pattern = "µM|uM", x = ENV$CONCENTRATION)){
      # Correct
      datasets$original_urine <- datasets$original %>%
        left_join(Q500_URINE_LIMITS, by = "Compound") %>%
        mutate(MetIDQ_Status = if_else(
          condition = MetIDQ_Status %in% c("< LLOQ", "> ULOQ") &
            !!sym(ENV$CONCENTRATION) >= LLOQ_µM &
            !!sym(ENV$CONCENTRATION) <=  ULOQ_µM,
          true = "Valid",
          false = MetIDQ_Status
        )) %>%
        select(-LLOQ_µM, -ULOQ_µM)
    } else {
      message(paste(
        "Error: Can't replace Q500 urine cal limits due to non-matching unit",
        "(needs µM or uM):", ENV$CONCENTRATION
      ))
    }
  }

  # 1. Discard unreliable measurements
  datasets$discarded <- discard_unreliable_measurements(
    data = datasets$original_urine, keep = ppparams$statuses_to_keep)

  # 2. Remove unreliable compounds
  #   a. Based on missing values ratio in QCs (samples of type ENV$SAMPLE_TYPE_REFERENCE_QC)
  filter_res <- remove_compounds_na(
    data = datasets$discarded,
    target = ENV$CONCENTRATION,
    sample_types = c(ENV$SAMPLE_TYPE_REFERENCE_QC),
    max_ratio = ppparams$threshold_2a)
  datasets$filter_compounds_by_qc_mv_kept <- filter_res$data
  datasets$filter_compounds_by_qc_mv_removed <- filter_res$removed

  #   b. Based on %RSD of QCs (default >15%)
  filter_res <- remove_compounds_rsd(
    data = datasets$filter_compounds_by_qc_mv_kept,
    target = ENV$CONCENTRATION,
    sample_types = c(ENV$SAMPLE_TYPE_REFERENCE_QC),
    max_rsd = ppparams$threshold_2b)
  datasets$filter_compounds_by_qc_rsd_kept <- filter_res$data
  datasets$filter_compounds_by_qc_rsd_removed <- filter_res$removed

  #   c. Based on missing values ratio in Pooled QCs (samples of type SAMPLE_TYPE_POOLED_QC)
  filter_res <- remove_compounds_na(
    data = datasets$filter_compounds_by_qc_rsd_kept,
    target = ENV$CONCENTRATION,
    sample_types = c(SAMPLE_TYPE_POOLED_QC),
    max_ratio = ppparams$threshold_2c)
  datasets$filter_compounds_by_qc_pool_mv_kept <- filter_res$data
  datasets$filter_compounds_by_qc_pool_mv_removed <- filter_res$removed

  #   d. Based on %RSD of Pooled QCs (default >15%)
  filter_res <- remove_compounds_rsd(
    data = datasets$filter_compounds_by_qc_pool_mv_kept,
    target = ENV$CONCENTRATION,
    sample_types = c(SAMPLE_TYPE_POOLED_QC),
    max_rsd = ppparams$threshold_2d)
  datasets$filter_compounds_by_qc_pool_rsd_kept <- filter_res$data
  datasets$filter_compounds_by_qc_pool_rsd_removed <- filter_res$removed

  # 3. Remove underrepresented compounds
  #   a. Based on missing value ratio over all (!) study samples (>20%)
  filter_res <- remove_compounds_na(
    data = datasets$filter_compounds_by_qc_pool_rsd_kept,
    target = ENV$CONCENTRATION,
    sample_types = c(SAMPLE_TYPE_BIOLOGICAL),
    max_ratio = ppparams$threshold_3a)
  datasets$filter_compounds_by_sample_all_mv_kept <- filter_res$data
  datasets$filter_compounds_by_sample_all_mv_removed <- filter_res$removed

  #   b. Based on missing value ratio over all (!) study samples (>25%)
  filter_res <- remove_compounds_na(
    data = datasets$filter_compounds_by_sample_all_mv_kept,
    target = ENV$CONCENTRATION,
    sample_types = c(SAMPLE_TYPE_BIOLOGICAL),
    max_ratio = ppparams$threshold_3b)
  datasets$filter_compounds_by_sample_mv_kept <- filter_res$data
  datasets$filter_compounds_by_sample_mv_removed <- filter_res$removed

  #   c. Based on %RSD of biological samples (default <15%)
  filter_res <- remove_compounds_rsd_low(
    data = datasets$filter_compounds_by_sample_mv_kept,
    target = ENV$CONCENTRATION,
    sample_type = SAMPLE_TYPE_BIOLOGICAL,
    min_rsd = ppparams$threshold_3c)
  datasets$filter_compounds_by_sample_rsd_kept <- filter_res$data
  datasets$filter_compounds_by_sample_rsd_removed <- filter_res$removed

  # 4. Remove underrepresented samples
  #   a. Based on missing value ratio over compounds
  filter_res <- remove_samples_na(
    data = datasets$filter_compounds_by_sample_rsd_kept,
    target = ENV$CONCENTRATION,
    max_ratio = ppparams$threshold_4)
  datasets$filter_samples_by_compound_mv_kept <- filter_res$data
  datasets$filter_samples_by_compound_mv_removed <- filter_res$removed

  return(datasets)
}
