# Integrity checks for imported data
# Author: Mathias Kuhring


# Check for samples with same identifier but other different variables
get_sample_duplicates <- function(
  data,
  identifier = "Sample.Identification"
){
  # Separate sample info/meta data
  suppressWarnings(
    data_samples <-
      data %>%
      select(-one_of(c("Compound", PKG_ENV$COMPOUND_INFO_HEADER, "MetIDQ_Status")),
             -matches(TABLE_TYPES_REGEX_ALL)) %>%
      distinct()
  )

  # Check that each sample exists only once. Otherwise, technical columns might
  # be still present, leaving differences due the two runs (eg. LC1/LC2).
  dups <- data_samples %>% select(Sample.Name) %>% duplicated()

  if (any(dups)){
    dups <- data_samples$Sample.Name[dups]
    dups <- data_samples %>%
      filter(Sample.Name %in% dups) %>%
      arrange(Sample.Name) %>%
      group_by(Sample.Name) %>%
      select_if(~ length(unique(.)) > 1)
    return(dups)
  } else {
    return(data.frame())
  }
}
