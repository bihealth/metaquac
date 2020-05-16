# Integrity checks for imported data
# Author: Mathias Kuhring


# Check if typcial columns are available
check_required_columns <- function(header, required_columns, filename){
  unavailable_columns <- required_columns[!required_columns %in% header]
  if (length(unavailable_columns) > 0) {
    warning_text <- paste0(
      "Warning: Required columns missing in file (report progress likely to fail!):  \n  *",
      filename, "*  \n", "  Missing:\t", paste(unavailable_columns, collapse = ", "))
    message(warning_text)
  }
}


# Catch duplicate column names and rename them (except for compound names)
# (MetIDQ and study variables might collide, which would result in merge bugs later on)
handle_duplicate_headers <- function(header, index_first_compound, filename){
  header_dups <- duplicated(header)
  header_unique <- make.names(header[1:index_first_compound - 1], unique = TRUE)
  if (any(header_dups)) {
    warning_text <- paste0(
      "Warning: Found and renamed duplicated column names in file:  \n*", filename, "*  \n",
      "Before:\t", paste(header[header_dups], collapse = ",\t"), "  \n",
      "After:\t", paste(header_unique[header_dups], collapse = ",\t"))
    message(warning_text)
    cat("\n")
  }
  header[1:index_first_compound - 1] <- header_unique
  return(header)
}


# Check for samples with same identifier but other different variables
get_sample_duplicates <- function(
  data,
  identifier = "Sample.Identification"
){
  # Separate sample info/meta data
  suppressWarnings(
    data_samples <-
      data %>%
      select(-one_of(c("Compound", ENV$COMPOUND_INFO_HEADER, "MetIDQ_Status")),
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
