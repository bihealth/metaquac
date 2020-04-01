# Functions for optional metadata preprocessing
# Author: Mathias Kuhring


# Import and merge additional matedata
import_metadata <- function(
  data,
  metadata_file,
  column_duplicates = c("rename", "replace", "omit")[1]
){
  # Check parameters
  assertthat::assert_that(file.exists(metadata_file))
  assertthat::assert_that(length(column_duplicates) == 1)
  assertthat::assert_that(column_duplicates %in% c("rename", "replace", "omit"))

  # Laod additional metadata/annotation
  encoding <- readr::guess_encoding(file = metadata_file)
  cat(paste0("Guessed encoding ", encoding[[1,1]], "."))
  if (get_file_extension(metadata_file) == "csv") {
    df_annotation <- suppressMessages(readr::read_csv(
      file = metadata_file,
      locale = readr::locale(encoding = encoding[[1,1]]),
      na = c("", "NA", "\xc2\xa0"), # remove non-breaking spaces
      trim_ws = TRUE
    ))
  } else if (get_file_extension(metadata_file) == "tsv") {
    df_annotation <- suppressMessages(readr::read_tsv(
      file = metadata_file,
      locale = readr::locale(encoding = encoding[[1,1]]),
      na = c("", "NA", "\xc2\xa0"), # remove non-breaking spaces
      trim_ws = TRUE
    ))
  } else {
    message(paste0("Error: Unsupported text file format. Use csv or tsv!"))
  }

  # Make sure that the sample identification column is available in the
  # annotation dataframe and that it is of charactere type
  assertthat::assert_that("Sample.Identification" %in% names(df_annotation))
  df_annotation <- df_annotation %>%
    mutate(Sample.Identification = as.character(Sample.Identification))

  # Handle duplicated columns
  data_cols <- names(data)
  meta_cols <- names(df_annotation %>% select(-Sample.Identification))
  col_dups <- intersect(data_cols, meta_cols)
  if (length(intersect) > 0){
    if (column_duplicates == "rename") {
      renamed <- make.unique(c(data_cols, meta_cols))
      names(df_annotation)[(2):(length(meta_cols)+1)] <-
        renamed[(length(data_cols)+1):(length(renamed))]
    } else if (column_duplicates == "replace") {
      data <- data %>% select(-one_of(col_dups))
    } else { # "omit"
      df_annotation <- df_annotation %>% select(-one_of(col_dups))
    }
  }

  # Merge original data with metadata
  data <- left_join(data, df_annotation, by="Sample.Identification")

  return(data)
}


# Rename columns in the data
# Use a named vector:
# c(oldname1 = "newname1", oldname2 = "newname2", ...)
modify_metadata_names <- function(
  data,
  mapping
){
  assertthat::assert_that(length(mapping) > 0)
  assertthat::assert_that(class(mapping) == "character")
  assertthat::assert_that(!is.null(names(mapping)))
  assertthat::assert_that(!any(is.na(names(mapping))))

  # Make sure new names are unique
  renamed <- make.unique(c(names(data), mapping))
  old_names <- names(mapping)
  new_names <- renamed[(length(names(data))+1):(length(renamed))]
  mapping <- new_names
  names(mapping) <- old_names

  # Rename columns
  idx <- which(names(data) %in% names(mapping))
  names(data)[idx] <- mapping[names(data)[idx]]

  return(data)
}


# Change values in the data
# Use a named list with named vectors per columns, e.g.:
# list("columnX" = c("oldvalueA" = "newvalueA", "oldvalueB" = "newvalueB"),
#      "columnY" = c("1" = 5, "3" = 6, ...), ...)
modify_metadata_values <- function(
  data,
  mapping
){
  assertthat::assert_that(length(mapping) > 0)
  assertthat::assert_that(is.list(mapping))
  assertthat::assert_that(!is.null(names(mapping)))
  assertthat::assert_that(!any(is.na(names(mapping))))
  for (value_mods in mapping){
    assertthat::assert_that(length(value_mods) > 0)
    assertthat::assert_that(!is.null(names(value_mods)))
    assertthat::assert_that(!any(is.na(names(value_mods))))
    assertthat::assert_that(
      class(value_mods) %in%
        c("character", "complex", "integer", "logical", "numeric")
    )
  }

  # Remove mappings of unavailable columns
  mapping <- mapping[names(mapping) %in% names(data)]

  # Iterate mappings per column
  cols <- names(mapping)
  for (col in cols){
    # Recode column values
    data <- data %>%
      mutate(!!sym(col) := dplyr::recode(!!sym(col), !!!mapping[[col]]))
  }

  return(data)
}


# Extract additional metadata from a character column (e.g. Sample.Identifier)
extract_metadata <- function(
  data,
  output_column,
  split_number,
  input_column = "Sample.Identification",
  split_string = "-",
  sample_type = SAMPLE_TYPE_BIOLOGICAL
){
  data %>%
    mutate(
      UQ(sym(output_column)) := if_else(
        Sample.Type == sample_type,
        stringr::str_split_fixed(
          string = UQ(sym(input_column)), pattern = split_string,
          n = split_number)[,split_number],
        NA_character_)) %>%
    return()
}
