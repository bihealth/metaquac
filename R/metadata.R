# Functions for optional metadata preprocessing
# Author: Mathias Kuhring


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
