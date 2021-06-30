# Miscellaneous helper functions
# Author: Mathias Kuhring


# Modify p-values of specified columns in a data frame
table_mod_pvalue <- function(data,
                             pvalues_columns = NULL,
                             pvalues_action = "trim",
                             pvalues_digits = 5){
  # Modify p-values
  if (!is.null(pvalues_columns)){
    assert_that(all(pvalues_columns %in% names(data)))
    assert_that(pvalues_action %in% c("trim", "round", "format"))

    if (pvalues_action == "trim"){
      data <- data %>%
        mutate_at(pvalues_columns, trim_pvalues, digits=pvalues_digits)
    } else if (pvalues_action == "round") {
      data <- data %>%
        mutate_at(pvalues_columns, round_pvalues, digits=pvalues_digits)
    } else { # format
      data <- data %>%
        mutate_at(pvalues_columns, format_pvalue)
    }
  }

  return(data)
}


# Trim p-values down to the number of specified decimal digits
trim_pvalues <- function(pvalues, digits = 5){
  return(floor(pvalues * 10^digits) / 10^digits)
}


# Round p-values to the number of specified decimal digits
round_pvalues <- function(pvalues, digits = 5){
  return(round(pvalues, digits = digits))
}


# Format p-values to common publication decimal digits (returns as characters)
# > 0.01 -> two decimal digits
# >= 0.001 -> three decimal digits
# < 0.001 -> "<0.001"
format_pvalue <- function(pvalues){
  pvalues_big <- pvalues > 0.01
  pvalues_medium <- pvalues <= 0.01 & pvalues >= 0.001
  pvalues_small <- pvalues < 0.001

  assert_that(!any(pvalues_big & pvalues_medium & pvalues_small))
  assert_that(all(pvalues_big | pvalues_medium | pvalues_small))

  pvalues_out <- pvalues
  if(any(pvalues_big)){
    pvalues_out[pvalues_big] <- formatC(
      pvalues[pvalues_big], digits = 2, format = "f")
  }
  if(any(pvalues_medium)){
    pvalues_out[pvalues_medium] <-formatC(
      pvalues[pvalues_medium], digits = 3, format = "f")
  }
  if(any(pvalues_small)){
    pvalues_out[pvalues_small] <- "<0.001"
  }

  return(pvalues_out)
}


# Round specific columns in a data frame
table_mod_round <- function(data,
                            round_colums = NULL,
                            round_digits = 2){
  # Round columns
  if (!is.null(round_colums)){
    assert_that(all(round_colums %in% names(data)))
    data <- data %>%
      mutate_at(round_colums, round, digits=round_digits)
  }
  return(data)
}


# A DT datatable default
# Note: Call only directly from a notebook chunk, as DT datatables won't render
# if called within a function!
easy_datatable <- function(
  data,
  arrange_by = NULL,
  caption = NULL,
  round_colums = NULL,
  round_digits = 2,
  pvalues_columns = NULL,
  pvalues_action = "trim",
  pvalues_digits = 5,
  rownames = FALSE,
  class = "display compact nowrap",
  extensions = "Buttons",
  options = list(
    pageLength = 10,
    scrollX = TRUE,
    dom = "Bfrtip",
    buttons = c("csv")),
  # TODO: make stats default, so meas. has to be configured consciously
  show_type = c("measurements", "statistics")[1],
  show = ENV$TABLE_DISPLAY,
  export_csv = FALSE,
  export_path = ".",
  ...
){

  assertthat::assert_that(show_type %in% c("measurements", "statistics"))
  assertthat::assert_that(show %in% c("all", "stats", "none"))

  # Export data to file
  if (export_csv){
    readr::write_csv(x = data, path = export_path)
  }

  # Align caption left
  if (!is.null(caption)){
    caption = htmltools::tags$caption(style = 'text-align:left;', caption)
  }

  # Check if data should be shown
  show_data <- (show != "none") && !(show == "stats" && show_type == "measurements")

  # Control display of tables according to allowed types
  if (show_data){
    # Round columns
    data <- table_mod_round(
      data, round_colums = round_colums, round_digits = round_digits
    )

    # Modify p-values
    data <- table_mod_pvalue(
      data, pvalues_columns = pvalues_columns,
      pvalues_action = pvalues_action, pvalues_digits = pvalues_digits
    )

    # Sort table
    if (!is.null(arrange_by)) {
      data <- data %>%
        arrange(UQ(sym(arrange_by)))
    }

    # Create table
    table_out <- DT::datatable(data,
                               rownames = rownames,
                               caption = caption,
                               class = class,
                               extensions = extensions,
                               options = options,
                               ...)
  }
  else {
    # Create empty table
    table_out <- DT::datatable(
      data.frame("TABLE CONTENT DISABLED!"),
      rownames = FALSE,
      colnames = c(),
      caption = caption,
      options = list(pageLength = 1)
    )
  }

  return(table_out)
}


# Create a wide concentration table with compounds as rows vs samples as columns
# (plus some header rows with sample metadata)
wide_conc_table_compounds_x_samples <- function(
  data,
  value = ENV$CONCENTRATION,
  metadata = ENV$ALL_VARIABLES
) {
  names(value) <- NULL
  data_wide <- data %>%
  select(Compound,  Sample.Name, Sample.Identification, Sample.Type,
         any_of(c("Well.Position", "Sequence.Position")),
         any_of(metadata), all_of(value)) %>%
    tidyr::spread(key = Compound, value = value) %>%
    tibble::column_to_rownames("Sample.Name")  %>%
    t() %>%
    tibble::as.tibble(rownames = "Sample.Name")
  return(data_wide)
}


# Extract extension from a file path string and return it lowercase
get_file_extension <- function(filepath, to_lowercase = TRUE) {
  splits <- strsplit(x = filepath, split = ".", fixed = TRUE)[[1]]
  extension <- splits[length(splits)]
  if (to_lowercase) extension <- tolower(extension)
  return(extension)
}
