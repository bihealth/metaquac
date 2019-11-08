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
easy_datatable <- function(data,
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
                           ...){

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
  if (!is.null(arrange_by)){
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

  return(table_out)
}
