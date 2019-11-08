# Recursive calculations for nested study variables/design
# Author: Mathias Kuhring


# Recursive execution of a function based on nested study variables
# @param vars Nested list of nested variables
# @param end_fun Function used on (sub)data and each final variable
# @return A list with the returned data/object of each final function call
recursive_execution <- function(vars, end_fun, data = NULL, parent = NULL,
                                info = NULL, keep_sample_types = NULL,
                                separate_parent = TRUE, name = NULL){

  # End condition: single variable name
  if (length(vars) == 1 && class(vars) == "character"){
    return(list(end_fun(vars, data, info, parent)))
    # Otherwise it is a list
  } else if (class(vars) == "list"){
    results <- list()
    # Case for sublist,
    # i.e. data needs to be separated by groups in current variable
    if (!is.null(name)){
      # Get variable groups
      groups <- unique(data %>% filter(Sample.Type == SAMPLE_TYPE_BIOLOGICAL) %>% pull(name))

      # Iterate variables...
      for (i in seq_along(vars)){

        # Check whether groups should be separated for final single variables
        if (separate_parent || class(vars[[i]]) == "list"){
          # ... per groups separately
          for (group in groups){
            # Filter group specific samples
            if (is.null(keep_sample_types)){
              data_sub = data %>% filter(UQ(sym(name)) == group)
            } else {
              data_sub = data %>% filter(UQ(sym(name)) == group |
                                           Sample.Type %in% keep_sample_types)
            }
            # Apply recursion on variables
            results <- c(
              results,
              recursive_execution(vars[[i]], end_fun, data = data_sub,
                                  parent = name,
                                  name = names(vars)[i],
                                  info = paste0(info, name, " = ", group, " -> "),
                                  keep_sample_types = keep_sample_types,
                                  separate_parent = separate_parent)
            )
          }
        } else {
          # Apply recursion on variables
          results <- c(
            results,
            recursive_execution(vars[[i]], end_fun, data = data,
                                parent = name,
                                name = names(vars)[i],
                                info = paste0(info, name, " -> "),
                                keep_sample_types = keep_sample_types,
                                separate_parent = separate_parent)
          )
        }
      }
    } else { # Case for the initial list
      for (i in seq_along(vars)){
        results <- c(
          results,
          recursive_execution(vars[[i]], end_fun, data = data,
                              parent = NULL,
                              name = names(vars)[i],
                              keep_sample_types = keep_sample_types,
                              separate_parent = separate_parent)
        )
      }
    }
    return(results)
  } else {
    stop(paste(
      "Invalid data in study variables (use strings or named lists of strings):\t",
      paste(as.character(vars), collapse = ", ")))
  }
}
